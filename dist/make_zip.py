# -*- coding: utf-8 -*-
"""Пересобрать dist/MARK.lsp.zip и SHA256 в dist/README.md.

Дата внутри zip детерминирована:
1. `SOURCE_DATE_EPOCH`, если задан;
2. дата последнего коммита, изменившего `MARK.lsp`;
3. 2026-11-08 — только если git недоступен.
"""
from pathlib import Path
import datetime
import hashlib
import os
import re
import subprocess
import zipfile

ROOT = Path(__file__).resolve().parents[1]
MARK = ROOT / "MARK.lsp"
ZIP = ROOT / "dist" / "MARK.lsp.zip"
README = ROOT / "dist" / "README.md"
FALLBACK = datetime.datetime(2026, 11, 8, tzinfo=datetime.timezone.utc)


def build_timestamp():
    env = os.environ.get("SOURCE_DATE_EPOCH", "").strip()
    if env.isdigit():
        return int(env)
    try:
        out = subprocess.run(
            ["git", "-C", str(ROOT), "log", "-1", "--format=%at", "--", "MARK.lsp"],
            capture_output=True, text=True, check=True,
        ).stdout.strip()
        if out.isdigit():
            return int(out)
    except (OSError, subprocess.SubprocessError):
        pass
    return int(FALLBACK.timestamp())


stamp = datetime.datetime.fromtimestamp(build_timestamp(), datetime.timezone.utc)
date_time = (stamp.year, stamp.month, stamp.day, 0, 0, 0)

raw = MARK.read_bytes()
info = zipfile.ZipInfo("MARK.lsp", date_time=date_time)
info.compress_type = zipfile.ZIP_DEFLATED
info.external_attr = 0o644 << 16

with zipfile.ZipFile(ZIP, "w") as z:
    z.writestr(info, raw)

sha = hashlib.sha256(ZIP.read_bytes()).hexdigest().upper()
text = README.read_text(encoding="utf-8")
text = re.sub(r"SHA256:\s*`[0-9a-fA-F]{64}`", f"SHA256: `{sha}`", text)
README.write_text(text, encoding="utf-8", newline="\n")
print(f"MARK.lsp: {len(raw)} байт; zip: {ZIP.stat().st_size} байт; "
      f"дата {stamp:%Y-%m-%d}; SHA256 {sha}")
