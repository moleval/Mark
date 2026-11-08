# -*- coding: utf-8 -*-
"""Пересобрать dist/MARK.lsp.zip и SHA256 в dist/README.md."""
from pathlib import Path
import hashlib
import re
import zipfile

ROOT = Path(__file__).resolve().parents[1]
MARK = ROOT / "MARK.lsp"
ZIP = ROOT / "dist" / "MARK.lsp.zip"
README = ROOT / "dist" / "README.md"

raw = MARK.read_bytes()
info = zipfile.ZipInfo("MARK.lsp", date_time=(2026, 11, 8, 0, 0, 0))
info.compress_type = zipfile.ZIP_DEFLATED
info.external_attr = 0o644 << 16

with zipfile.ZipFile(ZIP, "w") as z:
    z.writestr(info, raw)

sha = hashlib.sha256(ZIP.read_bytes()).hexdigest().upper()
text = README.read_text(encoding="utf-8")
text = re.sub(r"SHA256:\s*`[0-9a-fA-F]{64}`", f"SHA256: `{sha}`", text)
README.write_text(text, encoding="utf-8", newline="\n")
print(f"MARK.lsp: {len(raw)} байт; zip: {ZIP.stat().st_size} байт; SHA256 {sha}")
