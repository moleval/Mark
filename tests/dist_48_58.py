# -*- coding: utf-8 -*-
"""dist/MARK.lsp.zip должен содержать текущий MARK.lsp и актуальный SHA256."""
from pathlib import Path
import hashlib
import re
import zipfile

ROOT = Path(__file__).resolve().parents[1]
MARK = ROOT / "MARK.lsp"
ZIP = ROOT / "dist" / "MARK.lsp.zip"
README = ROOT / "dist" / "README.md"

mark = MARK.read_bytes()
with zipfile.ZipFile(ZIP) as z:
    names = z.namelist()
    assert names == ["MARK.lsp"], f"в zip неожиданные файлы: {names}"
    packed = z.read("MARK.lsp")

assert packed == mark, "zip содержит не текущий MARK.lsp"
sha = hashlib.sha256(ZIP.read_bytes()).hexdigest().upper()
text = README.read_text(encoding="utf-8")
m = re.search(r"SHA256:\s*`([0-9a-fA-F]{64})`", text)
assert m, "в dist/README.md нет SHA256"
assert m.group(1).upper() == sha, \
    f"SHA256 в README {m.group(1)} != {sha}"

print(f"OK: zip = MARK.lsp, SHA256 {sha}")
