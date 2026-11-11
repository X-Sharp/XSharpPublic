"""Regression check for issue #1558.

CursorAdapter.DataSource must accept non-string values (for example a
numeric connection handle), so its declared type in the generated VFP UI
class must be OBJECT rather than STRING. Assigning a LONG to a STRING
property is a compile-time type error in XSharp.VFP.UI. OBJECT matches
the declaration in XSharp.VFP and XSharp.Data.
"""
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
TARGET = ROOT / "src" / "Runtime" / "XSharp.VFP.UI" / "Generated" / "CursorAdapter.generated.prg"

text = TARGET.read_text(encoding="utf-8-sig", errors="replace")
match = re.search(r"^\s*PROPERTY\s+DataSource\s+AS\s+(\w+)\s+AUTO", text, re.IGNORECASE | re.MULTILINE)
if match is None:
    print(f"FAIL: no DataSource property declaration found in {TARGET.name}")
    sys.exit(1)

declared = match.group(1).upper()
if declared != "OBJECT":
    print(f"FAIL: DataSource must be declared AS OBJECT, found AS {declared}")
    sys.exit(1)

print("PASS: DataSource is declared AS OBJECT")
sys.exit(0)
