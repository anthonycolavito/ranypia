"""Dumps pyanypia's 2026 Trustees Report data to JSON for make_sysdata.R.

Run from the repo root with pyanypia importable:
    python data-raw/make_sysdata.py
"""

import json
import pathlib

from pyanypia import _data2026 as d

out = {
    name: {"first": getattr(d, f"{name}_FIRST"), "values": list(getattr(d, name))}
    for name in ["FQ", "CPIINC", "BASE_OASDI", "BASE_77"]
    + [f"{s}_PROJ_ALT{a}" for s in ("FQINC", "CPIINC") for a in (1, 2, 3)]
}
path = pathlib.Path(__file__).parent / "trustees2026.json"
path.write_text(json.dumps(out, indent=1))
print(f"wrote {path}")
