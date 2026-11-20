#!/usr/bin/env python
"""
scenario_discovery.py
---------------------
Step 0 of WeeklyWorkflow: discover the OMR scenarios for a given week from the
OMRI file names in DataExternal\\<week>\\ and assign scenario letters (A, B, C, ...).

The file names are the ONLY source of truth. e.g.
    02_02_OMRI -5000.csv   ->  OMRI = -5000
    02_02_OMRI -6500.csv   ->  OMRI = -6500

Outputs (written into DataExternal\\<week>\\):
    scenarios_<week>.csv  - letter, omri, file          (for Python scripts)
    scenarios_<week>.cmd  - SET statements              (for WeeklyWorkflow.bat)

Usage:
    python scenario_discovery.py 20260203
    python scenario_discovery.py 20260203 --data-root "D:\\...\\DataExternal"

In other Python scripts:
    from scenario_discovery import load_scenarios
    scen = load_scenarios("20260203")      # {'A': {'omri': '-5000', 'file': '...'}, ...}
"""
import argparse
import csv
import re
import sys
from pathlib import Path

# ------------------------------------------------------------------ settings
# Matches "..._OMRI -5000.csv", "OMRI -3500.csv", "OMRI-2000.CSV", etc.
OMRI_RE = re.compile(r"OMRI\s*(-?\d+)\s*\.csv$", re.IGNORECASE)

# Letter-assignment rule.
# Values listed here get the first letters, in this order (keeps historical
# A=-5000, B=-3500, C=-2000). Any other OMRI values are appended afterwards,
# most-negative first. Set to [] to simply sort every scenario most-negative first.
LEGACY_ORDER = [-5000, -3500, -2000]

# Downstream .inp templates may only exist for A-D. Raise once InputUpdates.py
# has been made scenario-agnostic.
MAX_SCENARIOS = 4

LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
# ---------------------------------------------------------------------------


def default_data_root():
    return Path(__file__).resolve().parent / "DataExternal"


def find_omri_files(week_dir):
    """Return {omri_int: (label, filename)}. Raises on duplicate OMRI values."""
    found = {}
    for f in sorted(Path(week_dir).glob("*.csv")):
        if f.name.startswith("~$"):          # Excel temp/lock files
            continue
        m = OMRI_RE.search(f.name)
        if not m:
            continue
        label = m.group(1)
        value = int(label)
        if value in found:
            raise ValueError(
                f"Two files share OMRI {label}:\n  {found[value][1]}\n  {f.name}\n"
                "Remove or rename one so each scenario has a single file."
            )
        found[value] = (label, f.name)
    return found


def order_values(values):
    legacy = [v for v in LEGACY_ORDER if v in values]
    others = sorted(v for v in values if v not in LEGACY_ORDER)
    return legacy + others


def discover(week, data_root=None):
    root = Path(data_root) if data_root else default_data_root()
    week_dir = root / week
    if not week_dir.is_dir():
        raise FileNotFoundError(f"Folder not found: {week_dir}")

    found = find_omri_files(week_dir)
    if not found:
        raise FileNotFoundError(
            f"No OMRI files (*OMRI <value>.csv) found in {week_dir}")

    ordered = order_values(found.keys())
    if len(ordered) > MAX_SCENARIOS:
        raise ValueError(
            f"{len(ordered)} OMRI files found but MAX_SCENARIOS = {MAX_SCENARIOS}. "
            "Raise MAX_SCENARIOS once downstream inputs support more scenarios.")

    scen = {}
    for letter, v in zip(LETTERS, ordered):
        label, fname = found[v]
        scen[letter] = {"omri": label, "file": fname}
    return week_dir, scen


def write_outputs(week, week_dir, scen):
    csv_path = week_dir / f"scenarios_{week}.csv"
    cmd_path = week_dir / f"scenarios_{week}.cmd"

    with open(csv_path, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["letter", "omri", "file"])
        for letter, d in scen.items():
            w.writerow([letter, d["omri"], d["file"]])

    lines = ["@echo off",
             f'set "SCEN_COUNT={len(scen)}"',
             f'set "SCEN_LIST={" ".join(scen.keys())}"']
    for letter, d in scen.items():
        lines.append(f'set "SCEN_{letter}_OMRI={d["omri"]}"')
        lines.append(f'set "SCEN_{letter}_FILE={d["file"]}"')
    cmd_path.write_text("\n".join(lines) + "\n")
    return csv_path, cmd_path


def load_scenarios(week, data_root=None):
    """For use by other scripts. Returns {'A': {'omri','file'}, ...} in letter order."""
    root = Path(data_root) if data_root else default_data_root()
    path = root / week / f"scenarios_{week}.csv"
    if not path.exists():
        raise FileNotFoundError(
            f"{path} not found - run scenario_discovery.py {week} first.")
    with open(path, newline="") as fh:
        return {r["letter"]: {"omri": r["omri"], "file": r["file"]}
                for r in csv.DictReader(fh)}


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("week", help="Week identifier, e.g. 20260203")
    ap.add_argument("--data-root", default=None,
                    help="Path to DataExternal (default: next to this script)")
    args = ap.parse_args()

    try:
        week_dir, scen = discover(args.week, args.data_root)
    except (FileNotFoundError, ValueError) as e:
        print(f"ERROR: {e}")
        sys.exit(1)

    csv_path, cmd_path = write_outputs(args.week, week_dir, scen)

    print(f"Scenarios for {args.week}: {len(scen)} found")
    for letter, d in scen.items():
        print(f"  {letter} = OMRI {d['omri']:>6}   ({d['file']})")
    print(f"Wrote {csv_path.name} and {cmd_path.name}")


if __name__ == "__main__":
    main()
