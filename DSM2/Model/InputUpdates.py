import re
import shutil
import sys
from pathlib import Path

# --------------------------------------------------
# Project paths (robust to where script is run from)
# --------------------------------------------------
PROJECT_ROOT = Path.cwd()          # e.g. D:\Model
INPUT_ROOT = PROJECT_ROOT / "Input"
BACKUP_ROOT = PROJECT_ROOT / "backup"

# --------------------------------------------------
# Backup logic (preserves directory structure)
# --------------------------------------------------
def backup_file(path: Path):
    rel = path.relative_to(PROJECT_ROOT)
    backup_path = BACKUP_ROOT / rel
    backup_path.parent.mkdir(parents=True, exist_ok=True)

    if not backup_path.exists():
        shutil.copy2(path, backup_path)

# --------------------------------------------------
# Robust DSM2-safe key replacement
# --------------------------------------------------
def replace_key_value(lines, key, new_value):
    """
    Replaces everything after KEY + whitespace on a line.
    Preserves spacing and line endings.
    """
    pattern = re.compile(rf"^({key})(\s+).*", re.IGNORECASE)
    out = []

    for line in lines:
        newline = "\n" if line.endswith("\n") else ""
        core = line.rstrip("\n")

        m = pattern.match(core)
        if m:
            spacing = m.group(2)
            core = f"{m.group(1)}{spacing}{new_value}"

        out.append(core + newline)

    return out

# --------------------------------------------------
# Load inputs file (key = value format)
# --------------------------------------------------
def load_inputs_file(path):
    values = {}

    with open(path, "r") as f:
        for lineno, line in enumerate(f, start=1):
            line = line.strip()

            if not line or line.startswith("#"):
                continue

            if "=" not in line:
                raise ValueError(f"Invalid line {lineno}: {line}")

            key, value = line.split("=", 1)
            values[key.strip()] = value.strip()

    return values

# --------------------------------------------------
# Forecast configuration files
# --------------------------------------------------
def update_forecast_configs(values):
    keys = [
        "WEEK",
        "START_DATE",
        "FORE_START_DATE",
        "QUAL_START_DATE",
        "EC_START_DATE",
        "END_DATE",
        "QUAL_END_DATE",
        "WARM_END_DATE",
    ]

    for f in INPUT_ROOT.glob("configuration_forecast*.inp"):
        backup_file(f)
        lines = f.read_text().splitlines(keepends=True)

        for key in keys:
            lines = replace_key_value(lines, key, values[key])

        f.write_text("".join(lines))
        print(f"Updated {f.relative_to(PROJECT_ROOT)}")

# --------------------------------------------------
# PTM config*.inp files
# --------------------------------------------------
def update_ptm_configs(values):
    for sub in ["np", "pp", "sp"]:
        folder = INPUT_ROOT / "PTM" / sub
        if not folder.exists():
            continue

        for f in folder.glob("config*.inp"):
            backup_file(f)
            lines = f.read_text().splitlines(keepends=True)

            for key in ["WEEK", "PTM_START_DATE", "END_DATE"]:
                lines = replace_key_value(lines, key, values[key])

            f.write_text("".join(lines))
            print(f"Updated {f.relative_to(PROJECT_ROOT)}")

def update_ptm_particle_files(values):
    ptm_dirs = [
        INPUT_ROOT / "PTM" / "np",
        INPUT_ROOT / "PTM" / "pp",
        INPUT_ROOT / "PTM" / "sp",
    ]

    new_delay = values.get("PTM_DELAY")
    if not new_delay:
        print("PTM_DELAY not provided; skipping particle insertion updates.")
        return

    for folder in ptm_dirs:
        if not folder.exists():
            continue

        for f in folder.glob("ptm*_*.inp"):
            if "behavior" in f.name.lower():
                continue
            backup_file(f)
            lines = f.read_text().splitlines(keepends=True)
            out = []

            in_particle_block = False

            for line in lines:
                core = line.rstrip("\n")
                newline = "\n" if line.endswith("\n") else ""

                if core.strip() == "PARTICLE_INSERTION":
                    in_particle_block = True
                    out.append(core + newline)
                    continue

                if core.strip() == "END" and in_particle_block:
                    in_particle_block = False
                    out.append(core + newline)
                    continue

                if in_particle_block:
                    parts = core.split()
                    # NODE NPARTS DELAY DURATION
                    if len(parts) >= 4 and parts[0].isdigit():
                        parts[2] = new_delay
                        core = "\t".join(parts)

                out.append(core + newline)

            f.write_text("".join(out))
            print(f"Updated particle delay in {f.relative_to(PROJECT_ROOT)}")

# --------------------------------------------------
# PTM behavior inputs
# --------------------------------------------------
def update_ptm_behavior(values):
    folder = INPUT_ROOT / "PTM" / "sp"

    if not folder.exists():
        print("WARNING: PTM/sp folder not found, skipping PTM behavior files.")
        return

    files = list(folder.glob("ptm_behavior_inputs*.inp"))
    if not files:
        print("WARNING: No ptm_behavior_inputs*.inp files found.")
        return

    week = values["WEEK"]  # YYYYMMDD

    for f in files:
        backup_file(f)
        lines = f.read_text().splitlines(keepends=True)
        out = []

        for line in lines:
            core = line.rstrip("\n")
            newline = "\n" if line.endswith("\n") else ""

            # --------------------------------------------------
            # Simulation start date
            # --------------------------------------------------
            if core.startswith("Simulation_Start_Date"):
                core = f"Simulation_Start_Date: {values['SIM_START']}"

            # --------------------------------------------------
            # Release date table rows (MM/DD/YYYY ...)
            # --------------------------------------------------
            elif re.match(r"\d{2}/\d{2}/\d{4}", core):
                parts = core.split()
                parts[0] = values["RELEASE_DATE"]
                core = "\t".join(parts)

            # --------------------------------------------------
            # Output_Path: replace ONLY \YYYYMMDD\ directory
            # --------------------------------------------------
            elif core.startswith("Output_Path:"):
                # Replace only the FIRST \YYYYMMDD\ occurrence
                core = re.sub(
                    r"\\\d{8}\\",
                    rf"\\{week}\\",
                    core,
                    count=1
                )

            out.append(core + newline)

        f.write_text("".join(out))
        print(f"Updated {f.relative_to(PROJECT_ROOT)}")


# --------------------------------------------------
# Project structure sync + InputUpdates generation
# --------------------------------------------------
from datetime import datetime

DATA_EXTERNAL = PROJECT_ROOT / "DataExternal"
# Primary Output in project root
OUTPUT_ROOT_PROJECT = PROJECT_ROOT / "Output"

# Output in Model_NOPUMP (sibling folder)
OUTPUT_ROOT_NP = PROJECT_ROOT.parent / "Model_NOPUMP" / "Output"

def sync_output_folders():
    """
    Create missing Output/YYYYMMDD folders in both Model and Model_NOPUMP
    to match DataExternal.
    Returns sorted list of valid DataExternal date folders.
    """
    if not DATA_EXTERNAL.exists():
        raise FileNotFoundError("DataExternal folder not found.")

    OUTPUT_ROOT_PROJECT.mkdir(exist_ok=True)
    OUTPUT_ROOT_NP.mkdir(exist_ok=True)

    # Only accept YYYYMMDD folder names
    date_folders = [f for f in DATA_EXTERNAL.iterdir() if f.is_dir() and re.fullmatch(r"\d{8}", f.name)]
    date_folders.sort(key=lambda p: p.name)

    for folder in date_folders:
        for out_root in [OUTPUT_ROOT_PROJECT, OUTPUT_ROOT_NP]:
            out = out_root / folder.name

            # Create folder tree
            (out / "hydro").mkdir(parents=True, exist_ok=True)
            (out / "PTM" / "np").mkdir(parents=True, exist_ok=True)
            (out / "PTM" / "pp").mkdir(parents=True, exist_ok=True)
            (out / "PTM" / "sp").mkdir(parents=True, exist_ok=True)

            print(f"Output structure exists for {folder.name} in {out_root}")

    return date_folders


def parse_config_dates(config_path):
    """
    Extract required values from configuration_forecast.inp
    using whitespace-separated key/value format.
    """

    target_keys = {
        "WEEK",
        "START_DATE",
        "FORE_START_DATE",
        "QUAL_START_DATE",
        "EC_START_DATE",
        "END_DATE",
        "QUAL_END_DATE",
        "WARM_END_DATE",
    }

    values = {}

    with open(config_path, "r") as f:
        for raw_line in f:
            line = raw_line.strip()

            # Skip blanks and comments
            if not line or line.startswith("#"):
                continue

            # Remove inline comments
            if "#" in line:
                line = line.split("#", 1)[0].strip()

            parts = line.split()

            if len(parts) < 2:
                continue

            key = parts[0]
            val = parts[1]

            if key in target_keys:
                values[key] = val

    # Validate required keys
    required = [
        "START_DATE",
        "FORE_START_DATE",
        "QUAL_START_DATE",
    ]

    missing = [k for k in required if k not in values]
    if missing:
        raise ValueError(
            f"Missing required keys in configuration_forecast.inp: {missing}"
        )

    # --------------------------------------------------
    # WEEK comes from folder name (YYYYMMDD)
    # --------------------------------------------------
    values["WEEK"] = config_path.parent.name

    # Derived values
    values["PTM_START_DATE"] = values["QUAL_START_DATE"]
    values["SIM_START"] = values["START_DATE"]

    d_fore = datetime.strptime(values["FORE_START_DATE"], "%d%b%Y")
    d_qual = datetime.strptime(values["QUAL_START_DATE"], "%d%b%Y")

    values["RELEASE_DATE"] = d_fore.strftime("%m/%d/%Y")

    delay_days = (d_fore - d_qual).days
    values["PTM_DELAY"] = f"{delay_days}day"

    return values


def generate_input_updates_file(date_folder: Path):
    """
    Create InputUpdates_YYYYMMDD.txt in newest DataExternal folder.
    Skip if it already exists.
    """

    config_path = date_folder / "configuration_forecast.inp"

    if not config_path.exists():
        raise FileNotFoundError(
            f"configuration_forecast.inp not found in {date_folder}"
        )

    values = parse_config_dates(config_path)

    output_file = date_folder / f"InputUpdates_{date_folder.name}.txt"

    if output_file.exists():
        print(f"{output_file.name} already exists — skipping creation.")
        return output_file

    with open(output_file, "w") as f:
        for key, value in values.items():
            f.write(f"{key} = {value}\n")

    print(f"Created {output_file.name}")

    return output_file

def refresh_timeseries_folder(date_folder: Path):
    """
    Clear Input/timeseries and copy .dss files
    from the given DataExternal date folder.
    """

    timeseries = INPUT_ROOT / "timeseries"

    if not timeseries.exists():
        raise FileNotFoundError("Input/timeseries folder not found.")

    print("\nRefreshing Input/timeseries...")

    # --------------------------------------------------
    # Delete existing files (files only, not folders)
    # --------------------------------------------------
    for item in timeseries.iterdir():
        if item.is_file():
            item.unlink()
            print(f"Deleted {item.name}")

    # --------------------------------------------------
    # Copy DSS files
    # --------------------------------------------------
    dss_files = list(date_folder.glob("*.dss"))

    if not dss_files:
        print(f"WARNING: No .dss files found in {date_folder}")
        return

    for src in dss_files:
        dst = timeseries / src.name
        shutil.copy2(src, dst)
        print(f"Copied {src.name}")

def prepare_project_structure(week: str):
    """
    Use a specific DataExternal/YYYYMMDD folder.
    Returns path to the InputUpdates file.
    """

    # Create Output folder tree first
    sync_output_folders()

    # Then prepare the specific DataExternal week folder
    target = DATA_EXTERNAL / week

    if not target.exists():
        raise FileNotFoundError(f"DataExternal folder not found: {week}")

    print(f"Using DataExternal folder: {week}")

    # Generate InputUpdates file + refresh Input/timeseries
    inputs_file = generate_input_updates_file(target)
    refresh_timeseries_folder(target)

    # ---- NEW: also ensure Output folder tree for this week exists ----
    week_output = OUTPUT_ROOT_PROJECT / week
    (week_output / "hydro").mkdir(parents=True, exist_ok=True)
    (week_output / "PTM" / "np").mkdir(parents=True, exist_ok=True)
    (week_output / "PTM" / "pp").mkdir(parents=True, exist_ok=True)
    (week_output / "PTM" / "sp").mkdir(parents=True, exist_ok=True)
    print(f"Ensured Output folder structure for week {week}")

    return inputs_file

# --------------------------------------------------
# Main
# --------------------------------------------------
if __name__ == "__main__":

    if len(sys.argv) < 2:
        raise ValueError("Week identifier required (YYYYMMDD)")

    week_id = sys.argv[1]

    if not re.fullmatch(r"\d{8}", week_id):
        raise ValueError("Invalid week identifier format. Use YYYYMMDD.")

    date_folder = DATA_EXTERNAL / week_id

    if not date_folder.exists():
        raise FileNotFoundError(f"DataExternal folder not found: {week_id}")

    # Generate InputUpdates file + refresh timeseries + ensure Output folder
    inputs_file = prepare_project_structure(week_id)

    # Load values and run updates
    values = load_inputs_file(inputs_file)

    REQUIRED_KEYS = [
        "WEEK",
        "START_DATE",
        "FORE_START_DATE",
        "QUAL_START_DATE",
        "EC_START_DATE",
        "END_DATE",
        "QUAL_END_DATE",
        "WARM_END_DATE",
        "PTM_START_DATE",
        "RELEASE_DATE",
        "SIM_START",
    ]

    missing = [k for k in REQUIRED_KEYS if k not in values]
    if missing:
        raise ValueError(f"Missing required inputs: {missing}")

    update_forecast_configs(values)
    update_ptm_configs(values)
    update_ptm_behavior(values)
    update_ptm_particle_files(values)

    print("\nAll updates complete.")
