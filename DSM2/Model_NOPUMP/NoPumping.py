import shutil
from pathlib import Path
from pyhecdss import DSSFile
import pandas as pd
import os
import stat
import time

# ------------------------------
# Paths
# ------------------------------
root_np = Path(__file__).parent.resolve()   # the folder where this script lives
source_lm = root_np.parent / "Model"
input_src = source_lm / "Input"
input_dst = root_np / "Input"

# Step 1: Copy Input folder
def remove_readonly(func, path, _):
    os.chmod(path, stat.S_IWRITE)
    func(path)

def safe_rmtree(path, retries=3, delay=1):
    for i in range(retries):
        try:
            shutil.rmtree(path, onerror=remove_readonly)
            return
        except PermissionError as e:
            print(f"Retry {i+1}/{retries} failed: {e}")
            time.sleep(delay)
    raise

# use it
if input_dst.exists():
    print(f"Removing existing {input_dst}")
    safe_rmtree(input_dst)

print(f"Copying {input_src} to {input_dst}")
shutil.copytree(input_src, input_dst)

# Step 2: Open forecast.dss
forecast_dss_path = input_dst / "timeseries" / "forecast.dss"
if not forecast_dss_path.exists():
    raise FileNotFoundError(f"{forecast_dss_path} not found!")

print(f"Opening DSS file: {forecast_dss_path}")
dss = DSSFile(str(forecast_dss_path))

# Step 3: Read DSS catalog into DataFrame
print("Reading DSS catalog...")
catalog_df = dss.read_catalog()

# Specify nodes and scenario letters
nodes = ["CHDMC004", "CHSWP003"]
scenario_letters = ["A", "B", "C", "D"]

# Filter catalog for matching paths
df_filtered = catalog_df[
    (catalog_df["B"].isin(nodes)) &    # channel nodes
    (catalog_df["E"] == "1DAY") &      # daily interval
    (catalog_df["F"].str.endswith(tuple([f"21{sc}" for sc in scenario_letters])))  # scenario letters
]

paths_to_zero = dss.get_pathnames(df_filtered)
print(f"Found {len(paths_to_zero)} matching entries in catalog.")

# Zero out each timeseries
for path in paths_to_zero:
    print(f"Zeroing timeseries: {path}")
    try:
        # Read regular time series
        df_ts, units, period_type = dss.read_rts(path)

        # Set all values to zero
        df_ts.iloc[:, 0] = 0.0

        # Write back to DSS
        dss.write_rts(path, df_ts, units, period_type)

    except Exception as e:
        print(f"WARNING: Could not zero {path}: {e}")


print("Done updating DSS file.")
