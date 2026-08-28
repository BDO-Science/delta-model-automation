import subprocess
import pandas as pd
import sys
import os

# Get Week from command line (passed by batch file)
Week = sys.argv[1]

# Build CSV path
csv_path = os.path.join("..", "Model", "DataExternal", Week, "zoi_bins.csv")
print(f"Reading CSV file: {csv_path}")

# Read CSV file
df = pd.read_csv(csv_path)

print("Columns detected:")
print(df.columns.tolist())

# Clean column names
df.columns = df.columns.str.strip()

# Extract rows
w1 = df.iloc[0]
w2 = df.iloc[1]
w3 = df.iloc[2]

# Extract values
InflowBin_W1 = str(w1["Delta Inflow Bin"]).strip().lower()
InflowBin_W2 = str(w2["Delta Inflow Bin"]).strip().lower()
InflowBin_W3 = str(w3["Delta Inflow Bin"]).strip().lower()

Week1_dates = str(w1["Forecast Week"]).strip()
Week2_dates = str(w2["Forecast Week"]).strip()
Week3_dates = str(w3["Forecast Week"]).strip()

SacramentoFlow_W1 = str(w1["Sacramento River at Freeport (cfs)"])
SanJoaquinFlow_W1 = str(w1["San Joaquin River at Vernalis (cfs)"])
SacramentoFlow_W2 = str(w2["Sacramento River at Freeport (cfs)"])
SanJoaquinFlow_W2 = str(w2["San Joaquin River at Vernalis (cfs)"])
SacramentoFlow_W3 = str(w3["Sacramento River at Freeport (cfs)"])
SanJoaquinFlow_W3 = str(w3["San Joaquin River at Vernalis (cfs)"])

# Debug print (optional)
print("Parsed values:")
print(InflowBin_W1, InflowBin_W2, InflowBin_W3)
print(Week1_dates)

# Run main script
subprocess.run([
    "python",
    "zoi_pp_html.py",
    Week,
    InflowBin_W1, InflowBin_W2, InflowBin_W3,
    Week1_dates, Week2_dates, Week3_dates,
    SacramentoFlow_W1, SanJoaquinFlow_W1,
    SacramentoFlow_W2, SanJoaquinFlow_W2,
    SacramentoFlow_W3, SanJoaquinFlow_W3
], check=True)