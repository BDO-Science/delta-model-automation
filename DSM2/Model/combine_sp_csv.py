# -*- coding: utf-8 -*-
"""
Created on Tue Dec 23 15:29:08 2025

@author: awitt
"""

#code to combine csv files in one folder
import pandas as pd
import glob
import os
import argparse
import sys
from pathlib import Path

from scenario_discovery import load_scenarios

#%%############################################################################
# Function to check valid date input to python command line
def valid_date(fs):
    """ An ArgParse Validator for Forecast Start & End Input by the User on CMD

    The argparse library can accept a user created validation function for
    custom data types. This function checks the input `s` which is a string
    date and attempts to convert it to a pandas datetime TimeStamp. If the
    datetime conversion fails, the function raises an error.

    Parameters
    ----------
    s: string (date)
        `s` is a date string input from the user for either the
        --forecast_start or --forecast_end command line input

    Returns
    -------
    anonymous: pandas datetime Timestamp

    Raises
    ------
    ValueError:
        `msg` is passed to argparse.ArgumentTypeError to stop the script at
        the command line input checks/parsing to immediately notify the user
        if the input date string cannot be converted

    """

    try:
        #test to make sure command line input is formatted correctly
        #should be YYYYmmdd, e.g., 20260203, as number and not string
        pd.to_datetime(fs, format="%Y%m%d")
        return fs
    except ValueError:
        msg = "Not a valid date: '{0}' must be YYYYMMDD.".format(fs)
        raise argparse.ArgumentTypeError(msg)
        
# Function to grab all scenario data and combine into one csv
def combine_sp(forecast_start):

    # DEFINE GLOBAL PARAMETERS ###############################################
    realtime_date = forecast_start

    # Active scenarios for this week: {'A': {'omri': '-5000', ...}, ...}
    scen = load_scenarios(forecast_start)
    # Order: most negative OMR first (e.g. -6500, -5000, -3500, -2000), which is the
    # order the previous hard-coded D, A, B, C produced. Letters follow the week's files.
    scenario_order = {
        letter: i for i, (letter, d) in enumerate(
            sorted(scen.items(), key=lambda kv: int(kv[1]["omri"])))
    }
    omr_by_letter = {letter: int(d["omri"]) for letter, d in scen.items()}
    print("Active scenarios (combine order): " + ", ".join(
        f"{l} (OMR {omr_by_letter[l]})" for l in scenario_order))

    master_fpath = Path(__file__).resolve().parent.parent   # DSM2 folder (parent of Model)
    folder_path = master_fpath / "Model" / "Output" / realtime_date / "PTM" / "sp"

    def combine_files(pattern, output_filename):

        # Find matching files
        all_files = glob.glob(str(folder_path / pattern))

        # Remove previously combined file if present
        all_files = [
            f for f in all_files
            if os.path.basename(f) != output_filename
        ]

        if len(all_files) == 0:
            print(f"No files found matching {pattern}")
            return

        def get_scenario(filepath):
            """
            Extract scenario from filename.
            Example:
                survival_A.csv -> A
                travel_D.csv   -> D
            """
            return os.path.basename(filepath).split("_")[-1].split(".")[0]

        # Keep only the active scenarios; flag anything else as stale
        active_files, stale_files = [], []
        for f in all_files:
            (active_files if get_scenario(f) in scenario_order else stale_files).append(f)
        for f in stale_files:
            print(f"WARNING: ignoring file from a non-active scenario: {os.path.basename(f)}")

        # Every active scenario must be present, otherwise the combined file is incomplete
        found = {get_scenario(f) for f in active_files}
        missing = [l for l in scenario_order if l not in found]
        if missing:
            raise FileNotFoundError(
                f"{pattern}: no file for scenario(s) {missing} in {folder_path}")

        # Sort files by scenario order (most negative OMR first)
        all_files = sorted(
            active_files,
            key=lambda f: scenario_order[get_scenario(f)]
        )

        print("\nFiles being combined:")
        for f in all_files:
            print(f"  {os.path.basename(f)}")

        # Read files
        df_list = []

        for filename in all_files:
            df = pd.read_csv(filename)

            scenario = get_scenario(filename)
            df["Model_Run"] = scenario
            df["OMR"] = omr_by_letter[scenario]

            df_list.append(df)

        # Combine
        combined_df = pd.concat(df_list, ignore_index=True)

        # Save
        combined_df.to_csv(folder_path / output_filename, index=False)

        print(
            f"Successfully combined {len(all_files)} files into "
            f"{output_filename}"
        )

    # Combine survival files
    combine_files(
        pattern="survival*.csv",
        output_filename="survival_combined.csv"
    )

    # Combine travel files
    combine_files(
        pattern="travel*.csv",
        output_filename="travel_combined.csv"
    )
        
#%%############################################################################

#main code to run functions in parallel
if __name__ == "__main__":
    
    #collect forecast start date from code input
    parser = argparse.ArgumentParser()
    parser.add_argument("--forecast_start", "-fs", type=valid_date, required=True,
                        help="Provide a valid forecast start date \
                        e.g. 20180823")
    # creates an args object from the parsed user input
    args = parser.parse_args()
    # assigns args object into a pythong dictionary
    ini_dict = vars(args)
    forecast_start = ini_dict.get("forecast_start")
    
    #run code to combine csvs into one
    try:
        combine_sp(forecast_start)
    except (FileNotFoundError, ValueError) as e:
        print(f"ERROR: {e}")
        sys.exit(1)
