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
from pathlib import Path

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

    master_fpath = Path.cwd().parent
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

        # Desired scenario order
        scenario_order = {
            "D": 0,
            "A": 1,
            "B": 2,
            "C": 3
        }

        def get_scenario(filepath):
            """
            Extract scenario from filename.
            Example:
                survival_A.csv -> A
                travel_D.csv   -> D
            """
            return os.path.basename(filepath).split("_")[-1].split(".")[0]

        # Sort files by desired scenario order
        all_files = sorted(
            all_files,
            key=lambda f: scenario_order.get(get_scenario(f), 999)
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
    parser.add_argument("--forecast_start", "-fs", type=valid_date,
                        help="Provide a valid forecast start date \
                        e.g. 20180823")
    # creates an args object from the parsed user input
    args = parser.parse_args()
    # assigns args object into a pythong dictionary
    ini_dict = vars(args)
    forecast_start = ini_dict.get("forecast_start")
    
    #run code to combine csvs into one
    combine_sp(forecast_start)
