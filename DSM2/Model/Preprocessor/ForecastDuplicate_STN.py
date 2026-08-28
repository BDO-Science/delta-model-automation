# Required imported python libraries
# Python default libraries, no need to install
import os
import datetime
import sys
import logging
import argparse
import re
# This tool originally used vtools (written by Jon Shu CADWR)
# to read *.dss data, but vtools required Py2.7
# This tool now uses pyhecdss which is a tool written by Nicky Sandhu for Py3.*
# Current github repo for pyhecdss:
# https://github.com/CADWRDeltaModeling/pyhecdss
import pyhecdss
# Data manipulation libraries
import pandas as pd

# Global pyhecdss variables
pyhecdss.set_message_level(0)  # 0 is little output 10 is all output
pyhecdss.set_program_name('PYTHON')

def CreateLogger(log_file):
    """ Zack's Generic Logger function to create onscreen and file logger

    Parameters
    ----------
    log_file: string
        `log_file` is the string of the absolute file pathname for writing the
        log file too, which is a mirror of the onscreen log display.

    Returns
    -------
    logger: logging object

    Notes
    -----
    This function is completely generic and can be used in any python code.
    The handler.setLevel can be adjusted from logging.INFO to any of the other
    options such as DEBUG, ERROR, WARNING in order to restrict what is logged.

    """
    logger = logging.getLogger()
    logger.setLevel(logging.INFO)
    # Create console handler and set level to info
    handler = logging.StreamHandler()
    handler.setLevel(logging.INFO)
    formatter = logging.Formatter("%(levelname)s - %(message)s")
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    # Create error file handler and set level to info
    handler = logging.FileHandler(log_file,  "w", encoding=None, delay="true")
    handler.setLevel(logging.INFO)
    formatter = logging.Formatter("%(levelname)s - %(message)s")
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    return logger

def CreateDuplicatesForecastDss(ini_dict):
    """ Duplicates forecast.dss records

    This function does not duplicate all the records
    in the forecast.dss file provided. Instead it
    determines which A records from their B,C part
    do not have duplicates for other scenarios
    (i.e. scenarios B, C, D, etc.) and duplicates
    them. Thus the preprocessor must be run
    beforehand to have an effect.

    Parameters
    ----------
    ini_dict: dict
        `ini_dict` is the input dictionary from
        the parsed command-line arguments.
        Format e.g. 
        {"forecast": "/path/to/forecast.dss"}

    """
    ans = input("Would you like this tool to duplicate " +
                "the rest of the forecast.dss records for you?[Y/N]")
    if not ans == 'Y':
        logging.warning("You have chosen to not have this tool duplicate the rest " +
                        "of the forecast.dss records for you. You must do this yourself.")
        sys.exit(0)
    logging.info("You have chosen to have this tool duplicate the rest of the records" +
                 " in the forecast.dss file that was not duplicated by the preprocessor.")
    # creates a file object from the forecast.dss file
    logging.info("Using the forecast.dss file: {}".format(ini_dict.get("forecast")))
    dss_file_obj = pyhecdss.DSSFile(ini_dict.get("forecast"))
    # returns a pandas dataframe with the *.dss pathnames broken-up
    # into columns from /A/B/C/D/E/F/, this is filter-able by pandas
    # column logic
    catalog_df = dss_file_obj.read_catalog()
    logging.info("{}".format(catalog_df))
    pathnames_lst = dss_file_obj.get_pathnames(catalog_df)
    for p in pathnames_lst:
        logging.info("{}".format(p))
    scenarios = catalog_df['F'].unique()
    logging.info("{}".format(scenarios))
    pathnames_dict = {}
    for name, group in catalog_df.groupby(['B', 'C']):
        logging.info("Groupby loop working on {}".format(name))
        g_scenarios = group['F'].values.tolist()
        logging.info("Found scenarios: {}".format(g_scenarios))
        compare = all(item in g_scenarios for item in scenarios)
        logging.info("Compare scenarios: {}".format(compare))
        difference = list(set(scenarios).difference(set(g_scenarios)))
        logging.info("Groupby Scenarios vs Compare Scenarios: {}".format(difference))
        if compare == False and difference:
            logging.info("Create: {}".format(difference))
            cr = group.loc[group.F.str.contains('A')]
            create_orig_pathname = dss_file_obj.get_pathnames(cr)
            assert len(create_orig_pathname) == 1
            new_pathnames_dict_key = create_orig_pathname[0]
            logging.info("Original pathname for duplication: {}".format(new_pathnames_dict_key))
            d_pathnames_lst = []
            for d in difference:
                cr_dict = cr.to_dict(orient='list')
                cr_dict['F'] = d
                temp_df = pd.DataFrame.from_dict(cr_dict)
                path_dup = dss_file_obj.get_pathnames(temp_df)[0]
                logging.info("New pathname for duplication: {}".format(path_dup))
                d_pathnames_lst.append(path_dup)
            pathnames_dict[new_pathnames_dict_key] = d_pathnames_lst
    logging.info("***Beginning Writing Sequence for Duplication***")
    for k_orig in list(pathnames_dict.keys()):
        for new_path in pathnames_dict.get(k_orig):
            logging.info("Original Pathname: {}".format(k_orig))
            logging.info("Duplication Pathname: {}".format(new_path))
            # Read original pathname data regular or irregular time-series
            # Write duplicate pathname as regular or irregular time-series
            if not 'IR' in k_orig:
                temp_df, temp_unit, temp_type = dss_file_obj.read_rts(k_orig)
                #temp_df = temp_df.shift(1, freq='D')
                dss_file_obj.write_rts(new_path, temp_df, temp_unit, temp_type)
                logging.info("Wrote {} as a regular time-series".format(new_path))
            else:
                temp_df, temp_unit, temp_type = dss_file_obj.read_its(k_orig)
                modified_new_path = new_path.split("/")
                ival = modified_new_path[5]
                modified_new_path[4] = ""
                modified_new_path = "/".join(modified_new_path)                
                dss_file_obj.write_its(modified_new_path, temp_df, temp_unit, temp_type, interval=ival)
                logging.info("Wrote {} as an irregular time-series using interval: {}".format(modified_new_path, ival))
    dss_file_obj.close()
    return 0

if __name__ == "__main__":
    # begins global runtime clock
    start = datetime.datetime.now()
    # creates a parser object from argparse library for cmd line args
    parser = argparse.ArgumentParser()
    parser.add_argument("--forecast", "-f", type=str,
                        help="Provide the absolute file pathname to the \
                        forecast input *.dss file that Ian provided")
    # creates an args object from the parsed user input
    args = parser.parse_args()
    # assigns args object into a pythong dictionary
    ini_dict = vars(args)
    # creates global logger object for logging
    abspath = os.path.abspath(__file__)
    pydir_name = os.path.dirname(abspath)
    log_file = os.path.join(pydir_name, "log_ForecastDuplicate_{}.log"
                            .format(datetime.datetime.date(start)))
    CreateLogger(log_file)
    # Remove possible *.dsc, *.dsd, and *.dsk files which are related to *.dss files
    # These files can cause unexpected errors in duplication if not removed beforehand
    data_dir = os.path.dirname(ini_dict.get("forecast"))
    del_lst = []
    for f in os.listdir(data_dir):        
        if f.endswith(".dsc") or f.endswith(".dsd") or f.endswith(".dsk"):
            del_lst.append(f)
    if del_lst:
        logging.warning("Detected *.dsc, *.dsd, or *.dsk files.  \n    {}    \nThese files may cause unexpected errors if not removed.".format(del_lst))
        del_response = input("Would you like to remove the following files [Y/N]?")
        if del_response == 'Y':
            for d in del_lst:
                logging.warning("Attempting deleting of file: {} in Data Folder".format(d))
                abs_path_delete = os.path.join(data_dir, d)
                if os.path.isfile(abs_path_delete):
                    os.remove(abs_path_delete)
                    logging.warning("Deleted file: \n {}".format(abs_path_delete))
        else:
            logging.warning("Script terminating before duplication because of *.dsc, *.dsd, or *.dsk file detected in Data Folder.")
            sys.exit(0)
    # Runs main function for creating duplicate forecast dss records,
    # not duplicated during the preprocessor
    CreateDuplicatesForecastDss(ini_dict)
    # stop global runtime clock
    elapsed_time = datetime.datetime.now() - start
    # displays runtime
    logging.info("Runtime: {} seconds".format(elapsed_time))