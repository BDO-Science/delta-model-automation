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

def valid_letter(c):
    """ argparse validator: a single capital letter """
    if not re.fullmatch(r"[A-Z]", c):
        raise argparse.ArgumentTypeError("Not a single uppercase letter: '{}'".format(c))
    return c


def ResolveScenarioFParts(all_f, letters):
    """ Maps each active scenario letter to its F-part in forecast.dss

    CSVToDSM2_pyhecdss.py builds each scenario's F-part by replacing "A" in the
    baseline F-part with the scenario letter. This function reverses that
    convention: it finds the baseline F-part (contains 'A') for which every
    active letter's F-part is actually present in the file.

    Parameters
    ----------
    all_f: iterable of str
        every unique F-part in the forecast.dss catalog
    letters: list of str
        the active scenario letters for this week, e.g. ['A', 'B', 'C']

    Returns
    -------
    (baseline_f, {letter: f_part})

    Raises
    ------
    ValueError if a scenario's records are missing (preprocessor did not
    write them) or if the baseline F-part is ambiguous.
    """
    fset = set(all_f)
    candidates = sorted(f for f in fset
                        if 'A' in f and all(f.replace('A', L) in fset for L in letters))
    if len(candidates) == 1:
        base = candidates[0]
        return base, {L: base.replace('A', L) for L in letters}
    if len(candidates) > 1:
        raise ValueError("Ambiguous baseline F-part. Candidates: {}".format(candidates))

    # nothing matched -> report what is missing for the closest baseline guess
    guesses = sorted(f for f in fset if 'A' in f)
    if not guesses:
        raise ValueError("No baseline F-part containing 'A' found. F-parts in file: {}"
                         .format(sorted(fset)))
    best = min(guesses, key=lambda f: sum(f.replace('A', L) not in fset for L in letters))
    missing = [L for L in letters if best.replace('A', L) not in fset]
    raise ValueError("forecast.dss has no records for scenario(s) {} (expected F-part like '{}'). "
                     "Did the preprocessor run for every scenario? F-parts in file: {}"
                     .format(missing, best.replace('A', missing[0]), sorted(fset)))


def CreateDuplicatesForecastDss(ini_dict):
    """ Duplicates forecast.dss records for the ACTIVE scenarios only

    Records that the preprocessor did not write (B/C part combinations that
    only exist for the baseline) are copied from the baseline record to every
    active scenario that lacks them. Scenario letters come from
    --scenarios (the OMRI files found for the week), not from whatever
    F-parts happen to be in the file, so stale scenarios are ignored.

    Parameters
    ----------
    ini_dict: dict
        Format e.g. {"forecast": "/path/to/forecast.dss",
                     "scenarios": ["A", "B", "C"], "yes": False}
    """
    if not ini_dict.get("yes"):
        ans = input("Would you like this tool to duplicate " +
                    "the rest of the forecast.dss records for you?[Y/N]")
        if ans.strip().upper() != 'Y':
            logging.warning("You have chosen to not have this tool duplicate the rest " +
                            "of the forecast.dss records for you. You must do this yourself.")
            sys.exit(1)
    logging.info("This tool will duplicate the rest of the records" +
                 " in the forecast.dss file that were not duplicated by the preprocessor.")
    # creates a file object from the forecast.dss file
    logging.info("Using the forecast.dss file: {}".format(ini_dict.get("forecast")))
    dss_file_obj = pyhecdss.DSSFile(ini_dict.get("forecast"))
    # returns a pandas dataframe with the *.dss pathnames broken-up
    # into columns from /A/B/C/D/E/F/, this is filter-able by pandas
    catalog_df = dss_file_obj.read_catalog()
    logging.info("{}".format(catalog_df))

    letters = ini_dict.get("scenarios")
    all_f = catalog_df['F'].unique().tolist()
    base_f, f_by_letter = ResolveScenarioFParts(all_f, letters)
    active_f = list(f_by_letter.values())
    logging.info("Baseline F-part: {}".format(base_f))
    logging.info("Active scenarios -> F-parts: {}".format(f_by_letter))
    ignored = sorted(set(all_f) - set(active_f))
    if ignored:
        logging.info("Ignoring F-parts not part of this week's scenarios: {}".format(ignored))

    pathnames_dict = {}
    for name, group in catalog_df.groupby(['B', 'C']):
        logging.info("Groupby loop working on {}".format(name))
        g_scenarios = group['F'].values.tolist()
        logging.info("Found scenarios: {}".format(g_scenarios))
        difference = [f for f in active_f if f not in g_scenarios]
        logging.info("Active scenarios missing for this record: {}".format(difference))
        if not difference:
            continue
        cr = group.loc[group.F == base_f]
        create_orig_pathname = dss_file_obj.get_pathnames(cr)
        if len(create_orig_pathname) != 1:
            raise RuntimeError(
                "Cannot duplicate B={}, C={}: expected exactly one baseline ({}) record "
                "but found {}. Scenario(s) missing: {}".format(
                    name[0], name[1], base_f, len(create_orig_pathname), difference))
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
    n_written = 0
    for k_orig in list(pathnames_dict.keys()):
        for new_path in pathnames_dict.get(k_orig):
            logging.info("Original Pathname: {}".format(k_orig))
            logging.info("Duplication Pathname: {}".format(new_path))
            # Read original pathname data regular or irregular time-series
            # Write duplicate pathname as regular or irregular time-series
            if not 'IR' in k_orig:
                temp_df, temp_unit, temp_type = dss_file_obj.read_rts(k_orig)
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
            n_written += 1
    logging.info("Duplicated {} record(s) for scenario(s) {}".format(n_written, letters))
    dss_file_obj.close()
    return 0


if __name__ == "__main__":
    # begins global runtime clock
    start = datetime.datetime.now()
    # creates a parser object from argparse library for cmd line args
    parser = argparse.ArgumentParser()
    parser.add_argument("--forecast", "-f", type=str, required=True,
                        help="Provide the absolute file pathname to the \
                        forecast input *.dss file that Ian provided")
    parser.add_argument("--scenarios", "-s", nargs="+", required=True,
                        type=valid_letter,
                        help="Active scenario letters for this week, e.g. -s A B C \
                        (WeeklyWorkflow.bat passes the discovered scenario list)")
    parser.add_argument("--yes", "-y", action="store_true",
                        help="Answer Y to all prompts (delete stale *.dsc/*.dsd/*.dsk \
                        files and duplicate the remaining records)")
    # creates an args object from the parsed user input
    args = parser.parse_args()
    # assigns args object into a pythong dictionary
    ini_dict = vars(args)
    if not os.path.isfile(ini_dict.get("forecast")):
        print("ERROR: forecast file not found: {}".format(ini_dict.get("forecast")))
        sys.exit(1)
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
        del_response = 'Y' if ini_dict.get("yes") else input("Would you like to remove the following files [Y/N]?")
        if del_response.strip().upper() == 'Y':
            for d in del_lst:
                logging.warning("Attempting deleting of file: {} in Data Folder".format(d))
                abs_path_delete = os.path.join(data_dir, d)
                if os.path.isfile(abs_path_delete):
                    os.remove(abs_path_delete)
                    logging.warning("Deleted file: \n {}".format(abs_path_delete))
        else:
            logging.warning("Script terminating before duplication because of *.dsc, *.dsd, or *.dsk file detected in Data Folder.")
            sys.exit(1)
    # Runs main function for creating duplicate forecast dss records,
    # not duplicated during the preprocessor
    try:
        CreateDuplicatesForecastDss(ini_dict)
    except (ValueError, RuntimeError) as e:
        logging.error(str(e))
        sys.exit(1)
    # stop global runtime clock
    elapsed_time = datetime.datetime.now() - start
    # displays runtime
    logging.info("Runtime: {} seconds".format(elapsed_time))