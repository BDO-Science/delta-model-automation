# -*- coding: utf-8 -*-
"""
Created on Thu Dec 11 11:18:34 2025

@author: awitt


Date: January 11, 2026
Author: Adam Witt  Email: adam.witt@stantec.com
Code adapted from original author: Sai Nudurupati  Email: sai.nudurupati@jacobs.com
Code prepared for US Bureau of Reclamation under 2025 - 2030 LTO Technical Support contract

Description: Script prepared to compute proportion overlap ratios to compare 
hourly time series of velocities in the Delta for two different real time scenarios 
(e.g., with puming and without puming out of the Delta).

The current configuration of the code uses the following OMR bins:
    '-6500', '-5000', '-3500', '-2000'. 
The current configuration of the code computes weekly overlap ratios for 3 separate 
weeks - week 1, week 2, and week 3 - representing three weeks of a forward
looking Delta operation + boundary conditions 

Flags used in MedVelDirChngBool_OMR_%s_Mons_%s_%s.csv outputs:
0: No change in Median Velocity Direction between the compared alternatives
1: Change in Median Velocity Direction between the compared alternatives
-9999: This station_id was skipped for this month & OMR_bin
-8888: scipy.optimize.bisect threw an error

To run this code, please modify the following:
    forecast_start to reflect date of DSM2 model runs
    master_fpath should be the main folder houseing DSM2 outputs (currently does not need to be modified)
    generate csv files from h5 output using the code h5py_to_csv at:
        U:\184031982\6.0 Studies and Reports\Task_02_Real_Time_Assessment\DSM2_real_time_support\ZOI
 """
 
 
#%%########################################
# Import required packages
import pandas as pd
import numpy as np
import hdf5plugin #added - may be needed for newer versions of python
import h5py
import os
import scipy.stats
import scipy.integrate
import datetime as dt
from concurrent.futures import ProcessPoolExecutor
import argparse
from pathlib import Path
#%%#############################################################################
# Function to compute proportion ratios for two scenarios for all channels
def return_proportion_ratio_df(n_stat, NAA_gdf_daily_f, NAA_NP_gdf_daily_f,
                               min_station_data_all_stations, max_station_data_all_stations,
                               cols, column_name=['Proportion_Ratio']):
    proportion_ratios = []
    change_in_med_velocity = np.full(n_stat, -9999)
    for station_id in range(n_stat):
        if station_id % 100 == 0:   # Debug
            print("Station_id = %s" % station_id)
        min_all_data = min_station_data_all_stations[station_id]
        max_all_data = max_station_data_all_stations[station_id]
        NAA_inputs_station = NAA_gdf_daily_f[station_id].values.flatten()
        NAA_NP_inputs_station = NAA_NP_gdf_daily_f[station_id].values.flatten()
        try:
            PA_density = scipy.stats.gaussian_kde(NAA_inputs_station)
        except:
            proportion_ratios.append(-1)
            continue
        try:
            NP_density = scipy.stats.gaussian_kde(NAA_NP_inputs_station)
        except:
            proportion_ratios.append(-1)
            continue
        #check if both values are near 0 velocity and constant for entire period, if so, set to -1
        if len(set(NAA_inputs_station)) == 1 and len(set(NAA_NP_inputs_station)) == 1 and \
            abs(NAA_inputs_station[0]) < 0.001 and abs(NAA_NP_inputs_station[0]) < 0.001:
            proportion_ratios.append(-1)
            continue
    
        min_data = min(min(NAA_inputs_station),min(NAA_NP_inputs_station))
        max_data = max(max(NAA_inputs_station),max(NAA_NP_inputs_station))
        def y_pts(pt):
            y_pt = min(PA_density(pt),NP_density(pt))
            return y_pt
        
        new_min = min_data
        min_test = y_pts(new_min)
        while min_test > 0.001:
            new_min += -0.01
            min_test = y_pts(new_min)
      
        new_max = max_data
        max_test = y_pts(new_max)
        while max_test > 0.001:
            new_max += 0.01
            max_test = y_pts(new_max)

        if min_all_data > new_min: min_all_data = new_min
        if max_all_data < new_max: max_all_data = new_max

        #overlap = scipy.integrate.quad(y_pts, min_data, max_data)
        overlap = scipy.integrate.quad(y_pts, new_min, new_max)
        proportion_ratios.append(overlap[0])
        
        # Calculate median velocity (velocity with half of curve area on either side) for each curve
        target = 0.5
        try:
            vel_NAA = scipy.optimize.bisect(
              lambda x: scipy.integrate.quad(PA_density,min_data,x)[0] - target,
              min_data, max_data
            )
        except:
            vel_NAA = -8888

        try:
            vel_NAA_NP = scipy.optimize.bisect(
              lambda x: scipy.integrate.quad(NP_density,min_data,x)[0] - target,
              min_data, max_data
            )
        except:
            vel_NAA_NP = -8888
        
        
        # Recording flag if median velocity changed
        if vel_NAA == -8888 or vel_NAA_NP == -8888:
            change_in_med_velocity[station_id] = -8888
        elif (vel_NAA < 0 and vel_NAA_NP < 0) or (vel_NAA >= 0 and vel_NAA_NP >= 0):
            change_in_med_velocity[station_id] = 0
        elif (vel_NAA < 0 and vel_NAA_NP >= 0) or (vel_NAA >= 0 and vel_NAA_NP < 0):
            change_in_med_velocity[station_id] = 1
        else:
            change_in_med_velocity[station_id] = -1
        
        
    return(
        pd.DataFrame(
            proportion_ratios,
            cols,
            column_name,
        ),
        pd.DataFrame(
            list(change_in_med_velocity),
            cols,
            column_name,
        )
    )
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
        
    
#%%############### FUNCTION TO CALCULATE PROPORTIONAL OVERLAP ######################
#define function to generate proportional overlap data in parallel
#previous code: forecast_start was defined as global parameter and manually updated
#current code: forecast_start provided as input in command line
def generate_weekly_data(run_week_loop, scenario, forecast_start):
        
    #DEFINE INITIAL CUSTOM GLOBAL PARAMETERS #####################################################
    #define starting date for analysis, must match same date used to name DSM2 Model\Output\ folder
    #forecast_start = '20260421' #will start 00:00 on this day
    #define master file path where model code and latest DSM2 inputs and outputs are hosted
    # Path to THIS script's folder
    script_dir = Path(__file__).resolve().parent

    # One directory up from the script
    master_fpath = script_dir.parent
    
    
    #DEFINE OTHER INITIAL PARAMETERS #####################################################
    #define OMR scenario dictionary
    scn_dict = {'A': -5000, 'B': -3500, 'C': -2000, 'D': -6500}
    #A: OMRNeg5000
    #B: OMRNeg3500
    #C: OMRNeg2000
    #D: OMRNeg6500
    
    #define flow bins from input data
    #code not currently necessary - can delete but requires changing res_df exception in prop overlap code
    flow_bins = ['na','na','na']
    
    #define all dates for analysis based on start date, including end of each week and forecast end
    # 1. Parse the YYYYMMDD string into a datetime object
    # %Y for full year, %m for month, %d for day
    forecast_start_obj = dt.datetime.strptime(forecast_start, "%Y%m%d")
    
    # 2. Add the number of days using timedelta
    week1_end_obj = forecast_start_obj + dt.timedelta(days=7)
    # 3. Format the new datetime object back into a YYYYMMDD string
    week1_end = week1_end_obj.strftime("%Y%m%d") # 7 days after forecast_start start
    
    # 2. Add the number of days using timedelta
    week2_end_obj = week1_end_obj + dt.timedelta(days=7)
    # 3. Format the new datetime object back into a YYYYMMDD string
    week2_end = week2_end_obj.strftime("%Y%m%d") # 7 days after forecast_start start
    
    # 2. Add the number of days using timedelta
    week3_end_obj = week2_end_obj + dt.timedelta(days=7)
    # 3. Format the new datetime object back into a YYYYMMDD string
    week3_end = week3_end_obj.strftime("%Y%m%d") # 7 days after forecast_start start
    
    #forecast_end = '2026024' #1 day after forecast end, e.g., reflects midnight of 20230315, 15 min after 11:45pm on 20230314

    ################################################################################
    #LOOP THROUGH EACH SCENARIO AND WEEK TO CALCULATE VELOVITY OVERLAPS ############
    
    #loop through OMR scenario (4 total)
    #for sss in range(4): #should be (4) for all 4 scenarios
    #define scenario
    #sss=1
    sss=scenario
    
    #define scenario for this loop
    scn = list(scn_dict.keys())[sss]
    print ('\n\nNow starting scenario %s' % scn)
    
    #OMR value and flow bin for this scenario - need to define for each realtime run
    omr_val = scn_dict.get(scn)
    
    # Inputs: Input Filepaths and Filenames for HDF5 files (DSM2 output h5 files for each OMR scenario)
    #currently, csv files are generated with separate code, h5py_to_csv
    # Alt NAA (current realtime model run with pumping)
    alt_NAA_fpath=r'%s\\Model\\Output\\%s\\hydro' % (master_fpath,forecast_start)   # Path to the folder where the h5 file (DSM2 output) for Alternative 1 is located)
    alt_NAA_fname = os.path.join(alt_NAA_fpath, '%s-21%s.h5' % (forecast_start,scn))   # Edit the name of the h5 file for Alternative 1
    #P_channel_area = '%s-21%s_channel_area.csv' % (forecast_start,scn)
    #P_channel_flow = '%s-21%s_channel_flow.csv' % (forecast_start,scn)
                              
    # Alt NAA_NP (current realtime model run without pumping)
    alt_NAA_NP_fpath=r'%s\\Model_NOPUMP\\Output\\%s\\hydro' % (master_fpath,forecast_start)   # Path to the folder where the h5 file (DSM2 output) for Alternative 1 is located)
    alt_NAA_NP_fname = os.path.join(alt_NAA_NP_fpath, '%s-21%s.h5' % (forecast_start,scn))   # Edit the name of the h5 file for Alternative 2
    #NP_channel_area = '%s-21%s_NP_channel_area.csv' % (forecast_start,scn)
    #NP_channel_flow = '%s-21%s_NP_channel_flow.csv' % (forecast_start,scn)
        
    # Output file names alternatives extension
    altExtension = "realtimeforecast_%s" % forecast_start    # Extension used for naming output csv files (current form: Alt1_identifier__Alt2_identifier)
    
    # Today's date to be used for output filenames
    #DofOutput = dt.datetime.today().strftime('%Y%m%d')  # Today's Date used for naming output csv files (can be replaced with a string or '')  
    
    
    ###################### READ IN DSM2 PUMPING DATA ############################
    #############################################################################
    # Alt NAA
    #alt_NAA = h5py.File(alt_NAA_fname,'r')
    
    # Open the HDF5 file in read mode ('r')
    # this step is necessary in case the code pauses it does not corrupt the h5 file
    with h5py.File(alt_NAA_fname, 'r') as f:
        #print('starting import of h5 pumping data')

        #define alt_NAA from h5 file
        alt_NAA = f
    
        ############### READ IN DSM2 CHANNEL INFORMATION FROM h5 file #######
        # ONLY DO ONCE AS PUMPING AND NNO PUMPING CHANNEL INFO ARE THE SAME #
    
        # Mapping between channel ids and channel numbers
        channel_numbers=pd.DataFrame(alt_NAA.get('/hydro/geometry/channel_number')[:])
        channel_index2number=channel_numbers[0].to_dict()
        #channel_number2index= {value: key for key, value in channel_index2number.items()}
        
        # Mapping between channel ids and upnode (node)
        nodes_df = pd.DataFrame(alt_NAA.get('/hydro/input/channel')[:])
        
        #define parameters for pumping dataframe
        column_names = ['channel_id', 'channel_number', 'node']
        channel_ids = np.array(list(channel_index2number.keys()))
        channel_numbers = np.array(list(channel_index2number.values()))
        nodes = nodes_df['upnode'].values   # Reading directly from the h5 file
        cols = pd.MultiIndex.from_arrays([channel_ids, channel_numbers, nodes], names=tuple(column_names))
        
        # Locations (UPSTREAM/DOWNSTREAM) data from HDF5 output file
        #Just using Alt NAA (as it is the same for both scenarios)
        #Using UPSTREAM to define channel area and flow
        #this is kept as a reminder...code not used since it is pulled in from csv (selection of upstream is embedded in the csv generation code)
        #channel_location=pd.DataFrame(alt_NAA.get('/hydro/geometry/channel_location')[:],dtype=str)
         
        ###################### READ IN DSM2 PUMPING DATA ###############################
    
        #change to pumping directory
        os.chdir(r'%s\\Model\\Output\\%s\\hydro' % (master_fpath,forecast_start))   # Path to the folder where the h5 file (DSM2 output) for pumping file is located)
        
        # Alt NAA scenario
        NAA_flowdata = alt_NAA.get('/hydro/data/channel flow')
    
        #define start time and data frequency (15 minutes) from h5 file
        start_time = pd.to_datetime(NAA_flowdata.attrs['start_time'][0].decode('UTF-8'))
        t_index = pd.date_range(start_time,freq='15min',periods=NAA_flowdata.shape[0])
    
        #define data path within h5 file    
        data_key1 = '/hydro/data/channel area/' # Replace with the actual dataset name inside the H5 file
        #define channel area data for each node for each timestep, 0 in array indicates 'UPSTREAM' values
        NAA_upstr_area_h5 = alt_NAA[data_key1][:, :, 0]
        # Convert the NumPy array to a pandas DataFrame
        NAA_upstr_area = pd.DataFrame(np.array(NAA_upstr_area_h5))
        #define data path within h5 file    
        data_key2 = '/hydro/data/channel flow/' # Replace with the actual dataset name inside the H5 file
        #define channel flow data for each node for each timestep, 0 in array indicates 'UPSTREAM' values
        NAA_vals_h5 = alt_NAA[data_key2][:, :, 0]
        # Convert the NumPy array to a pandas DataFrame
        NAA_vals = pd.DataFrame(np.array(NAA_vals_h5))
        #print('completed import of h5 pumping data')

    ###################### CREATE VELOCITY DATAFRAME #######################
    #NAA_upstr_area = pd.read_csv(P_channel_area, sep='\t', header = None)   #if using txt file with tab separated data
    #NAA_upstr_area = pd.read_csv(P_channel_area, header = None)
    #NAA_vals = pd.read_csv(P_channel_flow, sep='\t', header = None) #if using txt file with tab separated data
    #NAA_vals = pd.read_csv(P_channel_flow, header = None)
    NAA_vels = np.divide(NAA_vals, NAA_upstr_area)  # Calculating velocity
    NAA_vels.columns = cols             
    NAA_vels.index = t_index    
    #define velocity dataframe for entire simulation period         
    NAA_df_long = NAA_vels
    #save full velocity timeseries as csv (only save during week 1 since this is looped many times)
    if run_week_loop == 1:
        vel_csvname = '%s_%s_output_velocity_by_node.csv' % (forecast_start,scn)
        NAA_df_long.loc[pd.to_datetime(forecast_start):pd.to_datetime(week3_end)].\
            to_csv(os.path.join(master_fpath, 'ZOI', forecast_start, 'output_csvs', vel_csvname), float_format='%.4f')
    
    ###################### READ IN DSM2 NO PUMPING DATA ##########################
    ##############################################################################
    
    # Alt NAA_NP
    #alt_NAA_NP = h5py.File(alt_NAA_NP_fname,'r')
    with h5py.File(alt_NAA_NP_fname, 'r') as f:
        #print('starting import of h5 no pumping data')
        #define alt_NAA_NP from h5 file
        alt_NAA_NP = f
       
        ###################### READ IN DSM2 PUMPING DATA ###############################
        #change to no pumping directory
        os.chdir(r'%s\\Model_NOPUMP\\Output\\%s\\hydro' % (master_fpath, forecast_start))   # Path to the folder where the h5 file (DSM2 output) for Alternative 1 is located)
        
        # Alt NAA_NP scenario
        NAA_NP_flowdata = alt_NAA_NP.get('/hydro/data/channel flow')
        
        #define start time and data frequency (15 minutes) from h5 file
        start_time=pd.to_datetime(NAA_NP_flowdata.attrs['start_time'][0].decode('UTF-8'))
        t_index = pd.date_range(start_time,freq='15min',periods=NAA_NP_flowdata.shape[0])
    
        #define data path within h5 file    
        data_key1 = '/hydro/data/channel area/' # Replace with the actual dataset name inside the H5 file
        #define channel area data for each node for each timestep, 0 in array indicates 'UPSTREAM' values
        NAA_NP_upstr_area_h5 = alt_NAA_NP[data_key1][:, :, 0]
        # Convert the NumPy array to a pandas DataFrame
        NAA_NP_upstr_area = pd.DataFrame(np.array(NAA_NP_upstr_area_h5))
        #define data path within h5 file    
        data_key2 = '/hydro/data/channel flow/' # Replace with the actual dataset name inside the H5 file
        #define channel flow data for each node for each timestep, 0 in array indicates 'UPSTREAM' values
        NAA_NP_vals_h5 = alt_NAA_NP[data_key2][:, :, 0]
        #Convert the NumPy array to a pandas DataFrame
        NAA_NP_vals = pd.DataFrame(np.array(NAA_NP_vals_h5))
        #print('completed import of h5 no pumping data')

    
    ###################### CREATE VELOCITY DATAFRAME #######################
    #NAA_NP_upstr_area = pd.read_csv(NP_channel_area, sep='\t', header = None)   #if using txt file with tab separated data
    #NAA_NP_upstr_area = pd.read_csv(NP_channel_area, header = None)
    #NAA_NP_vals = pd.read_csv(NP_channel_flow, sep='\t', header = None)    #if using txt file with tab separated data
    #NAA_NP_vals = pd.read_csv(NP_channel_flow, header = None) #use for txt file
    NAA_NP_vels = np.divide(NAA_NP_vals, NAA_NP_upstr_area)  # Calculating velocity
    NAA_NP_vels.columns = cols             
    NAA_NP_vels.index = t_index             
    #define velocity dataframe for entire simulation period         
    NAA_NP_df_long = NAA_NP_vels
    #save full velocity timeseries as csv (only save during week 1 since this is looped many times)
    if run_week_loop == 1:
        vel_csvname = '%s_%s_NP_output_velocity_by_node.csv' % (forecast_start,scn)
        NAA_NP_df_long.loc[pd.to_datetime(forecast_start):pd.to_datetime(week3_end)].\
            to_csv(os.path.join(master_fpath, 'ZOI', forecast_start, 'output_csvs', vel_csvname), float_format='%.4f') 
            
    #generate list of weekly inputs for parallel processing - pumping
    NAA_df_w1 = NAA_df_long.loc[pd.to_datetime(forecast_start):pd.to_datetime(week1_end)].copy() #week 1
    NAA_df_w2 = NAA_df_long.loc[pd.to_datetime(week1_end):pd.to_datetime(week2_end)].copy() #week 2
    NAA_df_w3 = NAA_df_long.loc[pd.to_datetime(week2_end):pd.to_datetime(week3_end)].copy() #week 3
    NAA_df_w = [NAA_df_w1, NAA_df_w2, NAA_df_w3]
    NAA_df = NAA_df_w[run_week_loop-1]
    
    #generate list of weekly inputs for parallel processing - no pumping
    NAA_NP_df_w1 = NAA_NP_df_long.loc[pd.to_datetime(forecast_start):pd.to_datetime(week1_end)].copy() #week 1 end
    NAA_NP_df_w2 = NAA_NP_df_long.loc[pd.to_datetime(week1_end):pd.to_datetime(week2_end)].copy() #week 2 end
    NAA_NP_df_w3 = NAA_NP_df_long.loc[pd.to_datetime(week2_end):pd.to_datetime(week3_end)].copy() #week 3 end
    NAA_NP_df_w = [NAA_NP_df_w1, NAA_NP_df_w2, NAA_NP_df_w3]
    NAA_NP_df = NAA_NP_df_w[run_week_loop-1]

#### LOOP THROUGH WEEKS AND COMPUTE PROPORTIONAL OVERLAP FOR EACH WEEK ####

#loop through each week (3 total) to compute proportional overlap each week
#for iii in range(1,4): #should be (1,4) for all 3 weeks
    print ('\nNow starting scenario %s week %s' %  (scn, run_week_loop))
    #define run week (1, 2, or 3)
    run_week = run_week_loop
    
    #flow bin for this week - need to define for each realtime run
    flow_bin = flow_bins[run_week_loop-1]
    

    ###################### RESAMPLE TO HOURLY AVERAGE #########################
    # Resampling data from 15 min to hourly frequency
    # Alt NAA
    NAA_df_hourly = NAA_df.resample('h').mean()
    # Alt NAA_NP
    NAA_NP_df_hourly = NAA_NP_df.resample('h').mean()


    ####################### CALCULATE MAX AND MIN VALUES ##########################
    # Calculating minimum values of NAA and NAA_NP scenarios
    min_station_NAA = NAA_df_hourly.min(axis=0).values
    min_station_NAA_NP = NAA_NP_df_hourly.min(axis=0).values
    
    # Calculating maximum values of NAA and NAA_NP scenarios
    max_station_NAA = NAA_df_hourly.max(axis=0).values
    max_station_NAA_NP = NAA_NP_df_hourly.max(axis=0).values
    
    # Caculating minimum of NAA and NAA_NP
    min_station_data_all_stations = np.min(np.vstack((min_station_NAA, min_station_NAA_NP)), axis=0)
    max_station_data_all_stations = np.max(np.vstack((max_station_NAA, max_station_NAA_NP)), axis=0)
    
    #########################################################################
    # Adding a "month_year" column to add columns from 
    NAA_df_hourly['month_year'] = NAA_df_hourly.index.to_period('M')
    NAA_NP_df_hourly['month_year'] = NAA_NP_df_hourly.index.to_period('M')
    
    
    ################# COMPUTE PROPORTION RATIOS #############################

    # Computing proportion ratios for all stations for given OMR range + specific week - also writing the dfs to csv files
    # Initialize files
    nstations_test = []
    nstations = min_station_data_all_stations.shape[0]
    nstations_test.append(nstations)
    
    # Prepare data for proportional calculation routine and run routine
    mon_df_shape = NAA_df_hourly.shape[0]
    # Run proportional calculation routine and save results to results df (res_df)
    if NAA_df_hourly.shape[0] != 0 and NAA_NP_df_hourly.shape[0] != 0:
        res_df, med_vel_flag_df = return_proportion_ratio_df(nstations, NAA_df_hourly, NAA_NP_df_hourly,
                                            min_station_data_all_stations, max_station_data_all_stations,
                                            cols, column_name=['Proportion_Ratio'])
    else:   # If none of the days meet the OMR value and the month criteria -> return -1s
        res_df = pd.DataFrame(np.full(cols.shape, -1), cols, flow_bin)
        med_vel_flag_df = pd.DataFrame(np.full(cols.shape, -1), cols, flow_bin)
    
    
    # Save results as csv files
    # first define the name
    if omr_val < 0:
        csvname = 'DEZOI_OMR_Neg%s_%s_H_short_week%s.csv'%(str(abs(omr_val)), altExtension, run_week)
        med_vel_flag_csvname = 'MedVelDirChngBool_OMR_Neg%s_%s_H_week%s.csv'%(str(abs(omr_val)), altExtension, run_week)
    else:
        csvname = 'DEZOI_OMR_%s_%s_H_short_week%s.csv'%(str(omr_val), altExtension, run_week)
        med_vel_flag_csvname = 'MedVelDirChngBool_OMR_%s_%s_H_week%s.csv'%(str(omr_val), altExtension, run_week)
    

    #then save to file
    res_df.to_csv(os.path.join(master_fpath, 'ZOI', forecast_start, 'output_csvs', csvname))
    med_vel_flag_df.to_csv(os.path.join(master_fpath, 'ZOI', forecast_start, 'output_csvs', med_vel_flag_csvname))
    prop_ratios_mon_dfs_nvals = mon_df_shape
    
    pd.DataFrame(prop_ratios_mon_dfs_nvals, columns = ['ndays'], index=['test - flow bin']).to_csv(os.path.join(master_fpath, 'ZOI', forecast_start, 'output_csvs','n_days_for_calc_prop_ratios.csv'))
    print ('\nCompleted scenario %s week %s' %  (scn, run_week_loop))


################################### PARALLEL CODE #################################################
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
    
    
    #check if forecast_start folder is in ZOI directory, if not, create
        # Path to THIS script's folder
    script_dir = Path(__file__).resolve().parent

    # One directory up from the script
    master_fpath1 = script_dir.parent
    ZOIoutputFolderName = 'ZOI\\%s' % forecast_start   # Currently writes output csv files to this folder located in the same folder as this notebook
    ZOIoutputFolder = os.path.join(master_fpath1, ZOIoutputFolderName)
    
    # Create outputFolder if it doesn't already exist
    if not os.path.isdir(ZOIoutputFolder):
       os.mkdir(ZOIoutputFolder)
    
    #define proportional overlap csv output folder location and directory
    csvoutputFolderName = "output_csvs"   # Currently writes output csv files to this folder located in the same folder as this notebook
    csvoutputFolder = os.path.join(ZOIoutputFolder, csvoutputFolderName)
    
    # Create outputFolder if it doesn't already exist
    if not os.path.isdir(csvoutputFolder):
       os.mkdir(csvoutputFolder)
    
    
    #define inputs for parallel code (1 input per parallel process = 12 inputs)
    weeks = [1,2,3,1,2,3,1,2,3,1,2,3] 
    #define OMR scenarios to be run in analaysis
    #0 = A = -5,000
    #1 = B = -3,500
    #2 = C = -2,000
    #3 = D = -6,250
    scenarios = [0,0,0,1,1,1,2,2,2,3,3,3]
    forecast_starts = [forecast_start]*12
    
    #run parallel process (spawn 12 processes, one for each scenario (4) and week (3))
    with ProcessPoolExecutor() as executor:
        executor.map(generate_weekly_data, weeks, scenarios, forecast_starts)
    
   