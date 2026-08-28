FOLDER/FILE STRUCTURE:
##########################################################################################################
backup:			previous input files that have since been edited by InputUpdates.py
bin:			folder containing DSM2 binaries

Data External:	folder containing weekly data from DWR and BDO. Organized by date recieved.

Input:
	hydro:						folder containing hydro related model files
	timeseries:					folder containing dss files
	common_input:				folder containing common input model files
	PTM:						folder containing input for np, pp, and sp runs
	configuration_forecast.inp:	configuration file for model run
	hydro.inp:					hydro model master file
	
Output: subfolder structure to house model runs weekly

Preprocessor: location of preprocessor scripts and activites
	CSVToDSM2_pyhecdss.py for each scenario (A, B, C) with corresponding excel file (5000, 3500, 2000)
	 python CSVToDSM2_pyhecdss.py -c "..\DataExternal\20251230\01_05_OMRI -5000.csv" -f "..\Input\timeseries\forecast.dss" -d "..\DataExternal\20251230\dicu.dss" -fs 2025-12-30 -fe 2026-01-20 -sd A
	ForecastDuplicate_STN.py for each scenario (A, B, C) 
	 python .\ForecastDuplicate_STN.py --forecast "Y", -f ..\Input\timeseries\forecast.dss

combine_sp_csv.py		combines ECOPTM csvs into a single table
environment.yml	environment file for running workflow
InputUpdates.py			python code to update model setup files and folder directories
WeeklyWorkflow.bat		batch file to run DSM2 workflow:updates inputs, creates directories, preprocesses data, runs DSM2 hydro and ptm, runs No Pumping, combines survival ECOPTM csvs

############################################################################################################
WORKFLOW 
*activate environment_LM.yml if you do not have existing environment

1. download new DataExternal from server
2. open console and activate ops_dsm2_production environment
3. call WeeklyWorkflow.bat, type in the date (ex. 20260203)
4. upload output folders back to server
e