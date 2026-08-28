############################################################################################################
DATA PREPROCESSING WORKFLOW 

1. Obtain new DSM2 model input data from DWR and save to new folder in Model\DataExternal\Date for specific date (e.g., 20260106) 
2. Obtain new Delta operations csv files from Reclamation and save to newly created folder (currently providing OMR at -2000, -3500, -5000)
3. Extend data timeseries end date in each provided csv to align with DWR provided data
4. Load boundary flow data from OMR -5000 csv into OMR_optimization.xlsm to  generate OMR -6250 export scenario
5. Save new csv for OMR at -6250
6. Load four scenarios into flow_export_review.xlsx to review flow and export timeseries
7. Load plots and data tables to real-time assessment summary document
8. Notify modeler data is ready for DSM2 and PTM model simulations
*improvements: combine steps 2-6 using newly developed scenario data development tool (Roja leading)

############################################################################################################
ZOI POSTPROCESSING WORKFLOW 

1. Create new folder in ZOI for specific date (e.g., 20260106) 
2. Use HDFViewer to convert each h5 tide file into a channel area and channel flow text file (need to automate) saved in each Model or Model_NP Output folder (need to integrate to python code)
3. Run DSM2_DEZOI_20260107_realtime_3weeks_4scenarios.py to develop proportional overlap calculations csv outputs and auto save to specified date folder in ZOI
4. Notify Roja to develop zone of influence maps and altered channel length data tables and save to server
6. Load maps data tables to real-time assessment summary document
*improvements: combine steps 2-4 by resolving h5py code issue and combining 2-3, then bringing code from 4 into python environment


############################################################################################################
PTM POSTPROCESSING WORKFLOW 

1. Load NP and PP DSS excel tool templates from Model\Output\__PTM_postprocessor into Model\Output\Date\PTM folder
2. In each excel tool, load the PTM output dss files for that week (16 files in each tool)
3. Run macro to save figures as images
4. Upload NP and PP images and data tables to real-time assessment summary document
*improvements: develop script to automate steps 1-4 


############################################################################################################
ECO-PTM POSTPROCESSING WORKFLOW 

1. Copy combine_csv.py from Model\Output\__PTM_postprocessor into Model\Output\Date\PTM folder
2. Update date in code to match date of model run and run code to combine csvs into survival_combined.csv
3. Upload data tables to real-time assessment summary document
*improvements: develop script to automate steps 1-3 

