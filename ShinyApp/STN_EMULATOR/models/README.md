This repository holds the set tof machine learning models developed as part of CCWD's hydraulic footprint project.
The goal of the hydraulic footprint project is to develop quick and intuitive tools for real-time and large-scale assessment of the potential effects of CVP and SWP alteranative operations on listed species in the Sacramento-San Joaquin Delta.

The models include:

1. DSM2 PTM emulator models for end-of-7-day and 30-day percent entainment of surface-oriented particles into the CVP and SWP export facilities (Cliftonr Court Forebay and Jones Pumping Plant). The models were built with lightGBM and exported as .txt files. The required inputs, in particular order, are (1) location (by DSM2 node, as categorical variable), (2) combined export (Clifton Court inflow and Jones pumping), (3) San Joaquin River flow at Vernalis, (4) Sacramento River flow at Freeport, (5) a seasonality/Delta gross depletion term, (6) a Delta gross accretion term, and (7) combined east side Delta boundary flows (Mokelumne, Calaveras, and Consumnes), and (8) the flow through Delta Cross Channel. 

Among those, (5), (6), and (8) warrants elaboration. The seasonality/Delta gross depletion follows a fixed seasonal pattern that is consistent with the monthly pattern in [DAYFLOW](https://data.cnra.ca.gov/dataset/dayflow/resource/776b90ca-673e-4b56-8cf3-ec26792708c3), but converted to TAF/month. The specific values are:
| Month | Seasonal Gross Delta Depletion (TAF/month) |
|-------|--------------------------------------------|
|October|144.67|
|November|102.94|
|December|127.96|
|January|74.4|
|February|49.04|
|March|80.55|
|April|111.87|
|May|149.66|
|June|222.96|
|July|267.59|
|August|232.73|
|September|156.61|

The Delta gross accretion is also consistent with the Delta precipitation runoff estimates in DAYFLOW, where the precipitation at Stockton Fire Station No. 4 is applied over the Delta, and converted to TAF/month.

For the Delta Cross Channel flow, it is recommended to preprocess measured data so that when the gates are closed the numeric value is zero, because the models were fit with simulated data from CalSim and DSM2. This variable is used for estimating 30-day entrainment, but is not needed for 7-day entrainment.

2. DSM2 ECO-PTM emulator models for end-of-150-day combined through-Delta survival and routing into the inteior Delta (Georgiana Slough and Delta Cross Channel) of salmon particles.  The models were built with lightGBM and exported as .txt files. The required inputs are, in particular order, are (1) Sacramento River flow at Freeport, (2) Yolo Bypass flow, (3) Mokelumne River flow before its confluence with Consumnes River, and (4) status of Delta Cross Channel gate opening as Boolean, where 0 means closed and 1 means open.

3. Model for the entrainment event horizon, i.e., distance from Clifton Court Forebay gate along Old and Middle river corridor to the spatial boundary of a certain level of entrainment risk. The model was built with lightGBM and exported as .txt file. The required inputs are, in particular order, are (1) entrainment risk level, expressed and entrainment percentage from 15 to 80 at an interval of 5, (2) combined export (Clifton Court inflow and Jones pumping), (3) San Joaquin River flow at Vernalis, (4) Sacramento River flow at Freeport, (5) a seasonality/Delta gross depletion term, (6) a Delta gross accretion term, and (7) combined east side Delta boundary flows (Mokelumne, Calaveras, and Consumnes).
The Delta gross depletion and accretion terms are the same as above for the PTM emulator.

4. Dataset of entrainment scale membership, i.e., a constructed scale from 1 to 10 indicating low to hight overall entrainment risk in central and south Delta. The dataset includes 7-day and 30-day PTM entrianment at various locations, and the assignment of entrainment scale. Prediction can be made by applying nearest neighbor algorithm (or other variants or similar methods) with PTM or PTM emulator outputs.

5. A lookup table for DSM2 node to show the name of key Delta locations, their longitude and latitude coordinates, and their associated DSM2 node.


For further questions, please reach out to Ching-Fu Chang (cchang@ccwater.com)
