#01-data_prep.R - First updates to most recent CDFW FTP site versions of 20mm 
#and SLS databases, then extracts ingests data from in-season excel data


library(tidyverse)
library(here)
library(CDECRetrieve)
library(xml2)
library(lubridate)
library(deltamapr)
library(sf)
library(openxlsx)
library(EflowStats)
library(officer)
library(LTMRdata)
library(deltamapr)
library(readxl)
library(stringr)
library(jagsUI)
library(coda)

source("code/helper_functions.R")

#Workflow for updating to most recent CDFW FTP version of catch databases.
#This likely will only need to be run a the beginning of each entrainment season
#because they are only updated on an annual basis

######################START CDFW UPDATE#########################################
#===============================================================================
###SLS###
## SLS Survey url, name of zip file, and name of database within the zip file:
surveyURL <- "https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip"
zipFileName <- "SLS.zip"
dbName <- "SLS.accdb"

localZipFile <- file.path(tempdir(),zipFileName)
download.file(url=surveyURL, destfile=localZipFile)
localDbFile <- unzip(zipfile=localZipFile, exdir=file.path(tempdir()))

# MS access database set up----
# File path to Access database (Salvage)
db_path <- file.path(tempdir(),"SLS.accdb")

keepTables <- c("Catch","FishCodes","Lengths",
                "Tow Info","Water Info","Meter Corrections", "SLS Stations")

SLSTables <- bridgeAccess(db_path,
                          tables = keepTables,
                          script = file.path("code/helper_functions.R"))

#MWT data setup ----

SLSTables$Catch <- SLSTables$Catch %>%
  transmute(Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
            Station = as.character(Station),
            across(c(Tow, FishCode, Catch, CatchID), as.integer))

SLSTables$Lengths <- SLSTables$Lengths %>%
  transmute(Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
            Station = as.character(Station),
            across(c(Tow, FishCode, Length, entryorder), as.integer))

SLSTables$`Meter Corrections` <- SLSTables$`Meter Corrections` %>%
  transmute(StudyYear = as.double(StudyYear),
            MeterSerial = as.integer(MeterSerial),
            CalibrationDate = as.Date(CalibrationDate),
            kfactor = as.double(kfactor),
            Notes = as.character(Notes))

SLSTables$`Tow Info` <- SLSTables$`Tow Info`%>%
  transmute(Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
            Station = as.character(Station),
            Tow = as.integer(Tow),
            Time = as.POSIXct(Time),
            Tide = as.character(Tide),
            BottomDepth = as.integer(BottomDepth),
            CableOut = CableOut * 0.3048,
            Duration = as.double(Duration),
            across(c(NetMeterSerial, NetMeterStart, NetMeterEnd, NetMeterCheck),
                   as.integer),
            Notes_tow = as.character(Comments))

SLSTables$`Water Info` <- SLSTables$`Water Info`%>%
  transmute(Survey = as.integer(Survey),
            Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
            Station = as.character(Station),
            TopTemp = as.double(TopTemp),
            across(c(TopEC, BottomEC, Secchi, NTU, FNU), as.integer),
            Notes_env = as.character(Comments))

# SLS used to have a 20 mm station table that has changed to be "SLS Stations"
SLSTables$`SLS Stations` <- SLSTables$`SLS Stations` %>%
  transmute(Station = as.character(Station),
            Latitude = (LatD + LatM/60 + LatS/3600),
            Longitude = -(LonD + LonM/60 + LonS/3600))

# Manipulating the data tables --------------------------------------------

waterInfo <- SLSTables$`Water Info` %>%
  mutate(# Converting secchi from cm to m
    Secchi = Secchi/100,
    # Converting EC to salinity
    # Per SOP normalized at TopTemp = 25C; units are in millisiemens
    Sal_surf = wql::ec2pss(TopEC/1000, t = 25),
    Sal_bot = wql::ec2pss(BottomEC/1000, t = 25),
    # This is to take care of how the floats are read between Access and readr methods...
    Temp_surf = round(TopTemp, 2),
    # Changing Date to as.Date
    Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"))

towInfo <- SLSTables$`Tow Info` %>%
  # 1 parsing error because time was not recorded for that row
  # Now, joining to the Meter Corrections table to calculate tow volume later
  # This is based on the "WEB_Raw_Catch_Volum_Info" query in the SLS Access query database
  # First need to create a study year column; from the query above, the "WEB_VolumeInfo" query calculates year from the
  # towInfo table.
  mutate(StudyYear = year(Date),
         # Changing Date from POSIXct format to simply Date for easy of use
         # Timezone data is still retained in the Datetime column
         Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles")) %>%
  left_join(SLSTables$`Meter Corrections` %>%
              # There are duplicated values here in this table; will simply distinct() them
              # Confirmed via email with Adam, ES of Native Fish unit as of 10-27-2021
              distinct(),
            by = c("StudyYear", "NetMeterSerial" = "MeterSerial")) %>%
  # Moving on to various required calculations
  mutate(Datetime = ymd_hms(if_else(is.na(Time), NA_character_, paste(Date, paste(hour(Time), minute(Time), second(Time), sep=":"))),
                            tz = "America/Los_Angeles"),
         # Now to turn tide into factors as outlined in the metadata file
         Tide = case_when(Tide == 1 ~ "High Slack",
                          Tide == 2 ~ "Ebb",
                          Tide == 3 ~ "Low Slack",
                          Tide == 4 ~ "Flood",
                          TRUE ~ NA_character_),
         # Converting bottom depth to meters
         Depth = BottomDepth * 0.3048,
         # Calculating tow volume for the fish net, 0.37 as the area of the mout of the net (per Access + SLS SOP document)
         Tow_volume = NetMeterCheck * kfactor * 0.37)

catch <- SLSTables$Catch %>%
  mutate(Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"))

lengths <- SLSTables$Lengths %>%
  mutate(Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles")) %>%
  # Calculating total number of fish measured (across all lengths) and # of fish measured
  # per Date, Station, Tow, and FishCode
  # This is to calculate plus counts later in dfFin
  group_by(Date, Station, Tow, FishCode, Length)%>%
  summarise(LengthFrequency=n(), .groups="drop")%>%
  group_by(Date, Station, Tow, FishCode) %>%
  mutate(TotalLengthMeasured=sum(LengthFrequency)) %>%
  ungroup()

# Now to combine the datasets together, following the relationship table in Access
# The tables go waterInfo -> towInfo -> Catch -> lengths
SLS <- waterInfo %>%
  full_join(towInfo,
            by = c("Date", "Station")) %>%
  full_join(catch,
            by = c("Date", "Station", "Tow"),multiple="all") %>%
  full_join(lengths,
            by = c("Date", "Station", "Tow", "FishCode"),multiple="all") %>%
  # Adding in taxa name based on Species Code.csv file
  left_join(Species %>%
              select(SLS_Code,
                     Taxa) %>%
              dplyr::filter(!is.na(SLS_Code)),
            by = c("FishCode"="SLS_Code")) %>%
  left_join(SLSTables$`SLS Stations`,
            by="Station")%>%
  # Merging the two comment columns together; they both have data in them
  mutate(Notes_tow=paste(Notes_tow, Notes_env, sep = "; ")) %>%
  arrange(Date, Datetime, Survey, Station, Tow, Taxa, Length) %>%
  mutate(Source = "SLS",
         SampleID=paste(Source, Date, Station, Tow), # Creating SampleID index
         Count = if_else(is.na(Length), as.numeric(Catch), (LengthFrequency/TotalLengthMeasured) * Catch),
         # Creating Length_NA_flag to parallel the other survey datasets in LTMR
         Length_NA_flag = case_when(is.na(Count) ~ "No fish caught",
                                    is.na(Length) & Count > 0 ~ "Unknown length",
                                    TRUE ~ NA_character_),
         # Creating Method column; Adam described this as an "Oblique tow", significantly diff from MWT
         Method = "Oblique tow",
         Station=as.character(Station),
         Count=if_else(Length_NA_flag=="No fish caught", 0, Count, missing=Count))%>% # Transform all counts for "No fish caught" to 0.
  # Removing CatchID and entryorder as they are not relevant to the dataset
  # Removing TopEC, BottomEC as they have been converted over the salinity already
  # Removing CBMeterSerial, CBMeterStart, CBMeterEnd, CBMeterCheck as CB not ran on the SLS
  transmute(Source, Station, Latitude, Longitude,
            Date, Datetime, Survey, Depth, SampleID, Method,
            Tide, Sal_surf, Sal_bot, Temp_surf, Secchi,
            TurbidityNTU = NTU, TurbidityFNU = FNU, Tow_volume,
            Cable_length=CableOut, Tow_duration=Duration,
            Taxa, Length, Count, Length_NA_flag,
            Notes_tow, Notes_flowmeter = Notes)


#===============================================================================
###20mm###

## 20mm Survey url, name of zip file, and name of database within the zip file:
surveyURL <- "https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/20mm_New.zip"
zipFileName <- "20mm_New.zip"
dbName <- "20mm_New.accdb"

## Download and unzip the file:
localZipFile <- file.path(tempdir(),zipFileName)
download.file(url=surveyURL, destfile=localZipFile)
localDbFile <- unzip(zipfile=localZipFile, exdir=file.path(tempdir()))

## Save select tables:
keepTables <- c("Tow","FishSample","FishLength","Survey","Station",
                "20mmStations","Gear","GearCodesLkp","MeterCorrections","SampleCode")

## Open connection to the database:
data <- bridgeAccess(file.path(tempdir(), dbName),
                     tables = keepTables,
                     script = file.path("code/helper_functions.R"))

# # If you instead want to write the csv files
# for(tab in keepTables) {
# 	tmp <- DBI::dbReadTable(con, tab)
# 	write.csv(tmp, file=file.path("data-raw","20mm",paste0(tab,".csv")), row.names=FALSE)
# }

#########################################################################################
## Create and save compressed data files using raw tables.

Survey <- data$Survey %>%
  mutate(SurveyID = as.integer(SurveyID),
         SampleDate = parse_date_time(SampleDate, "%Y-%m-%d", tz="America/Los_Angeles"),
         Survey = as.integer(Survey),
         Comments = as.character(Comments))

Station <- data$Station %>%
  mutate(across(c(StationID, SurveyID, Station), as.integer),
         across(c(StartLatDeg, StartLatMin, StartLatSec, StartLonDeg, StartLonMin, StartLonSec, Temp, TopEC, BottomEC, Secchi, FNU, NTU),
                as.numeric),
         Comments = as.character(Comments))

Tow <- data$Tow %>%
  mutate(across(c(TowID, StationID, TowNum, Tide), as.integer),
         TowTime = force_tz(as.POSIXct(TowTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "America/Los_Angeles"),
         across(c(BottomDepth, CableOut, Duration), as.numeric))

Gear <- data$Gear %>%
  mutate(across(c(GearID, TowID, GearCode, MeterSerial), as.integer),
         across(c(MeterStart, MeterEnd, MeterCheck), as.numeric),
         Comments = as.character(Comments))

GearCodesLkp <- data$GearCodesLkp %>%
  mutate(across(c(GearCode, Order), as.integer),
         across(c(Gear, GearDescription), as.character),
         Active = as.logical(Active))

MeterCorrections <- data$MeterCorrections %>%
  mutate(across(c(StudyYear, MeterSerial), as.integer),
         CalibrationDate = as.Date(CalibrationDate),
         kFactor = as.numeric(kFactor),
         Notes = as.character(Notes))

TmmStations <- data$`20mmStations` %>%
  mutate(across(c(Station), as.integer),
         across(c(LatD, LatM, LatS, LonD, LonM, LonS), as.numeric),
         across(c(RKI, Location, Notes), as.character))

FishSample <- data$FishSample %>%
  mutate(across(everything(), as.integer))

FishLength <- data$FishLength %>%
  mutate(across(c(FishLengthID, FishSampleID), as.integer),
         Length = as.numeric(Length),
         across(c(AdFinPresent, ReleasedAlive), as.logical))

MeterCorrections_avg <- MeterCorrections %>%
  dplyr::group_by(MeterSerial) %>%
  dplyr::summarize(kFactor_avg=mean(kFactor), .groups="keep") %>%
  dplyr::ungroup()

sample20mm <- Survey %>%
  dplyr::inner_join(Station, by="SurveyID") %>%
  dplyr::inner_join(Tow, by="StationID") %>%
  dplyr::inner_join(Gear, by="TowID") %>%
  dplyr::left_join(GearCodesLkp, by="GearCode") %>%
  dplyr::left_join(TmmStations, by="Station") %>%
  dplyr::mutate(StudyYear=lubridate::year(SampleDate)) %>%
  dplyr::left_join(MeterCorrections, by=c("StudyYear","MeterSerial")) %>%
  dplyr::left_join(MeterCorrections_avg, by="MeterSerial") %>%
  dplyr::filter(GearCode == 2) %>%
  dplyr::mutate(kFactor_final=ifelse(!is.na(kFactor), kFactor, kFactor_avg),
                Tow_volume=1.51*kFactor_final*MeterCheck) %>%
  dplyr::arrange(SampleDate, Survey, Station, TowNum) %>%
  dplyr::mutate(Source="20mm",
                SampleID=paste(Source, 1:nrow(.)),
                Tow_direction=NA,
                ## Tide codes from 20mmDataFileFormat_New_102621.pdf on the CDFW ftp site:
                Tide=dplyr::recode(Tide, `1`="High Slack", `2`="Ebb", `3`="Low Slack", `4`="Flood"),
                TowTime=paste(hour(TowTime), minute(TowTime), second(TowTime), sep=":"),
                Datetime=paste(SampleDate, TowTime),
                Date=lubridate::parse_date_time(SampleDate, "%Y-%m-%d", tz="America/Los_Angeles"),
                Datetime=lubridate::parse_date_time(dplyr::if_else(is.na(TowTime), NA_character_, Datetime),
                                                    "%Y-%m-%d %%H:%M:%S", tz="America/Los_Angeles"),
                Depth=BottomDepth*0.3048, # Convert depth to m from feet
                Cable_length=CableOut*0.3048, # Convert to m from feet
                Method="20mm net",
                Temp_surf=Temp,
                ## Convert conductivity to salinity; TopEC is in micro-S/cm; input should
                ##		be in milli-S/cm:
                Sal_surf=wql::ec2pss(TopEC/1000, t=25),
                Latitude=(LatD + LatM/60 + LatS/3600),
                Longitude= -(LonD + LonM/60 + LonS/3600)) %>%
  dplyr::select(GearID, Source, Station, Latitude, Longitude, Date, Datetime, Survey,
                TowNum, Depth, SampleID, Method, Tide, Sal_surf, Temp_surf,
                Secchi, Tow_volume, Tow_direction, Cable_length,
                Duration, FNU, NTU, Comments, Comments.x, Comments.y)


## Note:
## GearID 33397 has two entries for fish code 10 in FishSample
## GearID 35766 has two entries for fish code 2 in FishSample
## Based on lengths, probably a matter of being ID'd in the field vs. in the lab.

fish20mm_totalCatch <- FishSample %>%
  dplyr::select(GearID, FishSampleID, FishCode, Catch)

fish20mm_individLength <- FishSample %>%
  dplyr::inner_join(FishLength, by="FishSampleID") %>%
  dplyr::select(GearID, FishSampleID, FishCode, Length, Catch)

fish20mm_lengthFreq_measured <- fish20mm_individLength %>%
  dplyr::filter(!is.na(Length)) %>%
  dplyr::group_by(GearID, FishSampleID, FishCode, Length) %>%
  dplyr::summarize(LengthFrequency=n(), .groups="keep") %>%
  dplyr::ungroup()

fish20mm_adjustedCount <- fish20mm_totalCatch %>%
  dplyr::left_join(fish20mm_lengthFreq_measured,
                   by=c("GearID","FishSampleID","FishCode")) %>%
  dplyr::group_by(GearID, FishSampleID, FishCode) %>%
  dplyr::mutate(TotalMeasured=sum(LengthFrequency, na.rm=T)) %>%
  dplyr::ungroup() %>%
  ## Add total catch numbers:
  ## There are some cases where the number of fish measured is greater than the
  ## catch value in the FishSample table. In these cases, use the number measured.
  dplyr::mutate(CatchNew=ifelse(TotalMeasured > Catch, TotalMeasured, Catch)) %>%
  ## Calculate length-frequency-adjusted counts:
  dplyr::mutate(Count=(LengthFrequency/TotalMeasured)*CatchNew) %>%
  dplyr::left_join(Species %>% ## Add species names
                     dplyr::select(TMM_Code, Taxa) %>%
                     dplyr::filter(!is.na(TMM_Code)),
                   by=c("FishCode"="TMM_Code"))

## Examine the cases where number measured > catch:
count_mismatch <- subset(fish20mm_adjustedCount, CatchNew != Catch)
nrow(count_mismatch)


## Join sample and fish info:
TMM <- sample20mm %>%
  dplyr::left_join(fish20mm_adjustedCount %>%
                     dplyr::select(GearID, FishCode, Taxa, Length,
                                   LengthFrequency, Catch, CatchNew, Count),
                   by="GearID") %>%
  ## Add reasoning for any NA lengths:
  dplyr::mutate(Length_NA_flag=if_else(is.na(Catch), "No fish caught", NA_character_),
                Station=as.character(Station))

## There are some cases where:
##  -positive catch is indicated in FishSample, but there are no corresponding records
##		in FishLength.
## In these cases, use the Catch value from FishSample as Count and change
##	Length_NA_flag:
index_1 <- which(!is.na(TMM$Catch) & is.na(TMM$Count))
TMM[index_1, ]
TMM$Count[index_1] <- TMM$Catch[index_1]
TMM$Length_NA_flag[index_1] <- "Unknown length"
TMM[index_1, ]

## 2021-10-25: Adam Chorazyczewski of CDFW confirmed that catch for this
## record is 1:
index_2 <- which(as.character(TMM$Date) == "2021-03-23" & TMM$Station == 610 &
                   TMM$TowNum == 1 & TMM$Taxa == "Gasterosteus aculeatus" &
                   is.na(TMM$Catch))
TMM[index_2, ]
TMM[index_2,c("Catch","Count")] <- 1
TMM[index_2,"Length_NA_flag"] <- "Unknown length"
TMM[index_2, ]


## set Count for all
TMM<-TMM%>%
  mutate(Count=if_else(Length_NA_flag=="No fish caught", 0, Count, missing=Count),
         Length=as.numeric(Length))

## Create final measured lengths data frame:
TMM_measured_lengths <- TMM %>%
  dplyr::select(SampleID, Taxa, Length, LengthFrequency) %>%
  dplyr::filter(!is.na(LengthFrequency))%>% # Remove fish that weren't measured
  dplyr::rename(Count=LengthFrequency)

## Renaming the turbidity field to be added. Name convention based on discussion
## with Sam and Dave
## Now remove extra fields:
TMM <- TMM %>%
  dplyr::rename(TurbidityNTU = NTU,
                TurbidityFNU = FNU) %>%
  dplyr::select(-GearID, -Duration, -Comments, -Comments.x,
                -Comments.y, -FishCode, -Catch, -CatchNew,
                -LengthFrequency, -TowNum)

save(TMM,SLS,file="data/CDFWdata_lastUpdated.rda")

###############################################################################
#####################END CDFW UPDATE###########################################
#==============================================================================

load("data/CDFWdata_lastUpdated.rda")

#Load habitat volumes from Six Agency redesign effort
habVols <- read_csv("data/raster_volumes_jan2026_strata.csv")%>%
  filter(IncludeSLS==T)%>%
  mutate(volume_0to15 = (`volume_0to15`)*1000000000,
         volume_15tomax = (`volume_15tomax`)*1000000000,
         total_volume = (total_volume_km3)*1000000000)%>%
  select(SubRegion,Region,Stratum,volume_0to15,volume_15tomax,total_volume)%>%
  mutate(volume_15tomax = if_else(is.na(volume_15tomax)==T,0,volume_15tomax))


towInfo <- bind_rows(TMM,SLS)%>%
  select(Source:TurbidityNTU,Tow_duration)%>%distinct()

dataLFS_temp <- towInfo%>%left_join(bind_rows(TMM,SLS)%>%
                                      select(SampleID,Date,Taxa,Length,Count)%>%
                                      mutate(Month=month.abb[month(Date)])%>%
                                      filter(Taxa =="Spirinchus thaleichthys")%>%
                                      Make_Wide())%>%
  select(-`Age-1`,-`Age-2+`)%>%
  rename("Catch"="Age-0")%>%
  replace_na(list(Catch=0))%>%
  mutate(WY = get_waterYear(Date))%>%
  filter(is.na(Longitude)==F)

#=================Bring in new SLS data from excel files====================

#This is going to pull in the SLS data from excel files provide in-season. If thre are
#multiple matching SLS data files in the /data folder then it will take the one with 
#the most recent date suffix. e.g. for the end of 2026 SLS it's pulling from 
#`SLS catch update 4.7.26.xlsx`.

#The function is currently designed to work with with the workbook structure that was used in 
#2026, but is not particularly resilient to changes, so the read_latest_suffixed_xlsx()
#may need updating in 2027. 


newDat_List <- read_latest_suffixed_xlsx()[-length(read_latest_suffixed_xlsx(verbose = F))]

#Check how many surveys are included
names(newDat_List)

#check variable names in one of the survey sheets
names(newDat_List[[2]])

#Pull out data frames for each survey from list of dataframes returned by read_xl
#Make the appropriate number of survey objects given what is included in the 
#most recent data file. 

survey1 <- newDat_List[[3]]%>%select(Date,Survey,`Smelt Catch`,Station,`Sample Status`,`Secchi (CM)`)%>%
  rename(Catch = `Smelt Catch`)%>%
  mutate(Tow_volume=185,
         Secchi = `Secchi (CM)`,
         WY = get_waterYear(Date),
         Station = as.character(Station),
         Source = "SLS",
         Date = as_date(Date),
         Catch = if_else(is.na(Catch)==T,0,Catch))%>%
  filter(is.na(Station)==F,`Sample Status`!="Not Sampled")%>%
  select(-`Secchi (CM)`)

survey2 <- newDat_List[[4]]%>%select(Date,Survey,`Smelt Catch`,Station,`Sample Status`,`Secchi (CM)`)%>%
  rename(Catch = `Smelt Catch`)%>%
  mutate(Tow_volume=185,
         Secchi = `Secchi (CM)`,
         WY = get_waterYear(Date),
         Station = as.character(Station),
         Source = "SLS",
         Date = as_date(Date),
         Catch = if_else(is.na(Catch)==T,0,Catch))%>%
  filter(is.na(Station)==F,`Sample Status`!="Not Sampled")%>%
  select(-`Secchi (CM)`)

survey3 <- newDat_List[[5]]%>%select(Date,Survey,`Smelt Catch`,Station,`Sample Status`,`Secchi (CM)`)%>%
  rename(Catch = `Smelt Catch`)%>%
  mutate(Tow_volume=185,
         Secchi = `Secchi (CM)`,
         WY = get_waterYear(Date),
         Station = as.character(Station),
         Source = "SLS",
         Date = as_date(Date),
         Catch = if_else(is.na(Catch)==T,0,Catch))%>%
  replace_na(list(`Sample Status`="Not Processed",WY=2026))%>%
  filter(is.na(Station)==F,`Sample Status`!="Not Sampled")%>%
  select(-`Secchi (CM)`)

survey4 <- newDat_List[[6]]%>%select(Date,Survey,`Smelt Catch`,Station,`Sample Status`,`Secchi (CM)`)%>%
  rename(Catch = `Smelt Catch`)%>%
  mutate(Tow_volume=185,
         Secchi = `Secchi (CM)`,
         WY = get_waterYear(Date),
         Station = as.character(Station),
         Source = "SLS",
         Date = as_date(Date),
         Catch = if_else(is.na(Catch)==T,0,Catch))%>%
  replace_na(list(`Sample Status`="Not Processed",WY=2026))%>%
  filter(is.na(Station)==F,`Sample Status`!="Not Sampled")%>%
  select(-`Secchi (CM)`)


survey5 <- newDat_List[[7]]%>%select(Date,Survey,`Smelt Catch`,Station,`Sample Status`,`Secchi (CM)`)%>%
  rename(Catch = `Smelt Catch`)%>%
  mutate(Tow_volume=185,
         Secchi = `Secchi (CM)`,
         WY = get_waterYear(Date),
         Station = as.character(Station),
         Source = "SLS",
         Date = as_date(Date),
         Catch = if_else(is.na(Catch)==T,0,Catch))%>%
  replace_na(list(`Sample Status`="Not Processed",WY=2026))%>%
  filter(is.na(Station)==F,`Sample Status`!="Not Sampled")%>%
  select(-`Secchi (CM)`)

survey6 <- newDat_List[[8]]%>%select(Date,Survey,`Smelt Catch`,Station,`Sample Status`,`Secchi (CM)`)%>%
  rename(Catch = `Smelt Catch`)%>%
  mutate(Tow_volume=185,
         Secchi = `Secchi (CM)`,
         WY = get_waterYear(Date),
         Station = as.character(Station),
         Source = "SLS",
         Date = as_date(Date),
         Catch = if_else(is.na(Catch)==T,0,Catch))%>%
  replace_na(list(`Sample Status`="Not Processed",WY=2026))%>%
  filter(is.na(Station)==F,`Sample Status`!="Not Sampled")%>%
  select(-`Secchi (CM)`)

#Combine all survey data frames into single object. Adjust as needed based on 
#current number of surveys 

newData <- bind_rows(survey1,survey2,survey3,survey4,survey5,survey6)%>%
  mutate(SampleID = paste("SLS New", seq(1:n())))


#Set up spatial data
library(sf)

#A delta base layer
marsh <- sf::st_read(
  "C:/Users/40545/Documents/GitHub/integrated-delta-fish-surveys/DATA/SpatialData/hydro-delta-marsh/hydro_delta_marsh.shp")

#EDSM subregion polygons
SubRegions<- st_read(
  "C:/Users/40545/Documents/GitHub/integrated-delta-fish-surveys/Analyses/LFS Entrainment Modeling/data/fmwt_regions_contiguous/fmwt_regions_contiguous.shp")%>%
  select(-c(Shape_Leng,Shape_Area))%>%
  st_transform(crs=st_crs(marsh))

label_pts <- st_point_on_surface(SubRegions)

#Cross reference sampling stations with EDSM polygons
EDSM_Strata <- st_intersection(deltamapr::R_EDSM_Strata_1718P1%>%
                                 st_transform(crs=st_crs(marsh)),label_pts)%>%
  data.frame()%>%
  select(-geometry)%>%
  rename(SubRegion=Name)

#Create a temporary spatial object from data frame of SLS and 20mm stations
Stations_Temp <-dataLFS_temp%>%
  select(Source,Station,Longitude,Latitude)%>%
  distinct()%>%
  filter(is.na(Latitude)==F)%>%
  st_as_sf( coords = c("Longitude", "Latitude"), 
            crs = st_crs(marsh), agr = "constant")

#Create a final table of stations with EDSM subregion included
Stations <- st_intersection(Stations_Temp,SubRegions)%>%
  data.frame()%>%
  select(-geometry)%>%
  rename(SubRegion=Name)


#Make final LFS data frame with subregion included
dataLFS <- dataLFS_temp%>%
  mutate(Secchi = Secchi*100)%>%
  mutate(`Sample Status`="Processed")%>%
  bind_rows(newData)%>%
  mutate(Station = str_remove(Station,fixed("*")))%>%
  left_join(Stations,by=c("Source","Station"))%>%
  group_by(WY,Survey)%>%
  mutate(survDate = min(Date,na.rm=T))%>%
  ungroup()

#Save a cross reference of subregion volume and EDSM strata
habVols%>%left_join(EDSM_Strata)%>%write_csv("Strata.csv")

#Summarize the proportion of samples processed for each survey
Processing_Report <- dataLFS%>%filter(WY==2026)%>%left_join(habVols%>%select(Region,SubRegion))%>%
  group_by(Survey,survDate,Region)%>%summarize(Percent_Processed = round(length(`Sample Status`[`Sample Status`=="Processed"])/
                                                                   n(),2))

  write.csv(Processing_Report,file="outputs/processing_summary.csv")
  
Sampled_Summary <- dataLFS%>%filter(WY==2026)%>%select(WY,Survey,Station,`Sample Status`)

#Make data compatible with Kimmerer Code

#Get station mean values for salinity to use if and until they can be provided in the updates
meanSal <- dataLFS%>%group_by(Station)%>%summarise(sal.surf = mean(Sal_surf,na.rm=T))

#Make data in required format for Kimmerer model
dataKimmerer <- dataLFS%>%
  filter(WY>2020,`Sample Status`=="Processed",Source=="SLS",Survey<10,
         #Remove survey 1 from 2022 because of missing secchi and small number of stations sampled
         !(WY==2022&Survey==1))%>%
  left_join(meanSal,by="Station")%>%
  mutate(sal.surf=if_else(is.na(Sal_surf)==F,Sal_surf,sal.surf))%>%
  select(SampleID,WY,Station,sal.surf,TurbidityNTU,Secchi,Tow_volume,Catch,survDate,Survey)%>%
  mutate(SampleID = as.numeric(paste0(WY,0,Survey,Station)),#Make sample ID numeric and Kimmerer format
         mo= month(survDate))%>%
  relocate(mo,.after=WY)%>%
  rename("sampleid"=SampleID,
         "year" = WY,
         "station" = Station,
         "turbidity" = TurbidityNTU,
         "secchi" = Secchi,
         "vfm3" = Tow_volume,
         "lonsme" = Catch,
         "date" = survDate,
         "survey" = Survey)%>%
  mutate(logsal = log(sal.surf),
         loglfs = log(lonsme),
         yearfact = as.character(year),
         loglfs = if_else(is.infinite(loglfs)==T,0,loglfs),
         date=as_date(date))%>%
  group_by(year)%>%
  mutate(iyear = cur_group_id()+12)%>%
  ungroup()%>%
  arrange(date)%>%
  group_by(year,survey)%>%
  mutate(isurv = cur_group_id()+68,
         survey = isurv,
         station = as.numeric(station),
         survfact = as.character(survey))%>% #just following Kimmerer template, survey and isurvey are the same
  ungroup()%>%
  #Removing rows with missing values
  filter(is.na(secchi)==F
         )%>%
  relocate(survfact,.after=yearfact)

#Extract dates for Kimmerer model
kimDates <- dataKimmerer%>%
  mutate(date=as_date(date))%>%
  pull(date)%>%unique()


#===============================================================================
#Get export data
load("data/Water_Data.rda")

Water_Data <- 
    CDECRetrieve::cdec_query("TRP", "70", "D", "2019-01-01")%>%
      rename("CVP" = "parameter_value",
             "date" = "datetime")%>%
      mutate(date = as_date(date))%>%
      select(date,CVP)%>%
  left_join(
    CDECRetrieve::cdec_query("HRO", "70", "D", "2019-01-01")%>%
      rename("SWP" = "parameter_value",
             "date" = "datetime")%>%
      mutate(date = as_date(date))%>%
      select(date,SWP), by="date")%>%
  filter(is.na(CVP)==F)%>%
  mutate(exp = (SWP+CVP)*0.028317)

slsDate_New <- Water_Data%>%
  filter(date%in%kimDates)%>%
  select(-CVP,-SWP)%>%
  mutate(year = year(date),
         expDailym3 = exp*86400)%>%
  filter(year>2020)%>%
  arrange(date)%>%
  mutate(x2 = NA,
         survey = row_number()+68)

#===============================================================================
#Replace missing secchi and salinity values in 2026 data for Kimmerer 28 stations
#Make sure complete data even when stations are missing


staVolSalinity_New <- dataKimmerer%>%select(survey,station,year,lonsme,logsal,secchi)%>%
  arrange(survey,station)%>%
  left_join(data.frame(station=c(405, 411, 418, 501, 504, 508, 513, 519, 520, 602, 703, 704, 705, 706, 707, 
                                 711, 716, 723, 801, 804, 809, 812, 815, 901, 902, 914, 915, 918),
                       sta = c(1,2,3,5,6,7,9,8,10,4,12:19,11,20:28),
                       newValues = c(504,504,504,418,NA,NA,NA,504,NA,NA,NA,NA,NA,NA,NA,707,707,707,NA,NA,NA,NA,NA,815,NA,NA,NA,NA),
            volm3 = c(216441474.6, 61642003.74, 85454088.77, 49254005.17, 75384637.29, 72135874.82, 68590646.73,
                      83015433.19, 15976666.95, 121261764.1, 36572451.97, 26573596.48, 25024910.14, 26947000.92,
                      43597540.04, 52095772.89, 54467789.81, 27571703.21, 53596381.54, 57515378.48, 83180350.67,
                      54903364.23, 129887484.8, 49912684.34, 44610142.94, 81776064.28, 31150186.93, 18597361.37)))%>%
  relocate(sta,newValues,volm3,.after=station)%>%
  filter(is.na(sta)==F)%>%
  complete(station,survey)%>%
  #fill in NA values created by Complete
  group_by(station)%>%
  mutate(sta = if_else(is.na(sta)==T,mean(sta,na.rm=T),sta),
         volm3 = if_else(is.na(volm3)==T,mean(volm3,na.rm=T),volm3))%>%
  group_by(survey)%>%
  mutate(year=if_else(is.na(year)==T,mean(year,na.rm=T),year))%>%
  group_by(station,year)%>%
  mutate(logsal = if_else(is.na(logsal)==T,mean(logsal,na.rm=T),logsal),
         secchi = if_else(is.na(secchi)==T,round(mean(secchi,na.rm=T)),secchi))%>%
  arrange(survey,station)

#Save new JAGS data as multi-tab excel workbook

wb <- createWorkbook()

addWorksheet(wb, "slstoJAGS")
writeData(wb, "slstoJAGS", dataKimmerer)

addWorksheet(wb, "staVolSalinity")
writeData(wb, "staVolSalinity", staVolSalinity_New)

addWorksheet(wb, "slsDate")
writeData(wb, "slsDate", slsDate_New)

saveWorkbook(wb, "data/JAGSdat_NewYearsOnly.xlsx", overwrite = TRUE)


           