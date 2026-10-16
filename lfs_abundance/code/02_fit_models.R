#02_fit_models.R

#Using data objects created in 01_data_prep.R

#Optional: If data prep has not been previously run
source("code/01_data_prep.R")

############################################################
#This first section is based on the code provided by Wim Kimmerer and updates
#the estimates reported in his 2022 paper. 

#==========================================================
#
#    SLS R code to setup JAGS run.R
#    
#==========================================================
# This code is specific to the Bayesian analysis of catch data
# on longfin smelt in the San Francisco Estuary, as described
# by Kimmerer and Gross (2022) Population abundance and 
# diversion losses in a threatened estuarine pelagic fish. 
# Submitted to Estuaries and Coasts.
# 
# Data are for larval longfin smelt and comprise catches in
# a sampling program ("Smelt Larva Survey") run by the
# California Department of Fish and Wildlife beginning in 2009.
#==========================================================
# Inputs are provided in a file "slsJAGSInput.xlsx"
# which has three sheets of data that can all be read in at once
# using the "read.xlsx" function in the "openxlsx" library
# Metadata is in Metadata SLS Data prepared for JAGS.docx
# Output is from a JAGS file: 
# "SLS Jags Code Fit catch to log sal and secchi Rev 2022-01-03.txt"
# This code calls that file to conduct the analysis.
# 


#Clean up

rm(list = setdiff(ls(), c("dataKimmerer","slsDate_New","staVolSalinity_New","dataLFS","Design_Based","habVols","Sampled_Summary")))


names <- getSheetNames("data/slsJAGSInput.xlsx")

for (nmi in names[1:3]){
  cat(nmi, "\t")
  xtemp <- read.xlsx("data/slsJAGSInput.xlsx", sheet=nmi)
  assign(nmi, xtemp)
}

#Fix variable types
slsToJAGS$yearfact <- as.character(slsToJAGS$yearfact)
slsToJAGS$survfact <- as.character(slsToJAGS$survfact)
slsToJAGS$date <- as.Date(slsToJAGS$date, origin = "1899-12-30")
slsDate$date <- as.Date(slsDate$date, origin = "1899-12-30")
slsDate_New$x2 <- as.numeric(slsDate_New$x2)

#remove extra stations in new data
kimStations <- slsToJAGS%>%filter(year==2019)%>%select(station)%>%distinct()%>%pull(station)

slsToJAGS_New <- dataKimmerer%>%filter(station%in%kimStations)

#Bind old and new data
slsToJAGS <- bind_rows(slsToJAGS,slsToJAGS_New)
slsDate <- bind_rows(slsDate,slsDate_New)
staVolSalinity <- bind_rows(staVolSalinity, staVolSalinity_New%>%arrange(survey,station))


#---------------------------------
# Extract parameters

yrs       <- unique(slsToJAGS$year)
nYear     <- length(yrs)
iyear     <- 1:nYear
nSample   <- nrow(slsToJAGS)
nSurv     <- length(unique(slsToJAGS$isurv))
nSurvYr   <- unique(slsToJAGS[,c("year","survey")])
yrStart   <- aggregate(nSurvYr$survey, list(nSurvYr$year), FUN=head,1)[,2]
yrEnd     <- aggregate(nSurvYr$survey, list(nSurvYr$year), FUN=tail,1)[,2]


#-------------------------------
# Set up JAGS run

source("code/JAGS post-processing functions.txt")

data       <- list(nSample    = nSample, 
                   nSurv      = nSurv,
                   nYear      = nYear,
                   isurv      = slsToJAGS$isurv,
                   logsal     = slsToJAGS$logsal,
                   logsal2    = slsToJAGS$logsal^2,
                   secchi     = slsToJAGS$secchi,
                   catch      = slsToJAGS$lonsme,
                   nsampNew   = nrow(staVolSalinity),
                   survNew    = staVolSalinity$survey,
                   logsalNew  = staVolSalinity$logsal,
                   logsalNew2 = staVolSalinity$logsal^2,
                   secchiNew  = staVolSalinity$secchi,
                   staVolume  = staVolSalinity$volm3,
                   sampleVol  = rep(187, 3051), #Have to change these to be same as nrow(staVolSalinity)
                   expm3d     = slsDate$expDailym3,
                   survStart  = seq(1,  3051, by=28), #Have to change these to be same as nrow(staVolSalinity)
                   survEnd    = seq(28, 3051, by=28), #Have to change these to be same as nrow(staVolSalinity)
                   deltaStart = seq(25, 3051, by=28), #Have to change these to be same as nrow(staVolSalinity)
                   yrStart    = yrStart,
                   yrEnd      = yrEnd

)

parameters <- c("alpha", "a","b","b2","sec",
                "Predict","resid",
                "predictNew", "meanPred","predDensity",
                "staPopSize","survPopSize",
                "exportPop","dailyLoss","dailyPLoss",
                "mnPopSize","mnDailyLoss","mnDailyPLoss"
)  

inits <- NULL

inits <- 
  list(list("a"=rep(3,nSurv), "b"=rep(0.24,nSurv), "b2"=rep(-0.1, nSurv), "sec" = rep(-0.02, nSurv), "alpha"= runif(1,0.5,1.5)),
       list("a"=rep(3,nSurv), "sec" = rep(-0.02, nSurv), "b"=rep(0.24,nSurv), "b2"=rep(-0.1, nSurv), "alpha"= runif(1, 1,4)),
       list("a"=rep(3,nSurv), "b"=rep(0.24,nSurv), "b2"=rep(-0.1, nSurv), "sec" = rep(-0.02, nSurv), "alpha"= runif(1, 0.5, 3)) )

#Set the MCMC parameters here. Time to run increase substantially with nb, ni and nc

nt <- 10     # Thinning ratio
nb <- 500   # Length of burn-in before thinning 
ni <- 2000   # Length of run INCLUDING burn-in
nc <- 3      # Number of chains (replicate series of MCMC iterations)


# Run JAGS  
# This can take a few hours so run it only when necessary data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCC. Also 

jagsRunName    <- "code/SLS Jags Code Fit catch to log sal and secchi Rev 2022-01-03.txt"


  out <- jags(data=data, 
              inits=inits, 
              parameters, 
              jagsRunName,
              n.chains = nc,            
              n.thin = nt, 
              n.iter = ni,
              n.burnin = nb)
  
  save(out, file="outputs/SLS JAGS Long 2026-04-21.Rdata")  # pick any name
  
#==========================================================
# End of Kimmerer Model Fitting
#==========================================================  
####################################################################
 
#Load an existing JAGS model output 
load("outputs/SLS JAGS Long 2026-04-21.rdata")


#Extract mean population estimate
meanPop <- staVolSalinity%>%mutate(meanEst = out$mean$staPopSize)

#Plot survival estimates through time
meanPop%>%group_by(survey,year)%>%summarize(survPop = (sum(meanEst)))%>%
  ggplot(aes(x=survey,y=survPop,fill=as.factor(year)))+geom_bar(stat="identity")

#Save updated Kimmerer abundance estimates as .csv
meanPop%>%
  filter(year>2024)%>%
  select(year,survey,station,meanEst)%>%
  rename("Station" = "station",
         "Survey" = "survey")%>%
  arrange(Survey)%>%
  mutate(Survey = Survey-97)%>%
  left_join(Sampled_Summary%>%mutate(Station=as.numeric(Station)),by=c("Survey","Station"))%>%
  replace_na(list(`Sample Status` = "Not Sampled",
                  WY=2026))%>%
  write.csv(file="Outputs/kimmerer_2026_April21.csv")


############################################################################
#####################Design-based abundance estimates#######################
############################################################################


#Estimate regional larval LFS abundance from Smelt Larval Survey data and 
#Calculate the proportion of larvae in areas of elevated entrainment risk
#This is the design-based implementation that expands regional catch based on 
#estimated total water volumes and CPUV

#===========================================================================
options(scipen = 999)

#Caclulate design-based abundance estimate at coarse spatial scale
coarse<- Design_Based(dataLFS%>%filter(Source=="SLS",WY>2020),
                      Final_Grouping = c("survDate","WY","Source","Survey","Region"))#%>%
# mutate(survDate = as_date(survDate))%>%
# #mutate(Risk = if_else(Region=="South Delta",T,F))%>%
# filter(survDate>=as_date("2025-12-01"),is.na(Region)==F)%>%
# select(survDate,Survey,Region,Abundance,lower,upper)%>%
# pivot_wider(names_from=Region,values_from=c(Abundance,lower,upper))

#Caclulate design-based abundance estimate at fine spatial scale
fine<- Design_Based(dataLFS%>%filter(Source=="SLS",WY%in%c(2026)))%>%
  mutate(survDate = as_date(survDate))#%>%
#mutate(Risk = if_else(Region=="South Delta",T,F))%>%
#filter(survDate>=as_date("2025-12-01"),is.na(SubRegion)==F)%>%
#select(survDate,Survey,Region,Abundance,lower,upper)%>%
#pivot_wider(names_from=Region,values_from=c(Abundance,lower,upper))

#Summarize the design-based outputs
db <- coarse%>%filter(WY>2022,Survey<10)%>%group_by(WY,Survey)%>%summarise(Est=log10(sum(Abundance)))%>%pull(Est)

#Extract the population mean estimates from the Kimmerer model output
kim <- meanPop%>%filter(year>2022)%>%group_by(survey)%>%summarise(kim = log10(sum(meanEst)))%>%pull(kim)

#Look at a log-log scatterplot comparison of the methods
plot(db,kim)

#Combine abundance estimates and save as .csv
fine%>%select(survDate:Region,Abundance)%>%rename("Subregion_Abund"=Abundance)%>%left_join(coarse%>%select(survDate:Region,Abundance))%>%
  filter(WY==2026)%>%arrange(survDate)%>%rename("Regional_Abundance"=Abundance)%>%
  mutate(Subregion_Abund = round(Subregion_Abund),Regional_Abundance = round(Regional_Abundance))%>%
  write_csv("outputs/2026 LFS Abundances through SLS Survey 6 (Final)_April 21st.csv")

