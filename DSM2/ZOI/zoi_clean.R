# contour_maps_inflow.R ######
# Updated: 10/17/2023
# Catarina Pien and Lisa Elliott (USBR)
# cpien@usbr.gov; lelliott@usbr.gov

#  This code uses zone of influence modeling results (DSM2) to create contours showing
#  how zone of influence changes from operational facilities based on pumping
#  Contour lines of a specific level are then compared between different flow levels
#  to indicate how changing OMR will influence the zone of influence.
args <- commandArgs(trailingOnly = TRUE)
#print(commandArgs(trailingOnly=TRUE))
Week <- args[1]
#print(Week)
InflowBin_W1 <- args[2]
InflowBin_W2 <- args[3]
InflowBin_W3 <-args[4]
Week1_dates <-args[5]
Week2_dates <-args[6]
Week3_dates <-args[7]
SacramentoFlow_W1 <-args[8]
SanJoaquinFlow_W1 <-args[9]
SacramentoFlow_W2 <-args[10]
SanJoaquinFlow_W2 <-args[11]
SacramentoFlow_W3 <-args[12]
SanJoaquinFlow_W3 <-args[13]



#########################################################################
####loading libraries
##################################################################
#dir.create("C:/Rlibs/4.1", recursive = TRUE, showWarnings = FALSE)
#.libPaths(c("C:/Rlibs/4.1", .libPaths()))
#.libPaths()
library(here)
library(remotes)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(knitr)
library(tidyr)
library(sf)
library(stars)
library(readr)
library(janitor)
library(sp)
library(gstat)
library(raster)
library(tmap)
#library(rgdal)
library(ggmap)
library(ggspatial)
library(deltamapr)
library(viridis)
library(randomcoloR)


################################################################################
# Functions ------------------------------------
############################################################################
# 1. Create data frame for each month and flow level
# @group = inflow group
# @flow = OMR flow group
# produces an "sp" object
create_df <- function(groupname, flow) {
  group_data <- zoi_channel_long %>%
    dplyr::select(OMR_Flow, node, channel_number, length_feet, upnode, downnode, group, overlap)
  group_node <- inner_join(nodes_4326, group_data) %>%
    mutate(overlap = ifelse(overlap<0, NA, overlap)) %>%
    na.omit()# may be some missing

  df_filtered <- group_node %>% filter(OMR_Flow == flow,
                                       group == groupname)
  print(paste("Group:", groupname, "Flow:", flow))
  print(paste("Rows after filter:", nrow(df_filtered)))
  df_filt_sp <- as(df_filtered, "Spatial")
  
  # return(df_filtered)
  return(df_filt_sp)
}


# 2. create spatial interpolations of proportion overlap from data frame within delta mask
# @df = data frame
# @mask = spatial shapefile mask
# produces a raster
interp_nodes <- function(df, mask=delta_sp) {
  
  grd <- as.data.frame(spsample(df, "regular", n = 50000))
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add projection information to the empty grid
  proj4string(df) <- proj4string(df) # Temp fix until new proj env is adopted
  proj4string(grd) <- proj4string(df)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  group.idw <- gstat::idw(overlap ~ 1, df, newdata=grd, idp=2.0)
  
  # Convert to raster object then clip to Delta
  raster       <- raster(group.idw)
  raster_mask     <- mask(raster, mask)
  
  return(raster_mask)
  
}

# 3. Contour making function
# @inflow_list = list of inflow rasters
# @inflow_group = inflow group name (e.g. lolo)
# combines contours for one inflow group into one place
# arranges contours in the correct order and groups by contour level
# add OMR names back in
# # produces data frame of contour lines
create_contour <- function(inflow_list, inflow_group) {
  c1 <- mapply(rasterToContour, inflow_list, levels = 0.75)
  c2 <- mapply(rasterToContour, inflow_list, levels = 0.25)
  cont <- rbind(c1, c2)
  contours_lolo <- lapply(cont, fortify) %>%
    bind_rows(.id = "id") %>%
    mutate(group2 = inflow_group) %>%
    mutate(flow = case_when(id %in% c(1,2) ~ "-2000",
                            id %in% c(3,4) ~ "-3500",
                            id %in% c(5,6) ~ "-5000",
                            id %in% c(7,8) ~ "-6500"),
           contour = case_when(id %in% c(1,3, 5, 7) ~ 0.75,
                               id %in% c(2,4,6, 8) ~ 0.25))
}


# 4. Map making function
# @group = inflow group
# @clevel = proportion overlap contour level (0.75, 0.25)
# produces map
make_map <- function(grp, clevel){
  
  # Make a dataset that filters contours for group and contour level of interest. This needs to run
  # after you have already made the contours_all file (run all the contours and combine)
  contourGroup <- contours_all %>%
    filter(group2 == grp & contour == clevel) %>%
    mutate(grouper = paste0(group, "_", flow))
  # Map not including basemap
  (ggplot() +
      geom_sf(data = delta_4326, fill = NA, inherit.aes = FALSE, linetype = "dashed") +
      geom_sf(data = WW_Delta_crop, fill = "lightskyblue2", color = "lightskyblue2", alpha = 0.7, inherit.aes = FALSE) +
      geom_sf(data = nodes_4326, size = 0.4, color = "gray30", inherit.aes = FALSE) +
      geom_path(data = contourGroup, aes(x = long, y = lat, group  = grouper, color= OMR_flow), linewidth = 1, inherit.aes = FALSE) +
      annotation_north_arrow(location = "tr", which_north = "true",
                             pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                             style = north_arrow_fancy_orienteering) +
      annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
      scale_color_manual("OMR Flow (cfs)", values = cpal[c(1,2,3,4,5)]) +
      labs(title = paste(grp, "contour", clevel))+
      theme_classic())
}


Z0I_path <- file.path(here(Week,"output_csvs"))
print(Z0I_path)
setwd(file.path(here(Week)))
###################################################################################
# Read/Join data ---------------------------------------------------------
####################################################################################
delta <- st_read(here("ZOI_Shapefiles/BDO-Science contour-zone-of-influence main shapefiles/Bay_Delta_Poly_New.shp"))

weeks = paste0("week",1:3)
for (i in weeks){

#listing all zoi csv files within the directory
zoi_file_NAA= list.files(Z0I_path, pattern = paste0("DEZOI_OMR.*", i,".*csv$"), full.names = TRUE)
# Read all ZOI CSV files into a single dataframe, add a file-based identifier,
# and assign an OMR flow category based on keywords in each source filename.
# The OMR_Flow value is parsed from the filename (e.g., Neg6500, Neg5000, etc.)
# and stored as a character variable for downstream grouping and plotting.
zoi_data <- lapply(zoi_file_NAA, read_csv) %>%
  bind_rows(.id = "id") %>%
  mutate(
    file = zoi_file_NAA[as.numeric(id)],
    OMR_Flow = dplyr::case_when(
      stringr::str_detect(file, "Neg6500") ~ "-6500",
      stringr::str_detect(file, "Neg2000") ~ "-2000",
      stringr::str_detect(file, "Neg3500") ~ "-3500",
      stringr::str_detect(file, "Neg5000") ~ "-5000",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(-file)

#reading nodes shapefile
nodes <- st_read(here("ZOI_Shapefiles/BDO-Science contour-zone-of-influence main shapefiles/nodes.shp")) %>%
  dplyr::select(node)
#transform the coordinate
nodes_4326 <- st_transform(nodes, crs = 4326) %>%
  mutate(points = "DSM2 nodes")
#reading channel csv file
channels0 <- read_csv(here("ZOI_Shapefiles/channel_std_delta_grid_NAVD.csv")) %>%
  janitor::clean_names()  %>%
  rename(channel_number = chan_no)%>%
  rename(length_feet= length)

# Drop nodes that are causing issues
dropNodes <- c(146, 147, 148, 206, 242, 246, 432, 433, 434)
# Drop duplicate channels
channels1 <- channels0[!channels0$upnode %in% dropNodes, ]
channels <- channels1[!channels1$downnode %in% dropNodes, ]
#channels <- channels0
total_channel_length <- sum(channels$length_feet)

# Join channel lengths with zoi data
zoi_channel <- left_join(zoi_data, channels)
zoi_channel <- merge(zoi_data, channels, by = "channel_number")

# Change projections to 4326 (WGS)
delta_4326 <- st_transform(delta, crs = 4326) %>%
  mutate(line = "analysis boundary")
nodes_4326 <- st_transform(nodes, crs = 4326) %>%
  mutate(points = "DSM2 nodes")
WW_Delta_4326 <- st_transform(WW_Delta, crs = st_crs(delta_4326))
WW_Delta_crop <- st_crop(WW_Delta_4326,xmin = -122.2, xmax = -121, ymin = 37.5, ymax = 38.8) %>%
  filter(HNAME!= "SAN FRANCISCO BAY")

# Convert data frame to long
# Create a modified version of the ZOI channel dataset by assigning a fixed
# grouping label ("himed") and renaming the Proportion_Ratio field to overlap
# for consistency with other datasets and downstream analysis.

if (i == "week1") {
  inflowbin <- InflowBin_W1
  Week_dates <- Week1_dates
  Sac_Flow <- SacramentoFlow_W1
  SanJoan_Flow <-SanJoaquinFlow_W1
} else if (i == "week2") {
  inflowbin <- InflowBin_W2
  Week_dates <- Week2_dates
  Sac_Flow <- SacramentoFlow_W2
  SanJoan_Flow <-SanJoaquinFlow_W2
} else {
  inflowbin <- InflowBin_W3
  Week_dates <- Week3_dates
  Sac_Flow <- SacramentoFlow_W3
  SanJoan_Flow <-SanJoaquinFlow_W3
}



zoi_channel_long <- zoi_channel %>%
  dplyr::mutate(group = inflowbin) %>%
  dplyr::rename(overlap = Proportion_Ratio)
cat("\nAvailable OMR_Flow values:\n")
print(unique(zoi_channel_long$OMR_Flow))

cat("\nAvailable group values:\n")
print(unique(zoi_channel_long$group))

# Look at data
summary_vals <- zoi_channel_long %>%
  filter(overlap>0) %>%
  group_by(group, OMR_Flow) %>%
  summarize(min = min(overlap),
            max = max(overlap),
            mean = mean(overlap)) %>%
  ungroup()


########################################################################
# Analysis ------------------------------
##########################################################################
## 1. Create data frames for each inflow-OMR group --------------------

### himed --------------
himed_2000_sp <- create_df(groupname = inflowbin, flow = "-2000")
himed_3500_sp <- create_df(groupname = inflowbin, flow = "-3500")
himed_5000_sp <- create_df(groupname = inflowbin, flow = "-5000")
himed_6500_sp <- create_df(groupname = inflowbin, flow = "-6500")


## 2. Create interpolations (takes a little bit) ------------------

# Turn delta shapefile into spatial polygon object
delta_sp <- as(delta_4326, "Spatial")


r.himed1 <- interp_nodes(himed_2000_sp)
r.himed2 <- interp_nodes(himed_3500_sp)
r.himed3 <- interp_nodes(himed_5000_sp)
r.himed4 <- interp_nodes(himed_6500_sp)


## 3. Create contours --------------------------

# 0.25 represents contour at which 25% overlap exists (less similar)
# 0.75 represents contour at which 75% overlap exists (more similar)

### 3.1. make list of contours for each inflow group ------

himed_list <- c(r.himed1, r.himed2, r.himed3, r.himed4)

### 3.2. create contours ------
contours_himed <- create_contour(himed_list, inflow_group = inflowbin)

### 3.3. Combine all contours ------------
# switching factor levels will allow color palette to work right
contours_all <- rbind( contours_himed) %>%
  mutate(OMR_flow = factor(flow, levels = c("-2000", "-3500", "-5000","-6500")))



#################################################################
# Make maps -----------------------
###########################################################################
# https://stackoverflow.com/questions/34153462/plot-spatiallinesdataframe-with-ggplot2
# Define color palette
cpal <- RColorBrewer::brewer.pal(6, "YlOrBr")[2:6]
#install.packages("randomcoloR")
library(randomcoloR)
n <- 15
palette <- distinctColorPalette(n)


## 0.75 ---------------
inflow_order = c("lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi")


# Make one file with all inflow groups
contourGroup <- contours_all %>%
  filter(contour == 0.25) %>%
  mutate(grouper = paste0(group, "_", flow, group2),
         label = paste0(group2, "_", flow)) %>%
  rename(Inflow = group2) %>%
  mutate(Inflow = factor(Inflow, levels = inflow_order))


## 0.75 ---------

contourGroup2 <- contours_all %>%
  filter(contour == 0.75) %>%
  mutate(grouper = paste0(group, "_", flow, group2),
         label = paste0(group2, "_", flow))%>%
  rename(Inflow = group2) %>%
  mutate(Inflow = factor(Inflow, levels = inflow_order))





### Individual facets by inflow group --------------zoomin
(map_75_fhihi <- ggplot() +
    geom_sf(data = WW_Delta_crop, fill = "gray90", color = "gray70", alpha = 0.7, inherit.aes = FALSE) +
    geom_path(data = contourGroup2 %>% filter(Inflow==inflowbin), aes(x = long, y = lat, group  = grouper, color= OMR_flow, linetype = OMR_flow), linewidth = 1.25, inherit.aes = FALSE) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    ylim(c(37.7, 38.1)) +
    xlim(c(-121.8, -121.25)) +
    coord_sf(expand = FALSE) +
    scale_color_viridis_d("OMR Flow (cfs)") +
    scale_linetype_discrete("OMR Flow (cfs)") +
    labs(title = paste0("0.75 Contour\n",paste0(Week_dates,"\n"), paste0("Sacramento Flow = ",Sac_Flow," cfs\n"),
                        paste0("San Joaquin Flow = ",SanJoan_Flow, " cfs\n"), paste0("Inflow bin = ", inflowbin)))+
    theme_classic()+
    theme(axis.text = element_text(size = 11),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5),
          axis.title = element_blank(),
          legend.position = "top",
          legend.box = "vertical"))


  ggsave(filename = paste0("0.75Contour_",i,".png"),
         plot = map_75_fhihi,
         width = 6.077,
         height = 7.2,
         dpi = 300,
         limitsize= FALSE)
		 
  # >>>>> ADDED FOR 0.75 CONTOUR SHAPEFILE EXPORT >>>>>
  # Create a separate output folder for shapefile components for this week
  shp_dir <- file.path(getwd(), paste0("0.75Contour_", i, "_shapefile"))
  dir.create(shp_dir, showWarnings = FALSE, recursive = TRUE)
  # >>>>> ADDED FOR 0.75 CONTOUR SHAPEFILE EXPORT >>>>>
  # Rebuild the 0.75 contours directly from rasters as spatial objects
  contour_75_list <- list(
    `-2000` = rasterToContour(r.himed1, levels = 0.75),
    `-3500` = rasterToContour(r.himed2, levels = 0.75),
    `-5000` = rasterToContour(r.himed3, levels = 0.75),
    `-6500` = rasterToContour(r.himed4, levels = 0.75)
  )
  # >>>>> ADDED FOR 0.75 CONTOUR SHAPEFILE EXPORT >>>>>
  # Convert each contour to sf and attach attributes
  contour_75_sf_list <- lapply(names(contour_75_list), function(flow_name) {
    this_sf <- st_as_sf(contour_75_list[[flow_name]])
    this_sf$OMR_flow <- flow_name
    this_sf$InflowBin <- inflowbin
    this_sf$Week <- i
    this_sf$Week_dates <- Week_dates
    this_sf$ContourLevel <- 0.75
    this_sf$SacFlow_cfs <- Sac_Flow
    this_sf$SanJoaquinFlow_cfs <- SanJoan_Flow
    this_sf
  })
  # >>>>> ADDED FOR 0.75 CONTOUR SHAPEFILE EXPORT >>>>>
  # Combine all OMR-flow contour lines for this week into one shapefile
  contour_75_sf <- do.call(rbind, contour_75_sf_list)
  # >>>>> ADDED FOR 0.75 CONTOUR SHAPEFILE EXPORT >>>>>
  # Write shapefile
  st_write(
    contour_75_sf,
    dsn = file.path(shp_dir, paste0("0.75Contour_", i, ".shp")),
    delete_layer = TRUE,
    quiet = TRUE
  )
}


