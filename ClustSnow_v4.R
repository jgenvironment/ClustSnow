################################################################################################
####### This script runs the ClustSnow workflow first presented in Geissler et al. (2023) ######
####### created by Joschka Geissler; Version 4.0, last updated 2024-12-30                 ######
################################################################################################

#-------------------------------------------------------
# Load Functions and define working directory
# NOTE: This Rscript must be placed in the same directory as the ClustSnow_functions script.
#-------------------------------------------------------
library(raster)
library(readxl)
library(stringr)
library(ggplot2)
library(caret)
library(nixmass)

working_directory <- dirname(rstudioapi::getSourceEditorContext()$path) # Get Location of Rscript
source(paste0(working_directory,'\\ClustSnow_Functions_v4.R'))

# 1) Enter path to input data. dir_in should contain the following files. See ReadMe for more info.
#              -> 'hs_time_series.csv'       (HS Time Series with dates in first column and Location_ID as header.)
#              -> 'hs_sensor_location.shp'   (Sensor Locations as .shp-file containing an Location_ID attribute. )
#              -> 'hs_raster_stack.tif'      (LiDAR snow distribution data, stacked into one .tif file)

dir_in <- paste0(working_directory,"\\Data_ClustSnow\\Input") 
dir_out <- paste0(working_directory,"\\Data_ClustSnow\\Output") 

# Enter date format of hs_time_series.csv file.
date_format <- '%d.%m.%Y'

# Enter attribute name of hs_sensor_location shapefile that equals the headings of hs_time_series.csv.
name_attribute <- 'Bez'

# Choose the calibration data that should be used to set the parameters of ClustSnow and the delta.snow.model.
calibration_data <- getCalibrationData()
calibration_data <- calibration_data[calibration_data$site == select.list(c('AL','SL'),'AL',title = 'Select Calibration Dataset!'),]

#---------------------------------------------------------------------------------------------------------------------------#
### Workflow Preparation ###

setwd(dir_in) # Set Working Directory
nclass <- calibration_data$n_class

# Load Data
hs_sensor_location <- shapefile(list.files(dir_in,pattern = 'hs_sensor_location.shp',full.names = T))  # Location of measured time series
hs_time_series <- read.csv(list.files(dir_in,pattern = 'hs_time_series.csv',full.names = T),sep=';')   # Snow depth time series
hs_raster_stack <- stack(list.files(dir_in,pattern = 'hs_raster_stack.tif',full.names = T))            # Spatial snow depth maps

# Apply workflow
# 1a) Get cluster
set.seed(123)
rf_raster <- getCluster(calibration_data,data_for_prediction=hs_raster_stack)

# 1b) Order clusters based on mean observed snow depths.
rf_raster <- orderCluster(rf_raster,hs_raster_stack)

# 1c) Save clusters
writeRaster(rf_raster,paste0(dir_out,'\\cluster.tif'))

# 2a) Derive synthetic time series for each cluster based on observed time series.
HS.synth <- hs.synth(
  timeseries=hs_time_series,
  cluster_map=rf_raster,
  snomos=hs_sensor_location,
  nclass=calibration_data$n_class)

# 2b) Save synthetic snow depth time series.
write.csv(HS.synth,paste0(dir_out,'\\hs_synth.csv'))

# 3a ) Derive synthetic SWE time series from snow depth time series using delta.snow.model (Winkler et al. 2021)

# Data check
is_continous <- all(seq.Date(as.Date(HS.synth$dates[1],format = date_format),as.Date(HS.synth$dates[nrow(HS.synth)],format = date_format),by='days')==
                      as.Date(HS.synth$dates,format = date_format))
if(!is_continous){
  stop('snow depth time series must be continous!')
}
SWE.synth <- apply.delta.Snow.Model(calibration_data,HS.synth)

# 3b) save synthetic SWE time series.
write.csv(SWE.synth,paste0(dir_out,'\\swe_synth.csv'))

# 4a) Cluster-based spatial extrapolation of synthetic time series.
hs_daily <- create_Map(class_result = rf_raster,
                       timeseries = HS.synth,
                       clusterID_to_timeseriesID=data.frame(class=1:nclass,class_bez=paste('X',1:nclass,sep='')))

# 4b) Save extrapolated snow depth maps.
writeRaster(hs_daily,paste0(dir_out,'\\hs_daily.tif'))

# 4c) Cluster-based spatial extrapolation of synthetic time series.
swe_daily <- create_Map(class_result = rf_raster,
                        timeseries = SWE.synth,
                        clusterID_to_timeseriesID=data.frame(class=1:nclass,class_bez=paste('X',1:nclass,sep='')))

# 4b) Save extrapolated SWE maps.
writeRaster(swe_daily,paste0(dir_out,'\\swe_daily.tif'),overwrite=T)
