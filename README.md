#####################
#### cluster_snow ###
#####################

Workflow presented in Geissler et al. (2023) to derive daily HS and SWE based on observations. SWE maps are only derived if the time series are continous and snow depth time series start with 0 m.
Please mind the directory structure required.
![grafik](https://user-images.githubusercontent.com/132678556/236832304-a619eb17-cb3a-4378-97ec-f2b7b1a42bd2.png)

Required Input data and format:

# TIME-SERIES
Name: hs_time_series.csv 

Seperator: ;

Delimiter: .

Please indicate the date format in the R-script.

![grafik](https://user-images.githubusercontent.com/132678556/236829160-d806ac74-130c-4aef-ac0c-e28c4d777bb8.png)

# Sensor Locations
Shapefile containing all sensor locations. Inticate the name of the attribute containing the sensor ids in the data input section of the R-Script. Sensor ids must correspond to the header of the time series.

Name: hs_sensor_location.shp

# HS maps
Raster stack containing spatial maps of snow depth, acquired for instance with UAV-based LiDAR. 

Name: hs_raster_stack.tif

###############################################################################################
##                              CloudCompare_CoregisterLiDAR.R                               ##
## Co-Registration of LiDAR-derived point clouds using Cloud Compare Command Line Mode.      ##
## Wrapper Script for running the workflow in Co-Registration Workflow in R.                 ##
## Note: Directories, AOI and filtering thresholds need to be adapted to individual dataset. ##
## More Info: Geissler et al. 2023                                                           ##
###############################################################################################
