########################################################################################
####### This script implements the workflow presented in Geissler et al. (2023)   ######
####### created by Joschka Geissler, 2023-05-05, Version: 1                       ######
########################################################################################

#---------------------------------------------------------------------------------------------------------------------------#

### USER INPUT ###

# 1) Enter path to input data. Path should contain the three following files. See ReadMe for more info.
#              -> 'hs_raster_stack.tif'      (LiDAR snow distribution data, stacked into one .tif file)
#              -> 'hs_time_series.csv'       (HS Time Series with dates in first column and Location_ID as header.)
#              -> 'hs_sensor_location.shp'   (Sensor Locations as .shp-file containing an Location_ID attribute. )

dir_in  <- '.\\Input'
dir_out <- '.\\Output'

# Enter date format of hs_time_series.csv
date_format <- '%d.%m.%Y'
name_attribute <- 'id_1'

#---------------------------------------------------------------------------------------------------------------------------#
### LOAD PACKAGES AND FUNCTIONS ###

#install.packages('raster')
library(raster)
library(readxl)
library(stringr)
library(ggplot2)
library(caret)
library(nixmass)

# Function to get Clusters för LiDAR HS maps (Section 2.2.2)
getCluster <- function(nclass,ntrees,sample_length,kmeans_maxiter,kmeans_nstart,data_for_prediction){
  
  # Prepare Data
  data_for_prediction <- data_for_prediction %>%
                                as.data.frame(xy=T)%>%
                                na.omit()
  
  subsample_kmeans <-data_for_prediction[sample(nrow(data_for_prediction),sample_length),] # Get data-subset for kMeans
  subsample_kmeans_nocoords <- subsample_kmeans[,3:length(subsample_kmeans)]
  
  data_for_prediction_nocoords <- data_for_prediction[,3:length(subsample_kmeans)]
  
  # Perform Unsupervised Classification
  E_rf <- kmeans(x=subsample_kmeans_nocoords,centers= nclass, iter.max = kmeans_maxiter, nstart = kmeans_nstart) # K-Means
  rf <- train(x = subsample_kmeans_nocoords, y = as.factor(E_rf$cluster), method="rf", ntree=ntrees,importance=T)# Train RandomForest
  rf_predict <- as.data.frame(predict(rf,data_for_prediction_nocoords,type='prob'))                              # Predict RandomForest
  
  # Reframe to Raster
  rf_predict$rf_predict <-  colnames(rf_predict)[apply(rf_predict,1,which.max)]
  rf_result <- cbind(data_for_prediction,rf_predict) 
  rf_result$rf_predict <- as.character(rf_result$rf_predict)
  rf_result <- rasterFromXYZ(rf_result)
  rf_result <- rf_result[[(nlayers(rf_result)-nclass):nlayers(rf_result)]]
  return(rf_result)
  
}

# Function to get synthetic timeseries for cluster (Section 2.2.3)
hs.synth <- function(timeseries,cluster_map,snomos,nclass){

  snomos_filtered<-snomos[snomos@data[,name_attribute]%in% colnames(timeseries),]
  snomos.df <- data.frame()
  snomos.df <- data.frame(class_bez=snomos_filtered@data[,name_attribute],cluster=raster::extract(cluster_map,snomos_filtered))
  snomos.df <- snomos.df[,c(1,(nlayers(cluster_map)-nclass):nlayers(cluster_map)+1)]
  snomos.df <- snomos.df[order(snomos.df$cluster.rf_predict),]
  snomos.df <- snomos.df[snomos.df$class_bez%in%colnames(timeseries),]
  snomos.df.rel <- snomos.df
  snomos.df.rel <- na.omit(snomos.df.rel)
  
  for (i in 2:(nclass+1)) {
    snomos.df.rel[,i] <- snomos.df.rel[,i]/sum(snomos.df.rel[,i],na.rm=T)
  }
  
  HS.synth <- data.frame(dates=timeseries[,str_detect(colnames(timeseries),pattern = 'date')])
  HS.synth[,paste('X',(1:nclass),sep='')]<-0
  
  for (SnoMoS in snomos.df.rel$class_bez) {
    p_snomos <- snomos.df.rel[snomos.df.rel$class_bez==SnoMoS,2:(nclass+1)]
    
    for (cluster in 1:nclass) {
      HS.synth[,paste('X',as.character(cluster),sep='')]<- HS.synth[,paste('X',as.character(cluster),sep='')]+
        as.numeric(p_snomos[cluster])*as.data.frame(timeseries[,SnoMoS])
      
    }
    
    
  }
  return(HS.synth)
  
}

# Function to create synthetic maps based on clusters and time series (Section 2.2.3)
create_Map  <- function(class_result,clusterID_to_timeseriesID,timeseries){

  result.stack <- stack()
  date <- timeseries[,str_detect(colnames(timeseries),pattern = 'date')]
  date <- as.POSIXlt(date,format=date_format,tz= "UTC")

  
  for (i in 1:length(date)) {
    day <- date[i]
    sensors <- colnames(timeseries[-1])
    
    data_snomos <-data.frame(t(timeseries[as.POSIXlt(as.character(timeseries$dates),format=date_format,tz= "UTC")%in%as.character(day),]))
    data <- data_snomos[-1,]
    data_snomos <- data.frame(class_bez=sensors,data=data)
    
    hsmap_data <- merge(data_snomos, clusterID_to_timeseriesID, by = "class_bez", all.x=TRUE, all.y=TRUE)
    hsmap_data <- na.omit(hsmap_data)
    
    
      prob_data <- as.data.frame(class_result,xy=T)
      prob_data.df <- prob_data[,c(1,2,(ncol(prob_data)-(nclass)):(ncol(prob_data)-1))]
      prob_data.df <- na.omit(prob_data.df)
      
      for (class in 1:nclass) {
        
        ncol <- class+2 
        hs <- hsmap_data$data[hsmap_data$class==class]
        
        prob_data.df[,ncol]<-prob_data.df[,ncol]* as.numeric(hs)
        
      }
      prob_data.df$hs <- apply(prob_data.df[,3:ncol(prob_data.df)],1,sum,na.rm=T)
      
      hs_map <- rasterFromXYZ(prob_data.df[,c(1,2,ncol(prob_data.df))])

    names(hs_map)<-paste(as.character(day),sep='')
    result.stack <- stack(result.stack,hs_map)
  }
  return(result.stack)
}

# Delta Snow
apply.delta.Snow.Model <- function(timeseries){
  require(nixmass)
  timeseries<-HS.synth
  timeseries$dates <- as.POSIXlt(timeseries$dates,format=date_format,tz= "UTC") 
  SWE.all <- as.data.frame(timeseries)

    for (col in 2:ncol(timeseries)) {
      swe_col <- swe.delta.snow(data.frame(date=as.character(SWE.all$dates),hs=as.numeric(unlist(timeseries[,get('col')]))),
                                rho.max  = get('rho.max',envir = .GlobalEnv),
                                rho.null = get('rho.null',envir = .GlobalEnv),
                                c.ov     = get('c.ov',envir = .GlobalEnv),
                                k.ov = get('k.ov',envir = .GlobalEnv),
                                k = get('k',envir = .GlobalEnv),
                                tau = get('tau',envir = .GlobalEnv),
                                eta.null = get('eta.null',envir = .GlobalEnv))
      
      SWE.all[,get('col')] <- swe_col 
    }

  
  return(SWE.all)
}


#---------------------------------------------------------------------------------------------------------------------------#
### Workflow Preparation ###

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # Set Working Directory

# Load Data
hs_sensor_location <- shapefile(list.files(dir_in,pattern = '.shp',full.names = T))
hs_raster_stack <- stack(list.files(dir_in,pattern = '.tif',full.names = T))
hs_time_series <- read.csv(list.files(dir_in,pattern = '.csv',full.names = T),sep=';')

# Workflow parameters
sample_length  = 1600
kmeans_maxiter = 16
kmeans_nstart  = 39
nclass         = 4
mtry           = 4
ntrees         = 451

# Apply Workflow
rf_raster <- getCluster(nclass,
                        ntrees,
                        sample_length,
                        kmeans_maxiter,
                        kmeans_nstart,
                        data_for_prediction=hs_raster_stack)

writeRaster(rf_raster,paste0(dir_out,'\\cluster.tif'))

# Perform SnoMoS Association
HS.synth <- hs.synth(
  timeseries=hs_time_series,
  cluster_map=rf_raster,
  snomos=hs_sensor_location,
  nclass)

write.csv(HS.synth,paste0(dir_out,'\\hs_synth.csv'))

# Derive Daily HS
hs_daily <- create_Map(class_result = rf_raster,
                       timeseries = HS.synth,
                       clusterID_to_timeseriesID=data.frame(class=1:nclass,class_bez=paste('X',1:nclass,sep='')))


writeRaster(hs_daily,paste0(dir_out,'\\hs_daily.tif'))

# Derive SWE.synth using delta.snow.model (Winkler et al. 2021)
# Model Parameters

is_continous <- all(seq.Date(as.Date(HS.synth$dates[1],format = date_format),as.Date(HS.synth$dates[nrow(HS.synth)],format = date_format),by='days')==
  as.Date(HS.synth$dates,format = date_format))
if(is_continous){

rho.max=422
rho.null=134 
c.ov=0.0005104722 
k.ov=9.2
k=0.028
tau=0.036
eta.null=20000000

SWE.synth <- apply.delta.Snow.Model(HS.synth)

write.csv(SWE.synth,paste0(dir_out,'\\swe_synth.csv'))

# Derive Daily SWE
swe_daily <- create_Map(class_result = rf_raster,
                       timeseries = SWE.synth,
                       clusterID_to_timeseriesID=data.frame(class=1:nclass,class_bez=paste('X',1:nclass,sep='')))


writeRaster(swe_daily,paste0(dir_out,'\\swe_daily.tif'))
}
if(!is_continous){
  print('No SWE data was derived as input is not continous.')
}

