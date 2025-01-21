# Function to load the calibration results as presented in the main publication.
getCalibrationData <- function(){
  
calibration_data <- data.frame(site='AL',
                               sample_length  = 1600,
                               n_class        = 4,
                               kmeans_maxiter = 16,
                               kmeans_nstart  = 39,
                               mtry           = 4,
                               n_trees         = 451,
                               c.ov           = 12.111 * 10^-4,
                               rho.max        = 405.556,
                               rho.null       = 96.667, 
                               k.ov           = 0.01,
                               k              = 5 * 10^-2,
                               tau            = 1 * 10^-2,
                               eta.null       = 5.22222*10^6)

calibration_data <- rbind(calibration_data,data.frame(site='SL',
                                                      sample_length  = 1600,
                                                      n_class        = 4,
                                                      kmeans_maxiter = 16,
                                                      kmeans_nstart  = 39,
                                                      mtry           = 4,
                                                      n_trees         = 451,
                                                      c.ov           = 7.77777 * 10^-4,
                                                      rho.max        = 333.3333,
                                                      rho.null       = 200,
                                                      k.ov           = 0.01,
                                                      k              = 15.7777 * 10^-2,
                                                      tau            = 1 * 10^-2,
                                                      eta.null       = 15.7777*10^6))

return(calibration_data)
}

# Function to derive clusters from a series of (LiDAR) snow depth maps.
getCluster <- function(calibration_data,data_for_prediction){
  
  # Set Workflow Parameters to Calibration Values
  nclass=calibration_data$n_class
  ntrees=calibration_data$n_trees
  sample_length=calibration_data$sample_length
  kmeans_maxiter=calibration_data$kmeans_maxiter
  kmeans_nstart=calibration_data$kmeans_nstart
  
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

# Function to order Clusters from according to their observed mean snow depth.
orderCluster <- function(cluster_in,snow_stack){
  cluster <- cluster_in
  
  cluster <- cluster[[nlayers(cluster)]]
  names(cluster)<-'cluster'
  cluster <- raster::resample(cluster,snow_stack,method='ngb')
  
  snow_stack <- stack(snow_stack,cluster)
  snow_stack_df <- as.data.frame(snow_stack,xy=F)
  snow_stack_df <- na.omit(snow_stack_df)
  
  # Order Clusternumbering according to mean snow depth
  # Obtain cluster assignments
  cluster_assignments <-snow_stack_df$cluster
  
  # Calculate mean values for each cluster in the original data_for_prediction
  mean_values <- aggregate(rowMeans(snow_stack_df[colnames(snow_stack_df)!='cluster']), by =list(snow_stack_df$cluster) , FUN = mean)
  
  # Sort mean_values by cluster
  mean_values <- mean_values[order(mean_values$x,decreasing = T), ]
  mean_values$new_cluster <- 1:nrow(mean_values)
  mean_values
  nmlist <- mean_values$new_cluster
  names(nmlist)<-mean_values$Group.1
  
  cluster_assignments<- as.numeric(as.character(plyr::revalue(as.factor((cluster_assignments)),replace=nmlist)))
  
  snow_stack_df <- as.data.frame(snow_stack,xy=T)
  snow_stack_df <- na.omit(snow_stack_df)
  snow_stack_df$cluster <- cluster_assignments
  
  cluster <- rasterFromXYZ(data.frame(x=snow_stack_df$x,
                                      y=snow_stack_df$y,
                                      cluster=snow_stack_df$cluster))
  cluster <- raster::resample(cluster,cluster_in,method='ngb')
  
  cluster_in[[nlayers(cluster_in)]] <- cluster
  names(cluster_in)<-c(mean_values[order(mean_values$Group.1),'new_cluster'],'cluster')
  
  cluster_in <- cluster_in[[order(names(cluster_in))]]
  cluster_in <- cluster_in[[c(2:nlayers(cluster_in),1)]]
  
  return(cluster_in)
  
}

# Function to get synthetic timeseries for cluster.
hs.synth <- function(timeseries,cluster_map,snomos,nclass){
  
  for (column in 2:ncol(timeseries)) {
    timeseries[,column]<-as.numeric(timeseries[,column])
    
  }
  
  snomos_filtered<-snomos[snomos@data[,name_attribute]%in% colnames(timeseries),]
  snomos.df <- data.frame()
  snomos.df <- data.frame(class_bez=snomos_filtered@data[,name_attribute],cluster=raster::extract(cluster_map,snomos_filtered))
  snomos.df <- snomos.df[,c(1,(nlayers(cluster_map)-nclass):nlayers(cluster_map)+1)]
  snomos.df <- snomos.df[order(snomos.df[,ncol(snomos.df)]),]
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

# Function to create synthetic maps based on a spatial extrapolation of synthetic time series based on the clusters.
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
    #class<-1
    for (class in 1:nclass) {
      
      ncol <- class+2 
      hs <- hsmap_data$data[hsmap_data$class==class]
      
      prob_data.df[,ncol]<-prob_data.df[,ncol]* as.numeric(hs)
      
    }
    
    head(prob_data.df)
    prob_data.df$hs <- apply(prob_data.df[,3:ncol(prob_data.df)],1,sum,na.rm=T)
    
    hs_map <- rasterFromXYZ(prob_data.df[,c(1,2,ncol(prob_data.df))])
    
    names(hs_map)<-paste(as.character(day),sep='')
    result.stack <- stack(result.stack,hs_map)
  }
  return(result.stack)
}

# Application of Delta Snow Model to derive synthetic SWE time series from HS time series.
apply.delta.Snow.Model <- function(calibration_data, timeseries){
  require(nixmass)
  #timeseries<-HS.synth
  timeseries$dates <- as.POSIXlt(timeseries$dates,format=date_format,tz= "UTC") 
  SWE.all <- as.data.frame(timeseries)
  
  for (col in 2:ncol(timeseries)) {
    swe_col <- swe.delta.snow(data.frame(date=as.character(SWE.all$dates),hs=as.numeric(unlist(timeseries[,get('col')]))),
                              rho.max  = calibration_data$rho.max,
                              rho.null = calibration_data$rho.null,
                              c.ov     = calibration_data$c.ov,
                              k.ov = calibration_data$k.ov,
                              k = calibration_data$k,
                              tau = calibration_data$tau,
                              eta.null = calibration_data$eta.null)
    
    SWE.all[,get('col')] <- swe_col 
  }
  
  SWE.all$dates<-format(as.POSIXct(SWE.all$dates),format='%d.%m.%Y')
  return(SWE.all)
}
