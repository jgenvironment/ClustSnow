#################################################################################
###Automatic Processing of LiDAR-Data in R via CloudCompare cmd line mode.###
#################################################################################

#Choose Folder containing LiDAR Point Cloud to Process
dir <- choose.dir()
flights <- str_sub(list.files(dir,pattern='.laz',recursive = T,include.dirs = T,full.names = F),
                   end=-5)

#Define AOI and Global Shift to apply (here: Alptal, Switzerland)
aoi <- '11 228 81 487 86 517 249 511.246002197 641.098022461 394.053009033 717.25 411.17098999 916.111022949 374.989013672 1030.41503906 307.243011475 1024.71398926 220.057006836 873.109008789 194.522003174 705.539001465 226.223007202 611.616027832'
global_shift <- '-477904.00 -5209700.00 -1132.00'

# 1st Step: Filter individual LiDAR SnowON-Surveys (LAZ-Format)
process_laz(dir,flights[1],global_shift,aoi)
process_laz(dir,flights[2],global_shift,aoi)

# 2nd Step: Merge Individual SnowON-Surveys (LAZ-Format)
merge_flights(dir,flights[1],global_shift)

# 3rd Step: Co-register Merged Snowon to Snowoff
snowoff_nonground         <- file.choose() # Choose Merged, co-registered, vegetation of snowoff-flight (LAZ-Format)
coregister_flights(dir,snowoff_nonground,global_shift)

# 4th Step: Rename transformation file
transformation_file       <- rename_transformation_file(dir,flights)

# 5th Step: Apply transformation to snowoff-point cloud
transformed_ground_points <- apply_transformation(dir,transformation_file,global_shift)

# 6th Step: Rasterize to DSM
rasterize_to_DSM(transformed_ground_points,global_shift)

#################################################################################
###Functions###
#################################################################################

process_laz <- function(dir,filename,global_shift,aoi){
  
  file_short <- data.frame(str_split(filename,pattern = '/'))
  file_short <- file_short[nrow(file_short),]
  
  logname   <- paste(dir,paste('RAW',"\\Log_Filtering_",file_short,'.txt',sep = ''),sep="/")
  outputname <- paste(dir,'OldLAZ',sep='\\',paste(file_short,'cut.filter.bin',sep="_"))
  laz_file <- list.files(dir,pattern=paste0(file_short,'.laz'),recursive = T,include.dirs = T,full.names = T)
  
  cmd <- paste(dir,paste(filename,'Filter.cmd',sep="_"),sep='/')
  
  cat('"C:\\Program Files\\CloudCompare\\CloudCompare" ^',sep="\n",file = cmd )
  cat(paste0('-LOG_FILE "',logname,'" ^'),file=cmd,append=T,sep="\n")
  cat('-CLEAR -AUTO_SAVE OFF ^',file=cmd,append=T,sep="\n")
  cat(paste0('-o -GLOBAL_SHIFT ',global_shift,' "',laz_file,'" ^'),file=cmd,append=T,sep="\n")
  cat(paste0('-CROP2D Z ',aoi,' ^'),file=cmd,append=T,sep="\n")
  cat('-SET_ACTIVE_SF 0 ^',file=cmd,append=T,sep="\n")
  cat('-FILTER_SF MIN 0.22 ^',file=cmd,append=T,sep="\n")
  cat('-SET_ACTIVE_SF 1 ^',file=cmd,append=T,sep="\n")
  cat('-FILTER_SF MIN 0.07 ^',file=cmd,append=T,sep="\n")
  cat('-SET_ACTIVE_SF 2 ^',file=cmd,append=T,sep="\n")
  cat('-FILTER_SF MIN 0.07 ^',file=cmd,append=T,sep="\n")
  cat('-SET_ACTIVE_SF 3 ^',file=cmd,append=T,sep="\n")
  cat('-FILTER_SF MIN 0.1 ^',file=cmd,append=T,sep="\n")
  cat('-SET_ACTIVE_SF 4 ^',file=cmd,append=T,sep="\n")
  cat('-FILTER_SF MIN 0.06 ^',file=cmd,append=T,sep="\n")
  cat('-SET_ACTIVE_SF 5 ^',file=cmd,append=T,sep="\n")
  cat('-FILTER_SF MIN 0.04 ^',file=cmd,append=T,sep="\n")
  cat('-SET_ACTIVE_SF 10 ^',file=cmd,append=T,sep="\n")
  cat('-FILTER_SF 15 MAX ^',file=cmd,append=T,sep="\n")
  cat('-SOR 50 10 ^',file=cmd,append=T,sep="\n")
  cat(paste0('-SAVE_CLOUDS FILE "',outputname,'"'),file=cmd,append=T,sep="\n")
  
  
  system2(eval(cmd))
}   #Filter outliers from single flights!

merge_flights <- function(dir,filename,global_shift){
  
  file_short <- data.frame(str_split(filename,pattern = '/'))
  file_short <- file_short[nrow(file_short),]
  file_short_merge <- paste0(str_sub(file_short,1,8))
  
  logname   <- paste(dir,paste("\\RAW\\Log_Merging",'.txt',sep = ''),sep="/")
  inputname <- list.files(dir,pattern=paste0('.cut.filter.bin'),recursive = T,include.dirs = T,full.names = T)
  outputname <- paste(dir,'OldLAZ',paste0(file_short_merge,'_cut.filter.merged.bin'),sep="\\")
  outputname_ground <- paste(dir,'OldLAZ',paste0(file_short_merge,'_cut.filter.merged.ground.bin'),sep="\\")
  outputname_nonground <- paste(dir,'OldLAZ',paste0(file_short_merge,'_cut.filter.merged.nonground.bin'),sep="\\")
  
  cmd <- paste(dir,'RAW',paste(file_short_merge,'Merge.cmd',sep="_"),sep='/') 
  
  if (length(inputname)==1) {
    cat('"C:\\Program Files\\CloudCompare\\CloudCompare" ^',sep="\n",file = cmd )
    cat(paste0('-LOG_FILE "',logname,'" ^'),file=cmd,append=T,sep="\n")
    cat('-CLEAR -AUTO_SAVE OFF ^',file=cmd,append=T,sep="\n")
    cat(paste0('-o -GLOBAL_SHIFT ',global_shift,' "',inputname,'" ^'),file=cmd,append=T,sep="\n")
    cat('-CSF -SCENES SLOPE -CLOTH_RESOLUTION 0.4 -MAX_ITERATION 500 -CLASS_THRESHOLD 0.5 -PROC_SLOPE ^',file=cmd,append=T,sep="\n")
    cat(paste0('-SAVE_CLOUDS FILE "',outputname_ground,' ',outputname_nonground,'"'),file=cmd,append=T,sep="\n")
    cat('-CLEAR',file=cmd,append=T,sep="\n")
    
    system2(eval(cmd))
    
    file.rename(inputname,outputname)
    
  }
  
  if (length(inputname)==2) {
    
    cat('"C:\\Program Files\\CloudCompare\\CloudCompare" ^',sep="\n",file = cmd )
    cat(paste0('-LOG_FILE "',logname,'" ^'),file=cmd,append=T,sep="\n")
    cat('-CLEAR -AUTO_SAVE OFF ^',file=cmd,append=T,sep="\n")
    for (i in 1:length(inputname)) {
      cat(paste0('-o -GLOBAL_SHIFT ',global_shift,' "',inputname[i],'" ^'),file=cmd,append=T,sep="\n")
    }
    cat('-ICP -MIN_ERROR_DIFF 1e-6 -ROT XYZ -OVERLAP 100 -RANDOM_SAMPLING_LIMIT 10000000 -FARTHEST_REMOVAL ^',file=cmd,append=T,sep="\n")
    cat('-MERGE_CLOUDS ^',file=cmd,append=T,sep="\n")
    cat(paste0('-SAVE_CLOUDS FILE "',outputname,'" ^'),file=cmd,append=T,sep="\n")
    cat('-CSF -SCENES SLOPE -CLOTH_RESOLUTION 0.4 -MAX_ITERATION 500 -CLASS_THRESHOLD 0.5 -PROC_SLOPE ^',file=cmd,append=T,sep="\n")
    cat(paste0('-SAVE_CLOUDS FILE "',outputname_ground,' ',outputname_nonground,'"'),file=cmd,append=T,sep="\n")
    cat('-CLEAR',file=cmd,append=T,sep="\n")
    
    
    system2(eval(cmd))
  }
  
  else{print('Invalid Number of Inputfiles!')}
  
}#Merge single flights using ICP and classify using CSF!

coregister_flights <- function(dir,snowoff_nonground,global_shift){
  
  logname   <- paste0(dir,"\\RAW\\Log_Coregistration",'.txt',sep="")
  inputname <-   list.files(dir,pattern=paste0('.merged.nonground.bin'),recursive = T,include.dirs = T,full.names = T)
  name_short <- str_sub(inputname,-40,-33)
  
  cmd <- paste(dir,'RAW',paste(name_short,'Coregister.cmd',sep="_"),sep='/')
  
  cat('"C:\\Program Files\\CloudCompare\\CloudCompare" ^',sep="\n",file = cmd )
  cat(paste0('-LOG_FILE "',logname,'" ^'),file=cmd,append=T,sep="\n")
  cat('-CLEAR -AUTO_SAVE OFF ^',file=cmd,append=T,sep="\n")
  cat(paste0('-o -GLOBAL_SHIFT ',global_shift,' "',inputname,'" ^'),file=cmd,append=T,sep="\n")
  cat(paste0('-o -GLOBAL_SHIFT ',global_shift,' "',snowoff_nonground,'" ^'),file=cmd,append=T,sep="\n")
  cat('-ICP -MIN_ERROR_DIFF 1e-6 -ROT XYZ -OVERLAP 100 -RANDOM_SAMPLING_LIMIT 10000000 -FARTHEST_REMOVAL -ROT NONE ^',file=cmd,append=T,sep="\n")
  
  system2(eval(paste(cmd,sep='/')))
} #Coregister to snowoff-vegetation!

rename_transformation_file <- function(dir,filename){
  filename<-filename[1]
  
  file_short <- data.frame(str_split(filename,pattern = '/'))
  file_short <- file_short[nrow(file_short),]
  file_short_merge <- paste0(str_sub(file_short,1,8))
  
  snowon<-file_short_merge
  snowoff<-'20220428'
  name_transformation_file_old <- list.files(dir,pattern = 'nonground_REGISTRATION_MATRIX',recursive = T,full.names = T)
  name_transformation_file_new <- paste0(dir,'\\OldLAZ\\',snowon,'_To_',snowoff,'_REGISTRATION_MATRIX.txt')
  
  file.rename(name_transformation_file_old,name_transformation_file_new)
  
  return(name_transformation_file_new)
}

apply_transformation <- function(dir,transformation_file,global_shift){
  
  name_short <-   list.files(dir,pattern=paste0('.merged.nonground.bin'),recursive = T,include.dirs = T,full.names = T)
  name_short <- str_sub(name_short,-40,-33)
  
  logname   <- paste(dir,'RAW',paste("Log_transformation",'.txt',sep = ''),sep="\\")
  inputname <-   list.files(dir,pattern=paste0('.merged.bin'),recursive = T,include.dirs = T,full.names = T)
  outputname <- paste(dir,'OldLAZ',paste0(name_short,'_cut.filter.merged.coreg.bin'),sep="\\")
  outputname_ground <- paste(dir,'OldLAZ',paste0(name_short,'_cut.filter.merged.coreg.ground.bin'),sep="\\")
  outputname_nonground <- paste(dir,'OldLAZ',paste0(name_short,'_cut.filter.merged.coreg.nonground.bin'),sep="\\")
  
  cmd <- paste(dir,paste('\\RAW\\',str_sub(transformation_file,-44,-37),'Transformation.cmd',sep="_"),sep='')
  
  cat('"C:\\Program Files\\CloudCompare\\CloudCompare" ^',sep="\n",file = cmd )
  cat(paste0('-LOG_FILE "',logname,'" ^'),file=cmd,append=T,sep="\n")
  cat('-CLEAR -AUTO_SAVE OFF ^',file=cmd,append=T,sep="\n")
  cat(paste0('-o -GLOBAL_SHIFT ',global_shift,' "',inputname,'" ^'),file=cmd,append=T,sep="\n")
  cat(paste0('-APPLY_TRANS "',transformation_file,'" ^'),file=cmd,append=T,sep="\n")
  cat(paste0('-SAVE_CLOUDS FILE "',outputname,'" ^'),file=cmd,append=T,sep="\n")
  cat('-CSF -SCENES SLOPE -CLOTH_RESOLUTION 0.4 -MAX_ITERATION 500 -CLASS_THRESHOLD 0.5 -PROC_SLOPE ^',file=cmd,append=T,sep="\n")
  cat(paste0('-SAVE_CLOUDS FILE "',outputname_ground,' ',outputname_nonground,'"'),file=cmd,append=T,sep="\n")
  
  system2(eval(cmd))
  
  return(outputname_ground)
}

rasterize_to_DSM <- function(transformed_ground_points,global_shift){
  
  logname   <- paste(dir,'RAW',paste("Log_Rasterization",'.txt',sep = ''),sep="\\")
  file_short_merge <- paste0(str_sub(transformed_ground_points,-43,-36))
  
  
  cmd <- paste(dir,'RAW',paste(file_short_merge,'Rasterization.cmd',sep="_"),sep='/')
  
  cat('"C:\\Program Files\\CloudCompare\\CloudCompare" ^',sep="\n",file = cmd )
  cat(paste0('-LOG_FILE "',logname,'" ^'),file=cmd,append=T,sep="\n")
  cat('-CLEAR -AUTO_SAVE OFF ^',file=cmd,append=T,sep="\n")
  cat(paste0('-o -GLOBAL_SHIFT ',global_shift,' "',transformed_ground_points,'" ^'),file=cmd,append=T,sep="\n")
  cat(paste0('-RASTERIZE -GRID_STEP 0.25 -EMPTY_FILL CUSTOM_H -CUSTOM_HEIGHT 1000 -OUTPUT_RASTER_Z ^'),file=cmd,append=T,sep="\n")  
  
  system2(eval(cmd))
  
}
