
  
    Path_psyprofolder <-'C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/DRYER/1_Greenhouse_DRYER/data/Psypro_input'
    Time  <- 'T0'
    Species <- 'Epe.fal'
    Series<- '0'
    
    path <- file.path(Path_psyprofolder, Time, Species, Series)
    
    Path_calibration<- "C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/DRYER/1_Greenhouse_DRYER/data/Psypro_calibration/calib.csv"
    
    CalibrationPsypro <- read_delim(Path_calibration, ";", escape_double = FALSE, trim_ws = TRUE)
    
    CalibrationPsyproNONA<- drop_na(CalibrationPsypro) 


    a <- toString(file.info(path,pattern=".csv", full.names=TRUE)$mtime)
    
    a1 <- BBmisc::explode(a,sep=", ")[1]
    a2 <- BBmisc::explode(a1,sep=" ")[1]
    a3 <- BBmisc::explode(a1,sep=" ")[2]
    
    calib <- CalibrationPsypro
    
    set <- subset(calib, Serie == Serie) 
    digits <- strsplit(set$Sensor, "")
    
    
    digits2 <- NULL
    
    v <- NULL
    
    for (i in 1:length(digits)) {
      # for each element of the list
      if (is.na(as.numeric(tail(digits[[i]],1)))== T){
        last_digit <- head(tail(digits[[i]],2),1)
        v <- last_digit
      }
      else {
        last_digit <- tail(digits[[i]],1)
        v <- last_digit
      } # take the last character
      
      digits2 <- c(digits2,last_digit) # and store it
    }
    
    
    set <- cbind(set,as.numeric(digits2))
    
    colnames(set) <- c("serie","sensor","slope","intercept","R2","pos_sensor")
    set <- set[order(set$pos_sensor),]
    
    setwk(path)
    for (i in set$pos_sensor) { # For each sensor in your set
      df <-read_csv(paste0("P02_Ps#",i,"_50point.csv")) #load the sensor's data file
      assign(paste0("sensor_",i),tail(df[,5:6],50) )
      #renaming it and taking only "PsyuV" and "sec" columns and 50 last points
    }
    
    
    
    
    
    PSYPRO_THESE(Path_calibration = "C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/DRYER/1_Greenhouse_DRYER/data/Psypro_calibration/calib.csv", Path_psyprofolder = 'C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/DRYER/1_Greenhouse_DRYER/data/Psypro_input',Time = 'T0', Species = 'Epe.fal', Serie = '0', Path_results = "C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/DRYER/1_Greenhouse_DRYER/data/Psypro_results_1312")


