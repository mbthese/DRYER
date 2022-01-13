test <- function(n1, n2) {
  result  <- n1 + n2
  return(result)
}
test(3,8)


PSYPRO_THESE <-
  function(Path_calibration,Path_psyprofolder, Time, Species, Series,Path_results) {
    library(readr)
    library(BBmisc)
    library(tidyverse)
    
# Path_calibration: The path that leads to the file containing the sensor's calibration of your psypro sensors

# Path_psyprofolder : The path to the folder containing all the psypro data
    
# Time: 'T0', 'T21' etc... campaign 
    
# Species : 'Epe.fal' or 'Jaccop'...
    
# Series: '0', '1', up to '6'

# All the above info will lead to what you want to analyze: 16 files. 2 per sensor of the psypro
   
path <- file.path(Path_psyprofolder, Time, Species, Series)

    
# path_result: path to the file in which you want to store the graphs.
    
#setwd(path)  # Set the working directory
 ###----------

CalibrationPsypro <- read_delim(Path_calibration, ";", escape_double = FALSE, trim_ws = TRUE)
    
CalibrationPsyproNONA<- drop_na(CalibrationPsypro) 
    # get the excel file with wire's calibrations
    
    a <- toString(file.info(path,pattern=".csv", full.names=TRUE)$mtime)
    # Get the informations of the files in the directory
    # Here we are looking for the date and time of creation
    
    a1 <- BBmisc::explode(a,sep=", ")[1]
    # Take only the date and time of the first file (they've all
    # been created at the same moment)
    a2 <- BBmisc::explode(a1,sep=" ")[1]
    # From this we extract the date
    a3 <- BBmisc::explode(a1,sep=" ")[2]
    # And then the time
    # You can check the content of a/a1/a2/a3 by typing them in
    # the console 
    
    calib <- CalibrationPsypro
    # Rename the calib file
    
    set <- calib %>% subset(Serie == Series)
    # We make a subset of the file with the sensors
    # that are in the used set.
    # If you don't have defined serie in your calibration file
    # but only the "name" of your sensor use the other script              
    
    digits <- strsplit(set$Sensor, "")
    # create a list containing each character of the name of a sensor
    
    digits2 <- NULL
    # create a vector to store the final digit of sensor's name
    # digit that indicates the position of the sensor and thus
    # the corresponding output file of the psypro
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
    set <- cbind(set,as.numeric(digits2)) # then add it to the data frame
    
    colnames(set) <- c("serie","sensor","slope","intercept","R2","pos_sensor")
    # rename col to be cleaner
    
    set <- set[order(set$pos_sensor),]
    # Order the data frame so the first row contains the
    # parameter of the sensor in position one on the psypro
    # All this must be done for "homemade" series of wire.
    # If you used the default one (11,12...18 ; 21,22...28 ; etc...)
    # Then you can skip all the part about the digits.
    
    #rm(pathcalib,pathpsypro,a,a1,a2,a3,i,digits2,last_digit)
    # clean your environement
    
    
    
    
    #let's have fun####
    
    setwd(path)
    
    for (i in set$pos_sensor) { # For each sensor in your set
      df <-read_csv(paste0("P02_Ps#",i,"_50point.csv")) #load the sensor's data file
      assign(paste0("sensor_",i),tail(df[,5:6],50) )
      #renaming it and taking only "PsyuV" and "sec" columns and 50 last points
    }
    
   # Path_results = "C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/DRYER/1_Greenhouse_DRYER/data/Psypro_results_1312" 
    
    for (i in set$pos_sensor){ #for each sensor
      a <- get(paste0("sensor_",i)) # take corresponding data
      b <- set[i,1] # take corresponding sensor ID
      g <- a %>%  ggplot(aes(x = sec,y =`psy uV`))+
        geom_point( ) +
        ggtitle ( label = paste0("sensor_",b,"  Pos_",i))
      ggsave(filename = paste0("sensor_",i,'.jpg'), plot = g, device = "jpg", path = Path_results)
      g2 <- a %>% ggplot(aes(x = sec,y =`psy uV`))+
        geom_point( ) +
        ggtitle ( label = paste0("sensor_",b,"  Pos_",i))
      ggsave(filename = paste0("sensor_",i,'_non_standard.jpg'), plot = g2, device = "jpg", path = Path_results)
      # Plot psy against the time in sec
      assign(paste0("curve_sensor_",i),a[2:5,]) # take the 2 to 5 points of the curve (is it enough??)
      
    }
    
    
    Water_potential <- NULL # Create an empty vector to store Waterpot values
    for(i in 1:nrow(set)){ #for each sensor
      j <- set[i,6]
      b <- get(paste0("curve_sensor_",j)) # take the dots of the corresponding curve
      Water_potential <- c(Water_potential,as.numeric(set[i,3])*coef(lm(b$`psy uV`~b$sec))[["(Intercept)"]]+as.numeric(set[i,4]))  
    }
    #the water potential is sensor's slope*intercept of the lm + sensor's intercept
    
    
    #Bind#####
    
    
    data <- cbind(date = a2,time = a3,set[,c(1,2,6,3,4)],Water_potential,R2 = set[,5]) 
    # bind water pot values to the set dataframe, date and time column contain
    # value extracted and stored in a2 and a3
    
    setwd(Path_results)
    write.csv(data, paste0("Result",".csv"),row.names = F)
    # Save it in the working directory
  }


PSYPRO_THESE(Path_calibration = "C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/DRYER/1_Greenhouse_DRYER/data/Psypro_calibration/calib.csv", Path_psyprofolder = 'C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/DRYER/1_Greenhouse_DRYER/data/Psypro_input',Time = 'T71', Species = 'Ef_Ih', Series = '0', Path_results = "C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/DRYER/1_Greenhouse_DRYER/data/Psypro_results_1312/T71/Ef_Ih/0")


