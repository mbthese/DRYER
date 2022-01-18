#--------------------
#ACP trial on non-destructive data
#--------------------


#----------install packages

#install.packages("FactoMineR")
library(FactoMineR)
#install.packages("Factoshiny")
library(Factoshiny)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("dplyr")
library(dplyr)
#install.packages("googleAuthR")
library(devtools)
#install.packages("searchConsoleR")
library(googleAuthR)
#install.packages("devtools")
library(searchConsoleR)
#install.packages("gargle")
library(gargle)
#install.packages("googleAuthR")
library(googleAuthR)
#install.packages("googlesheets4")
library(googlesheets4)
#install.packages("googledrive")
library(googledrive)
#install.packages("factoextra ")
library(factoextra)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("rstatix")
library(rstatix)



#authentification for the google sheets where I have the autorisation (I'm not authorized to log on Marion's drive), so this is the function I need : 

gs4_auth(email = "alice.bordes@outlook.com")


### loading of the google sheets
#non destructive 
T0_NONdestructive <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hxXws0kkgQhkC3T023X6QZw5o-RAjnNo8c3_KcWfTpw/edit#gid=799954127", range = "non_destructif")

T0_NONdestructive <- subset(T0_NONdestructive, select = Time:WUE)

T21_NONdestructive <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Vkmx3XCVR4L0QG22b_-zcIFutezT59gUlmr1ALW4V2Y/edit#gid=258591157",range = "non_destructif")

T21_NONdestructive <- subset(T21_NONdestructive, select = Time:WUE)

T27_NONdestructive <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1SQekhd-T9yTKDTILUuHF0NY3Nlggy4enzFhSXLxJlK0/edit#gid=822184942",range = "non_destructif")

T27_NONdestructive <- subset(T27_NONdestructive, select = Time:WUE)

T51_NONdestructive<- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1lmgpcbXdVcC2de3_-uc3g0w_P8Ab7z1m4faLhd2EmXI/edit#gid=539250691", range = "non_destructif")

T51_NONdestructive <- subset(T51_NONdestructive, select = Time:WUE)

T57_NONdestructive<- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1rh34QE6izuGEnB4eQFLn4lv5oEf2CaFscT1Tsjc6NQA/edit#gid=822184942", range = "non_destructif")

T57_NONdestructive <- subset(T57_NONdestructive, select = Time:WUE)

T71_NONdestructive <- 
  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1al3U5pv2FQ0dWF6sbroB_6kJdzW1OnaLVpohykcKx7c/edit#gid=258591157", range = "non_destructif")

T71_NONdestructive <- subset(T71_NONdestructive, select = Time:WUE)


Data_NonDes <- bind_rows(T0_NONdestructive,T21_NONdestructive,T27_NONdestructive, T51_NONdestructive, T57_NONdestructive, T71_NONdestructive)



#destructive
T0_destructive <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hxXws0kkgQhkC3T023X6QZw5o-RAjnNo8c3_KcWfTpw/edit#gid=799954127", range = "destructif")

T0_destructive <- T0_destructive[1:14]

T21_destructive <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Vkmx3XCVR4L0QG22b_-zcIFutezT59gUlmr1ALW4V2Y/edit#gid=258591157",range = "destructif")

T21_destructive <- select(T21_destructive, select = -c(14:18))
T21_destructive <- T21_destructive[1:14]

T27_destructive <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1SQekhd-T9yTKDTILUuHF0NY3Nlggy4enzFhSXLxJlK0/edit#gid=822184942",range = "destructif")

T27_destructive <- T27_destructive[1:19]

T51_destructive<- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1lmgpcbXdVcC2de3_-uc3g0w_P8Ab7z1m4faLhd2EmXI/edit#gid=539250691", range = "destructif")

T51_destructive <- T51_destructive[1:16]

T57_destructive<- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1rh34QE6izuGEnB4eQFLn4lv5oEf2CaFscT1Tsjc6NQA/edit#gid=822184942", range = "destructif")

T57_destructive <- T57_destructive[1:16]

T71_destructive <- 
  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1al3U5pv2FQ0dWF6sbroB_6kJdzW1OnaLVpohykcKx7c/edit#gid=258591157", range = "destructif")

T71_destructive <- T71_destructive[1:16]


DataDes <- bind_rows(T0_destructive,T21_destructive,T27_destructive, T51_destructive, T57_destructive, T71_destructive)
View(DataDes)

#diameter,height and nb of leaves
T0_dhl<- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1PZOyGfKu66WJ0feJVmBp9NzVJK3BK7fv/edit#gid=770489906", range = "Indv")

T21_dhl<- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Vkmx3XCVR4L0QG22b_-zcIFutezT59gUlmr1ALW4V2Y/edit#gid=734171086", range = "hauteur/diametre/nbf")

T27_dhl<- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1n3ldEJC4Bf9Oe88nySSKhv6ytAbXHwqW/edit#gid=770489906", range = "Indv")

T51_dhl<- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/10kbyBcxeZGw495qgXnaCy0YYpv_9Xscs/edit#gid=858865895", range = "Indv")

T57_dhl<- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1_UnP4VR6LTcRvTAkWtNRTwoYsdYDCzEX/edit#gid=770489906", range = "Indv")

T71_dhl<- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1PZOyGfKu66WJ0feJVmBp9NzVJK3BK7fv/edit#gid=770489906", range = "Indv")




View(Data_NonDes)
str(Data_NonDes)
Data_NonDes$Block<-as.character(Data_NonDes$Block)
Data_NonDes$Sblock<-as.character(Data_NonDes$Sblock)
Data_NonDes$UniqueCode<-as.character(Data_NonDes$UniqueCode)



Data_NonDes.acp<-Data_NonDes %>% select(-c(Height,Diameter,Nb_leaves,WiltingStage,RealTreatment,R,gs_bis)) 
View(Data_NonDes.acp)

PCAshiny(Data_NonDes.acp)



#-----------

#Statistical tests

#Quelles sont les modalités qui influent sur le gs ? 


#-data  
summary(Data_NonDes)
Data_NonDes$Time<-as.factor(Data_NonDes$Time)
unique(Data_NonDes$Time)
Data_NonDes$Time<-as.factor(Data_NonDes$Treatment)
unique(Data_NonDes$Treatment)
Data_NonDes$Time<-as.factor(Data_NonDes$Species)
unique(Data_NonDes$Species)


#--------------gs

#verifying the conditions of tests application
#assumptions for anova : normality of residuals, independence of samples, homoscedasticity



#homoscedasticity check : bartlett.test
  # Interpretation bartlett test : if p-val >= 0,05 --> variances are equal, if p-val>0,05 --> variances are significatively different
  bartlett.test(data=Data_NonDes,gs~Time)
  bartlett.test(data=Data_NonDes,gs~Treatment)
  bartlett.test(data=Data_NonDes,gs~Species)
    #results : p-val < 0,05 : different variances between groups for each modality (Time, Treatment, Species) --> anova not practicable --> we can use the Anova with Welch correction test (welch_anova_test) (but only for one factor analysis) which doesn't need to fulfill homoscedasticity 

  #creation linear model
  lm.gs.treatment<-lm(data=Data_NonDes,gs~Treatment)
  qqnorm(residuals(lm.gs.treatment),main="Distribution plot of residuals - gs ~ Treatment")
  ggplot((lm.gs.treatment),aes(x=.resid))+
    geom_histogram(colour="black",fill="white")+
    labs(title="Distribution plot of residuals - gs ~Treatment")
  
  lm.gs.time<-lm(data=Data_NonDes,gs~Time)
  qqnorm(residuals(lm.gs.time),main="Distribution plot of residuals - gs ~ Time")
  ggplot((lm.gs.time),aes(x=.resid))+
    geom_histogram(colour="black",fill="white")+
    labs(title="Distribution plot of residuals - gs ~ Time")

  lm.gs.species<-lm(data=Data_NonDes,gs~Species)
  qqnorm(residuals(lm.gs.species),main="Distribution plot of residuals - gs ~ Species")
  ggplot((lm.gs.species),aes(x=.resid))+
    geom_histogram(colour="black",fill="white")+
    labs(title="Distribution plot of residuals - gs ~ Species")
  
      #result : normality assumption  seems graphically fulfill

  
welch_anova_test(data=Data_NonDes,gs~Treatment)
welch_anova_test(data=Data_NonDes,gs~Time)
welch_anova_test(data=Data_NonDes,gs~Species)
  
  #result : p-val<0,05 for the 3 variables --> effect of the Time, of the Treatment and of the Species on gs
  

