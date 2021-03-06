#--------------------
#Statistical analysis
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
#install.packages("openxlsx")
library(openxlsx)
#install.packages("DescTools")
library(DescTools)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("cowplot")
library(cowplot)
#install.packages("readxl")
library(readxl)

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

for (i in (1:nrow(Data_NonDes)))
{
  if ((Data_NonDes$Time[i]=="t51")&&(Data_NonDes$Treatment[i]=="D1"))
    {
      Data_NonDes$Treatment[i]<-"R1"
    }
}

for (i in (1:nrow(Data_NonDes)))
{
  if ((Data_NonDes$Time[i]=="t57")&&(Data_NonDes$Treatment[i]=="D2"))
  {
    Data_NonDes$Treatment[i]<-"R2"
  }
}

for (i in (1:nrow(Data_NonDes)))
{
  if ((Data_NonDes$Time[i]=="t101")&&(Data_NonDes$Treatment[i]=="D3"))
  {
    Data_NonDes$Treatment[i]<-"R3"
  }
}
View(Data_NonDes)

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

for (i in (1:nrow(DataDes)))
{
  if ((DataDes$Time[i]=="t51")&&(DataDes$Treatment[i]=="D1"))
  {
    DataDes$Treatment[i]<-"R1"
  }
}

for (i in (1:nrow(DataDes)))
{
  if ((DataDes$Time[i]=="t57")&&(DataDes$Treatment[i]=="D2"))
  {
    DataDes$Treatment[i]<-"R2"
  }
}

for (i in (1:nrow(DataDes)))
{
  if ((DataDes$Time[i]=="t101")&&(DataDes$Treatment[i]=="D3"))
  {
    DataDes$Treatment[i]<-"R3"
  }
}
View(DataDes)

#diameter,height and nb of leaves

DHLT0 <- read_excel("Data_dhl/DiametreHauteurLeafT0.xlsx", 
                    sheet = "Indv")

DHLT21 <- read_excel("Data_dhl/DiametreHauteurLeafT21.xlsx", 
                     sheet = "Sheet1")

DHLT27 <- read_excel("Data_dhl/DiametreHauteurLeafT27.xlsx", 
                     sheet = "Indv")

DHLT51 <-read_excel("Data_dhl/DiametreHauteurLeafT51 - D1 Recovery.xlsx", 
                    sheet = "Indv")
DHLT51$T21_Height<-as.numeric(DHLT51$T21_Height)

DHLT57 <- read_excel("Data_dhl/DiametreHauteurLeafT57-D2-recovery.xlsx", 
                     sheet = "Indv")
DHLT57$comment_T71<-as.character(DHLT57$comment_T71)
DHLT57<-DHLT57[,-c(25:28)]

DHLT71 <- read_excel("Data_dhl/DiametreHauteurLeafT71.xlsx", 
                     sheet = "Indv")
DHLT71$T71_Height<-as.numeric(DHLT71$T71_Height)


a<-dplyr::left_join(DHLT0,DHLT21)
b<-dplyr::left_join(a,DHLT27)
c<-dplyr::left_join(b,DHLT51)
d<-dplyr::left_join(c,DHLT57)
DataDHL<-dplyr::left_join(d,DHLT71)
View(DataDHL)







#-----------

#Statistical tests

#ACP on non-destructive data
View(Data_NonDes)
str(Data_NonDes)
Data_NonDes$Block<-as.character(Data_NonDes$Block)
Data_NonDes$Sblock<-as.character(Data_NonDes$Sblock)
Data_NonDes$UniqueCode<-as.character(Data_NonDes$UniqueCode)



Data_NonDes.acp<-Data_NonDes %>% select(-c(Height,Diameter,Nb_leaves,WiltingStage,RealTreatment,R,gs_bis)) 
View(Data_NonDes.acp)

PCAshiny(Data_NonDes.acp)


#my functions
#############


dt.stats.tests<-function(dta,quanti.variable)
{
  quanti.variable <- deparse(substitute(quanti.variable))
  Treatment <- deparse(substitute(Treatment))
  Species <- deparse(substitute(Species))
  dta<-drop_na(dta, quanti.variable)
 
  
  dt.test<-data.frame(
    Factor=c("Treatment","Species"),
    bartlett_pvalue=c(bartlett.test(dta[[quanti.variable]]~dta[[Treatment]])$p.value,bartlett.test(dta[[quanti.variable]]~dta[[Species]])$p.value),
    "equal_variances"=c(bartlett.test(dta[[quanti.variable]]~dta[[Treatment]])$p.value>0.05,bartlett.test(dta[[quanti.variable]]~dta[[Species]])$p.value>0.05)) 
  
  print(paste("1) Are the residuals within the Treatment for ", quanti.variable, " following graphically a normal distribution? 2) Are the residuals within the Species for ", quanti.variable, " following graphically a normal distribution?"))
  answer<-scan(nmax=2, what=logical())
  
  dt.test$normality<-answer
  dt.test$test<-c(rep(0))
  
  for (i in 1:nrow(dt.test))
  {
    if(dt.test$equal_variances[i]==FALSE && dt.test$normality[i]==TRUE )
    {
      dt.test$test[i]<-"Anova with Welch correction"
     
    }
    if(dt.test$equal_variances[i]==FALSE && dt.test$normality[i]==FALSE )
    {
      dt.test$test[i]<-"Non-parametric"
      
    }
    if(dt.test$equal_variances[i]==TRUE && dt.test$normality[i]==TRUE )
    {
      dt.test$test[i]<-"Anova"
    
    }
  }
  
  

  return(dt.test)
  
}


histo<-function(data,quanti.variable)
{
  quanti.variable <- deparse(substitute(quanti.variable))
  Treatment <- deparse(substitute(Treatment))
  Species <- deparse(substitute(Species))
  
  
  lm.treatment<-lm(data[[quanti.variable]]~data[[Treatment]])
  histo.lm.treatment<-ggplot((lm.treatment),aes(x=.resid))+
    geom_histogram(colour="black",fill="white",bins=30)+
    labs(title=paste("Distribution plot of residuals - ",quanti.variable," ~ Treatment"))
  
  lm.species<-lm(data[[quanti.variable]]~data[[Species]])
  histo.lm.species<-ggplot((lm.species),aes(x=.resid))+
    geom_histogram(colour="black",fill="white",bins=30)+
    labs(title=paste("Distribution plot of residuals - ",quanti.variable," ~ Species"))
  
  windows(4,4)
  figure <- ggarrange(histo.lm.treatment,histo.lm.species,ncol = 2, nrow = 1,common.legend=TRUE)
  
  return(figure)
  
  
}

#############

#Quelles sont les modalités qui influent sur le gs ? 


#-data  
summary(Data_NonDes)
Data_NonDes$Time<-as.factor(Data_NonDes$Time)
Data_NonDes$Treatment<-as.factor(Data_NonDes$Treatment)
Data_NonDes$Species<-as.factor(Data_NonDes$Species)
Data_NonDes2<-drop_na(Data_NonDes, gs)

#--------------gs

#verifying the conditions of tests application
#assumptions for anova : normality of residuals, independence of samples, homoscedasticity


#normality distribution of the variable Treatment and Species for each effect
histo(Data_NonDes,gs)
histo(Data_NonDes,Chloro)
histo(Data_NonDes,FvFm)
histo(Data_NonDes,Asat)
histo(Data_NonDes,E)
histo(Data_NonDes,WUE)
#resluts : E Species no, FvFm Species ~no 

r<-dt.stats.tests(Data_NonDes,gs)
dt.stats.tests(Data_NonDes,Chloro)
dt.stats.tests(Data_NonDes,FvFm)
dt.stats.tests(Data_NonDes,Asat)
dt.stats.tests(Data_NonDes,E)
dt.stats.tests(Data_NonDes,WUE)

  

  
#result : p-val<0,05 --> significative effet of the variable
#if effect + normality ok --> test post-hoc :
  #Tukey test (paiwise.t.test) if equal variances
  #Tamhane test (tamhaneT2Test) if variances differ

welch_anova_test(data=Data_NonDes,gs~Treatment)
welch_anova_test(data=Data_NonDes,gs~Species)

anova(lm(data=Data_NonDes,Chloro~Treatment))
pairwise.t.test(x=Data_NonDes$Chloro,g=Data_NonDes$Treatment,p.adjust.method = "none")
welch_anova_test(data=Data_NonDes,Chloro~Species)

welch_anova_test(data=Data_NonDes,FvFm~Treatment)


welch_anova_test(data=Data_NonDes,Asat~Treatment)
welch_anova_test(data=Data_NonDes,Asat~Species)

welch_anova_test(data=Data_NonDes,E~Treatment)


welch_anova_test(data=Data_NonDes,WUE~Treatment)
anova(lm(data=Data_NonDes,WUE~Species))



#Is there an effect of the plant greenhouse location (Block & Sblock) on plant growth?

DataDHL.computed<-DataDHL
DataDHL.computed<-dplyr::mutate(DataDHL.computed, delta.Height.21days = T21_Height - T0_Height)
DataDHL.computed<-dplyr::mutate(DataDHL.computed, delta.Height.27days = T27_Height - T0_Height)
DataDHL.computed<-dplyr::mutate(DataDHL.computed, delta.Height.51days = T51_Height - T0_Height)
DataDHL.computed<-dplyr::mutate(DataDHL.computed, delta.Height.57days = T57_Height - T0_Height)
DataDHL.computed<-dplyr::mutate(DataDHL.computed, delta.Height.71days = T71_Height - T0_Height)

DataDHL.computed<-dplyr::mutate(DataDHL.computed, delta.Diameter.21days = T21_Diameter - T0_Diameter)
DataDHL.computed<-dplyr::mutate(DataDHL.computed, delta.Diameter.27days = T27_Diameter - T0_Diameter)
DataDHL.computed<-dplyr::mutate(DataDHL.computed, delta.Diameter.51days = T51_Diameter - T0_Diameter)
DataDHL.computed<-dplyr::mutate(DataDHL.computed, delta.Diameter.57days = T57_Diameter - T0_Diameter)
DataDHL.computed<-dplyr::mutate(DataDHL.computed, delta.Diameter.71days = T71_Diameter - T0_Diameter)

DataDHL.computed<-dplyr::mutate(DataDHL.computed, delta.Nbleaves.21days = T21_Nb_leaves - T0_Nb_leaves)
DataDHL.computed<-dplyr::mutate(DataDHL.computed, delta.Nbleaves.27days = T27_Nb_leaves - T0_Nb_leaves)
DataDHL.computed<-dplyr::mutate(DataDHL.computed, delta.Nbleaves.51days = T51_Nb_leaves - T0_Nb_leaves)
DataDHL.computed<-dplyr::mutate(DataDHL.computed, delta.Nbleaves.57days = T57_Nb_leaves - T0_Nb_leaves)
DataDHL.computed<-dplyr::mutate(DataDHL.computed, delta.Nbleaves.71days = T71_NbLeaves_Total - T0_Nb_leaves)


View(DataDHL)
View(DataDHL.computed)


histo(DataDHL.computed,delta.Height.21days)



