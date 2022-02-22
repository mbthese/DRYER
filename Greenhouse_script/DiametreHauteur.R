#T51--------------

library(tibble)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(googlesheets4)

mortality <- read_excel("data/202112_01_mortality/1-mortality stages011222.xlsx", 
                        sheet = "01_0812")
Dead <- subset(mortality, Alive_08_12!="1")

DH <- read_excel("data/20211214_J51/DH/DiametreHauteurLeafT51 - D1 Recovery.xlsx", sheet = "Sheet1")

DH <- anti_join(DH, Dead, by= "UniqueCode") 


Epefal_Iryhos_DH <- subset(DH, Species=="Epe.fal" | Species=="Iry.hos" )

Jaccop_Pteoff <- subset(DH, Species=="Jac.cop" | Species=="Pte.off" )

Symglo_tacmel <- subset(DH, Species=="Sym.off"|Species == "Tac.mel")
Symglo_tacmel2 <- Symglo_tacmel[,-10]

Virsur <- subset(DH, Species=="Vir.sur")
Virsur2 <- Virsur[,-10]


library(rio)
export(Epefal_Iryhos_DH, "data/20211214_J51/Ef_Ih_DH.csv")
export(Jaccop_Pteoff, "data/20211214_J51/Jc_Po_DH.csv")
export(Symglo_tacmel2, "data/20211214_J51/Sg_tc_DH.csv")
export(Virsur2, "data/20211214_J51/Vs_DH.csv")

#T57------------

library(tibble)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(googlesheets4)

mortality <- read_excel("Greenhouse_data/20211221_J57/DH/mortality.stages131222.xlsx")
View(mortality)
Dead <- subset(mortality, Alive_13_12!="1")

DH <- read_excel("data/20211221_J57/DH/DiametreHauteurLeafT57-D2-recovery.xlsx", sheet = "Indv")

DH <- anti_join(DH, Dead, by= "UniqueCode") 


Epefal_Iryhos_DH <- subset(DH, Species=="Epe.fal" | Species=="Iry.hos" )
Epefal_Iryhos_DH  <- subset(Epefal_Iryhos_DH , select = -c(T0_Remarques) )

Jaccop_Pteoff <- subset(DH, Species=="Jac.cop" | Species=="Pte.off" )

Symglo_tacmel <- subset(DH, Species=="Sym.off"|Species == "Tac.mel")
Symglo_tacmel2 <- Symglo_tacmel[,-10]

Virsur <- subset(DH, Species=="Vir.sur")
Virsur <- subset(Virsur , select = -c(T0_Remarques) )


library(rio)
export(Epefal_Iryhos_DH, "data/20211221_J57/DH/Ef_Ih_DH.csv")
export(Jaccop_Pteoff, "data/20211221_J57/DH/Jc_Po_DH.csv")
export(Symglo_tacmel2, "data/20211221_J57/Sg_tc_DH.csv")
export(Virsur, "data/20211221_J57/DH/Vs_DH.csv")


#T101 DH

library(tibble)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(googlesheets4)

mortality <-read.csv("C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/DRYER/3-DRYERproject/Greenhouse_data/202112_01_mortality/mortality.stages310122.csv", header=TRUE, sep=";")

Dead <- subset(mortality, Alive_31_01!="1")

DH <- read_excel("Greenhouse_data/DiametreHauteurNbLeaves/DiametreHauteurLeafT0.xlsx", sheet = "Indv", col_types = c("text", "numeric", "skip", "numeric", "numeric","text", "skip", "skip", "text", "skip", "skip", "skip", "skip"))

DH['Time'] <- 't101'
DH <- DH %>% add_column(Height = NA, Diameter = NA, NbLeaves = NA)
DH <- DH %>% filter(Treatment == 'C0' | Treatment =='D3' )

DH <- anti_join(DH, Dead, by= "UniqueCode") 


Epefal_Iryhos_DH <- subset(DH, Species=="Epe.fal" | Species=="Iry.hos" )

Jaccop_Pteoff <- subset(DH, Species=="Jac.cop" | Species=="Pte.off" )

Symglo_tacmel <- subset(DH, Species=="Sym.off"|Species == "Tac.mel")

Virsur <- subset(DH, Species=="Vir.sur")

library(rio)
export(Epefal_Iryhos_DH, "Greenhouse_data/20220203_J101/DH/Ef_Ih_DH.csv")
export(Jaccop_Pteoff, "Greenhouse_data/20220203_J101/DH/Jc_Po_DH.csv")
export(Symglo_tacmel, "Greenhouse_data/20220203_J101/DH/Sg_tc_DH.csv")
export(Virsur, "Greenhouse_data/20220203_J101/DH/Vs_DH.csv")
