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

mortality <- read_excel("data/20211221_J57/DH/mortality.stages131222.xlsx")
View(mortality_stages131222)
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
