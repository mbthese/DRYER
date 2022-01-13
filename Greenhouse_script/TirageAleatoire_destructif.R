#Tirage aleatoire -----
#pour les 5 individus traits destructifs par espèce sans remise

#import data
library(readxl)
data <- read_excel("data/Amont/DiametreHauteurLeaf.xlsx", 
                   sheet = "Indv")

#remove individuals that are dead
library(tidyr)
dataomit <- drop_na(data, Height)
data2<- dataomit
Bilan_t0<- table(data2$Species)

##T0----

#pour chaque espèce, je veux 5 individus dans le groupe control à detruire
#control group

ControlGroup <- data2[data2$Treatment == "C0", ]  

#Eperua falcata 

epefal_C0 <- ControlGroup[ControlGroup$Species == "Epe.fal", ] 

T0_epefal_destruction <-sample(epefal_C0$UniqueCode, 5, replace = FALSE)


#Iryanthera hostmannii

iryhos_C0 <- ControlGroup[ControlGroup$Species == "Iry.hos", ] 
T0_iryhos_destruction <-sample(iryhos_C0$UniqueCode, 5, replace = FALSE)


#Jacaranda copaia 

jaccop_C0 <- ControlGroup[ControlGroup$Species == "Jac.cop", ] 
T0_jaccop_destruction <-sample(jaccop_C0$UniqueCode, 5, replace = FALSE)

#Pterocarpus officinalis
pteoff_C0 <- ControlGroup[ControlGroup$Species == "Pte.off", ] 
T0_pteoff_destruction <-sample(pteoff_C0$UniqueCode, 5, replace = FALSE)

#Symphonia globilifera
symglo_C0 <- ControlGroup[ControlGroup$Species == "Sym.off", ] 
T0_symglo_destruction <-sample(symglo_C0$UniqueCode, 5, replace = FALSE)

RAB_symglo_destruction<- sample(symglo_C0$UniqueCode, 5, replace = FALSE)


#Tachigali melinonii
tacmel_C0 <- ControlGroup[ControlGroup$Species == "Tac.mel",]
T0_tacmel_destruction <-sample(tacmel_C0$UniqueCode, 5, replace = FALSE)

#Virola surinamensis
virsur_C0 <- ControlGroup[ControlGroup$Species== "Vir.sur",]
T0_virsur_destruction <-sample(virsur_C0$UniqueCode, 5, replace = FALSE)

print(T0_epefal_destruction)
print(T0_iryhos_destruction)
print(T0_jaccop_destruction)
print(T0_pteoff_destruction)
print(T0_symglo_destruction)
print(T0_tacmel_destruction)
print(T0_virsur_destruction)

Tirage_destruction_t0 <- do.call(rbind, Map(data.frame, "epefal"=T0_epefal_destruction, "iryhos"=T0_iryhos_destruction, "jaccop"=T0_jaccop_destruction, "pteoff"=T0_pteoff_destruction, "symglo"=T0_symglo_destruction, "tacmel"= T0_tacmel_destruction, "virsur"= T0_virsur_destruction ))


library(rio)
#export(Tirage_destruction_t0, "Tirage_destruction_t0.csv")



##T21 ----- 
#Dry treatment at 21 days - D1

#Retirer individus déjà selectionnés en T0

t0_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hxXws0kkgQhkC3T023X6QZw5o-RAjnNo8c3_KcWfTpw/edit#gid=799954127", range = "destructif")

#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tibble")
library(tibble)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
data <- read_excel("data/20211018_amont/DiametreHauteurLeaf.xlsx", 
                   sheet = "Indv")

dataomit <- drop_na(data, Height)

data2<- dataomit

Bilan_20211115 <- anti_join(data2, t0_destructifs, by= "UniqueCode")

#pour chaque espèce, je veux 5 individus dans le groupe traitement D1 et 3 individus dans le groupe control C0

#help by Tristan 
sp <- NULL
trtmt <- c(rep(c(rep("C0",3),rep("D1",5)),7))
code <- NULL
for (i in levels(as.factor(Bilan_20211115$Species))){
  control_frame <- Bilan_20211115[Bilan_20211115$Species== i&Bilan_20211115$Treatment == "C0",]
  a <-sample(control_frame$UniqueCode, 3, replace = FALSE)
  
  D1_frame <- Bilan_20211115[Bilan_20211115$Species== i&Bilan_20211115$Treatment == "D1",]
  b <-sample(D1_frame$UniqueCode, 5, replace = FALSE)
code <- c(code,a,b) #stockage
sp <- c(sp,rep(i,8)) #creation vecteur espèces mais on peut faire en dehors de la boucle comme trtmt


}
tirage_T21 <- data.frame(species=sp,treatment=trtmt,UniqueCode=code)

Bilan <- Bilan_20211115 %>% select(UniqueCode, Block, Sblock, Remarques)

tirage_T21 <- tirage_T21 %>% left_join(Bilan, by = "UniqueCode")

tirage_T21 <- tirage_T21 %>% relocate(UniqueCode)

tirage_T21 <- tirage_T21 %>% relocate(Block, .after = UniqueCode)

tirage_T21 <- tirage_T21 %>% relocate(Sblock, .after = Block)
tirage_T21 <- tirage_T21 %>% relocate(treatment, .after = Sblock)


tirage_T21 <- tirage_T21 %>% add_column (Height = NA, Diameter = NA, Nb_leaves = NA, RWC_FW = NA, RWC_SW = NA, RWC_DW = NA, LT1 = NA, LT2 = NA, LT3 = NA, LT4 = NA, LT5 = NA, MeanLT = NA, ScanLeaf = NA, ScanRoots = NA)

tirage_T21_destructif<- tirage_T21 %>% relocate(Remarques, .after = ScanRoots)

library(rio)
export(tirage_T21_destructif, "tirage_T21_destructif.csv")


## T27 ----

#Dry treatment at 27 days - D2

#Retirer individus déjà selectionnés en T0 et T21

t0_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hxXws0kkgQhkC3T023X6QZw5o-RAjnNo8c3_KcWfTpw/edit#gid=799954127", range = "destructif")

t21_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Vkmx3XCVR4L0QG22b_-zcIFutezT59gUlmr1ALW4V2Y/edit#gid=822184942", range= "destructif")

#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tibble")
library(tibble)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
data <- read_excel("data/20211018_amont/DiametreHauteurLeaf.xlsx", 
                   sheet = "Indv")

dataomit <- drop_na(data, Height)

data2<- dataomit

Bilan_20211115 <- anti_join(data2, t0_destructifs, by= "UniqueCode")

Bilan_20211122 <- anti_join(Bilan_20211115, t21_destructifs, by= "UniqueCode")


#pour chaque espèce, je veux 5 individus dans le groupe traitement D2 et 3 individus dans le groupe control C0

#help by Tristan 
sp <- NULL
trtmt <- c(rep(c(rep("C0",3),rep("D2",5)),7))
code <- NULL
for (i in levels(as.factor(Bilan_20211122$Species))){
  control_frame <- Bilan_20211122[Bilan_20211122$Species== i&Bilan_20211122$Treatment == "C0",]
  a <-sample(control_frame$UniqueCode, 3, replace = FALSE)
  
  D2_frame <- Bilan_20211122[Bilan_20211122$Species== i&Bilan_20211122$Treatment == "D2",]
  b <-sample(D2_frame$UniqueCode, 5, replace = FALSE)
  code <- c(code,a,b) #stockage
  sp <- c(sp,rep(i,8)) #creation vecteur espèces mais on peut faire en dehors de la boucle comme trtmt
  
  
}
tirage_T27 <- data.frame(species=sp,treatment=trtmt,UniqueCode=code)

Bilan <- Bilan_20211122 %>% select(UniqueCode, Block, Sblock, Remarques)

tirage_T27 <- tirage_T27 %>% left_join(Bilan, by = "UniqueCode")

tirage_T27 <- tirage_T27 %>% relocate(UniqueCode)

tirage_T27 <- tirage_T27 %>% relocate(Block, .after = UniqueCode)

tirage_T27 <- tirage_T27 %>% relocate(Sblock, .after = Block)
tirage_T27 <- tirage_T27 %>% relocate(treatment, .after = Sblock)


tirage_T27 <- tirage_T27 %>% add_column (Height = NA, Diameter = NA, Nb_leaves = NA, RWC_FW = NA, RWC_SW = NA, RWC_DW = NA, LT1 = NA, LT2 = NA, LT3 = NA, LT4 = NA, LT5 = NA, MeanLT = NA, ScanLeaf = NA, ScanRoots = NA)

tirage_T27_destructif<- tirage_T27 %>% relocate(Remarques, .after = ScanRoots)

library(rio)
export(tirage_T27_destructif, "tirage_T27_destructif.csv")

## T51 -----------

#Dry treatment D1 + recovery

#Retirer individus déjà selectionnés en T0, T21, T27-----

t0_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hxXws0kkgQhkC3T023X6QZw5o-RAjnNo8c3_KcWfTpw/edit#gid=799954127", range = "destructif")

t21_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Vkmx3XCVR4L0QG22b_-zcIFutezT59gUlmr1ALW4V2Y/edit#gid=822184942", range= "destructif")

t27_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1SQekhd-T9yTKDTILUuHF0NY3Nlggy4enzFhSXLxJlK0/edit#gid=822184942", range= "destructif")

#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tibble")
library(tibble)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
data <- read_excel("data/20211018_amont/DiametreHauteurLeaf.xlsx", 
                   sheet = "Indv")

dataomit <- drop_na(data, Height)

data2<- dataomit

Bilan_20211115 <- anti_join(data2, t0_destructifs, by= "UniqueCode")

Bilan_20211122 <- anti_join(Bilan_20211115, t21_destructifs, by= "UniqueCode")

Bilan_20211214 <- anti_join(Bilan_20211122, t27_destructifs, by= "UniqueCode")

#S'il y a des morts entre temps

mortality <- read_excel("data/202112_01_mortality/1-mortality stages011222.xlsx", 
                        sheet = "01_0812")

mortality2 <- subset(mortality, Alive_08_12!="0")

Dead <- anti_join(Bilan_20211214, mortality2, by= "UniqueCode") 

Bilan_20211214_update <- anti_join(Bilan_20211214, Dead, by ="UniqueCode")

Bilan <- Bilan_20211214_update
#pour chaque espèce, je veux 5 individus dans le groupe traitement D1 et 3 individus dans le groupe control C0

#help by Tristan 
sp <- NULL
trtmt <- c(rep(c(rep("C0",3),rep("D1",5)),7))
code <- NULL
for (i in levels(as.factor(Bilan$Species))){
  control_frame <- Bilan[Bilan$Species== i&Bilan$Treatment == "C0",]
  a <-sample(control_frame$UniqueCode, 3, replace = FALSE)
  
  D1_frame <- Bilan[Bilan$Species== i&Bilan$Treatment == "D1",]
  b <-sample(D1_frame$UniqueCode, 5, replace = FALSE)
  code <- c(code,a,b) #stockage
  sp <- c(sp,rep(i,8)) #creation vecteur espèces mais on peut faire en dehors de la boucle comme trtmt
  
  
}
tirage_T51 <- data.frame(species=sp,treatment=trtmt,UniqueCode=code)

Bilan <- Bilan %>% select(UniqueCode, Block, Sblock, Remarques)

tirage_T51 <- tirage_T51 %>% left_join(Bilan, by = "UniqueCode")

tirage_T51 <- tirage_T51 %>% relocate(UniqueCode)

tirage_T51 <- tirage_T51 %>% relocate(Block, .after = UniqueCode)

tirage_T51 <- tirage_T51 %>% relocate(Sblock, .after = Block)
tirage_T51 <- tirage_T51 %>% relocate(treatment, .after = Sblock)


tirage_T51 <- tirage_T51 %>% add_column (Height = NA, Diameter = NA, Nb_leaves = NA, RWC_FW = NA, RWC_SW = NA, RWC_DW = NA, LT1 = NA, LT2 = NA, LT3 = NA, LT4 = NA, LT5 = NA, MeanLT = NA, ScanLeaf = NA, ScanRoots = NA)

tirage_T51_destructif<- tirage_T51 %>% relocate(Remarques, .after = ScanRoots)

library(rio)
export(tirage_T51_destructif, "tirage_T51_destructif.csv")


## T57 -----------

#Dry treatment D2 + recovery

#Retirer individus déjà selectionnés en T0, T21, T27, T51-----

t0_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hxXws0kkgQhkC3T023X6QZw5o-RAjnNo8c3_KcWfTpw/edit#gid=799954127", range = "destructif")

t21_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Vkmx3XCVR4L0QG22b_-zcIFutezT59gUlmr1ALW4V2Y/edit#gid=822184942", range= "destructif")

t27_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1SQekhd-T9yTKDTILUuHF0NY3Nlggy4enzFhSXLxJlK0/edit#gid=822184942", range= "destructif")

t51_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1lmgpcbXdVcC2de3_-uc3g0w_P8Ab7z1m4faLhd2EmXI/edit#gid=822184942", range = "destructif")

#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tibble")
library(tibble)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
data <- read_excel("data/20211018_amont/DiametreHauteurLeaf.xlsx", 
                   sheet = "Indv")

dataomit <- drop_na(data, Height)

data2<- dataomit

Bilan_20211115 <- anti_join(data2, t0_destructifs, by= "UniqueCode")

Bilan_20211122 <- anti_join(Bilan_20211115, t21_destructifs, by= "UniqueCode")

Bilan_20211214 <- anti_join(Bilan_20211122, t27_destructifs, by= "UniqueCode")

Bilan_20211220 <- anti_join(Bilan_20211214, t51_destructifs, by="UniqueCode" )

#S'il y a des morts entre temps

mortality <- read_excel("data/202112_01_mortality/1-mortality stages011222.xlsx", 
                        sheet = "01_0812")

mortality2 <- subset(mortality, Alive_08_12!="0")

Dead <- anti_join(Bilan_20211220, mortality2, by= "UniqueCode") 

Bilan_20211220_update <- anti_join(Bilan_20211220, Dead, by ="UniqueCode")

Bilan <- Bilan_20211220_update

#pour chaque espèce, je veux 5 individus dans le groupe traitement D2 et 3 individus dans le groupe control C0

#help by Tristan 
sp <- NULL
trtmt <- c(rep(c(rep("C0",3),rep("D2",5)),7))
code <- NULL
for (i in levels(as.factor(Bilan$Species))){
  control_frame <- Bilan[Bilan$Species== i&Bilan$Treatment == "C0",]
  a <-sample(control_frame$UniqueCode, 3, replace = FALSE)
  
  D2_frame <- Bilan[Bilan$Species== i&Bilan$Treatment == "D2",]
  b <-sample(D2_frame$UniqueCode, 5, replace = FALSE)
  code <- c(code,a,b) #stockage
  sp <- c(sp,rep(i,8)) #creation vecteur espèces mais on peut faire en dehors de la boucle comme trtmt
  
  
}
tirage_T57 <- data.frame(species=sp,treatment=trtmt,UniqueCode=code)

Bilan <- Bilan %>% select(UniqueCode, Block, Sblock, Remarques)

tirage_T57 <- tirage_T57 %>% left_join(Bilan, by = "UniqueCode")

tirage_T57 <- tirage_T57 %>% relocate(UniqueCode)

tirage_T57 <- tirage_T57 %>% relocate(Block, .after = UniqueCode)

tirage_T57 <- tirage_T57 %>% relocate(Sblock, .after = Block)
tirage_T57 <- tirage_T57 %>% relocate(treatment, .after = Sblock)


tirage_T57 <- tirage_T57 %>% add_column (RWC_FW = NA, RWC_SW = NA, RWC_DW = NA, LT1 = NA, LT2 = NA, LT3 = NA, LT4 = NA, LT5 = NA, MeanLT = NA, ScanLeaf = NA, ScanRoots = NA)

tirage_T57_destructif<- tirage_T57 %>% relocate(Remarques, .after = ScanRoots)

library(rio)
export(tirage_T57_destructif, "tirage_T57_destructif.csv")

##T71---------------

#Dry treatment D3

#Retirer individus déjà selectionnés en T0, T21, T27, T51, T57-----

t0_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hxXws0kkgQhkC3T023X6QZw5o-RAjnNo8c3_KcWfTpw/edit#gid=799954127", range = "destructif")

t21_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Vkmx3XCVR4L0QG22b_-zcIFutezT59gUlmr1ALW4V2Y/edit#gid=822184942", range= "destructif")

t27_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1SQekhd-T9yTKDTILUuHF0NY3Nlggy4enzFhSXLxJlK0/edit#gid=822184942", range= "destructif")

t51_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1lmgpcbXdVcC2de3_-uc3g0w_P8Ab7z1m4faLhd2EmXI/edit#gid=822184942", range = "destructif")

t57_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1rh34QE6izuGEnB4eQFLn4lv5oEf2CaFscT1Tsjc6NQA/edit#gid=539250691", range = "destructif")

#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tibble")
library(tibble)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
data <- read_excel("data/20211018_amont/DiametreHauteurLeaf.xlsx", 
                   sheet = "Indv")

dataomit <- drop_na(data, Height)

data2<- dataomit

Bilan_20211115 <- anti_join(data2, t0_destructifs, by= "UniqueCode")

Bilan_20211122 <- anti_join(Bilan_20211115, t21_destructifs, by= "UniqueCode")

Bilan_20211214 <- anti_join(Bilan_20211122, t27_destructifs, by= "UniqueCode")

Bilan_20211220 <- anti_join(Bilan_20211214, t51_destructifs, by="UniqueCode" )

Bilan_20220105 <- anti_join(Bilan_20211220, t57_destructifs, by= "UniqueCode")

#S'il y a des morts entre temps

mortality <- read_excel("data/202112_01_mortality/mortality.stages20220104_2.xlsx")

mortality2 <- subset(mortality, Alive_04_01_2022!="0")

Dead <- anti_join(Bilan_20220105, mortality2, by= "UniqueCode") 

Bilan_20220105_update <- anti_join(Bilan_20220105, Dead, by ="UniqueCode")

Bilan <- Bilan_20220105_update

#pour chaque espèce, je veux 5 individus dans le groupe traitement D3 et 3 individus dans le groupe control C0

#help by Tristan 
sp <- NULL
trtmt <- c(rep(c(rep("C0",3),rep("D3",5)),7))
code <- NULL
for (i in levels(as.factor(Bilan$Species))){
  control_frame <- Bilan[Bilan$Species== i&Bilan$Treatment == "C0",]
  a <-sample(control_frame$UniqueCode, 3, replace = FALSE)
  
  D2_frame <- Bilan[Bilan$Species== i&Bilan$Treatment == "D3",]
  b <-sample(D2_frame$UniqueCode, 5, replace = TRUE)
  code <- c(code,a,b) #stockage
  sp <- c(sp,rep(i,8)) #creation vecteur espèces mais on peut faire en dehors de la boucle comme trtmt
  
  
}
tirage_T71 <- data.frame(species=sp,treatment=trtmt,UniqueCode=code)

Bilan <- Bilan %>% select(UniqueCode, Block, Sblock, Remarques)

tirage_T71 <- tirage_T71 %>% left_join(Bilan, by = "UniqueCode")

tirage_T71 <- tirage_T71 %>% relocate(UniqueCode)

tirage_T71 <- tirage_T71 %>% relocate(Block, .after = UniqueCode)

tirage_T71 <- tirage_T71 %>% relocate(Sblock, .after = Block)
tirage_T71 <- tirage_T71 %>% relocate(treatment, .after = Sblock)


tirage_T71 <- tirage_T71 %>% add_column (RWC_FW = NA, RWC_SW = NA, RWC_DW = NA, LT1 = NA, LT2 = NA, LT3 = NA, LT4 = NA, LT5 = NA, MeanLT = NA, ScanLeaf = NA, ScanRoots = NA)

tirage_T71_destructif<- tirage_T71 %>% relocate(Remarques, .after = ScanRoots)

library(rio)
export(tirage_T71_destructif, "tirage_T71_destructif.csv")

