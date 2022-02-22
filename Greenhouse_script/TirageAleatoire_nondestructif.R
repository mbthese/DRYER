#Tirage aleatoire pour les 8 individus traits nondestructifs 


library(readxl)
data <- read_excel("data/Amont/DiametreHauteurLeaf.xlsx", 
                   sheet = "Indv")
library(tidyr)
dataomit <- drop_na(data, Height)

data2<- dataomit

Bilan_t0<- table(data2$Species)

#pour chaque espèce, je veux 8 individus sur l'ensemble des individus


##T0------

#Eperua falcata -----

t0_epefal <- data2[data2$Species == "Epe.fal", ] 
t0_epefal_NONdest <- sample(t0_epefal$UniqueCode, 8, replace = FALSE) #si on met true, on peut avoir 2 fois le meme individu

RAB_t0_epefal_NONdest <-sample(t0_epefal$UniqueCode, 8, replace = FALSE)


#Iryanthera hostmannii-----

t0_iryhos <- data2[data2$Species == "Iry.hos", ] 
t0_iryhos_NONdest <- sample(t0_iryhos$UniqueCode, 8, replace = FALSE) #si on met true, on peut avoir 2 fois le meme individu

RAB_t0_iryhos_NONdest <-sample(t0_iryhos$UniqueCode, 8, replace = FALSE)


#Jacaranda copaia -----

t0_jaccop <- data2[data2$Species == "Jac.cop", ] 
t0_jaccop_NONdest <- sample(t0_jaccop$UniqueCode, 8, replace = FALSE) #si on met true, on peut avoir 2 fois le meme individu

RAB_t0_jaccop_NONdest <-sample(t0_jaccop$UniqueCode, 8, replace = FALSE)


#Pterocarpus officinalis----

t0_pteoff <- data2[data2$Species == "Pte.off", ] 
t0_pteoff_NONdest <- sample(t0_pteoff$UniqueCode, 8, replace = FALSE) #si on met true, on peut avoir 2 fois le meme individu en meme temps

RAB_t0_pteoff_NONdest <-sample(t0_pteoff$UniqueCode, 8, replace = FALSE)


#Symphonia globilifera----

t0_symglo <- data2[data2$Species == "Sym.off", ] 
t0_symglo_NONdest <- sample(t0_symglo$UniqueCode, 8, replace = FALSE) #si on met true, on peut avoir 2 fois le meme individu

RAB_t0_symglo_NONdest <-sample(t0_symglo$UniqueCode, 8, replace = FALSE)

#Tachigali melinonii----

t0_tacmel <- data2[data2$Species == "Tac.mel", ] 
t0_tacmel_NONdest <- sample(t0_tacmel$UniqueCode, 8, replace = FALSE) #si on met true, on peut avoir 2 fois le meme individu

RAB_t0_tacmel_NONdest <-sample(t0_tacmel$UniqueCode, 8, replace = FALSE)


#Virola surinamensis----

t0_virsur <- data2[data2$Species == "Vir.sur", ] 
t0_virsur_NONdest <- sample(t0_virsur$UniqueCode, 8, replace = FALSE) #si on met true, on peut avoir 2 fois le meme individu

RAB_t0_virsur_NONdest <-sample(t0_virsur$UniqueCode, 8, replace = FALSE)

print(t0_epefal_NONdest)
print(t0_iryhos_NONdest)
print(t0_jaccop_NONdest)
print(t0_pteoff_NONdest)
print(t0_symglo_NONdest)
print(t0_tacmel_NONdest)
print(t0_virsur_NONdest)

Tirage_NONdes_t0 <- do.call(rbind, Map(data.frame, "epefal"=t0_epefal_NONdest, "iryhos"=t0_iryhos_NONdest, "jaccop"=t0_jaccop_NONdest, "pteoff"=t0_pteoff_NONdest, "symglo"=t0_symglo_NONdest, "tacmel"= t0_tacmel_NONdest, "virsur"= t0_virsur_NONdest ))


library(rio)
#export(Tirage_NONdes_t0, "Tirage_NONdes_t0.csv")


#PsyPro
Psypro_jaccop <- c(104,213,486,562,644,681,763,849)
sample(Psypro, 6, replace = FALSE)

Psypro_pteoff <- c(17,22,521,524,735,787,790,881)
sample(Psypro_pteoff, 6, replace = FALSE)

Psypro_Symglo <- c(25,32,161,206,556,559,757,894)
sample(Psypro_Symglo, 6, replace = FALSE)

Psypro_Tacmel <- c(178,179,180,241,340,437,549,803)
sample(Psypro_Tacmel, 6, replace = FALSE)

PsyPro_Virsur <- c(91,258,456,628,769,770,800,933)
sample(PsyPro_Virsur, 6, replace = FALSE)



##T21 ----- 
#Dry treatment at 21 days - D1

#Retirer individus déjà selectionnés en T0----

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

#pour chaque espèce, je veux 5 individus dans le groupe traitement D1 (mais en fait pour le tirage non destructif, tous les traitements D1, D2, D3 ont recu 21 jours de secheresse, donc on peut tirer aléatoirement des individus dans l'ensemble des traitements et pas que D1) et 3 individus dans le groupe control C0. Bilan = tirage de 3 control et 5 traitement tout confondus 
#au final c'est galere en pratique donc campagne t21 jaccop et pteoff and onwards, on preleve que dans D1

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

Bilan <- Bilan_20211115 %>% select(UniqueCode, Block, Sblock, Treatment, Remarques)

tirage_T21 <- tirage_T21 %>% left_join(Bilan, by = "UniqueCode")

tirage_T21 <- tirage_T21 %>% relocate(UniqueCode)

tirage_T21 <- tirage_T21 %>% relocate(Block, .after = UniqueCode)

tirage_T21 <- tirage_T21 %>% relocate(Sblock, .after = Block)
tirage_T21 <- tirage_T21 %>% relocate(treatment, .after = Sblock)


tirage_T21 <- tirage_T21 %>% add_column (Height = NA, Diameter = NA, Nb_leaves = NA, Chloro = NA, gs = NA, FvFm = NA, Asat = NA)

tirage_T21_NONdestructif<- tirage_T21 %>% relocate(Remarques, .after = Asat)

tirage_T21_NONdestructif <- tirage_T21_NONdestructif %>% rename(Real_Treatment = Treatment)

library(rio)
export(tirage_T21_NONdestructif, "tirage_T21_NONdestructif.csv")


##T27 --------------

#Dry treatment at 27 days - D2

#Retirer individus déjà selectionnés en T0 et T21-----

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


#pour chaque espèce, je veux 5 individus dans le groupe traitement D2 et 3 individus dans le groupe control C0. -----


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
tirage_T51<- data.frame(species=sp,treatment=trtmt,UniqueCode=code)

Bilan <- Bilan_20211122 %>% select(UniqueCode, Block, Sblock, Treatment, Remarques)

tirage_T51<- tirage_T51%>% left_join(Bilan, by = "UniqueCode")

tirage_T51<- tirage_T51%>% relocate(UniqueCode)

tirage_T51<- tirage_T51%>% relocate(Block, .after = UniqueCode)

tirage_T51<- tirage_T51%>% relocate(Sblock, .after = Block)
tirage_T51<- tirage_T51%>% relocate(treatment, .after = Sblock)


tirage_T51<- tirage_T51%>% add_column (Height = NA, Diameter = NA, Nb_leaves = NA, Chloro = NA, gs = NA, FvFm = NA, Asat = NA)

tirage_T51_NONdestructif<- tirage_T51%>% relocate(Remarques, .after = Asat)

tirage_T51_NONdestructif <- tirage_T51_NONdestructif %>% rename(Real_Treatment = Treatment)

library(rio)
export(tirage_T51_NONdestructif, "tirage_T51_NONdestructif.csv")

##T51--------------

#Dry treatment D1 + recovery of 29 days

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

#pour chaque espèce, je veux 5 individus dans le groupe traitement D1 et 3 individus dans le groupe control C0. -----


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

Bilan <- Bilan %>% select(UniqueCode, Block, Sblock, Treatment, Remarques)

tirage_T51 <- tirage_T51 %>% left_join(Bilan, by = "UniqueCode")

tirage_T51 <- tirage_T51 %>% relocate(UniqueCode)

tirage_T51 <- tirage_T51 %>% relocate(Block, .after = UniqueCode)

tirage_T51<- tirage_T51%>% relocate(Sblock, .after = Block)
tirage_T51<- tirage_T51%>% relocate(treatment, .after = Sblock)


tirage_T51<- tirage_T51%>% add_column (Height = NA, Diameter = NA, Nb_leaves = NA, Chloro = NA, gs = NA, FvFm = NA, Asat = NA)

tirage_T51_NONdestructif<- tirage_T51%>% relocate(Remarques, .after = Asat)

tirage_T51_NONdestructif <- tirage_T51_NONdestructif %>% rename(Real_Treatment = Treatment)

library(rio)
export(tirage_T51_NONdestructif, "tirage_T51_NONdestructif.csv")

##T57---------------

#Dry treatment D2 + recovery of 29 days

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

#pour chaque espèce, je veux 5 individus dans le groupe traitement D2 et 3 individus dans le groupe control C0. -----


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

Bilan <- Bilan %>% select(UniqueCode, Block, Sblock, Treatment, Remarques)

tirage_T57 <- tirage_T57 %>% left_join(Bilan, by = "UniqueCode")

tirage_T57 <- tirage_T57 %>% relocate(UniqueCode)

tirage_T57 <- tirage_T57 %>% relocate(Block, .after = UniqueCode)

tirage_T57<- tirage_T57%>% relocate(Sblock, .after = Block)
tirage_T57<- tirage_T57%>% relocate(treatment, .after = Sblock)


tirage_T57<- tirage_T57%>% add_column (Height = NA, Diameter = NA, Nb_leaves = NA, Chloro = NA, gs = NA, FvFm = NA, Asat = NA)

tirage_T57_NONdestructif<- tirage_T57%>% relocate(Remarques, .after = Asat)

tirage_T57_NONdestructif <- tirage_T57_NONdestructif %>% rename(Real_Treatment = Treatment)

library(rio)
export(tirage_T57_NONdestructif, "tirage_T57_NONdestructif.csv")

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

#comme on ne s'interesse plus aux D1 et D2, il n'y aura ici que les D3 et C0, d'ou le changemente brutal du nombre d'indv: 661 à 276

restant <- Bilan %>% count(Species, Treatment)

#pour chaque espèce, je veux 5 individus dans le groupe traitement D3 et 3 individus dans le groupe control C0. -----


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

Bilan <- Bilan %>% select(UniqueCode, Block, Sblock, Treatment, Remarques)

tirage_T71 <- tirage_T71 %>% left_join(Bilan, by = "UniqueCode")

tirage_T71 <- tirage_T71 %>% relocate(UniqueCode)

tirage_T71 <- tirage_T71 %>% relocate(Block, .after = UniqueCode)

tirage_T71<- tirage_T71%>% relocate(Sblock, .after = Block)
tirage_T71<- tirage_T71%>% relocate(treatment, .after = Sblock)


tirage_T71<- tirage_T71%>% add_column (Height = NA, Diameter = NA, Nb_leaves = NA, Chloro = NA, gs = NA, FvFm = NA, Asat = NA)

tirage_T71_NONdestructif<- tirage_T71%>% relocate(Remarques, .after = Asat)

tirage_T71_NONdestructif <- tirage_T71_NONdestructif %>% rename(Real_Treatment = Treatment)

library(rio)
export(tirage_T71_NONdestructif, "tirage_T71_NONdestructif.csv")

#T101 - R3 --------------


#Dry treatment D3

#Retirer individus déjà selectionnés en T0, T21, T27, T51, T57-----

t0_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hxXws0kkgQhkC3T023X6QZw5o-RAjnNo8c3_KcWfTpw/edit#gid=799954127", range = "destructif")

t21_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Vkmx3XCVR4L0QG22b_-zcIFutezT59gUlmr1ALW4V2Y/edit#gid=822184942", range= "destructif")

t27_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1SQekhd-T9yTKDTILUuHF0NY3Nlggy4enzFhSXLxJlK0/edit#gid=822184942", range= "destructif")

t51_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1lmgpcbXdVcC2de3_-uc3g0w_P8Ab7z1m4faLhd2EmXI/edit#gid=822184942", range = "destructif")

t57_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1rh34QE6izuGEnB4eQFLn4lv5oEf2CaFscT1Tsjc6NQA/edit#gid=539250691", range = "destructif")

t71_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1al3U5pv2FQ0dWF6sbroB_6kJdzW1OnaLVpohykcKx7c/edit#gid=822184942", range = "destructif")

#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tibble")
library(tibble)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
data <- read_excel("Greenhouse_data/20211018_amont/DiametreHauteurLeaf.xlsx", 
                   sheet = "Indv")

dataomit <- drop_na(data, Height)

data2<- dataomit

Bilan_20211115 <- anti_join(data2, t0_destructifs, by= "UniqueCode")

Bilan_20211122 <- anti_join(Bilan_20211115, t21_destructifs, by= "UniqueCode")

Bilan_20211214 <- anti_join(Bilan_20211122, t27_destructifs, by= "UniqueCode")

Bilan_20211220 <- anti_join(Bilan_20211214, t51_destructifs, by="UniqueCode" )

Bilan_20220105 <- anti_join(Bilan_20211220, t57_destructifs, by= "UniqueCode")

Bilan_20220203 <- anti_join(Bilan_20220105, t71_destructifs, by= "UniqueCode")


#S'il y a des morts entre temps------

mortality <-read.csv("C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/DRYER/3-DRYERproject/Greenhouse_data/202112_01_mortality/mortality.stages310122.csv", header=TRUE, sep=";")

Dead <- subset(mortality, Alive_31_01!="0")

Dead <- anti_join(Bilan_20220203, Dead, by= "UniqueCode") 

Bilan_20220203_update <- anti_join(Bilan_20220203, Dead, by ="UniqueCode")

Bilan <- Bilan_20220203_update

#comme on ne s'interesse plus aux D1 et D2----

restant <- Bilan %>% count(Species, Treatment)

#pour chaque espèce, je veux 5 individus dans le groupe traitement D3 et 3 individus dans le groupe control C0. -----


sp <- NULL
trtmt <- c(rep(c(rep("C0",3),rep("D3",5)),5))
code <- NULL
Bilan <- subset(Bilan, Species != "Epe.fal" & Species != "Vir.sur" ) 
#Il n'y a plus de Virsur ni de Epefal en D3: morts, sinon boucle ne fonctionne pas

for (i in levels(as.factor(Bilan$Species))){
  control_frame <- Bilan[Bilan$Species== i&Bilan$Treatment == "C0",]
  a <-sample(control_frame$UniqueCode, 3, replace = FALSE)
  
  D3_frame <- Bilan[Bilan$Species== i&Bilan$Treatment == "D3",]
  b <-sample(D3_frame$UniqueCode, 5, replace = TRUE)
  code <- c(code,a,b) #stockage
  sp <- c(sp,rep(i,8)) #creation vecteur espèces mais on peut faire en dehors de la boucle comme trtmt
  
  
}
tirage_T101 <- data.frame(species=sp,treatment=trtmt,UniqueCode=code)

Bilan <- Bilan %>% select(UniqueCode, Block, Sblock, Treatment, Remarques)

tirage_T101 <- tirage_T101 %>% left_join(Bilan, by = "UniqueCode")

tirage_T101 <- tirage_T101 %>% relocate(UniqueCode)

tirage_T101 <- tirage_T101 %>% relocate(Block, .after = UniqueCode)

tirage_T101<- tirage_T101%>% relocate(Sblock, .after = Block)
tirage_T101<- tirage_T101%>% relocate(treatment, .after = Sblock)


tirage_T101<- tirage_T101%>% add_column (Height = NA, Diameter = NA, Nb_leaves = NA, Chloro = NA, gs = NA, FvFm = NA, Asat = NA)

tirage_T101_NONdestructif<- tirage_T101%>% relocate(Remarques, .after = Asat)

tirage_T101_NONdestructif <- tirage_T101_NONdestructif %>% rename(Real_Treatment = Treatment)

library(rio)
export(tirage_T101_NONdestructif, "tirage_T101_NONdestructif.csv")

#Ajout Epe.fal et Vir.sur Controle---------

ControlGroup <- Bilan[Bilan$Treatment == "C0", ]  

#Eperua falcata 

epefal_C0 <- ControlGroup[ControlGroup$Species == "Epe.fal", ] 

T101_epefal_NONdestruction <-sample(epefal_C0$UniqueCode, 3, replace = FALSE)
#results: 217;508;494

#Virola surinamensis
virsur_C0 <- ControlGroup[ControlGroup$Species== "Vir.sur",]
T101_virsur_NONdestruction <-sample(virsur_C0$UniqueCode, 3, replace = FALSE)
#1results: 320;455;318
