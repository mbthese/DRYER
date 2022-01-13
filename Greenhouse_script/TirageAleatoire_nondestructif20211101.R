# tirage non destructif pour mesure gs entre les campagnes de prélèvement

#Tirage aleatoire pour les 8 individus (traits nondestructifs (3 Co et 5 autres traitements tout confondus)


library(readxl)
data <- read_excel("data/20211018_amont/DiametreHauteurLeaf.xlsx", 
                                          sheet = "Indv")
library(tidyr)
dataomit <- drop_na(data, Height)

data2<- dataomit

#moins ceux prélevés dreniere fois à t0
t0_destructifs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hxXws0kkgQhkC3T023X6QZw5o-RAjnNo8c3_KcWfTpw/edit#gid=799954127", range = "destructif")

##2021 11 01 gs------
install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
Bilan_20211101 <- anti_join(data2, t0_destructifs, by= "UniqueCode")
ControlGroup <- Bilan_20211101[Bilan_20211101$Treatment == "C0", ]  
TreatmentGroups <- anti_join(Bilan_20211101, ControlGroup, by ="UniqueCode") 
###Eperua falcata -------
#tirage de 3 control et 5 traitement tout confondus = 8 gs a mesurer total

epefal_c0<- ControlGroup[ControlGroup$Species == "Epe.fal", ] 
sample_epefal_c0 <- sample(epefal_c0$UniqueCode, 3, replace = FALSE)

epefal_T <- TreatmentGroups[TreatmentGroups$Species == "Epe.fal",]
sample_epefal_T <- sample(epefal_T$UniqueCode, 5, replace = FALSE)


### Iryanthera hostmannii ----------
#tirage de 3 control et 5 traitement tout confondus = 8 gs a mesurer total

iryhos_c0<- ControlGroup[ControlGroup$Species == "Iry.hos", ] 
sample_iryhos_c0 <- sample(iryhos_c0$UniqueCode, 3, replace = FALSE)

iryhos_T <- TreatmentGroups[TreatmentGroups$Species == "Iry.hos",]
sample_iryhos_T <- sample(iryhos_T$UniqueCode, 5, replace = FALSE)


### Jacaranda copaia ----------
#tirage de 3 control et 5 traitement tout confondus = 8 gs a mesurer total

jaccop_c0<- ControlGroup[ControlGroup$Species == "Jac.cop", ] 
sample_jaccop_c0 <- sample(jaccop_c0$UniqueCode, 3, replace = FALSE)

jaccop_T <- TreatmentGroups[TreatmentGroups$Species == "Jac.cop",]
sample_jaccop_T <- sample(jaccop_T$UniqueCode, 5, replace = FALSE)


### Pterocarpus officinalis ----------
#tirage de 3 control et 5 traitement tout confondus = 8 gs a mesurer total

pteoff_c0<- ControlGroup[ControlGroup$Species == "Pte.off", ] 
sample_pteoff_c0 <- sample(pteoff_c0$UniqueCode, 3, replace = FALSE)

pteoff_T <- TreatmentGroups[TreatmentGroups$Species == "Pte.off",]
sample_pteoff_T <- sample(pteoff_T$UniqueCode, 5, replace = FALSE)


### Symphonia globulifera ----------
#tirage de 3 control et 5 traitement tout confondus = 8 gs a mesurer total

symglo_c0<- ControlGroup[ControlGroup$Species == "Sym.off", ] 
sample_symglo_c0 <- sample(symglo_c0$UniqueCode, 3, replace = FALSE)

symglo_T <- TreatmentGroups[TreatmentGroups$Species == "Sym.off",]
sample_symglo_T <- sample(symglo_T$UniqueCode, 5, replace = FALSE)

### Tachigali melinonii ----------
#tirage de 3 control et 5 traitement tout confondus = 8 gs a mesurer total

tacmel_c0<- ControlGroup[ControlGroup$Species == "Tac.mel", ] 
sample_tacmel_c0 <- sample(tacmel_c0$UniqueCode, 3, replace = FALSE)

tacmel_T <- TreatmentGroups[TreatmentGroups$Species == "Tac.mel",]
sample_tacmel_T <- sample(tacmel_T$UniqueCode, 5, replace = FALSE)

### Virola surinamensis ----------
#tirage de 3 control et 5 traitement tout confondus = 8 gs a mesurer total

virsur_c0<- ControlGroup[ControlGroup$Species == "Vir.sur", ] 
sample_virsur_c0 <- sample(virsur_c0$UniqueCode, 3, replace = FALSE)

virsur_T <- TreatmentGroups[TreatmentGroups$Species == "Vir.sur",]
sample_virsur_T <- sample(virsur_T$UniqueCode, 5, replace = FALSE)


#BILAN --------------
Tirage_gs_c0_20211101 <- do.call(rbind, Map(data.frame, "epefal"=sample_epefal_c0, "iryhos"=sample_iryhos_c0, "jaccop"=sample_jaccop_c0, "pteoff"=sample_pteoff_c0, "symglo"=sample_symglo_c0, "tacmel"=sample_tacmel_c0, "virsur"=sample_virsur_c0 ))


Tirage_gs_T_20211101 <- do.call(rbind, Map(data.frame, "epefal"=sample_epefal_T, "iryhos"=sample_iryhos_T, "jaccop"=sample_jaccop_T, "pteoff"=sample_pteoff_T, "symglo"=sample_symglo_T, "tacmel"= sample_tacmel_T, "virsur"= sample_virsur_T ))

library(rio)
#export(Tirage_gs_c0_20211101 , "Tirage_gs_c0_20211101.csv")
#export(Tirage_gs_T_20211101 , "Tirage_gs_T_20211101.csv")

essai_graph_01 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1FaJTPoIdiUYW0xuebZsOPCclcZOxSZy6STZRLpl75VI/edit#gid=0", range = "gs")

library(dplyr)

essai_graph$Treatment[which(essai_graph$Treatment == "D1")] <- 'T'
essai_graph$Treatment[which(essai_graph$Treatment == "D2")] <- 'T'
essai_graph$Treatment[which(essai_graph$Treatment == "D3")] <- 'T'

library(ggplot2)

ggplot(essai_graph) +
  aes(x = Species, y = gs, fill = Treatment) +
  geom_boxplot(shape = "circle") +
  scale_fill_manual(values = c(C0 = "#6AA6E9", 
                               T = "#8F9599")) +
  theme_classic() +
  theme(legend.position = "none")





# 2021 11 08 gs ---------

install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
Bilan_20211108 <- anti_join(data2, t0_destructifs, by= "UniqueCode")
ControlGroup <- Bilan_20211101[Bilan_20211101$Treatment == "C0", ]  
TreatmentGroups <- anti_join(Bilan_20211101, ControlGroup, by ="UniqueCode") 
###Eperua falcata -------
#tirage de 3 control et 5 traitement tout confondus = 8 gs a mesurer total

epefal_c0<- ControlGroup[ControlGroup$Species == "Epe.fal", ] 
sample_epefal_c0 <- sample(epefal_c0$UniqueCode, 3, replace = FALSE)

epefal_T <- TreatmentGroups[TreatmentGroups$Species == "Epe.fal",]
sample_epefal_T <- sample(epefal_T$UniqueCode, 5, replace = FALSE)


### Iryanthera hostmannii ----------
#tirage de 3 control et 5 traitement tout confondus = 8 gs a mesurer total

iryhos_c0<- ControlGroup[ControlGroup$Species == "Iry.hos", ] 
sample_iryhos_c0 <- sample(iryhos_c0$UniqueCode, 3, replace = FALSE)

iryhos_T <- TreatmentGroups[TreatmentGroups$Species == "Iry.hos",]
sample_iryhos_T <- sample(iryhos_T$UniqueCode, 5, replace = FALSE)


### Jacaranda copaia ----------
#tirage de 3 control et 5 traitement tout confondus = 8 gs a mesurer total

jaccop_c0<- ControlGroup[ControlGroup$Species == "Jac.cop", ] 
sample_jaccop_c0 <- sample(jaccop_c0$UniqueCode, 3, replace = FALSE)

jaccop_T <- TreatmentGroups[TreatmentGroups$Species == "Jac.cop",]
sample_jaccop_T <- sample(jaccop_T$UniqueCode, 5, replace = FALSE)


### Pterocarpus officinalis ----------
#tirage de 3 control et 5 traitement tout confondus = 8 gs a mesurer total

pteoff_c0<- ControlGroup[ControlGroup$Species == "Pte.off", ] 
sample_pteoff_c0 <- sample(pteoff_c0$UniqueCode, 3, replace = FALSE)

pteoff_T <- TreatmentGroups[TreatmentGroups$Species == "Pte.off",]
sample_pteoff_T <- sample(pteoff_T$UniqueCode, 5, replace = FALSE)


### Symphonia globulifera ----------
#tirage de 3 control et 5 traitement tout confondus = 8 gs a mesurer total

symglo_c0<- ControlGroup[ControlGroup$Species == "Sym.off", ] 
sample_symglo_c0 <- sample(symglo_c0$UniqueCode, 3, replace = FALSE)

symglo_T <- TreatmentGroups[TreatmentGroups$Species == "Sym.off",]
sample_symglo_T <- sample(symglo_T$UniqueCode, 5, replace = FALSE)

### Tachigali melinonii ----------
#tirage de 3 control et 5 traitement tout confondus = 8 gs a mesurer total

tacmel_c0<- ControlGroup[ControlGroup$Species == "Tac.mel", ] 
sample_tacmel_c0 <- sample(tacmel_c0$UniqueCode, 3, replace = FALSE)

tacmel_T <- TreatmentGroups[TreatmentGroups$Species == "Tac.mel",]
sample_tacmel_T <- sample(tacmel_T$UniqueCode, 5, replace = FALSE)

### Virola surinamensis ----------
#tirage de 3 control et 5 traitement tout confondus = 8 gs a mesurer total

virsur_c0<- ControlGroup[ControlGroup$Species == "Vir.sur", ] 
sample_virsur_c0 <- sample(virsur_c0$UniqueCode, 3, replace = FALSE)

virsur_T <- TreatmentGroups[TreatmentGroups$Species == "Vir.sur",]
sample_virsur_T <- sample(virsur_T$UniqueCode, 5, replace = FALSE)


#BILAN --------------
Tirage_gs_c0_20211108 <- do.call(rbind, Map(data.frame, "epefal"=sample_epefal_c0, "iryhos"=sample_iryhos_c0, "jaccop"=sample_jaccop_c0, "pteoff"=sample_pteoff_c0, "symglo"=sample_symglo_c0, "tacmel"=sample_tacmel_c0, "virsur"=sample_virsur_c0 ))


Tirage_gs_T_20211108 <- do.call(rbind, Map(data.frame, "epefal"=sample_epefal_T, "iryhos"=sample_iryhos_T, "jaccop"=sample_jaccop_T, "pteoff"=sample_pteoff_T, "symglo"=sample_symglo_T, "tacmel"= sample_tacmel_T, "virsur"= sample_virsur_T ))

library(rio)
export(Tirage_gs_c0_20211108 , "Tirage_gs_c0_20211108.csv")
export(Tirage_gs_T_20211108 , "Tirage_gs_T_20211108.csv")

essai_graph_08 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1FaJTPoIdiUYW0xuebZsOPCclcZOxSZy6STZRLpl75VI/edit#gid=2004401054", range = "gs sem 08/11")

library(dplyr)

essai_graph_08$Treatment[which(essai_graph_08$Treatment == "D1")] <- 'T'
essai_graph_08$Treatment[which(essai_graph_08$Treatment == "D2")] <- 'T'
essai_graph_08$Treatment[which(essai_graph_08$Treatment == "D3")] <- 'T'

library(ggplot2)

ggplot(essai_graph_08) +
  aes(x = Species, y = gs, fill = Treatment) +
  geom_boxplot(shape = "circle") +
  scale_fill_manual(values = c(C0 = "#6AA6E9", 
                               T = "#8F9599")) +
  theme_classic() +
  theme(legend.position = "none")




