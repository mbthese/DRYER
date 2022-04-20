#ANOVA Camille ----

#the p-value for one-way ANOVA is less than 0.05 indicates that at least one of the tratment groups differens from the others. 

#est-ce ok de faire des ANOVA avec des facteurs (mes traitements?)

#voir l'effet bloc avec les prochaines mesures. on fera l'anova sur les deltas de croissances pour voir l'effet bloc. 

library(readxl)
data <- read_excel("data/Amont/DiametreHauteurLeaf.xlsx", 
                                  sheet = "Indv")

data$Treatment<- as.factor(data$Treatment)
data$Sblock<-as.factor(data$Sblock)
data$Species<-as.factor(data$Species)
data$Block<-as.factor(data$Block)

#quel type ANOVA? Type I or type II c'est les statistiqus utilisées à l'interieur de l'ANOVA. type II par defaut. 
#anova = comparaison de moyennes qui sont calculés grace à des modèles lineaires

# dire quel est mon modele linaire : souvent c'est lm, le basique de R

modele1 <- lm(Height~Sblock+Treatment, data=data) #sans les interactions. mais sinon la reponse au traitement en fonction des espèces on rajoute ":" ou "*" à verifier . * est un raccourci a*b c'est a+b+a:b. c'est à dire comme ca:

modele2 <- lm(Height~Sblock:Species+Treatment:Species, data=data)#je veux savoir s'il y a un effet block pour chaque espèce. 

modele3<- lm(Height~Sblock+Treatment+Species, data=data)

modele4<- lm(Height~Sblock+Treatment*Species, data=data) #pour voir l'effet des sous blocs, des traitements, traitements n fonciton espèce, et espèce.

#pour voir intraction avec ls espèces:
modele5<- lm(Height~Treatment*Species+Sblock, data=data)


summary(modele4)
summary(modele1) # dans les coef, mon traitement C0 est mon traitement de reference. A voir comment le faire manuellement si jamais ca change. et mes estimates des treatment D1,D2,D3 sont des valeurs qui s'ajoutent ou s'enlèvent par rapport à la reference. 

plot(modele4)
#pour quantifier et pas juste regarder graphiquement: shapiro-wilk (normalité des residus) et test de levene ou test de bartlett (pour l'homoskedasticity: homogeneité des variances) parce que c'est les hypotheses du modele lineaire qui on a dit : N(0,sigma).le mieux pour pas se prendre la tete : faire representation graphique, un histogram des residus et on regarde si c'est une gaussienne.

hist(modele4$residuals)
#apres les summary, il faut tester la distribution qui doit etre normale pour les résidus. 

#plusieurs manieres de faire une ANOVA

car::Anova(modele4)

stats::anova(modele1)

stats::aov(formula = Height~Sblock+Treatment, data=data)

#utiliser le test de tukey Honest Significant Differences HSD r mais est-ce reellement necessaire?: 

aov<-stats::aov(formula = Height~Sblock+Treatment, data=data)

TukeyHSD(aov)

#modele lineaire mixte

dataomit<- na.omit(data[,-12])

modelemixte<-lme4::lmer(Height~Treatment*Species+(1|Sblock), data = dataomit) #effet aleatoire sbloc sur l'intercept, voir si entre les blocs ca varie. la variance de l'effet bloc suffit. toutce qui est devant c'est l'effet fixe.

modelemixte2<-lme4::lmer(Height~Treatment+Species+(1|Sblock), data = data) 


summary(modelemixte)


#note pour apres: s'il faut transformer: miux vaut (jeanne) utiliser modele generalisé linéaire. generaliser : mettre une fontion lien comme log(). 


#effet aléatoire: voir s'il y a une variance au niveau des blocs. 

# Cours Marie-Pierre Etienne --------

#install.package('remotes')
remotes::install_github('MarieEtienne/coursesdata')
bats<- coursesdata::bats
bats$Diet <- as.factor(bats$Diet)

model_bat <- lm(AUD ~ Diet, data = bats)
summary(model_bat) #< p-value : 2e-16 *** il y a des différences entre les espèces.
