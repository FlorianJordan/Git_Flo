###### Rdy to go ##### 
getwd()
setwd("C:/Users/flori/Desktop/dossier_mere/universite/Session_3/ECL516_Ecologie_animale")
setwd("~/Desktop/maitrise /Credit_de_recherche")
dir()

library(ggplot2)
library(ggpubr)
#dev.off() si erreur graph
library(readxl)
library(rstatix)
library(ggpubr)
library(tidyverse)
library(lme4)
library(visreg)
library("readxl")
nest <- aov(CO2$Incubation_24h_ppm ~ CO2$Facteur_vdt / CO2$Facteur_litiere / CO2$Facteur_Chaux/CO2$Tx)
summary(nest)
summary(donnee_tableau_excel_CO2)


ggplot(CO2, aes(x=factor(CO2$Tx), y=CO2$Incubation_24h_ppm, fill=CO2$Facteur_litiere)) +
  geom_boxplot()
bxp <- ggboxplot(
  CO2, x = "Facteur_vdt", y = "Incubation_24h_ppm", 
  color = "Facteur_Chaux", palette = "jco", facet.by = "Facteur_litiere"
)
bxp
####### mesure répété CO2 ####

donnee_tableau_excel_CO2 <- read_excel("Donnee_CO2.xlsx")
CO2<-donnee_tableau_excel_CO2
summary(CO2)
CO2$Facteur_vdt<- as.factor(CO2$Facteur_vdt) 
CO2$Facteur_litiere<- as.factor(CO2$Facteur_litiere) 
CO2$Facteur_Chaux<- as.factor(CO2$Facteur_Chaux) 
CO2$Tx<- as.factor(CO2$Tx) 
summary(CO2)

mean(CO2$Incubation_24h_ppm2[CO2$Facteur_vdt=="Avec"])
((mean(N2O$Incubation_24h_ppm[N2O$Facteur_vdt=="Avec"&N2O$Facteur_litiere=="peuplier"&N2O$Facteur_Chaux=="sans"])*100)/mean(N2O$Incubation_24h_ppm[N2O$Facteur_vdt=="Avec"&N2O$Facteur_litiere=="erable/hetre"&N2O$Facteur_Chaux=="sans"]))-100
mean(N2O_ss_T0T1$Incubation_24h_ppm[N2O_ss_T0T1$Facteur_vdt=="Avec"&N2O_ss_T0T1$Facteur_litiere=="peuplier"&N2O_ss_T0T1$Facteur_Chaux=="avec"])
mean(N2O_ss_T0T1$Incubation_24h_ppm[N2O_ss_T0T1$Facteur_vdt=="Avec"&N2O_ss_T0T1$Facteur_litiere=="erable/hetre"&N2O_ss_T0T1$Facteur_Chaux=="avec"])



CO2 %>% group_by(Facteur_vdt, Facteur_litiere,Facteur_Chaux, Tx) %>%identify_outliers(Incubation_24h_ppm2)
CO2 %>% group_by(Facteur_vdt, Facteur_litiere,Facteur_Chaux, Tx) %>% shapiro_test(Incubation_24h_ppm2)
model  <- lm(Incubation_24h_ppm2 ~ Facteur_vdt*Facteur_litiere*Tx, data = CO2)
# Créer un QQ plot des résidus
ggqqplot(residuals(model))
# Calculer le test de normalité de Shapiro-Wilk
shapiro_test(residuals(model))
# Test levene
levene_test(data=CO2,Incubation_24h_ppm ~ Facteur_vdt*Facteur_litiere*Facteur_Chaux*Tx)

anovtest<-anova(model)
anovtest

Anova_Tot<-aov(data=CO2,Incubation_24h_ppm2 ~ Facteur_vdt*Facteur_litiere*Tx*Facteur_Chaux+Error(ID))
Anova_Tx<-aov(data=CO2,Incubation_24h_ppm2 ~ Tx+Error(ID))
Anova_Facteur_litiere<-aov(data=CO2,Incubation_24h_ppm2 ~ Facteur_litiere+Error(ID))
Anova_Facteur_vdt<-aov(data=CO2,Incubation_24h_ppm2 ~ Facteur_vdt+Error(ID))

summary(Anova_Tot)
summary(Anova_Tx)
summary(Anova_Facteur_litiere)
summary(Anova_Facteur_vdt)

ggplot(CO2, aes(x=factor(CO2$Facteur_litiere), y=CO2$Incubation_24h_ppm2, fill=CO2$Facteur_vdt)) +
  geom_boxplot()
ggplot(CO2, aes(x=factor(CO2$Facteur_litiere), y=CO2$Incubation_24h_ppm2)) +
  geom_boxplot()
ggplot(CO2, aes(x=factor(CO2$Facteur_vdt), y=CO2$Incubation_24h_ppm2)) +
  geom_boxplot()

model.tukey<- aov(Incubation_24h_ppm2 ~ Facteur_vdt,data = CO2)
TukeyHSD(model.tukey, conf.level=0.95)
plot(TukeyHSD(model.tukey, conf.level=0.95), las = 2)

Incub_T1<-CO2[1:32,]
Incub_T2<-CO2[33:64,]
Incub_T3<-CO2[65:96,]
Incub_T4<-CO2[97:128,]
 
summary(aov(data=Incub_T1,Incubation_24h_ppm2 ~ Facteur_vdt+Facteur_litiere+Error(ID)))
summary(aov(data=Incub_T2,Incubation_24h_ppm2 ~ Facteur_vdt+Facteur_litiere+Error(ID)))
summary(aov(data=Incub_T3,Incubation_24h_ppm2 ~ Facteur_vdt+Facteur_litiere+Error(ID)))
summary(aov(data=Incub_T4,Incubation_24h_ppm2 ~ Facteur_vdt+Facteur_litiere+Error(ID)))

####### Facteur Tx CO2 #####
# Anova répété 
Anova_Tx<-aov(data=CO2,Incubation_24h_ppm2 ~ Tx+Error(ID))
summary(Anova_Tx)
# t-test
Ttest_Tx<-CO2 %>%
  pairwise_t_test(
    Incubation_24h_ppm2 ~ Tx, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
Ttest_Tx
#Graph
Graph_Tx <- ggboxplot(CO2, x = "Tx", y = "Incubation_24h_ppm2",
                      color = "Tx", palette ="jco",
                      shape = "Tx",xlab = "Mesures", ylab = "Quantité de CO2 produit (ppm/jour)",legend.title = "Mesures")
Graph_Tx
my_comparisons <- list( c("T1", "T2"),c("T1","T3"),c("T1","T4") )
Graph_Tx + stat_compare_means(method = "t.test")+
  stat_pvalue_manual(Ttest_Tx, label = "p.adj.signif")

####### Facteur litiere CO2 et vdt #######
Anova_Facteur_litiere_vdt<-aov(data=CO2,Incubation_24h_ppm2 ~ Facteur_litiere+Facteur_vdt+Error(ID))
summary(Anova_Facteur_litiere_vdt)
bxp <- ggboxplot(
  CO2, x = "Facteur_litiere", y = "Incubation_24h_ppm",
  color = "Facteur_vdt", palette = "jco",xlab = "Mesures", ylab = "Quantité de CO2 produit (ppm/jour)",legend.title = "Vers de terre",ggtheme = theme_gray()
)
bxp

####### Facteur litiere CO2 #####
# Anova répété
Anova_Facteur_litiere<-aov(data=CO2,Incubation_24h_ppm2 ~ Facteur_litiere+Error(ID))
summary(Anova_Facteur_litiere)
# t-test
Ttest_Facteur_litiere<-CO2 %>%
  pairwise_t_test(
    Incubation_24h_ppm2 ~ Facteur_litiere, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
Ttest_Facteur_litiere
#Graph
Graph_litiere <- ggboxplot(CO2, x = "Facteur_litiere", y = "Incubation_24h_ppm2",
                           color = "Facteur_litiere", palette =c("#00AFBB", "#FC4E07"),
                           shape = "Facteur_litiere",xlab = "Litière", ylab = "Quantité de CO2 produit (ppm/jour)",legend.title = "Litière",ggtheme = theme_gray(),facet.by = "Tx")
Graph_litiere
my_comparisons <- list( c( "erable/hetre","peuplier") )
Graph_litiere + 
  stat_pvalue_manual(Ttest_Facteur_litiere,y.position = 10000, label = "p.adj.signif",bracket.nudge.y = -50)
model.tukey<- aov(Incubation_24h_ppm ~ Facteur_litiere,data = CO2)
TukeyHSD(model.tukey, conf.level=0.95)
####### Facteur vdt CO2 #####
# Anova répété 
Anova_Facteur_vdt<-aov(data=CO2,Incubation_24h_ppm2 ~ Facteur_vdt+Error(ID))
summary(Anova_Facteur_vdt)
# t-test
Ttest_Facteur_vdt <-CO2 %>%
  pairwise_t_test(
    Incubation_24h_ppm2 ~ Facteur_vdt, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
Ttest_Facteur_vdt
# Create the plot
#myplot <- ggboxplot(CO2, x = "Facteur_vdt", y = "Incubation_24h_ppm2")
# Add statistical test p-values
#Ttest_Facteur_vdt <- Ttest_Facteur_vdt %>% add_xy_position(x = "Facteur_vdt")
#myplot + stat_pvalue_manual(Ttest_Facteur_vdt, label = "p.adj.signif")

#Graph
Graph_vdt <- ggboxplot(CO2, x = "Facteur_vdt", y = "Incubation_24h_ppm2",
                       color = "Facteur_vdt", palette =c("#00AFBB", "#FC4E07"),
                       shape = "Facteur_vdt",xlab = "Vers de terre", ylab = "Quantité de CO2 produit (ppm/jour)",legend.title = "Vers de terre",ggtheme = theme_gray(),facet.by = "Tx")
Graph_vdt
my_comparisons <- list( c("Avec", "Sans") )
Graph_vdt + 
  stat_pvalue_manual(Ttest_Facteur_vdt, y.position = 10000, label = "p.adj.signif" ,bracket.nudge.y = -50)

model.tukey<- aov(Incubation_24h_ppm ~ Facteur_vdt*Tx,data = N2O)
TukeyHSD(model.tukey, conf.level=0.95)
####### mesure répété N2O ####
donnee_tableau_excel_N2O<-my_data <- read_excel("Donnee_N2O.xlsx")
N2O<-donnee_tableau_excel_N2O

summary(N2O)
N2O$Facteur_vdt<- as.factor(N2O$Facteur_vdt) 
N2O$Facteur_litiere<- as.factor(N2O$Facteur_litiere) 
N2O$Facteur_Chaux<- as.factor(N2O$Facteur_Chaux) 
N2O$Tx<- as.factor(N2O$Tx) 
summary(N2O)

N2O %>% group_by(Facteur_vdt, Facteur_litiere,Facteur_Chaux, Tx) %>%identify_outliers(Incubation_24h_ppm)
N2O %>% group_by(Facteur_vdt, Facteur_litiere,Facteur_Chaux, Tx) %>% shapiro_test(Incubation_24h_ppm)
N2O %>% levene_test(Incubation_24h_ppm ~ Facteur_vdt, Facteur_litiere,Facteur_Chaux,Tx)

model  <- lm(Incubation_24h_ppm ~ Facteur_vdt*Facteur_litiere*Facteur_Chaux*Tx, data = N2O)
# Créer un QQ plot des résidus
ggqqplot(residuals(model))

# Calculer le test de normalité de Shapiro-Wilk
shapiro_test(residuals(model))

levene_test(data=N2O,Incubation_24h_ppm ~ Facteur_vdt*Facteur_litiere*Facteur_Chaux*Tx)

anova_test(data=N2O,Incubation_24h_ppm ~ Facteur_vdt*Facteur_litiere*Facteur_Chaux*Tx + Error(Notest/(Facteur_vdt*Facteur_litiere*Facteur_Chaux*Tx)))

res.aov <- anova_test(
  data = N2O, dv = Incubation_24h_ppm,wid = Notest ,
  between  = c(Facteur_vdt,Facteur_litiere,Facteur_Chaux,Tx)
)
get_anova_table(res.aov)

ggplot(N2O, aes(x=factor(N2O$Tx), y=N2O$Incubation_24h_ppm, fill=N2O$Facteur_vdt)) +
  geom_boxplot()

Ttest_Facteur_litiereN2O <-N2O_ss_T0T1 %>%
  pairwise_t_test(
    Incubation_24h_ppm ~ Facteur_litiere, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
Ttest_Facteur_litiereN2O

bxp <- ggboxplot(
  N2O, x = "Tx", y = "Incubation_24h_ppm",
  color = "Facteur_vdt", palette = "jco",xlab = "Mesures", ylab = "Quantité de N2O produit (ppm/jour)",legend.title = "Vers de terre",ggtheme = theme_gray()
)
bxp

N2O_ss_T0T1<-N2O[65:160,]

bxp <- ggboxplot(
  N2O, x = "Facteur_litiere", y = "Incubation_24h_ppm",
  color = "Facteur_Chaux", palette = "jco",xlab = "Litière", ylab = "Quantité de N2O produit (ppm/jour)",legend.title = "Chaux",ggtheme = theme_gray()
)
bxp

bxp <- ggboxplot(
  N2O_ss_T0T1, x = "Facteur_litiere", y = "Incubation_24h_ppm",
  color = "Facteur_Chaux", palette = "jco",xlab = "Litière", ylab = "Quantité de N2O produit (ppm/jour)",legend.title = "Chaux",ggtheme = theme_gray(),facet.by = "Facteur_vdt",short.panel.labs = FALSE,panel.labs = list(Facteur_vdt = c("Avec vers de terre", "Sans vers de terre"))
)
bxp

Anova_N2O<-aov(data=N2O,Incubation_24h_ppm ~ Facteur_vdt*Facteur_litiere*Facteur_Chaux*Tx+Error(ID))
summary(Anova_N2O)
Anova_N2O<-aov(data=N2O_ss_T0T1,Incubation_24h_ppm ~ Facteur_litiere*Facteur_Chaux*Tx+Error(ID))
Anova_N2O<-aov(data=N2O,Incubation_24h_ppm ~ Facteur_litiere*Facteur_Chaux+Error(ID))
summary(Anova_N2O)

model.tukey<- aov(Incubation_24h_ppm ~ Facteur_vdt*Tx+Error(ID),data = N2O)
TukeyHSD(model.tukey, conf.level=0.95)
plot(TukeyHSD(model.tukey, conf.level=0.95), las = 2)

Graph_litiereN2O <- ggboxplot(N2O_ss_T0T1, x = "Facteur_litiere", y = "Incubation_24h_ppm",
                              color = "Facteur_litiere", palette =c("#00AFBB", "#FC4E07"),
                              shape = "Facteur_litiere",xlab = "Litière", ylab = "Quantité de CO2 produit (ppm/jour)",legend.title = "Litière",ggtheme = theme_gray())
Graph_litiereN2O
my_comparisons <- list( c( "erable/hetre","peuplier") )
Graph_litiereN2O + 
  stat_pvalue_manual(Ttest_Facteur_litiereN2O,y.position = 10000, label = "p.adj.signif",bracket.nudge.y = -50)
 