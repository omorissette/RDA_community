##########Preparation of workspace##########
rm(list=ls())

library(vegan)
library(MASS)
library(ecodist)

###########Set working directory##########
setwd("whatever/you/want")

###########Loading the data##########

data<-read.csv("K:/DFA/Expertises/EAE/Communications/Aquatic Invasions/01_donnees_brutes/cpue_seine_fig4.csv", header=T, sep=";", dec=",")

############Models littoral fish######################

modelETOL<-lme(fixed = sqrt(ETOL) ~ factor_NEME, random = ~ 1|stations, data= data, na.action = na.omit)
summary(modelETOL)

modelFUDI<-lme(fixed = sqrt(FUDI) ~ factor_NEME, random = ~ 1|stations, data= data, na.action = na.omit)
summary(modelFUDI)

modelLASI<-lme(fixed = sqrt(LASI) ~ factor_NEME, random = ~ 1|stations, data= data, na.action = na.omit)
summary(modelLASI)

modelNOAT<-lme(fixed = sqrt(NOAT) ~ factor_NEME, random = ~ 1|stations, data= data, na.action = na.omit)
summary(modelNOAT)

modelNOCR<-lme(fixed = sqrt(NOCR) ~ factor_NEME, random = ~ 1|stations, data= data, na.action = na.omit)
summary(modelNOCR)

modelNOHU<-lme(fixed = sqrt(NOHU) ~ factor_NEME, random = ~ 1|stations, data= data, na.action = na.omit)
summary(modelNOHU)

modelPEOM<-lme(fixed = sqrt(PEOM) ~ factor_NEME, random = ~ 1|stations, data= data, na.action = na.omit)
summary(modelPEOM)

###############Boxplot littoral fish################################

tiff("K:/DFA/Expertises/EAE/Communications/Aquatic Invasions/figure_4.tiff", width = 4, height = 7.3, units = "in", res=300)
par(mfrow = c(4,2),
    oma = c(5,3,0,0) + 0.1,
    mar = c(1,1,1,1) + 0.1)
boxplot(sqrt(ETOL)~factor_NEME, data=data, ylab = "CPUE (square root)", names = c("Pre-invasion", "Post-invasion"), col="light gray", boxwex = 0.7,cex.axis=0.9)
text(2,3.6, "Tessellated 
darter
p < 0.001 ", font=2)
boxplot(sqrt(FUDI)~factor_NEME, data=data, ylab = "CPUE (square root)", names = c("Pre-invasion", "Post-invasion"), col="light gray", boxwex = 0.7,cex.axis=0.9)
text(1.9,9.3, "Banded killifish
p = 0.171", font=1)
boxplot(sqrt(LASI)~factor_NEME, data=data, ylab = "CPUE (square root)", names = c("Pre-invasion", "Post-invasion"), col="light gray", boxwex = 0.7,cex.axis=0.9)
text(1.1,9.1, "Brook silverside
p = 0.005", font=2)
boxplot(sqrt(NOAT)~factor_NEME, data=data, ylab = "CPUE (square root)", names = c("Pre-invasion", "Post-invasion"), col="light gray", boxwex = 0.7,cex.axis=0.9)
text(1.1,5.1, "Emerald shiner
p < 0.001", font=2)
boxplot(sqrt(NOHU)~factor_NEME, data=data, ylab = "CPUE (square root)", names = c("Pre-invasion", "Post-invasion"), col="light gray", boxwex = 0.7,cex.axis=0.9)
text(1.05,3.35, "Spottail shiner
p = 0.287", font=1)
boxplot(sqrt(NOCR)~factor_NEME, data=data, ylab = "CPUE (square root)", names = c("Pre-invasion", "Post-invasion"), col="light gray", boxwex = 0.7,cex.axis=0.9)
text(1,7, "Golden shiner
     p = 0.335", font=1)
boxplot(sqrt(PEOM)~factor_NEME, data=data, ylab = "CPUE (square root)", names = c("Pre-invasion", "Post-invasion"), col="light gray", boxwex = 0.7,cex.axis=0.9)
text(1,3.7, "Trout-perch
p = 0.099", font=1)
title(xlab = "Round goby invasion",
      ylab = "CPUE (square root)",
      outer = TRUE, line = 2)
dev.off()


#######################Supplementary figures littoral######################

tiff("K:/DFA/Expertises/EAE/Communications/Aquatic Invasions/suppl_fig_2.tiff", width = 6, height = 6, units = "in", res=300)
par(mfrow = c(3,3),
    oma = c(2,2,0,0) + 0.1,
    mar = c(1,1,1,1) + 0.1)

interaction.plot(data$factor_NEME,trace.factor = data$stations, response = data$ETOL,type="b",
                 col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442"), lwd=2, 
                 xlab="Round goby invasion", ylab="Smallmouth bass CPUE",pch=c(14,15,16,17,18), trace.label="Fluvial sector", legend = F)
text(1.8,11.9, "Tessellated darter")
interaction.plot(data$factor_NEME,trace.factor = data$stations, response = data$FUDI,type="b",
                 col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442"), lwd=2, 
                xlab="Round goby invasion", ylab="Smallmouth bass CPUE",pch=c(14,15,16,17,18), trace.label="Fluvial sector", legend = F)
text(1.8,52, "Banded killifish")
interaction.plot(data$factor_NEME,trace.factor = data$stations, response = data$LASI,type="b",
                 col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442"), lwd=2, 
                 xlab="Round goby invasion", ylab="Smallmouth bass CPUE",pch=c(14,15,16,17,18), trace.label="Fluvial sector", legend = F)
text(1.2,57, "Brook silverside")
interaction.plot(data$factor_NEME,trace.factor = data$stations, response = data$NOAT,type="b",
                 col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442"), lwd=2, 
                 xlab="Round goby invasion", ylab="Smallmouth bass CPUE",pch=c(14,15,16,17,18), trace.label="Fluvial sector", legend = F)
text(1.2,18, "Emerald shiner")

interaction.plot(data$factor_NEME,trace.factor = data$stations, response = data$NOCR,type="b",
                 col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442"), lwd=2, 
                 xlab="Round goby invasion", ylab="Smallmouth bass CPUE",pch=c(14,15,16,17,18), trace.label="Fluvial sector", legend = F)
text(1.2,54, "Golden shiner")
interaction.plot(data$factor_NEME,trace.factor = data$stations, response = data$NOHU,type="b",
                 col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442"), lwd=2, 
                 xlab="Round goby invasion", ylab="Smallmouth bass CPUE",pch=c(14,15,16,17,18), trace.label="Fluvial sector", legend = F)
text(1.2,12.3, "Spottail shiner")
interaction.plot(data$factor_NEME,trace.factor = data$stations, response = data$PEOM,type="b",
                 col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442"), lwd=2, 
                 xlab="Round goby invasion", ylab="Smallmouth bass CPUE",pch=c(14,15,16,17,18), trace.label="Fluvial sector", legend = F)
text(1.15,9, "Trout-perch")
title(xlab = "Round goby invasion",
      ylab = "CPUE",
      outer = TRUE, line = 1,cex.lab=0.8)
dev.off()




######models predators#############
pred<-read.csv("K:/DFA/Expertises/EAE/Communications/Aquatic Invasions/01_donnees_brutes/cpue_gillnet_fig5.csv", header=T, sep=";", dec=",")

modelESLU<-lme(fixed = sqrt(ESLU) ~ factor_NEME, random = ~ 1|stations, data= pred, na.action = na.omit)
summary(modelESLU)
anova(modelESLU)

modelSAVI<-lme(fixed = sqrt(SAVI) ~ factor_NEME, random = ~ 1|stations, data= pred, na.action = na.omit)
summary(modelSAVI)
anova(modelSAVI)

modelSACA<-lme(fixed = sqrt(SACA) ~ factor_NEME, random = ~ 1|stations, data= pred, na.action = na.omit)
summary(modelSACA)
anova(modelSACA)

modelMIDO<-lme(fixed = sqrt(MIDO) ~ factor_NEME, random = ~ 1|stations, data= pred, na.action = na.omit)
summary(modelMIDO)
anova(modelMIDO)



############boxplot predators#############


tiff("K:/DFA/Expertises/EAE/Communications/Aquatic Invasions/figure_5.tiff", width = 4, height = 4, units = "in", res=300)
par(mfrow = c(2,2),
    oma = c(2,2,0,0) + 0.1,
    mar = c(1,1,1,1) + 0.1)
boxplot(sqrt(ESLU)~factor_NEME, data=pred, ylab = "CPUE (square root)", names = c("Pre-invasion", "Post-invasion"), col="light gray", boxwex = 0.7,cex.axis=0.7)
text(1,1.8, "Northern Pike
p = 0.002", font=2, cex=0.7)

boxplot(sqrt(SAVI)~factor_NEME, data=pred, ylab = "CPUE (square root)", names = c("Pre-invasion", "Post-invasion"), col="light gray", boxwex = 0.7,cex.axis=0.7)
text(1,2.6, "Walleye
p = 0.388", font=1, cex=0.7)

boxplot(sqrt(SACA)~factor_NEME, data=pred, ylab = "CPUE (square root)", names = c("Pre-invasion", "Post-invasion"), col="light gray", boxwex = 0.7,cex.axis=0.7)
text(1,1.3, "Sauger
p = 0.502", font=1, cex=0.7)

boxplot(sqrt(MIDO)~factor_NEME, data=pred, ylab = "CPUE (square root)", names = c("Pre-invasion", "Post-invasion"), col="light gray", boxwex = 0.7,cex.axis=0.7)
text(1.05,1.5, "Smallmouth bass
p = 0.067", font=1, cex=0.7)

title(xlab = "Round goby invasion",
      ylab = "CPUE (square root)",
      outer = TRUE, line = 1,cex.lab=0.8)
dev.off()


######supplementary figures predator##########

tiff("K:/DFA/Expertises/EAE/Communications/Aquatic Invasions/suppl_fig_3.tiff", width = 5.2, height = 7.8, units = "in", res=300)
par(mfrow = c(3,2),
    oma = c(2,2,0,0) + 0.1,
    mar = c(1,1,1,1) + 0.1)

interaction.plot(pred$factor_NEME,trace.factor = pred$stations, response = pred$ESLU,type="b",
                 col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442"), lwd=2, 
                 xlab="Round goby invasion", ylab="Smallmouth bass CPUE",pch=c(14,15,16,17,18), trace.label="Fluvial sector", legend = F)
text(1.8,3.1, "Northern pike")
interaction.plot(pred$factor_NEME,trace.factor = pred$stations, response = pred$SAVI,type="b",
                 col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442"), lwd=2, 
                 xlab="Round goby invasion", ylab="Smallmouth bass CPUE",pch=c(14,15,16,17,18), trace.label="Fluvial sector", legend = F)
text(1.08,7.8, "Walleye")
interaction.plot(pred$factor_NEME,trace.factor = pred$stations, response = pred$SACA,type="b",
                 col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442"), lwd=2, 
                 xlab="Round goby invasion", ylab="Smallmouth bass CPUE",pch=c(14,15,16,17,18), trace.label="Fluvial sector", legend = F)
text(1.1,1.7, "Sauger")
interaction.plot(pred$factor_NEME,trace.factor = pred$stations, response = pred$MIDO,type="b",
                 col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442"), lwd=2, 
                 xlab="Round goby invasion", ylab="Smallmouth bass CPUE",pch=c(14,15,16,17,18), trace.label="Fluvial sector", legend = F)
text(1.24,2.6, "Smallmouth bass")
interaction.plot(pred$factor_NEME,trace.factor = pred$stations, response = pred$MISA,type="b",
                 col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442"), lwd=2, 
                 xlab="Round goby invasion", ylab="Smallmouth bass CPUE",pch=c(14,15,16,17,18), trace.label="Fluvial sector", legend = F)
text(1.8,0.28, "Largemouth bass")
interaction.plot(pred$factor_NEME,trace.factor = pred$stations, response = pred$PEFL,type="b",
                 col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442"), lwd=2, 
                 xlab="Round goby invasion", ylab="Smallmouth bass CPUE",pch=c(14,15,16,17,18), trace.label="Fluvial sector", legend = F)
text(1.1,55, "Yellow perch")

title(xlab = "Round goby invasion",
      ylab = "CPUE",
      outer = TRUE, line = 1,cex.lab=0.8)

dev.off()

