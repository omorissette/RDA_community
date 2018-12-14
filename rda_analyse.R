#Preparation of workspace
rm(list=ls())

library(vegan)
library(MASS)
library(ecodist)

#Set working directory

#Loading the data
gillnet<-read.csv("cpue_gillnet_data.csv", sep=";",dec=".", header=T)
seine<-read.csv("cpue_seine_data.csv", sep=";",dec=".", header=T)

#Data normalisation and Redundancy analysis
std.seine <- decostand(seine[,3:41], method = "normalize")
std.gillnet <- decostand(gillnet[,3:27], method = "normalize")

rda.goby<-rda(std.seine[,1:39] ~ SAVI + ESLU + SACA + MIDO + PEFL + MISA, data=std.gillnet)
rda.goby <- ordistep(rda.goby)

summary(rda.goby)

anova(rda.goby)
RsquareAdj(rda.goby)

plot(rda.goby)

#Placing RDA scores in dedicated objects
scr<-scores(rda.goby)
scr
scr_bp <- scores(rda.goby, display="bp")
scr_bp


#Summary figure

tiff("RDA_goby.tiff", width = 5.3, height = 5.3, units = "in", res=300)

par(mai=c(.8,0.62,0.4,0.32))

ordiplot(rda.goby, type="n", ylab ="", xlab="", cex.axis=.6, xlim = c(-1, 1.5))
title(ylab="RDA 2 (16.2% of variance explained)", xlab="RDA 1 (10.2% of variance explained)", cex.lab=.7,line = 2)

points(scr$species[,1], scr$species[,2], pch=8,cex=.6, col = "lightgray")
points(scr$sites[,1], scr$sites[,2],pch=c(5,5,18,18,18,0,21,2,0,21,15,16,15,17,6,6,25,25), col = "black", cex=1, bg="white")
points(scr$sites[c(17,18,19),1], scr$sites[c(17,18,19),2],pch=c(25,25,25), col = "black", cex=1, bg="black")

text(scr$species[c("ETOL", "FUDI", "LASI", "NOAT","NOHU","NOCR", "PEOM"),1], 
     scr$species[c("ETOL", "FUDI", "LASI", "NOAT","NOHU","NOCR", "PEOM"),2], 
     rownames(scr$species[c("ETOL", "FUDI", "LASI", "NOAT","NOHU","NOCR", "PEOM"),]), pos=seq(1,4,1), cex=.5, col = "blue"
     ,font = 2)

arrows(0,0,scr_bp[,1], scr_bp[,2], length = .08,col = "red", lty = 1, lwd = 1.5,code = 2, cex = 0.5)
text(scr_bp[,1], scr_bp[,2], c("SAVI", "ESLU", "MIDO"), cex=.6, col = "red"
     ,font = 2,pos=1)

legend("bottomright", legend=c("Bécancour before", "St-Pierre before","Montréal before", 
                               "St-Louis before", "St-François before",
                               "Bécancour after", "St-Pierre after","Montréal after", 
                               "St-Louis after", "St-François after"), 
                               pch=c(6,5,2,1,0,25,23,24,19,15), pt.bg="black",cex=.7)

dev.off()

