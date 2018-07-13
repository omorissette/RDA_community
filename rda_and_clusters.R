#Cleaning workspace and loading packages
rm(list=ls())
library(vegan)
library(MASS)
library(ecodist)

#Loading data
data <-read.csv("littoral_fish_cpue.csv", header=TRUE, sep=",", dec=".")
filet <- read.csv("predators_cpue.csv", header=T, sep=",", dec=".")
years <-read.csv("sampling_years.csv")

#Normalization of data
std.rsi <- decostand(data, method = "normalize")
std.filet <- decostand(filet, method = "normalize")

#Redundancy analysis Contribution of predators abundance on littoral fish community
rda.rsi<-rda(std.rsi ~ SAVI + ESLU + SACA + MIDO + PEFL + MISA, data=std.filet)

#Automated variables selection 
rda.rsi <- ordistep(rda.rsi)

#summary of model and analysis of variance
summary(rda.rsi)
anova(rda.rsi)
RsquareAdj(rda.rsi)


###Plotting of RDA analysis
#Extraction of RDA scores for site (scr) and predators (bp)
scr<-scores(rda.rsi)
scr

scr_bp <- scores(rda.rsi, display="bp")
scr_bp


#Summary of RDA models

par(mai=c(.8,0.62,0.4,0.32))

ordiplot(rda.rsi, type="n", ylab ="", xlab="", cex.axis=.6, xlim = c(-1, 1.5))
title(ylab="RDA 2 (25.6% of variance explained)", xlab="RDA 1 (54.7% of variance explained)", cex.lab=.7,line = 2)

points(scr$sites[,1], scr$sites[,2],pch=c(5,5,18,18,18,0,1,2,0,1,15,16,15,17), col = "black", cex=1)
points(scr$species[,1], scr$species[,2], pch=8,cex=.6, col = "lightgray")
text(scr$species[c(15,16,18,27,32,29,30,37),1], scr$species[c(15,16,18,27,32,29,30,37),2], 
     rownames(scr$species[c(15,16,18,27,32,29,30,37),]), pos=seq(1,4,1), cex=.5, col = "blue"
     ,font = 2)

text(scr$sites[,1], scr$sites[,2], 
     years[,2], pos=seq(2,3,2), cex=.6, col = "darkgray"
     ,font = 2)

arrows(0,0,scr_bp[,1], scr_bp[,2], length = .08,col = "red", lty = 1, lwd = 1.5,code = 2, cex = 0.5)
text(scr_bp[,1], scr_bp[,2], c("SAVI", "ESLU", "MIDO"), cex=.6, col = "red"
     ,font = 2,pos=seq(1,3,1))


legend("bottomright", legend=c("St-Pierre before", "St-Fran?ois before", "St-Louis before", "Montreal before",
                               "St-Pierre after","St-Fran?ois after", "St-Louis after", "Montreal after"), pch=c(5,0,1,2,18,15,16,17), cex=.7)





#Cluster Analyses
#Computation of dissimilarity index
dist <- vegdist(data, method = "bray")

#Clustering analyses
csin <- hclust(dist, method="complete")
hcd <- as.dendrogram(csin)


#Dendrogram of clustering analysis
#Naming stations
name <-list("St.Pierre 1995", "St.Pierre 2002", "St.Pierre 2007", "St.Pierre 2013", "St.Pierre 2016","St.Francis 1996","St.Louis 1997",
            "Montreal-Sorel 2001", "St.Francis 2004", "St.Louis 2005","St.Francis 2009", "St.Louis 2011", "St.Francis 2014", "Montreal-Sorel 2015")

#Plotting of dendrogram
par(mai=c(.15,1,.4,.5))
plot(csin, hang=-1, main = "",xlab = "",labels=name,sub = "", ylab = "Bray Curtis Dissimilarity", cex.axis=0.8)
symbols(1:14, rep(0, 14), circles=rep(1, 14), add=TRUE, inches=.05,
        bg=rep(c("white", "black","white", "black"), c(2,3,5,4)), xpd=TRUE)

