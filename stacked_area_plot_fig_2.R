##########Cleaning the workspace##########
rm(list=ls())

##########Loading necessary libraries##########
library(vegan)
library(MASS)
library(ecodist)
library(ggplot2)
library(dplyr)
library(gridExtra)

##########Generating necessary functions##########
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

##########Loading data##########
setwd("whatever/you/like")
data <- read.csv("species_proportion_seine.csv", header=T, sep=";", dec=",")

##########Filtering the data##########
lsp <- filter(data, secteurs == "Lac Saint-Pierre") %>% droplevels()
lsl <- filter(data, secteurs == "Lac Saint-Louis") %>% droplevels()
mtl <- filter(data, secteurs == "Montréal-Sorel") %>% droplevels()
lsf <- filter(data, secteurs == "Lac Saint-François") %>% droplevels()
bc <- filter(data,secteurs == "Bécancour-Batiscan") %>% droplevels()

##########Creation of the color palette (colorblind-proof palette)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","light gray")

##########Creation of the figure, sorry for this huge and ugly code!######
tiff("C:/Users/MOROL1/Desktop/cpue_gobie.tiff", height = 4.8, width = 6.2, units = "in", res=300)
p1 <- ggplot(lsp, aes(x=annees,y=cpue,group=especes,fill=especes)) + 
  geom_area(position="fill" ,show.legend = F)+ggtitle("Lake Saint-Pierre (n=5)") +  
  ylab("Relative abundance (%)")+scale_fill_manual(values=cbPalette) + 
  theme(plot.title = element_text(size=8, face="bold"), axis.title.x = element_text(size=8),axis.title.y = element_text(size=8),
        axis.text.x = element_text(size=6.5),axis.text.y = element_text(size=6.5),panel.background = element_blank(),axis.line = element_line(colour = "black"))+scale_x_continuous(name ="Sampling years",breaks=c(1995,2002,
        2007, 2013, 2016),labels=c("1995","2002","2007", "2013", "2016"))+scale_y_continuous(labels=c("0", "25", "50", "75", "100"))

p2 <- ggplot(lsl, aes(x=annees,y=cpue,group=especes,fill=especes)) + 
  geom_area(position="fill" ,show.legend = F)+ggtitle("Lake Saint-Louis (n=3)")+xlab("Years") + 
  ylab("Relative abundance (%)")+scale_fill_manual(values=cbPalette) + 
  theme(plot.title = element_text(size=8, face="bold"), axis.title.x = element_text(size=8),axis.title.y = element_text(size=8),
        axis.text.x = element_text(size=6.5),axis.text.y = element_text(size=6.5),panel.background = element_blank(),axis.line = element_line(colour = "black"))+scale_x_continuous(name ="Sampling years",breaks=c(1997,2005,
      2011),labels=c("1997","2005","2011"))+scale_y_continuous(labels=c("0", "25", "50", "75", "100"))

p3 <- ggplot(lsf, aes(x=annees,y=cpue,group=especes,fill=especes)) + 
  geom_area(position="fill" ,show.legend = F)+ggtitle("Lake Saint-François (n=4)")+xlab("Years") + 
  ylab("Relative abundance (%)")+scale_fill_manual(values=cbPalette) + 
  theme(plot.title = element_text(size=8, face="bold"), axis.title.x = element_text(size=8),axis.title.y = element_text(size=8),
        axis.text.x = element_text(size=6.5),axis.text.y = element_text(size=6.5),panel.background = element_blank(),axis.line = element_line(colour = "black"))+scale_x_continuous(name ="Sampling years",breaks=c(1996,2004,
        2009, 2014),labels=c("1996","2004","2009", "2014"))+scale_y_continuous(labels=c("0", "25", "50", "75", "100"))

p4 <- ggplot(mtl, aes(x=annees,y=cpue,group=especes,fill=especes)) + 
  geom_area(position="fill",show.legend = F)+ggtitle("Montréal-Sorel (n=2)")+xlab("Years") +
  ylab("Relative abundance (%)")+labs(fill = "Species")+scale_fill_manual(values=cbPalette) + 
  theme(plot.title = element_text(size=8,face="bold"),legend.title = element_text(size=9, face="bold"),
        legend.text=element_text(size=7),axis.title.x = element_text(size=8),axis.title.y = element_text(size=8), 
        axis.text.x = element_text(size=6.5),axis.text.y = element_text(size=6.5),panel.background = element_blank(),axis.line = element_line(colour = "black"))+scale_x_continuous(name ="Sampling years",breaks=c(2001,2015),
                                                                                                                                                                                labels=c("2001","2015"))+scale_y_continuous(labels=c("0", "25", "50", "75", "100"))

p5 <- ggplot(mtl, aes(x=annees,y=cpue,group=especes,fill=especes)) + 
  geom_area(position="fill")+ggtitle("Montreal-Sorel (n=2)")+xlab("Years") +
  ylab("Relative abundance (%)")+labs(fill = "Species")+scale_fill_manual(values=cbPalette, labels=c("ETOL, Tessellated Darter",
  "FUDI, Banded killifish", "LASI, Brook silverside", "NEME, Round goby", "NOAT, Emerald shiner",
  "NOCR, Golden shiner","NOHU, Spottail shiner", "PEOM, Trout-perch", "Others, 32 species")) + 
  theme(plot.title = element_text(size=8,face="bold"),legend.title = element_text(size=8, face="bold"),
        legend.text=element_text(size=7),axis.title.x = element_text(size=6.5),axis.title.y = element_text(size=8), 
        axis.text.x = element_text(size=6.5),axis.text.y = element_text(size=6.5),panel.background = element_blank(),axis.line = element_line(colour = "black"))+scale_x_continuous(name ="Sampling years",breaks=c(2001,2015),
        labels=c("2001","2015"))+scale_y_continuous(labels=c("0", "25", "50", "75", "100"))

p6 <- ggplot(bc, aes(x=annees,y=cpue,group=especes,fill=especes)) + 
  geom_area(position="fill" ,show.legend = F)+ggtitle("Bécancour-Batiscan (n=5)") +  
  ylab("Relative abundance (%)")+scale_fill_manual(values=cbPalette) + 
  theme(plot.title = element_text(size=8, face="bold"), axis.title.x = element_text(size=8),axis.title.y = element_text(size=8),
        axis.text.x = element_text(size=6.5),axis.text.y = element_text(size=6.5),panel.background = element_blank(),axis.line = element_line(colour = "black"))+scale_x_continuous(name ="Sampling years",breaks=c(1996,2001,
        2008, 2012, 2017),labels=c("1996","2001","2008", "2012", "2017"))+scale_y_continuous(labels=c("0", "25", "50", "75", "100"))


leg <- g_legend(p5)
lay<- rbind(c(1,2,3),
            c(4,5,6))
grid.arrange(p6,p1,p4,p2,p3,leg, layout_matrix=lay)

dev.off()





