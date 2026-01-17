library(remotes)
remotes::install_github('flr/FLCore', ref='devel')

library(FLCore)
library(ggplotFL)
library(FLSkill)

library(plyr)
library(reshape)
library(ggridges)
library(ggpubr)

library(pROC)

source("C:/active/flr/FLSkill/R/plots.R")

testDF=data.frame(OM       =seq(0,2,length.out=101),
                  Indicator=seq(0,2,length.out=101)+rnorm(101,0,0.3),
                  Scenario ="Reference",stringsAsFactors=TRUE)

ggplot(testDF)+
  geom_point(aes(OM,Indicator))+
  xlab("True Values")+ylab("Indicator")

skillPlot(testDF,obs="OM",hat="Indicator")

pROC::auc(testDF$OM>1,testDF$Indicator)



