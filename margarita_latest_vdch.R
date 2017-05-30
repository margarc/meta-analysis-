getwd()
setwd("C:/Users/INSPIRON/Documents/Rch")
#############MAIN ANALYSES###################
#prevalence
prevalencedata <- read.csv("prevalence.rda", as.is=TRUE)
#type: prevalencedata to view the data
prevalencedata
library(meta)
#library(meta) will load the package
metaprop(vddch, totch, studlab=paste(study), data = prevalencedata)
#name this: mprev1
mprev1 <- metaprop(vddch, totch, studlab=paste(study), data = prevalencedata)
#test it below
mprev1
#for the forest plot:
forest(mprev1, comb.fixed=FALSE, xlab= "proportion")
#the above for the random effects model
#repeat to get the fixed effect model
######################################################
forest(mprev1, comb.random=FALSE, xlab= "proportion")
##############
##############
#now produce the funnel plot:
funnel(mprev1)
##############################
##############################
#Subgroup analyseis: children with sepsis
##############################
prevsepsis <- read.csv("withsepsis.rda", as.is=TRUE)
prevsepsis