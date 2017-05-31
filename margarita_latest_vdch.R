getwd()
setwd("C:/Users/INSPIRON/Documents/Rch")
####MAIN ANALYSES
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
####
####
#now produce the funnel plot:
funnel(mprev1)
####
####
#Subgroup analyseis: children with sepsis
####
prevsepsis <- read.csv("withsepsis.rda", as.is=TRUE)
prevsepsis
metaprop(vddseps, totseps, studlab=paste(study), data=prevsepsis)
mprevsepsis1 <- metaprop(vddseps, totseps, studlab=paste(study), data=prevsepsis)
mprevsepsis1
####
#get forest plot fixed effect model:
forest(mprevsepsis1, comb.random=FALSE, xlab= "proportion")
#get random effects model forest plot:
forest(mprevsepsis1, comb.fixed=FALSE, xlab= "proportion")
funnel(mprevsepsis1)
####
####
#MORTALITY
#open file: mortaldata.rda
mortality <- read.csv("mortaldata.rda", as.is=TRUE)
mortality
metabin(Eedeaddef, Nealldef, Ecdeadnodef, Ncallnondef, sm= "OR", method="I", data=mortality, studlab=study)
#check the above cz random&fixed give same results
##################################
metamortal <- metabin(Eedeaddef, Nealldef, Ecdeadnodef, Ncallnondef, sm= "OR", method="I", data=mortality, studlab=study)
metamortal
forest(metamortal, comb.fixed=FALSE, xlab= "odds ratio")
#yes why xlab= proportion in the other analysis? clarify
#same to get fixed effect model result:
forest(metamortal, comb.random=FALSE, xlab= "odds ratio")
#get funnel plot
funnel(metamortal)
#to do:
####
#senstivity analysis: high qualiy studies only prevalence
#sens analysis for prevalence in those 21 studies that reported vdd under our set threshold <20ng/ml
#sample size of study >=150 versus <150 
#cont here

