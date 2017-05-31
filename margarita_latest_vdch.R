getwd()
# always check the working directory & change if needed
setwd("C:/Users/INSPIRON/Documents/Rch")
# MAIN ANALYSES
# prevalence
prevalencedata <- read.csv("prevalence.rda", as.is=TRUE)
#type: prevalencedata to view the data
prevalencedata
library(meta)
#library(meta) will load the package
metaprop(vddch, totch, studlab=paste(study), data = prevalencedata)
# write the results
#
#
# name this: mprev1
mprev1 <- metaprop(vddch, totch, studlab=paste(study), data = prevalencedata)
# test it below
mprev1
# for the forest plot:
forest(mprev1, comb.fixed=FALSE, xlab= "proportion")
# the above for the random effects model
# repeat to get the fixed effect model
###
forest(mprev1, comb.random=FALSE, xlab= "proportion")
###
###
#now produce the funnel plot:
funnel(mprev1)
###
###
# Subgroup analyseis: children with sepsis
###
prevsepsis <- read.csv("withsepsis.rda", as.is=TRUE)
prevsepsis
metaprop(vddseps, totseps, studlab=paste(study), data=prevsepsis)
mprevsepsis1 <- metaprop(vddseps, totseps, studlab=paste(study), data=prevsepsis)
mprevsepsis1
###
#get forest plot fixed effect model:
forest(mprevsepsis1, comb.random=FALSE, xlab= "proportion")
#get random effects model forest plot:
forest(mprevsepsis1, comb.fixed=FALSE, xlab= "proportion")
funnel(mprevsepsis1)
#
# Can try using addtau2=TRUE to add between-study error.
# Also can try: funnel(trimfill(result.rd))
#
# MORTALITY
# open file: mortaldata.rda
mortality <- read.csv("mortaldata.rda", as.is=TRUE)
mortality
metabin(Eedeaddef, Nealldef, Ecdeadnodef, Ncallnondef, sm= "OR", method="I", data=mortality, studlab=study)
# check the above cz random&fixed give same results
###
metamortal <- metabin(Eedeaddef, Nealldef, Ecdeadnodef, Ncallnondef, sm= "OR", method="I", data=mortality, studlab=study)
metamortal
forest(metamortal, comb.fixed=FALSE, xlab= "odds ratio")
# why xlab= proportion in the other analysis? clarify
# same to get fixed effect model result:
forest(metamortal, comb.random=FALSE, xlab= "odds ratio")
# get funnel plot
funnel(metamortal)
# to do: 
# the following sensitivity analyses 
# for prevalence of: high qualiy studies only (>=7 starts) and low quality (<7 stars)
prevhighqual <- read.csv("highquality.rda", as.is=TRUE)
# then see the data:
prevhighqual
library(meta)
metaprop(vdd, totch, studlab=paste(study), data=prevhighqual)
# RESULT:
# Number of studies combined: k = 5
#
#                    proportion    95%-CI        z  p-value
# Fixed effect model       0.5991 [0.5532; 0.6433] --       --
# Random effects model     0.6602 [0.4936; 0.7948] --       --
#
# Quantifying heterogeneity:
# tau^2 = 0.5617; H = 3.57 [2.56; 4.99]; I^2 = 92.2% [84.7%; 96.0%]
# Test of heterogeneity:
#    Q d.f.  p-value
#     51.03    4 < 0.0001


# for prevalence in those 21 studies that reported vdd under our set threshold <20ng/ml
#
# for sample size of study >=150 versus <150 
#
# for papers that have common assay methods?! this was not done before so check 

