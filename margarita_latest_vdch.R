getwd()
# always check the working directory & change if needed
setwd("C:/Users/INSPIRON/Documents/Rch")
# MAIN ANALYSES
#
# PREVALENCE
prevalencedata <- read.csv("prevalence.rda", as.is=TRUE)
#type: prevalencedata to view the data
prevalencedata
library(meta)
#library(meta) will load the package
metaprop(vddch, totch, studlab=paste(study), data = prevalencedata)
# write the results:
# Number of studies combined: k = 25
#
#                        proportion          95%-CI              z  p-value
# Fixed effect model       0.4837        [0.4650; 0.5025]        --       --
# Random effects model     0.5519        [0.4533; 0.6466]        --       --
#
# Quantifying heterogeneity:
# tau^2 = 0.9211; H = 5.00 [4.46; 5.60]; I^2 = 96.0% [95.0%; 96.8%]
#
# Test of heterogeneity:
#      Q       d.f.              p-value
#   598.87     24                 < 0.0001
#--------------------------------------------------------------------------------------------------------
# name this: mprev1
mprev1 <- metaprop(vddch, totch, studlab=paste(study), data = prevalencedata)
# test it below
mprev1
# for the forest plot:
forest(mprev1, comb.fixed=FALSE, xlab= "proportion")
# the above for the random effects model
# repeat to get the fixed effect model
#
forest(mprev1, comb.random=FALSE, xlab= "proportion")

#Q: should x axes be labelled as "proportion" or "prevalence 95% CI") ???

#now produce the funnel plot:
funnel(mprev1)
#
# Subgroup analyseis: children with sepsis
#
prevsepsis <- read.csv("withsepsis.rda", as.is=TRUE)
prevsepsis
metaprop(vddseps, totseps, studlab=paste(study), data=prevsepsis)
mprevsepsis1 <- metaprop(vddseps, totseps, studlab=paste(study), data=prevsepsis)
mprevsepsis1
#
#get forest plot fixed effect model:
forest(mprevsepsis1, comb.random=FALSE, xlab= "proportion")
#get random effects model forest plot:
forest(mprevsepsis1, comb.fixed=FALSE, xlab= "proportion")
funnel(mprevsepsis1)
#
# Can try using addtau2=TRUE to add between-study error.
# Also can try: funnel(trimfill(result.rd))
# --------------------------------------------------------------------------------------------------------
# NOTES WE ADDED see below: 
# library(meta) will load the package
?metaprop
# TRY: prev_arc <- metaprop(vddch, totch, studlab=paste(study), data = prevalencedata, sm = "PAS")
# TRY: prev_logit <- metaprop(vddch, totch, studlab=paste(study), data = prevalencedata, sm = "PLOGIT")
# prev_arc
# str(prev_arc)
# head(prev_arc)
# prev_arc$event
# prev_arc$n
# prev_arc$method.ci
# summary(prev_arc)
# summary(prev_logit)
#
#--------------------------------------------------------------------------------------------------------------------
# MORTALITY
# open file: mortaldata.rda
mortality <- read.csv("mortaldata.rda", as.is=TRUE)
mortality
metabin(Eedeaddef, Nealldef, Ecdeadnodef, Ncallnondef, sm= "OR", method="I", data=mortality, studlab=study)
# check the above cz random&fixed give same results
#RESULTS:
# Number of studies combined: k = 11
#
#                         OR           95%-CI    z  p-value
# Fixed effect model   1.2284 [0.7964; 1.8945] 0.93   0.3522 
# Random effects model 1.2284 [0.7964; 1.8945] 0.93   0.3522
# Random & fixed same for mortality in critically ill children ...
#
# Quantifying heterogeneity:
# tau^2 = 0; H = 1.00 [1.00; 1.53]; I^2 = 0.0% [0.0%; 57.2%]
#
# Test of heterogeneity:
#    Q       d.f.        p-value
#   9.30     10          0.5039
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
#
# for prevalence in those 21 studies that reported vdd under our set threshold <20ng/ml
#
# for sample size of study >=150 versus <150 
#
# for papers that have common assay methods?!  this was not done before so check 

