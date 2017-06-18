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
# "High" vs "Low" quality studies: 
#
highqual <- read.csv("highq.rda", as.is=TRUE)
highqual
View(highqual)
library(meta)
metaprop(vdd, tot, studlab=paste(study), data=highqual)
# try with summary measure as: "PAS" i.e. arcsine transformation
metaprop(vdd, tot, studlab=paste(study), data =highqual, sm = "PAS")
# now try with PLOGIT i.e. logit transformation that is the default
metaprop(vdd, tot, studlab=paste(study), data =highqual, sm = "PLOGIT")
# Result HIGH QUALITY
# Number of studies combined: k = 10
#
#                         proportion      95%-CI      z  p-value
# Fixed effect model       0.5638         [0.5225;    0.6042] --       --
# Random effects model     0.6464         [0.4625;    0.7953] --       --
# Quantifying heterogeneity:
# tau^2 = 1.3142; H = 4.24 [3.46; 5.21]; I^2 = 94.4% [91.7%; 96.3%]
# Test of heterogeneity:
#     Q           d.f.           p-value
#    162.14        9            < 0.0001
# the above two ("PAS" vs "PLOGIT" give different results which one to use? default for proportions no?
? `meta-package`
?metaprop
# sm: A character string indicating which summary measure ("PFT", "PAS", "PRAW", "PLN", or "PLOGIT") 
# is to be used for pooling of studies, see Details.
#  LOW QUALITY
metaprop(vddch, totch, studlab=paste(study), data=lowquality, sm = "PLOGIT")
# Results
# Number of studies combined: k = 15
#                        proportion           95%-CI       z  p-value
# Fixed effect model       0.4632         [0.4423; 0.4842]   --       --
# Random effects model     0.4941         [0.3760; 0.6129]    --       --
#
# Quantifying heterogeneity:
# tau^2 = 0.8492; H = 5.47 [4.75; 6.29]; I^2 = 96.7% [95.6%; 97.5%]
# Test of heterogeneity:
#    Q              d.f.       p-value
#  418.70            14        < 0.0001
#----------------------------------------------------------------------
# for prevalence in those 21 studies that reported vdd under our set threshold <20ng/ml
# for sample size of study >=150 versus <150 
# sample size >=150
sslarge <- read.csv("ssabove150.rda", as.is=TRUE)
View(sslarge)
metaprop(vddch, totch, studlab=paste(study), data=sslarge, sm = "PLOGIT")
# RESULTS sample size >=150 
# Number of studies combined: k = 8
#                     proportion           95%-CI             z  p-value
# Fixed effect model       0.4553         [0.4329; 0.4778]     --       --
# Random effects model     0.4571         [0.3051; 0.6175]     --       --
# Quantifying heterogeneity:
# tau^2 = 0.8609; H = 7.04 [5.95; 8.33]; I^2 = 98.0% [97.2%; 98.6%]
# Test of heterogeneity:
#      Q       d.f.       p-value
      346.52    7        < 0.0001
# for papers that have common assay methods?!  this was not done before so check 

