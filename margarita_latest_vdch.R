getwd()
# or use source() like this e.g.  source("https://link.R")
# source {base} 
# source causes R to accept its input from a named file or URL or expressions or connections directly. 
# Input is read and parsed from that file until the end of that file is reached, then the parsed expressions are evaluated in order in 
# that chosen environment 
# What is the best way to save files? .rda or no <<-----------------
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
# sample size <150 
ssmall <- read.csv("ssbelow150.rda", as.is=TRUE)
View(ssmall)
metaprop(vddch, totch, studlab=paste(study), data=ssmall, sm = "PLOGIT")
# RESULTS sample size <150 
# Number of studies combined: k = 17
#                    proportion           95%-CI  z  p-value
# Fixed effect model       0.5465 [0.5130; 0.5795] --       --
# Random effects model     0.6024 [0.4705; 0.7209] --       --
# Quantifying heterogeneity:
tau^2 = 1.1189; H = 3.81 [3.24; 4.49]; I^2 = 93.1% [90.5%; 95.0%]
Test of heterogeneity:
      Q d.f.  p-value
 232.80   16 < 0.0001
# prevalence in those 21 studies that reported vdd <=50 nmol/L (20ng/ml i.e. the set threshold); excluding the 4 which used thresholds: 
# <=40 and <=37.4 nmol/L 
cutoff50 <- read.csv("cutoff.rda", as.is=TRUE)
View(cutoff50)
metaprop(vddch, totch, studlab=paste(study), data=cutoff50, sm = "PLOGIT")
# RESULTS
# Number of studies combined: k = 21
#                   proportion           95%-CI           z      p-value
# Fixed effect model       0.5019          [0.4823; 0.5215]    --       --
# Random effects model     0.5932          [0.4890; 0.6897]    --       --
#
# Quantifying heterogeneity:
# tau^2 = 0.8891; H = 5.12 [4.53; 5.78]; I^2 = 96.2% [95.1%; 97.0%]
# Test of heterogeneity:
#      Q      d.f.    p-value
#    523.68   20      < 0.0001


# for papers that have common assay methods?!  this was not done before so check 
# MORTALITY -deficient dead versus not deficient dead - 
mortalch <- read.csv("mortality.rda", as.is=TRUE)
mortalch
View(mortalch)
metabin(defdead, alldef, nodefdead, allnotdef, studlab=study, data=mortalch, method="Inverse", sm= "OR")
?metabin
# RESULTS
# Number of studies combined: k = 11
#
#                         OR           95%-CI    z  p-value
# Fixed effect model   1.2284 [0.7964; 1.8945] 0.93   0.3522
# Random effects model 1.2284 [0.7964; 1.8945] 0.93   0.3522
#
# Quantifying heterogeneity:
#  tau^2 = 0; H = 1.00 [1.00; 1.53]; I^2 = 0.0% [0.0%; 57.2%]
# Test of heterogeneity:
#    Q         d.f.        p-value
 #   9.30      10           0.5039
# Funnel plot (for mortality)
funnel(meta2, sm= "OR", comb.fixed =TRUE, level=0.95)
funnel(meta2$TE, meta2$seTE, sm= "OR", comb.fixed =TRUE, level=0.95)
#
# Radial plot -maybe this not needed 
# Radial or Galbraith plot it is the alternative of the forest plot [logOR] 
# Horizontal axis: 1/standard error
# Vertical axis: effect divided by standard error 
radial(meta2, level=0.95)
#
# use: metabias to test for funnel plot asymmetry for mortality outcome!
?metabias
# rank correlation test of funnel plot asymmetry: 
metabias(meta2, method.bias = "rank")
# data:  meta2
# z = 2.4133, p-value = 0.01581
# alternative hypothesis: asymmetry in funnel plot
# sample estimates:
#     ks         se.ks 
#  31.00000      12.84523 
# So if alpha 0.05 is used as the cut-off for significance, then here 0.01 is < 0.05 so the null hypothesis is rejected and alternative 
# is accepted i.e. that funnel plot is asymmetric.
##
# Rank correlation test of funnel plot asymmetry (with continuity correction)
metabias(meta2, method.bias="rank", correct=TRUE)
#
# The xlab option is used to label the x-axis: e.g. xlab= "xxxxxxxx units xxxx" 
# xlim=c( X, Y) used to specifiy limits of the x-axis e.g. xlim=c(-50, 10) means that limits are between -50 and 10 

# Find this paper: The arcsine is asinine: the analysis of proportions in ecology. Warton DI1, Hui FK. ok
# Check differences when using arcsine vs logit 
# Arcsine transformation (sm="PAS")
# Logit transformation (sm="PLOGIT", default)
# Log transformation (sm="PLN")--> what about this?! 
# Meta-analysis of proportions With default i.e. "PLOGIT"
#
#
checking <- read.csv("prevtrial.rda", as.is=TRUE)
check <- read.csv("trialprev.rda", as.is=TRUE)
check 
View(check)
library(meta)
# Try with Arcsine transformation (sm="PAS") first 
metaprop(vddch, totch, studlab=(study), data=check, sm = "PAS")
# Result:
# Number of studies combined: k = 25
#                        proportion           95%-CI          z          p-value
# Fixed effect model       0.4950       [0.4784; 0.5116]      --                --
# Random effects model     0.5492       [0.4433; 0.6529]      --                --
# Quantifying heterogeneity:
# tau^2 = 0.0698; H = 6.22 [5.64; 6.87]; I^2 = 97.4% [96.9%; 97.9%]
# Test of heterogeneity:
#      Q       d.f.            p-value
#    928.95    24             < 0.0001
