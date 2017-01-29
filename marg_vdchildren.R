getwd()
setwd("C:/Users/INSPIRON/Documents/Rch")
preval
metaprop(vddch, totch, studlab=paste(study), data = preval)
mpreval <- metaprop(vddch, totch, studlab=paste(study), data = preval)
mpreval
forest(mpreval, comb.fixed=FALSE, xlab= "proportion")
forest(mpreval, comb.random=FALSE, xlab= "proportion")
funnel(mpreval)
mort <- read.csv("mortality.rda", as.is=TRUE)
metabin(deaddef, allvdd, deadnotvdd, allnotvdd, sm= "OR", method="I", data=mort, studlab=study)
mmo
forest(mmo, comb.fixed=FALSE, xlab= "proportion")
forest(mmo, comb.random=FALSE, xlab= "proportion")
funnel(mmo)
mortn <- read.csv("mortnew.rda", as.is=TRUE)
mor <- metabin(Eedeaddef, Nealldef, Ecdeadnodef, Ncallnondef, sm= "OR", method="I", data=mortn, studlab=study)
forest(mor, comb.fixed=FALSE, xlab= "proportion")
forest(mor, comb.random=FALSE, xlab= "proportion")
funnel(mor)
#######################################################################################
#sensitivity analysis high quality studies only in mortality outcome 
#################################################################################
hqm <- read.csv("highqmort.rda", as.is=TRUE)
hqmm <- metabin(deaddef, alldef, deadnodef, allnodef, sm= "OR", method="I", data=hqm, studlab=study)
forest(hqmm, comb.fixed=FALSE, xlab= "proportion")
forest(hqmm, comb.random=FALSE, xlab= "proportion")
funnel(hqmm)
####################################################
#sens analysis high qualiy studies only prevalence 
##################################################
hqprev <- read.csv("hqonlyprev.rda", as.is=TRUE)
metaprop(vddch, totch, studlab=paste(study), data = hqprev)
sm150 <- read.csv("ssm150.rda", as.is=TRUE)
sm150
metaprop(vddch, totch, studlab=paste(study), data = sm150)
sless150 <- read.csv("ssless150new.rda", as.is=TRUE)
sless150 
metaprop(vddch, totch, studlab=paste(study), data = sless150)
#prevalence in those 21 studies that reported vdd under our set threshold <20ng/ml
getwd()
setwd("C:/Users/INSPIRON/Documents/Rch")
onlyset <- read.csv("only21st.rda", as.is=TRUE)
onlyset 