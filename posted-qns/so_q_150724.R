# author: Steve Harris
# date: 2015-07-24
# subject: How to calculate the estimator

# Readme
# ======
# Near/far matching and IV analysis
# - Create those matches with the greatest possible treatment separation
#   so use patients admitted when there are 'no' beds and compare to those
#   admitted when there are two or even three plus beds.
# - Do this within hospitals? To ensure that the instrument really
#   represents a random event

# How to think about instrumental variables
# -----------------------------------------
# http://andrewgelman.com/2007/12/09/how_to_think_ab/


# The trick: how to think about IV’s without getting too confused

# Suppose z is your instrument, T is your treatment, and y is your
# outcome. So the causal model is z -> T -> y. The trick is to think of
# (T,y) as a joint outcome and to think of the effect of z on each. For
# example, an increase of 1 in z is associated with an increase of 0.8 in
# T and an increase of 10 in y. The usual “instrumental variables” summary
# is to just say the estimated effect of T on y is 10/0.8=12.5, but I’d
# rather just keep it separate and report the effects on T and y
# separately.

# In Piero’s example, this translates into two statements: (a) States with
# higher penalties for murder had higher penalties for defamation, and (b)
# States with higher penalties for murder had less reporting of
# corruption.

# Todo
# ====
# TODO: 2015-07-28 - [ ] post to SO to ask how to calculate the IV ratio



# Log
# ===
# 2015-07-23
# - file duplicatd from prev labbook
# 2015-07-28
# - modified from 150724 for posting to SO


rm(list=ls(all=TRUE))
library(data.table)
setwd('/Users/steve/aor/p-me/stackexchange/paper-spotearly')
source("project_paths.r")
# Load data
load(paste0(PATH_DATA, '/paper-spotearly.RData'))
ls()

# Basic selection model 
# ----------------------
vars    <- c('age', 'male', 'sepsis_dx', 'v_ccmds', 
            'icnarc_score', 'id',
            'dead90', 'open_beds_max', 'icu_accept')
tdt <- wdt[,vars, with=FALSE]
tdt[, discourage := ifelse(open_beds_max<=0,1,ifelse(open_beds_max>=3,0,NA))]
str(tdt)

# Now save a sub-sample of the data
# ---------------------------------
setnames(tdt, 'sepsis_dx', 'diagnosis')
setnames(tdt, 'v_ccmds', 'ward')
setnames(tdt, 'v_ccmds', 'ward')
setnames(tdt, 'icnarc_score', 'illness')
setnames(tdt, 'dead90', 'dead')
setnames(tdt, 'icu_accept', 'treat')
tdt[,open_beds_max:=NULL]
tdt <- tdt[id<=1000]
str(tdt)

# Data now ready for posting 
setwd('/Users/steve/aor/p-me/stackexchange')
getwd()
write.csv(tdt, file="posted-data/150728_qn.csv")



# Now write your question
# -----------------------

rm(list=ls(all=TRUE))
library(RCurl)
library(data.table)
library(MatchIt)

# Load data from github
url <- "https://raw.githubusercontent.com/docsteveharris/"
file <- "datascibc-qns/master/posted-data/150728_qn.csv"
tdt <- data.table(read.csv(text = getURL(paste0(url,file))))
# Clean and restrict data, Remove missing values
tdt <- tdt[!is.na(discourage),.(id, dead, treat, discourage, age, male, illness)]
# tdt[rowSums(is.na(tdt[, vars, with = FALSE])) == 0, ]
str(tdt)

# Match data
f.discourage <- as.formula('discourage ~ age + male + illness')
matchit.out <- matchit(f.discourage, data=tdt)
summary(matchit.out, standardize=TRUE)
tdf <- data.frame(tdt)
mdt <- data.table(
    z0=tdf[matchit.out$match.matrix[,1], 'discourage'],
    d0=tdf[matchit.out$match.matrix[,1], 'treat'],
    y0=tdf[matchit.out$match.matrix[,1], 'dead'],
    z1=tdf[row.names(matchit.out$match.matrix), 'discourage'],
    d1=tdf[row.names(matchit.out$match.matrix), 'treat'],
    y1=tdf[row.names(matchit.out$match.matrix), 'dead'])
mdt[,id.pair := .I, by=.I]
head(mdt,10)
with(mdt, t.test(d1, d0, paired=TRUE))
with(mdt, t.test(y1, y0, paired=TRUE))

library(reshape2)
mdt.wide <- melt(mdt, id.vars='id.pair')
mdt.wide[, pair := ifelse(grepl('.*0', variable), 0, 1)]
mdt.wide[, varname := substr(variable, 1,1)]
mdt.long <- dcast.data.table(mdt.wide, id.pair + pair ~ varname)

# Now analyse
mdt.match <- match.data(matchit.out)
with(mdt.match, table(treat, discourage)) # treatment is 'discouraged'
with(mdt.match, table(dead, discourage))  # mortality is lower when discouraged

