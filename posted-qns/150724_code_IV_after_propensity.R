# author: Steve Harris
# date: 2015-07-24
# subject: How to calculate the estimator?


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

# Simple paired t-test    
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
