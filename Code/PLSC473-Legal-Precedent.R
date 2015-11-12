#######################################
# This is some code from PLSC 473,
# dated November 12, 2015.
#
# Note that anything on a line
# that starts with a "#" is a 
# comment, and will be ignored by
# the program (but not by you).
#######################################
# 
# Read in data:
#
# Load necessary R packages:
#
# By now, you should already have installed the R 
# packages "RCurl" and "plyr". If you have not, 
# uncomment and run this code:
#
# install.packages("RCurl")
# install.packages("plyr")
#
# Then do:

library(RCurl)
library(plyr)

# Read in "master" (votes + biographical + Segal-Cover) data:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC473-git/master/Data/PLSC473Votes.csv")
Votes <- read.csv(text = url) # read the "votes" data

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC473-git/master/Data/PLSC473Justices.csv")
Justices <- read.csv(text = url) # read the "justices" data

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC473-git/master/Data/SegalCover2015.csv")
SCScores <- read.csv(text = url) # read the "justices" data

# Merge:

JSC    <- merge(Justices, SCScores, by=c("JID"))
Master <- merge(Votes,JSC,by=c("JID"))

#############################################
# Analysis: Cases alteration of precedent...
#
# See the slides for details. We're examining two different
# phenomena: Sizes of majorities, and ideological voting
#
# First, how many cases are there in which precedent
# was overturned?

Cases <- ddply(Master, .(docketId), summarize,
               AltPrec = mean(precedentAlteration,na.rm=TRUE),
               MajVotes = mean(majVotes,na.rm=TRUE))
Ns <- table(Cases$AltPrec)

pdf("PrecAlterNs.pdf",6,5)
par(mar=c(4,4,2,2))
foo<-with(Cases, barplot(table(AltPrec),col="navy",ylim=c(0,11000),
                 names.arg=c("No Alteration","Alteration")))
text(foo,table(Cases$AltPrec),c(Ns),pos=3)
dev.off()

# Make a liberal vote variable:

Master$LibVote <- Master$direction-1

# Separate the "Master" and "Case" data into two piles:

AltV <- Master[Master$precedentAlteration==1,]
NoAltV <- Master[Master$precedentAlteration==0,]

AltC <- Cases[Cases$AltPrec==1,]
NoAltC <- Cases[Cases$AltPrec==0,]

# First, examine sizes of majority coalitions in altering
# vs. non-altering decisions, using the case-level data:

with(AltC, mean(MajVotes,na.rm=TRUE))
with(NoAltC, mean(MajVotes,na.rm=TRUE))

with(Cases, t.test(MajVotes~AltPrec))

# Non-precedent-altering cases have slightly but
# significantly larger majorities than those that alter
# existing precedent.
#
# Next: examine the correlation between Segal-Cover scores 
# and vote direction (liberal or conservative) in the two
# data sets:

options(digits=3)

with(AltV, cor(ideology,LibVote,use="complete.obs"))
with(NoAltV, cor(ideology,LibVote,use="complete.obs"))

# Correlations between ideology and voting are higher
# in cases involving alteration of precedent. 
