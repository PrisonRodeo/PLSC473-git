#######################################
# This is some code from PLSC 473,
# dated October 13, 2015.
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

LOP <- c("RCurl")  # All this is just to read the data 
#  from github
NP <- LOP[!(LOP %in% installed.packages()[,"Package"])]
if(length(NP)) install.packages(NP)
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

# Plot ideology vs. qualifications:

pdf("SCScores.pdf",7,6)
par(mar=c(4,4,2,2))
seed<-1234
with(SCScores, plot(qualifications,ideology,pch=20,
                    ylab="Ideology",xlab="Qualifications",
                    xlim=c(-0.1,1.2),ylim=c(0,1.1)))
with(SCScores, text(qualifications,jitter(ideology,30),
                    labels=SCJustice,pos=3,cex=0.8))
dev.off()

# Ideology & voting:

DF<-ddply(Master, "JID", summarize,
          LibVote = mean(direction-1,na.rm=TRUE))
DF<-merge(DF,SCScores,by="JID")

pdf("SCvsVotes.pdf",7,6)
par(mar=c(4,4,2,2))
seed<-1234
with(DF, plot(ideology,LibVote*100,pch=20,
              ylab="Liberal Voting Percent",xlab="Ideology",
              xlim=c(-0.1,1.1),ylim=c(25,80)))
with(DF, text(ideology,LibVote*100,ideology,
              labels=SCJustice,pos=3,cex=0.8))
with(DF, abline(lm(LibVote*100~ideology),lwd=3,col="red"))
dev.off()




