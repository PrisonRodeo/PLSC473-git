#######################################
# This is some code from PLSC 473,
# dated December 3, 2015.
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

# Read in "master" (votes + biographical) data:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC473-git/master/Data/PLSC473Votes.csv")
Votes <- read.csv(text = url) # read the "votes" data

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC473-git/master/Data/PLSC473Justices.csv")
Justices <- read.csv(text = url) # read the "justices" data

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC473-git/master/Data/DWNOMINATE.csv")
DWNOM <- read.csv(text = url) # read the DW-NOMINATE data

# Merge:

VotesPlus <- merge(Votes,Justices,by=c("JID"))
AllData <- merge(VotesPlus,DWNOM,by=c("term"))

#######################################
# See the slides for discussions of what DW-NOMINATE
# scores are, where to find them, etc. Here, 
# I've created a file that contains the median 
# DW-NOMINATE scores for the Senate for each term
# of the Court, and the same score for the sitting
# President. In general, higher DW-NOMINATE scores 
# denote more conservative senators / presidents.
#  
# We can take a look at how the DW-NOMINATE scores
# of the median senator and of the president have
# changed during the 1946-2014 period:

png("DWNOMS.png",640,500)
par(mar=c(4,4,2,2))
with(DWNOM, plot(term,SenMedian,t="l",lwd=2,col="black",
                  xlab="Term",ylab="Conservatism",
                  ylim=c(-1,1)))
with(DWNOM, lines(term,Prez,lty=2,lwd=2,col="red"))
abline(h=0,lty=3,lwd=1,col="black")
legend("topleft",c("Senate Median", "President"),bty="n",
       lwd=c(2,2),col=c("black","red"),lty=c(1,2))
dev.off()

# Next, we'll examine two things: (a) whether there is
# an association between justices' votes and the liberalism /
# conservatism of the president and/or the Senate, and (b)
# whether that association is stronger in constitutional cases
# than in statutory ones.
#
# First, let's see if the liberalism / conservatism of
# the Senate and/or the president correlate with the 
# votes of the justices. 
#
# To do that, we'll first create our standard "liberal
# vote" variable:

AllData$LiberalVote <- AllData$direction - 1

# Now, what was the average / mean DW-NOMINATE score
# in liberal votes vs. conservative ones?

with(AllData, t.test(SenMedian~LiberalVote))
with(AllData, t.test(Prez~LiberalVote))

# This indicates that there is a very slight positive 
# association between the median senator's liberalism
# and the votes of the justices, and a somewhat stronger
# association between that of the president and the
# justices' votes.
#
# We can see this graphically as well:

SenMeans <- aggregate(AllData$SenMedian, by=list(AllData$LiberalVote), 
                      FUN=mean,na.rm=TRUE)
PrezMeans <- aggregate(AllData$Prez, by=list(AllData$LiberalVote), 
                      FUN=mean,na.rm=TRUE)

png("AllDNOMMeans.png",640,500)
par(mar=c(4,4,2,2))
with(AllData, dotchart(c(SenMeans$x, PrezMeans$x),
                       pch=20,xlab="Mean Conservatism",
                       labels=c("Senate (Conservative Votes)",
                                "Senate (Liberal Votes)",
                                "President (Conservative Votes)",
                                "President (Liberal Votes)")))
dev.off()

# Next, we'll distinguish between statutory and 
# constitutional decisions. The variable with which
# we can do so is called "lawType." lawType equals
# 1 or 2 if the case is constitutional and 3 or 6
# if it is a statutory case (see the codebook for 
# full details on that variable).
#
# We'll create two data subsets: one that is ONLY
# constitutional cases, and the other that is
# only statutory cases:

AllData$Const <- 0
AllData$Const <- ifelse(AllData$lawType==1,1,AllData$Const)
AllData$Const <- ifelse(AllData$lawType==2,1,AllData$Const)
ConstCases <- AllData[AllData$Const==1,]

AllData$Stat <- 0
AllData$Stat <- ifelse(AllData$lawType==3,1,AllData$Stat)
AllData$Stat <- ifelse(AllData$lawType==6,1,AllData$Stat)
StatCases <- AllData[AllData$Stat==1,]

# Now examine the association between those variables and
# the votes of the justices for each of these two subsets.
# Our expectation is that the association will be stronger
# in statutory cases than in constitutional ones.

with(ConstCases, t.test(SenMedian~LiberalVote))
with(ConstCases, t.test(Prez~LiberalVote))

with(StatCases, t.test(SenMedian~LiberalVote))
with(StatCases, t.test(Prez~LiberalVote))

# It appears that there is a positive association between 
# presidential liberalism and liberal SCOTUS votes in both 
# types of cases, but that the association between Senate 
# liberalism and votes only holds in constitutional
# cases (but not statutory ones).
#
# 
#
#
#
#
# If you're still reading this, here's are logistic regressions
# of justices' votes on their own ideology, that of the president,
# that of the Senate, and the interaction of the former with
# the latter two for all cases, and split between statutory and
# constitutional ones (this was not discussed in class):

summary(with(AllData,
        glm(LiberalVote~ideo+SenMedian+Prez+ideo*SenMedian+ideo*Prez,
            family="binomial")))

summary(with(ConstCases,
             glm(LiberalVote~ideo+SenMedian+Prez+ideo*SenMedian+ideo*Prez,
                 family="binomial")))

summary(with(StatCases,
             glm(LiberalVote~ideo+SenMedian+Prez+ideo*SenMedian+ideo*Prez,
                 family="binomial")))



