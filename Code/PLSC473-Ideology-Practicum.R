#######################################
# This is some code from PLSC 473,
# dated October 22, 2015.
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

#############################################
# Analysis: Cases involving immigration...
#
# Subset immigration cases ONLY: Those are the ones
# where the "issue" variable has values in the range
# from 20260 to 20310.

Immig<-Master[Master$issue>20259,]
Immig<-Immig[Immig$issue<20311,]

# Check:

table(Immig$issue)

# Note that the data base defines pro-immigrant
# votes as "liberal." So we can create a pro-
# immigrant vote variable as:

Immig$ProImmVote = Immig$direction-1

# Now, examine the fraction of pro-immigration votes 
# over time. The first line creates a yearly summary
# of the fraction of pro-immigration votes by the
# Court; the second creates a "term" variable to 
# plot it against:

AnnImm<-with(Immig, prop.table(xtabs(~term+ProImmVote),1))
Term<-as.numeric(row.names(Annual))

# Now graph it:

pdf("AnnualProImmig.pdf",6,5)
plot(Term,AnnImm[,2]*100,t="o",pch=20,
     ylab="Pro-Immigrant Vote Percentage",
     main="Pro-Immigrant Votes, 1946-2014")
dev.off()

# How does this compare to mean Segal-Cover scores
# during those years?
#
# This code creates a data set with one observation
# per year, and calculates the mean Segal-Cover score 
# for the sitting justices in that year:

AnnSC <- ddply(Master, .(term), summarize,
               SCAvg = mean(ideology))

# Now plot:

pdf("AnnualIdeology.pdf",6,5)
par(mar=c(4,4,2,2))
with(AnnSC, plot(term,SCAvg,t="l",lwd=2,
     ylab="Mean Court Liberalism",
     xlab="Term"))
dev.off()


# For cases where there was at least one dissenting
# vote, what was the mean Segal-Cover score for the
# justices on each side? We can calculate this for
# each case where there was at least one dissenting
# vote:

CaseMeans<-ddply(Immig[Immig$minVotes>0,], 
                 .(docketId,ProImmVote,term),summarize,
                 SCMean=mean(ideology))
CaseMeans<-CaseMeans[is.na(CaseMeans$ProImmVote)==FALSE,]

# That last line just drops cases with missing values.
#
# Now we can plot the mean Segal-Cover score for the 
# justices on each side of each case:

pdf("Coalition-SCs.pdf",6,5)
with(CaseMeans[CaseMeans$ProImmVote==0,], 
     plot(term,SCMean,t="o",ylim=c(0,1),lwd=2,xlab="Term",
          pch=20,ylab="Mean Justice Liberalism",
          main="Mean Segal-Cover Scores by Pro/Anti-\nImmigrant Vote"))
with(CaseMeans[CaseMeans$ProImmVote==1,],
     lines(term,SCMean,t="o",pch=20,lwd=2,lty=2,col="red"))
legend("topright",bty="n",
      legend=c("Anti-Immigrant","Pro-Immigrant"),
      lwd=c(2,2),lty=c(1,2),col=c("black","red"))
dev.off()

# Is there a difference between Segal-Cover scores 
# of justices that cast pro- vs. anti-immigrant votes?

pdf("SCBoxplots.pdf",6,5)
par(mar=c(4,4,2,2))
with(Immig[Immig$minVotes>0,],
     boxplot(ideology~ProImmVote,ylim=c(0,1),
     names=c("Anti-Immigrant","Pro-Immigrant"),
     ylab="Justice Liberalism"))
dev.off()

# Pre- and post-1960:

pdf("SCBoxplots2.pdf",6,5)
par(mar=c(4,4,2,2))
par(mfrow=c(2,1))
with(Immig[Immig$minVotes>0 & Immig$term<1961,],
     boxplot(ideology~ProImmVote,ylim=c(0,1),
             names=c("Anti-Immigrant","Pro-Immigrant"),
             ylab="Justice Liberalism",
             main="1946-1960"))
with(Immig[Immig$minVotes>0 & Immig$term>1960,],
     boxplot(ideology~ProImmVote,ylim=c(0,1),
             names=c("Anti-Immigrant","Pro-Immigrant"),
             ylab="Justice Liberalism",
             main="1961-2014"))
dev.off()

# We also can do t-tests for the difference of means in the
# ideology scores, by how the justices voted:

with(Immig, t.test(ideology~ProImmVote))

# Does the difference change over time?
#
# Pre-1961 cases:

with(Immig[Immig$term<1961,], t.test(ideology~ProImmVote))

# Post-1960 cases:

with(Immig[Immig$term>1960,], t.test(ideology~ProImmVote))

# So, immigration clearly became more "politicized"
# after around 1960. But why? We'll look at two things:
# the fraction of cases that were unanimous, and the
# specific issues raised in the case.
#
# A unanimous cases is one where there are zero votes in
# the minority. So we can define a unanimous case indicator
# like this:

Immig$Unanimous <- ifelse(Immig$minVotes==0,1,0)

# Now make a data frame with one observation per case,
# by collapsing the nine votes from each case into a
# single percentage:

ImmCases<-ddply(Immig,.(docketId,Unanimous,term,issue),summarize,
             ProImmPct=mean(ProImmVote*100,na.rm=TRUE),
             MeanSC=mean(ideology,na.rm=TRUE))

# Next, create a pre- vs. post-1960 indicator in those 
# data:

ImmCases$Post1960<-ifelse(ImmCases$term>1960,1,0)

# Now...
#
# 1) Do we observe more unanimous cases before 1960 than
# after?

with(ImmCases, prop.table(xtabs(~Unanimous+Post1960),2))*100

# The answer is "no".
#
# 2) Does the "mix" of issues about immigration change
# after 1960?

with(ImmCases, prop.table(xtabs(~issue+Post1960),2))*100

# A: Yes; denaturalization cases were much more 
# prominent prior to 1960 than afterwards. The importance
# of this was discussed in class.
