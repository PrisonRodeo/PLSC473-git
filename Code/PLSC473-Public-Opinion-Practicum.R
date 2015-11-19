#######################################
# This is some code from PLSC 473,
# dated November 19, 2015.
#
# Note that anything on a line
# that starts with a "#" is a 
# comment, and will be ignored by
# the program (but not by you).
#######################################
# 
# Read in data:
#
# Load necessary R packages -- uncomment the next three
# lines if you haven't already installed the "RCurl" 
# and "plyr" packages:

# LOP <- c("RCurl")  # All this is just to read the data from github
# NP <- LOP[!(LOP %in% installed.packages()[,"Package"])]
# if(length(NP)) install.packages(NP)
library(RCurl)
library(plyr)

# Read in "master" (votes + biographical + Segal-Cover) data:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC473-git/master/Data/PLSC473Votes.csv")
Votes <- read.csv(text = url) # read the "votes" data

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC473-git/master/Data/PLSC473Justices.csv")
Justices <- read.csv(text = url) # read the "justices" data

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC473-git/master/Data/PolicyMoods.csv")
PolicyMoods <- read.csv(text = url) # read the "justices" data

# Merge votes and justices data:

Master <- merge(Votes,Justices,by=c("JID"))

#############################################
# Analysis: Public Opinion and Civil Rights
#
# First, extract data on mood that just focuses on 
# civil rights and liberties. In the Policy Agendas
# Project, these correspond to "MajorTopic" code "2".
#
# We can summarize the policy moods data:

summary(PolicyMoods)

# So, pull out data only where MajorTopic = 2:

CRMoods <- PolicyMoods[PolicyMoods$MajorTopic==2,]

# Note that these data have multiple observations for
# each year, corresponding to different sub-topics
# (e.g., race/ethnicity, gender, etc.). We'll start
# by creating a mood measure that combines all these
# different mood measures, by averaging across all
# of them:

CRMoodAnnual <- ddply(CRMoods, "Year", summarize,
                      CRMood = mean(Mood,na.rm=TRUE))

# These are now annual observations about overall public
# mood on civil rights and liberties, 1946-2011. We'll 
# add a "term" variable to these data that allows them
# to be merged with the voting / Master data:

CRMoodAnnual$term <- CRMoodAnnual$Year

# Next, we'll create our usual vote direction variable,
# and subset the "Master" data, extracting only
# cases involving civil rights and liberties (that is, 
# those with "issueArea" = 2):

Master$LibVote <- Master$direction-1
CRVotes <- Master[Master$issueArea==2,]

# Now we'll merge the civil rights mood data with
# this:

CRMerged <- merge(CRVotes, CRMoodAnnual, by=c("term"))

# Now, do a quick correlation and plot to see if
# there is any (bivariate) association between 
# civil rights mood and the justices' votes.
#
# Correlation:

with(CRMerged, cor(CRMood,LibVote,use="complete.obs"))

# Plot:

MeanMoodCon <- with(CRMerged[CRMerged$LibVote==0,], 
                    mean(CRMood,na.rm=TRUE))
MeanMoodLib <- with(CRMerged[CRMerged$LibVote==1,], 
                    mean(CRMood,na.rm=TRUE))

pdf("MeanMoodByVote.pdf",6,5)
fig<-with(CRMerged, barplot(c(MeanMoodCon,MeanMoodLib),ylim=c(0,60),
                       names.arg=c("Conservative Votes", "Liberal Votes"),
                       col="navy",ylab="Mean Civil Liberties Mood"))
text(fig,c(MeanMoodCon,MeanMoodLib),pos=3,
     round(c(MeanMoodCon,MeanMoodLib),2))
dev.off()

# t-test:

with(CRMerged, t.test(CRMood~LibVote))

# Each of these suggests that there is actually a
# very slight NEGATIVE correlation between public
# mood on civil rights and justices' votes. However,
# those bivariate correlations don't address
# other possible influences. 
#
# To do that, we'll estimate a regression model, 
# where we control for justices' ideology as 
# well:

Model <- with(CRMerged, lm(LibVote~ideo+CRMood))
summary(Model)

# These results suggest that, once we control for 
# a justice's ideology, public liberalism on civil 
# rights has a small, positive association with 
# liberal voting. Specifically, each one-unit 
# increase (i.e., more liberal) in mood is
# associated with a 0.001-unit increase in the
# probability of a liberal vote. This is a
# small, positive effect.
#
# Last, we'll examine whether mood's association 
# with votes is different for different justices.
# To do that, we'll calculate the correlation
# between mood and liberal votes for each justice
# separately.
#
# We can use the variable "JID" (justice ID) to
# do this. We'll start by creating a small data
# set of the justices, that just has their names,
# IDs, and Segal-Cover scores:

JSmall <- ddply(CRMerged, .(JID,justiceName), summarize,
                SCScore = mean(ideo,na.rm=TRUE),
                MeanTerm = mean(term,na.rm=TRUE))

# Sort those data by Segal-Cover score:

JSmall <- JSmall[order(JSmall$JID),]

# Now, using the "CRMerged" data frame calculate 
# the correlation between civil rights mood and
# vote for each justice. 
#
# To do this, take each justices' votes,
# calculate their correlation with mood,
# and record it in a column in JSmall.
# We can do this using ddply, but first we 
# have to write a function (called, creatively,
# "func") to calculate the correlation
# we want:

func <- function(CRMerged)
{
  return(data.frame(Corr = cor(CRMerged$LibVote, CRMerged$CRMood,
                              use="complete.obs")))
}

# Now we can use ddply to call that function.
# This command basically says "split the
# data in CRMerged into separate sets defined
# by JID, and calculate the correlation for each
# of them, then stash it in a file."

JCorrs <- ddply(CRMerged,"JID",func)

# Now we can merge this with the JSmall data
# we just created:

JMerged <- merge(JSmall,JCorrs,by=c("JID"))

# And we can see whether there is any pattern to
# the correlations, by plotting each correlation
# against its justices' corresponding Segal-
# Cover score:

pdf("CRMoodCorrs.pdf",6,5)
par(mar=c(4,4,2,2))
with(JMerged, plot(SCScore, Corr, pch=20,
              xlim=c(-0.1,1.1),ylim=c(-0.2,0.3),
              xlab="Justice Liberalism",
              ylab="Vote-Mood Correlation"))
with(JMerged, text(SCScore,Corr,pos=3,cex=0.9, 
                   labels=justiceName))
abline(h=0,lty=2)
dev.off()

# The correlations are generally slightly positive.
# If anything, there appears to be a weaker 
# typical correlation between mood and voting
# among more moderate justices than among the
# mode ideologically extreme ones.
#
# We can also look at whether the correlations
# have changed over time. Since mood is fixed
# within each term, it is a constant, so we can't
# calculate a term-specific correlation between
# mood and voting. We can (and did) calculate 
# the mean term during which a justice cast
# his or her votes, and plot the justice-specific
# correlations against that:

pdf("CRMoodCorrsTerm.pdf",6,5)
par(mar=c(4,4,2,2))
with(JMerged, plot(MeanTerm, Corr, pch=20,
                   xlim=c(1940,2020),ylim=c(-0.2,0.3),
                   xlab="Mean Term",
                   ylab="Vote-Mood Correlation"))
with(JMerged, text(MeanTerm,Corr,pos=3,cex=0.9, 
                   labels=justiceName))
abline(h=0,lty=2)
dev.off()

# It appears that some of the more recent justices
# have relatively high correlations, but the
# pattern is not all that strong.
