#######################################
# This is some code from PLSC 473,
# dated November 5, 2015.
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
# Analysis: Cases involving habeas corpus.
#
# Subset immigration cases ONLY: Those are the ones
# where the "issue" variable has values of 10020.

Habeas<-Master[Master$issue==10020,]

# Also, make a case-level data frame, for some
# summary statistics:

HCases<-ddply(Habeas, .(docketId,term,lawSupp),summarize,
              NMaj=sum(majority-1),
              ProHabeas = mean(decisionDirection-1))

# Plot number of cases per term:

TermData<-ddply(HCases,.(term),summarize,
                NCases=length(term),
                ProPct=(mean(ProHabeas)*100))

pdf("HabeasByTerm.pdf",6,5)
par(mar=c(4,4,2,2))
with(TermData, plot(term,NCases,t="l",lwd=3,xlab="Term",
                    ylab="Number of Cases",))
abline(v=1996,lwd=2,lty=2)
dev.off()

# How many pro- and anti-claimant outcomes?:

pdf("HabeasOutcomes.pdf",6,5)
par(mar=c(4,4,2,2))
barplot(table(HCases$ProHabeas),col="navy",
        names.arg=c("Anti-Habeas","Pro-Habeas"))
dev.off()

# How do pro- and anti-claimant outcomes vary over
# time?

pdf("HabeasOutcomesByTerm.pdf",6,5)
par(mar=c(4,4,2,2))
with(TermData, barplot(ProPct,xlab="Term",
                    names.arg=term,col="navy",
                    ylab="Percent Pro-Habeas",))
dev.off()

# Table of case-level laws:

table(HCases$lawSupp)

# Now, create variables to use in the analysis.
# The outcome is each justice's vote, either 
# pro-habeas ("liberal") or anti-habeas 
# ("conservative"):

Habeas$ProVote<-Habeas$direction-1

# Note that the variable "ideology" is the Segal-
# Cover score for each justice, coded with "1"
# as most liberal and "0" as most conservative.
#
# Is the defendant the petitioner? Code this 1
# if yes, 0 otherwise

Habeas$CrimPet<-ifelse(Habeas$petitioner==126,1,0)

# Was the legal matter in the case applicability
# of federal habeas, or something else?

Habeas$HabLaw<-ifelse(Habeas$lawSupp==341,1,0)

# Was there disagreement in the lower courts? That is
# the "lcDisagreement" variable; it equals "1" if there
# was such disagreement and 0 otherwise.

Habeas$Disagree<-Habeas$lcDisagreement

# Last: An indicator (=1) for cases heard after
# passage of the AEDPA in 1996:

Habeas$AEDPA<-ifelse(Habeas$term>1996,1,0)

##################################
# Some bivariate statistics, looking at the
# correlation between each of those variables
# and the votes of the justices:

options(digits=4)
Vars<-with(Habeas, data.frame(ProVote,ideology,CrimPet,
                          HabLaw,Disagree,AEDPA))
cor(Vars,use="complete.obs")

# Now, a multivariate / regression analysis:

Regression <- with(Habeas,
                   lm(ProVote~ideology+CrimPet+HabLaw+
                        Disagree+AEDPA))
summary(Regression)

# This suggests that:
# 
# - More liberal justices are substantially more likely to 
# vote in a pro-habeas direction than are conservatives.
#
# - Justices are also more likely to vote in a pro-habeas 
# direction in cases where the detainee is bringing the 
# petition (that is, when a pro-habeas vote is a vote to 
# reverse the lower court).
#
# - Justices are also (all else equal) more likely to vote 
# in a pro-habeas direction in cases decided after the 
# passage of the AEDPA in 1996.
#
# - Justices are somewhat less likely to vote in a 
# pro-habeas direction when the case involves a question 
# of the applicability of federal habeas law.
#
# - We find no conditional association between pro-habeas 
# votes and lower court disagreement.

