#######################################
# This is some code from PLSC 473,
# dated November 17, 2015.
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

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC473-git/master/Data/PublicMood.csv")
Mood <- read.csv(text = url) # read the "justices" data

# Merge:

Master   <- merge(Votes, Justices, by=c("JID"))

# Create a {0,1} liberal vote variable:

Master$LibVote <- Master$direction - 1

# Drop cases where SCOTUS affirmed the lower court:

Master$Rev <- ifelse(Master$caseDisposition>1 & 
                       Master$caseDisposition<5,1,0)
Reversals <- Master[Master$Rev==1,]

# Create three files for Criminal, CivLibs, and Economics:

CrimRev <- Reversals[Reversals$issueArea==1,]
CivlibRev <- Reversals[Reversals$issueArea==2,]
EconRev <- Reversals[Reversals$issueArea==8,]

# Aggregate the votes up to the term level...

CrimAnn <- ddply(CrimRev, "term", summarize,
                 SegalCover = mean(ideo,na.rm=TRUE)*100,
                 CrimLibPct = (mean(LibVote,na.rm=TRUE))*100)
CLAnn <- ddply(CivlibRev, "term", summarize,
                 CLLibPct = (mean(LibVote,na.rm=TRUE))*100)
EconAnn <- ddply(EconRev, "term", summarize,
                 EconLibPct = (mean(LibVote,na.rm=TRUE))*100)

# Build annual "time series" dataset:

colnames(Mood) <- c("term","Mood")
TSData <- merge(Mood,CrimAnn,by=c("term"))
TSData <- merge(TSData,CLAnn,by=c("term"))
TSData <- merge(TSData,EconAnn,by=c("term"))

# Examine plots & correlations:

pdf("MoodPlot.pdf",7,6)
par(mar=c(4,4,2,2))
with(TSData, plot(term,Mood,t="l",lwd=3,col="black",ylim=c(15,85),
                  xlab="Term",ylab="Liberalism"))
with(TSData, lines(term,CrimLibPct,lwd=2,col="blue"))
with(TSData, lines(term,CLLibPct,lwd=2,col="red"))
with(TSData, lines(term,EconLibPct,lwd=2,col="orange"))
legend("bottomleft",c("Mood","Criminal","Civil Rights","Economics"),
       lwd=c(3,2,2,2),col=c("black","blue","red","orange"),
       bty="n",cex=0.85)
dev.off()

# Correlations & scatterplots:

cor(TSData)

pdf("MoodScatters.pdf",7,3)
par(mar=c(4,4,2,2))
par(mfrow=c(1,3))
with(TSData, plot(Mood,CrimLibPct,pch=20,col="blue",
                  ylab="Criminal Liberalism"))
with(TSData, plot(Mood,CLLibPct,pch=20,col="red",
                  ylab="Civil Rights Liberalism"))
with(TSData, plot(Mood,EconLibPct,pch=20,col="orange",
                  ylab="Economic Liberalism"))
dev.off()

# BUT: Need to control for ideology, etc...
# 
# with(TSData, summary(lm(CrimLibPct~Mood+SegalCover)))
# with(TSData, summary(lm(CLLibPct~Mood+SegalCover)))
# with(TSData, summary(lm(EconLibPct~Mood+SegalCover)))
