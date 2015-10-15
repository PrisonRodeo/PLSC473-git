#######################################
# This is some code from PLSC 473,
# dated October 15, 2015.
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

# "Replicate"  (in part) Segal et al. (1995)
#
# Create data for Table 2:

CRCL<-Master[Master$issueArea<6,]
EC<-Master[Master$issueArea==8,]

Civ<-ddply(CRCL, c("JID","presname"), summarize,
          LibVote = mean(direction-1,na.rm=TRUE))
Civ<-merge(Civ,SCScores,by="JID")

Econ<-ddply(EC, c("JID","presname"), summarize,
           LibVote = mean(direction-1,na.rm=TRUE))
Econ<-merge(Econ,SCScores,by="JID")

# Plot Civil rights / liberties:

pdf("SCvsCRCL.pdf",4,6)
par(mar=c(4,4,4,2))
seed<-1234
with(Civ, plot(ideology,LibVote*100,pch=20,
              ylab="Liberal Voting Percent",xlab="Ideology",
              xlim=c(-0.2,1.2),ylim=c(10,100),
              main="Civil Rights and Liberties"))
with(Civ, text(ideology,LibVote*100,ideology,
              labels=SCJustice,pos=3,cex=0.8))
with(Civ, abline(lm(LibVote*100~ideology),lwd=3,col="red"))
dev.off()

# Plot economics:

pdf("SCvsEcon.pdf",4,6)
par(mar=c(4,4,4,2))
seed<-1234
with(Econ, plot(ideology,LibVote*100,pch=20,
               ylab="Liberal Voting Percent",xlab="Ideology",
               xlim=c(-0.2,1.2),ylim=c(10,100),
               main="Economics"))
with(Econ, text(ideology,LibVote*100,ideology,
               labels=SCJustice,pos=3,cex=0.8))
with(Econ, abline(lm(LibVote*100~ideology),lwd=3,col="red"))
dev.off()

# Table 3 correlations:

CivR1<-with(Civ[Civ$presname=="roosevelt, franklin delano" |
           Civ$presname=="truman, harry s.",], 
     cor(ideology,LibVote)) # FDR & Truman

CivR2<-with(Civ[Civ$presname!="roosevelt, franklin delano" &
                  Civ$presname!="truman, harry s.",], 
            cor(ideology,LibVote)) # Eisenhower-Obama

CivR3<-with(Civ, cor(ideology,LibVote)) # All

EconR1<-with(Econ[Econ$presname=="roosevelt, franklin delano" |
                  Econ$presname=="truman, harry s.",], 
            cor(ideology,LibVote)) # FDR & Truman

EconR2<-with(Econ[Econ$presname!="roosevelt, franklin delano" &
                  Econ$presname!="truman, harry s.",], 
            cor(ideology,LibVote)) # Eisenhower-Obama

EconR3<-with(Econ, cor(ideology,LibVote)) # All

c(CivR1,EconR1,CivR2,EconR2,CivR3,EconR3)

# Now, "Actual" vs. "Predicted". When we fit a regression
# line to some data, the difference between the line 
# and the actual point is called a "residual." The
# residual shows whether the justice in question voted
# more or less liberally than we would have expected
# them to based on their Segal-Cover score.
#
# We can do the same plots Segal et al. did, for the 
# post-Truman justices. 
#
# First, subset the data:

CivNew<-Civ[Civ$presname!="roosevelt, franklin delano" &
                Econ$presname!="truman, harry s.",]

EconNew<-Econ[Econ$presname!="roosevelt, franklin delano" &
                    Econ$presname!="truman, harry s.",]

# Now fit a regression line to the civil rights /
# liberties data, and generate predictions:

CFit<-with(CivNew, lm(LibVote*100~ideology))
CPreds<-CFit$fitted.values

# Now plot one vs. the other:

pdf("CivLibsAvP.pdf",7,6)
plot(CPreds,CivNew$LibVote*100,pch=20,
               ylab="Actual Support", 
               xlab="Predicted Support",
               xlim=c(10,100),ylim=c(10,100),
               main="Civil Rights and Liberties")
text(CPreds,CivNew$LibVote*100,
               labels=CivNew$SCJustice,pos=3,cex=0.8)
abline(a=0,b=1,lwd=2,col="red")
dev.off()

# Now repeat for economics cases...

EFit<-with(EconNew, lm(LibVote*100~ideology))
EPreds<-EFit$fitted.values

# Now plot one vs. the other:

pdf("EconAvP.pdf",7,6)
plot(EPreds,EconNew$LibVote*100,pch=20,
     ylab="Actual Support", 
     xlab="Predicted Support",
     xlim=c(10,100),ylim=c(10,100),
     main="Economics")
text(EPreds,EconNew$LibVote*100,
     labels=EconNew$SCJustice,pos=3,cex=0.8)
abline(a=0,b=1,lwd=2,col="red")
dev.off()

# Finally: Ginsburg and Breyer, vs. the Segal
# et al. predictions:

RBG.Civ<-with(Civ[Civ$SCJustice=="RBGinsburg",], LibVote*100)
RBG.Econ<-with(Econ[Econ$SCJustice=="RBGinsburg",], LibVote*100)
SGB.Civ<-with(Civ[Civ$SCJustice=="SGBreyer",], LibVote*100)
SGB.Econ<-with(Econ[Econ$SCJustice=="SGBreyer",], LibVote*100)

c(RBG.Civ,RBG.Econ,SGB.Civ,SGB.Econ)
