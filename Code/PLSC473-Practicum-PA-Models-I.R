#######################################
# This is some code from PLSC 473,
# dated October 6, 2015.
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

# install.packages("devtools")
library("devtools")
# install_github("aaboyles/hadleyverse")
library(hadleyverse)

# Alternatively, take out the "#"s and run this:
# install.packages(dplyr)
# library(dplyr)

# Read in "master" (votes + biographical) data:

LOP <- c("RCurl")  # All this is just to read the data 
                   #  from github
NP <- LOP[!(LOP %in% installed.packages()[,"Package"])]
if(length(NP)) install.packages(NP)
library(RCurl)

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC473-git/master/Data/PLSC473Votes.csv")
Votes <- read.csv(text = url) # read the "votes" data

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC473-git/master/Data/PLSC473Justices.csv")
Justices <- read.csv(text = url) # read the "justices" data

# Alternatively, you can read it in "locally" simply:
#
# Votes<-read.csv("PLSC473Votes.csv")
# Justices<-read.csv("PLSC473Justices.csv")

# Now, MERGE those two together. The variable "JID" 
# allows you to do this:

Master <- merge(Votes,Justices,by=c("JID"))

# Next, extract only the variables we are interested in and/or
# will need for the analysis. These include the justices' 
# backgrounds (urban / rural), the issue in each case, and the 
# vote of each justice, along with some justice and case
# identifiers.

MyVars <- c("JID","docketId","voteId","issue","issueArea",
          "direction","childsur")
Data<-Master[MyVars] # subsets the data

# Next, we'll want to ensure that our variables measure things
# the way we want them to. First, let's look at the "vote"
# variable:

table(Data$direction)

# That is coded strangely, with liberal votes coded as "1"
# and conservative as "2". Let's make it better:

Data$LiberalVote <- Data$direction-1

# That should give us a variable that =1 if the justice 
# voted "liberally" and 0 if not. Let's check:

with(Data, xtabs(~direction+LiberalVote))

# Now liberal votes are "1" and conservative are "0". Next, 
# examine the "background" variable:

table(Data$childsur)

# There are three values in the data: family farm, small
# town, and urban. Let's make a variable that =1 if the
# justice was from an urban area, and 0 otherwise:

Data$Urban <- ifelse(Data$childsur=="urban (l",1,0)

# Check to make sure it "worked":

with(Data, xtabs(~childsur+Urban))

# Finally, we want to draw out the cases in the data that 
# address the four issues we discussed. We can see from looking
# at the codebook that:

#  - Racial discrimination cases are issue numbers 20040 and
# 20050 (desegregation), 20070 (affirmative action), and 20080
# (protests re: racial discrimination). 

#  - Education cases are harder to locate; for now, we'll just
# look at those with issue code 20050 (school desegregation),
# 30180 (parochaid), and 20290 (immigrants' access to education).

#  - Crime is probably best just done using issueArea code #1
# (criminal cases)

#  - Gender discrimination issue codes are 20130 and 20140.
#
# Here's how we subset...

Data$keep<-0
Data$keep<-ifelse(Data$issue==20040,1,Data$keep)
Data$keep<-ifelse(Data$issue==20050,1,Data$keep)
Data$keep<-ifelse(Data$issue==20070,1,Data$keep)
Data$keep<-ifelse(Data$issue==20080,1,Data$keep)
RaceData <- Data[Data$keep==1,]

# Note that we now have a "smaller" dataset comprising only
# cases from those four issue areas.

table(RaceData$issue)

# We can do a similar thing for the other issues:

Data$keep<-0
Data$keep<-ifelse(Data$issue==20050,1,Data$keep)
Data$keep<-ifelse(Data$issue==30180,1,Data$keep)
Data$keep<-ifelse(Data$issue==20290,1,Data$keep)
EdData <- Data[Data$keep==1,]  # Education

CrimeData<-Data[Data$issueArea==1,]  # Criminal cases

Data$keep<-0
Data$keep<-ifelse(Data$issue==20130,1,Data$keep)
Data$keep<-ifelse(Data$issue==20140,1,Data$keep)
SexData <- Data[Data$keep==1,]  # Gender

####################################################
# Now we can begin to look at the distribution of 
# the "outcomes" of interest for each data set. We'll
# start with liberal voting percentages in each type
# of case:

prop.table(table(RaceData$LiberalVote))*100
prop.table(table(EdData$LiberalVote))*100
prop.table(table(CrimeData$LiberalVote))*100
prop.table(table(SexData$LiberalVote))*100

# We can do the same thing with plots:

barplot(prop.table(table(RaceData$LiberalVote))*100,
        names.arg=c("Conservative","Liberal"))

# We can also do a quick description of the "Urban"
# variable in the "entire" data set:

barplot(prop.table(table(Data$Urban))*100,
        names.arg=c("Rural","Urban"))

# And we can look at and compare the means of the "urban"
# variable across the four subsets of cases. These ought
# (we hope) to be roughly the same:

UrbanMeans<-c(mean(Data$Urban,na.rm=TRUE),
              mean(RaceData$Urban,na.rm=TRUE),
              mean(EdData$Urban,na.rm=TRUE),
              mean(CrimeData$Urban,na.rm=TRUE),
              mean(SexData$Urban,na.rm=TRUE))
UrbanMeans

# A picture is more compelling:

barplot(UrbanMeans*100,cex.names=0.6,
    names.arg=c("All","Race","Education","Criminal","Gender"))

# Finally, we can begin assessing the association between
# voting and urban / rural backgrounds. Since our two
# variables are both binary / dichotomous, we can do that
# most simply by looking at a cross-table of voting and
# urban/rural background.
#
# We'll start with "all" the data / every vote since 1946:

with(Data, prop.table(xtabs(~LiberalVote+Urban),2)) * 100

# This indicates that justices from rural backgrounds 
# vote liberally in all cases roughly 58 percent of
# the time, while those from urban backgrounds vote
# liberally nearly 50 percent of the time.
#
# We can look at the same crosstabs for the various
# subsets of cases that we created:

with(RaceData, prop.table(xtabs(~LiberalVote+Urban),2)) * 100
with(EdData, prop.table(xtabs(~LiberalVote+Urban),2)) * 100
with(CrimeData, prop.table(xtabs(~LiberalVote+Urban),2)) * 100
with(SexData, prop.table(xtabs(~LiberalVote+Urban),2)) * 100

# Next time, we'll go into more detail on the analyses,
# and also talk about presentation of findings.

