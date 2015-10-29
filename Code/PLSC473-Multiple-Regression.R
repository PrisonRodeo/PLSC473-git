#######################################
# This is some code from PLSC 473,
# dated October 29, 2015.
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

# Merge these together:

Master <- merge(Votes,Justices,by=c("JID"))

# Aggregate the data to the level of each justice (one line of data
# per justice), and calculate their overall (career) liberal voting
# percentage across all cases. Do that while retaining indicators for
# gender and party of the appointing president:

JData <- ddply(Master, .(JID,gender,prespart), summarize,
               LibPercent = mean((direction-1)*100,na.rm=TRUE))

# Both of our "independent" variables are binary. So we can examine
# the data by looking at a plot of the mean liberalism for each combination 
# of values on those variables:

Means <-ddply(JData, .(gender, prespart), summarize,
              MeanLib = mean(LibPercent))

pdf("MeansPartyGender.pdf",6,5)
par(mar=c(4,4,2,2))
with(Means, dotchart(MeanLib, pch=19, 
                xlim=c(30,70),
                labels=c("Female Democrat",
                         "Female Republican",
                         "Male Democrat",
                         "Male Republican")))
dev.off()

# Now fit a multivariate regression:

mregress <- with(JData, lm(LibPercent~gender+prespart))

summary(mregress)