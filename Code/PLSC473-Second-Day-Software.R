#######################################
# This is some code from PLSC 473,
# dated August 27, 2015.
#
# Note that anything on a line
# that starts with a "#" is a 
# comment, and will be ignored by
# the program (but not by you).
#######################################
#
# Basics: OBJECTS
#
# Assign the number "6" to an object
# named "A":

A <- 6

# Now, whenever you refer to "A",
# you're referring to the number 6.

A  # returns "6" in the window below.
   # The square braces and [1] indicate
   # that this is a single value
   # (one row, one column)

A + 5  # returns "11" in the window below.

A^4    # returns "1296" (6x6x6x6).

"A" # prints "A". Using quotes means
    # "take me literally; actually return
    # whatever is in the quotes."

# If we want to get rid of A, we can remove 
# it:

rm(A)

# Objects can have multiple elements.
# Here is the PSU women's volleyball
# team's overall winning percentages, 
# 2005-2014:

PSUWVB<-c(31/34,32/35,34/36,38/38,38/38,
          32/37,25/32,33/36,34/36,36/39)

# The "c()" says "combine these values into
# a vector or list." Note the new object 
# called "PSUWVB" in the "Environment" window.
#
# We can list this object:

PSUWVB

# We can do transformations on it:

PSUWVB * 100

# Note that we now have numbers in the 
# square braces that indicate the position
# of each element (number) in the object;
# the first one is [1], the second is [2],
# etc. We can assign those transformations 
# to other objects:

WinPct <- PSUWVB * 100

# We can also combine objects; usually, we
# do this by creating a "data frame."
# Think of a data frame as like an Excel
# spreadsheet. 
#
# So, we might create another object that lists
# the years from 2005 to 2014, in order:

Year <- c(2005,2006,2007,2008,2009,
          2010,2011,2012,2013,2014)

Year

# Note that a faster way to do this is to 
# use the "seq()" (for "sequence") command:

Year <- seq(2005,2014)

# We just "overwrote" the old "Year" object
# with the new one.
#
# Now we can combine these two objects into
# a single data frame, using the
# "data.frame" command:

VB <- data.frame(Year = Year,
                 WinProp = PSUWVB,
                 WinPct = WinPct)

# Note that there's now a new data frame
# called "DF" in the Environment window;
# it has 10 observations (rows) and
# three variables (columns).
#
# NOTE: If you want to "see" a data frame,
# you can simply click on it in the
# Environment window; this has the same
# effect as running "View()""

View(VB)

# There's lots more to say about objects, but 
# that's enough for now. 
#
####################
# READING IN DATA
#
# Most of the time, you won't want to enter
# data by hand. Instead, you will read data
# from something like a spreadsheet into
# Rstudio from some other source file. There
# are many ways to do this.
#
# RStudio can read data in many different
# formats, but the simplest is ".csv" 
# ("comma-separated values"), which is
# simply an Excel-type spreadsheet that 
# has been converted into plain text, with
# "cells" of the spreadsheet separated 
# from each other by a comma.
#
# One way we can read a file is to have it
# "locally" on our own machine:

SCOTUS <- read.csv("SCOTUS-votes.csv")

View(SCOTUS)

# These data are on U.S. Supreme Court 
# justices who have served since 1946.
# The first column is the justice's name,
# the second is the percentage of cases
# involving civil rights and liberties in
# which s/he voted in a pro-civil rights
# direction, and the third column records 
# whether (=1) or not (=0) the justice
# was appointed by a Republican president.
#
# We can also read the same data directly
# from the web, using a URL for the file.
# This requires a bit more complexity.
# You can ignore this part for now... 

LOP <- c("RCurl") 
NP <- LOP[!(LOP %in% installed.packages()[,"Package"])]
if(length(NP)) install.packages(NP)
library(RCurl)

# This part actually gets the data:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC473-git/master/Data/SCOTUS-votes.csv")
SCOTUS2 <- read.csv(text = url)
rm(url) # clean up

# From there, we can do things like summaries:

summary(SCOTUS)

##########################
# SINGLING OUT ELEMENTS:
#
# A data frame is rows and columns. We can
# extract rows, columns, or cells by specifying
# which one we need using the square-brace
# ("[]") notation. Watch what happens when we
# print SCOTUS:

SCOTUS

# The notation "FOO[R,C]" denotes the Rth
# row and the Cth column of the object called
# "FOO." So if we wanted to get Samuel Alito's
# row from the SCOTUS data, we could enter:

SCOTUS[24,]

# The fact that we left the second value
# blank means "give me all the columns
# from that row." Similarly, if we just 
# wanted the column with the justices' 
# voting percentages, we would enter:

SCOTUS[,2]

# which means "give me the values of 
# all the rows from column 2 of SCOTUS."
# A single value would then just be:

SCOTUS[24,3]

# which tells us that Alito was appointed 
# by a Republican. This is useful for a
# lot of things; for example, we can use
# it along with conditional statements to
# subset the data, like this:

GOPJustices <- SCOTUS[SCOTUS$GOP==1,]

# That means "Create an object called 
# 'GOPJustices' that consists of all of
# the columns of SCOTUS, but that only
# includes the rows that represent GOP
# appointees." We can see that the command
# worked:

View(GOPJustices)

#####################
# PLOTTING / GRAPHS
#
# R / RStudio is great for visualizing and
# plotting / graphing data. This is a fast
# introduction to the most basic graphing
# command, called "plot."
#
# A (very) basic plot of the volleyball team's
# winning percentage by year looks like this:

plot(VB$Year,VB$WinPct,t="l")

# The plot appears at the lower right.
# Note that the dollar-sign "XXX$YYY" 
# notation means "use the element YYY
# from object XXX." We can get around this
# in a few ways, the best probably being
# to use the "with()" command:

with(VB, plot(Year,WinPct,t="l"))

# This says "Use the object called 'VB'
# to draw a plot of the objects 'WinPct'
# and 'Year'." We can add a bunch of things
# to make it nicer looking:

with(VB, plot(Year,WinPct,t="l",lwd=2,col="navy",
      main="PSU Women's Volleyball Winning\nPercentages, 2005-2014",
      xlab="Season",ylab="Winning Percentage"))

# We can do a similar thing with the 
# Supreme Court data:

with(SCOTUS, plot(CRPercent~GOP))

# This is a (bad) "scatterplot"; the values
# of CRPercent are vertical, and the values
# of GOP (either 1 for Republican appointees
# or 0 for Democratic ones) are horizontal.
