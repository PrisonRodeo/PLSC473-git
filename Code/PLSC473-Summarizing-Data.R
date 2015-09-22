#######################################
# This is some code from PLSC 473,
# dated September 22, 2015. 
#
# Note that anything on a line
# that starts with a "#" is a 
# comment, and will be ignored by
# the program (but not by you).
#######################################
#
# Read in the Africa (2001) data:

Africa <- read.csv("Class Data/Africa2001.csv")

# Show first five lines:

head(Africa)

# Frequency tables:

wartab<-xtabs(~Africa$internalwar)
wartab
prop.table(wartab)
xtabs(~Africa$intensity)
prop.table(xtabs(~Africa$intensity))
xtabs(~Africa$polity)

# Means, standard deviations, etc.

mean(Africa$population)
median(Africa$population)
sd(Africa$population)

mean(Africa$adrate)
median(Africa$adrate)
sd(Africa$adrate)

# Global summary statistics:

summary(Africa)

# Basic histogram:

hist(Africa$muslperc,main="",xlab="Percent")

# Prettier histogram:

pdf("MuslimHistogram.pdf",7,5)
hist(Africa$muslperc,breaks=11,col="darkgreen",
     main="African Muslim Population Percentages, 2001",
     xlab="Percent",ylab="Frequency")
dev.off()

# Histogram of POLITY:

pdf("POLITYHistogram.pdf",7,5)
hist(Africa$polity,breaks=seq(-10,10),col="darkred",
     main="African POLITY Scores, 2001",
     xlab="Score",ylab="Frequency",xlim=c(-10,10))
dev.off()

# Kernel density plots, POLITY: 

pdf("POLITYDensity.pdf",7,5)
plot(density(Africa$polity,na.rm=TRUE,
            from=-10,to=10),lwd=2,
     main="African POLITY Scores, 2001",
     xlab="Score",ylab="Density",)
dev.off()

pdf("POLITYDensity2.pdf",7,5)
plot(density(Africa$polity,na.rm=TRUE,
             from=-10,to=10,adjust=0.25),lwd=2,
     main="African POLITY Scores, 2001",
     xlab="Score",ylab="Density")
dev.off()

# Kernel density plot, Muslim percentage: 

pdf("MuslimDensity.pdf",7,5)
plot(density(Africa$muslperc,na.rm=TRUE,
             adjust=0.5,from=0,to=100),lwd=2,
     main="African Muslim Percentages, 2001",
     xlab="Percent",ylab="Density")
dev.off()

# Single boxplot:

pdf("POLITYBoxplot.pdf",7,5)
boxplot(Africa$polity,main="African POLITY Scores, 2001",
        ylim=c(-10,10))
dev.off()

# Boxplot w/outliers: Population

pdf("PopBoxplot.pdf",7,5)
boxplot(Africa$population,
        main="African Population, 2001")
dev.off()

# Multiple boxplots:

pdf("MultiBoxplot.pdf",7,5)
with(Africa, boxplot(adrate,muslperc,literacy,
    main="African Data, 2001",
    names=c("HIV Rate","Muslim Pct.","Literacy")))
dev.off()
