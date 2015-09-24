#######################################
# This is some code from PLSC 473,
# dated September 24, 2015.
#
# Note that anything on a line
# that starts with a "#" is a 
# comment, and will be ignored by
# the program (but not by you).
#######################################
# Read in "Africa 2001" data:

Africa <- read.csv("Class Data/Africa2001.csv")
summary(Africa)

# Boxplots:

pdf("HIVBySahara.pdf",7,5)
with(Africa, boxplot(adrate~subsaharan,ylab="Percent",
             main="HIV Prevalence by Region"))
dev.off()

pdf("HIVByInternalWar.pdf",7,5)
with(Africa, boxplot(adrate~internalwar,ylab="Percent",
             main="HIV Prevalence by Internal War"))
dev.off()

# Density plots:

pdf("HIVByRegionDensity.pdf",7,5)
with(Africa[Africa$subsaharan=="Not Sub-Saharan",], 
     plot(density(adrate),lwd=2,col="black",
     main="HIV Prevalence",xlim=c(0,50),xlab="Percent"))
with(Africa[Africa$subsaharan=="Sub-Saharan",], 
     lines(density(adrate),lwd=2,col="red",lty=2))
dev.off()

pdf("HIVByInternalDensity.pdf",7,5)
with(Africa[Africa$internalwar==1,], 
     plot(density(adrate),lwd=2,col="red",lty=2,
     main="HIV Prevalence",xlim=c(0,50),xlab="Percent"))
with(Africa[Africa$internalwar==0,], 
     lines(density(adrate),lwd=2,col="black"))
dev.off()

# Scatterplots:

pdf("HIVLiteracyScatter.pdf",7,5)
with(Africa, plot(literacy,adrate,pch=19,
     xlab="Literacy Rate",ylab="HIV Prevalence",
     main="Literacy and HIV Prevalence"))
dev.off()

pdf("HIVMuslimScatter.pdf",7,5)
with(Africa, plot(muslperc,adrate,pch=19,
     xlab="Percent Muslim",ylab="HIV Prevalence",
     main="Muslim Population and HIV Prevalence"))
dev.off()

pdf("HIVPOLITYScatter.pdf",7,5)
with(Africa, plot(polity,adrate,pch=19,
     xlab="POLITY Score",ylab="HIV Prevalence",
     main="POLITY and HIV Prevalence"))
dev.off()

# Crosstabs:

with(Africa, xtabs(~internalwar+subsaharan))
with(Africa, prop.table(xtabs(~internalwar+subsaharan)))
with(Africa, prop.table(xtabs(~internalwar+subsaharan),1)) # <- "row marginals"
with(Africa, prop.table(xtabs(~internalwar+subsaharan),2)) # <- "column marginals"

# Differences of Means:

with(Africa, t.test(adrate~subsaharan))
with(Africa, t.test(internalwar~subsaharan))

# Correlation:

with(Africa, cor(adrate,literacy))
with(Africa, cor(adrate,muslperc))
with(Africa, cor(adrate,popden))

# Bivariate regression:

fit<-with(Africa, lm(adrate~literacy))
summary(fit)

pdf("HIVLiteracyOLS.pdf",7,5)
with(Africa, plot(literacy,adrate,pch=19,
     xlab="Literacy Rate",ylab="HIV Prevalence",
     main="Literacy and HIV Prevalence"))
abline(fit,lwd=3,col="red")
dev.off()
