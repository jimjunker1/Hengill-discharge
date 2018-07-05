
library(discharge) # load discharge package

sanpedro=read.csv(file.choose(),header=T) # load dataset with two columns, date and discharge. In excel use custom date format to get YYYY-MM-DD

sp.flows<-asStreamflow(sanpedro,start.date="1936-01-01", end.date="2014-12-31",river.name="San Pedro River") # convert data into a streamflow object

sp.fourier<-fourierAnalysis(sp.flows, stationary=T) # fast Fourier analysis to quantify seasonal variation


sp.lows<-sigmaLowFlows(sp.flows) # identifying seasonal low flows
sp.highs<-sigmaHighFlows(sp.flows)

write.csv(sp.lows$onesigma.events,file = "San Pedro Low Flow Extremes.csv")
write.csv(sp.highs$onesigma.events,file = "San Pedro High Flow Extremes.csv")

sp.extremes<-annualExtremes(sp.flows)


### graphs 

# hydrograph style plot
plot(sp.fourier, plot.type="hydrograph") # plot of seasonal signal (line) and observed discharges (dots)

# extreme events by ordinal day (one sigma events)

max<-max(range(sp.lows$onesigma.events$resid.sig)[2],
         range(sp.highs$onesigma.events$resid.sig)[2])

plot(sp.lows$onesigma.events$jday, abs(sp.lows$onesigma.events$resid.sig), 
     pch=16, cex=1,
     xlab="Ordinal day", ylab="Residual event magnitude",
     ylim=c(0, 1.15*max),
     xlim=c(0, 365))
abline(h=2*sp.lows$sigma.lfb, lty=2, col="black", lwd=2.5) # line for 2*sigma threshold ( 2 sd) for low flows

points(sp.highs$onesigma.events$jday, sp.highs$onesigma.events$resid.sig, 
       pch=1)
abline(h=2*sp.highs$sigma.hfb, lty=3, col="grey", lwd=2.5) # line for 2*sigma threshold ( 2 sd) for high flows

# seasonal extreme events by ordinal day (two sigma events only)

max<-max(range(sp.lows$twosigma.events$resid.sig)[2],
         range(sp.highs$twosigma.events$resid.sig)[2])

plot(sp.lows$twosigma.events$jday, abs(sp.lows$twosigma.events$resid.sig), 
     pch=16, cex=1,
     xlab="Ordinal day", ylab="Residual event magnitude",
     ylim=c(0, 1.15*max),
     xlim=c(0, 365))
abline(h=2*sp.lows$sigma.lfb, lty=2, col="black", lwd=2.5) # line for 2*sigma threshold ( 2 sd) for low flows

points(sp.highs$twosigma.events$jday, sp.highs$twosigma.events$resid.sig, 
       pch=1)
abline(h=2*sp.highs$sigma.hfb, lty=3, col="grey", lwd=2.5) # line for 2*sigma threshold ( 2 sd) for high flows

# seasonal extreme events by year (one sigma events only)

plot(sp.lows$onesigma.events$year, abs(sp.lows$onesigma.events$resid.sig), 
     pch=16,
     xlab="Year", ylab="Residual event magnitude", 
     ylim=c(0, 1.15*max))
abline(h=2*sp.lows$sigma.lfb, lty=2, col="black", lwd=2.5)

points(sp.highs$onesigma.events$year, abs(sp.highs$onesigma.events$resid.sig), 
       pch=1, cex=1)
abline(h=2*sp.highs$sigma.hfb, lty=3, col="grey", lwd=2.5)

# seasonal extreme events by year (two sigma events only)

plot(sp.lows$twosigma.events$year, abs(sp.lows$twosigma.events$resid.sig), 
     pch=16,
     xlab="Year", ylab="Residual event magnitude", 
     ylim=c(0, 1.15*max))
abline(h=2*sp.lows$sigma.lfb, lty=2, col="black", lwd=2.5)

points(sp.highs$twosigma.events$year, abs(sp.highs$twosigma.events$resid.sig), 
       pch=1, cex=1)
abline(h=2*sp.highs$sigma.hfb, lty=3, col="grey", lwd=2.5)


#### below is code from Ruhi's 2014 GCB paper for graphs

###############################################################
##################### FIGURE 1A #####################
###############################################################
### this is an S3 class "streamflow" > PLOT SYCAMORE CREEK, DATA WITH SIGNAL 
summary(sycamore)
flows<-fourierAnalysis(sycamore)
plot(flows, plot.type="hydrograph")
summary(flows) ############ figure 2a

# here are some different ways we can use the revised annualExtremes
#absolute extremes
abs.extremes<-annualExtremes(sycamore, moving.avg=FALSE)
names(abs.extremes)

#output includes all columns in the input matrix
abs.extremes$annual.max[1:5,]
# there are so many 0 flow days that annual mins is huge matrix
dim(abs.extremes$annual.min)

#now find residual extremes
#save vector of residuals
syc.resids<-flows$signal$resid

#output will include all columns in flows$signal
resid.extremes<-annualExtremes(flows$signal, data.vec=syc.resids,moving.avg=FALSE)
resid.extremes$annual.max ### MAX ANOMALIES
resid.extremes$annual.min ### MIN ANOMALIES

# we can also make the output smaller
output.matrix<-flows$signal[,c(1,4,7:11)]
resid.extremes.small<-annualExtremes(output.matrix, data.vec=syc.resids,moving.avg=FALSE)
resid.extremes.small$annual.max

# for the next part, I call another package that estimates
# the log pearson III quantiles
#install.packages("lmom")
library("lmom")

syc.events<-defineExtremeEvents(sycamore)
#this is a list object
class(syc.events)

#get the annual extremes into a different vector
# for acf function
syc.max<-abs.extremes$annual.max$ldis.corrupt

# this function needs some work
acf.max<-acfFunc(syc.max)
acf.min<-acfFunc(syc.min)
s.lf<-sigmaLowFlows(flows$signal, resid.column=10)
names(s.lf)
s.lf$twosigma.events
s.hf<-sigmaHighFlows(flows$signal, resid.column=10)

# the sigma.lfb is the one based on slope
s.lf$sigma.lfb
s.hf$sigma.hfb

max<-max(range(s.lf$onesigma.events$resid.sig)[2],
         range(s.hf$onesigma.events$resid.sig)[2])

###############################################################
##################### FIGURE 1 B #####################
###############################################################
##
max<-max(range(s.lf$onesigma.events$resid.sig)[2],
         range(s.hf$onesigma.events$resid.sig)[2])

plot(s.lf$onesigma.events$jday, abs(s.lf$onesigma.events$resid.sig), 
     pch=16, cex=1,
     xlab="Ordinal day of event", ylab="Residual event magnitude",
     ylim=c(0, 1.15*max),
     xlim=c(0, 365))
abline(h=2*s.lf$sigma.lfb, lty=2, col="grey", lwd=2.5)

points(s.hf$onesigma.events$jday, s.hf$onesigma.events$resid.sig, 
       pch=1)
abline(h=2*s.hf$sigma.hfb, lty=3, col="grey", lwd=2.5)

###############################################################
##################### FIGURE 2 #####################
###############################################################

# plot of annual extreme residuals
# x axis is year, y axis is absolute residual

plot(resid.extremes$annual.max$year, abs(resid.extremes$annual.max$resid.sig), 
     pch=1,
     xlab="Year", ylab="Residual event magnitude", 
     ylim=c(0, 1.15*max))
abline(h=2*s.lf$sigma.lfb, lty=2, col="grey", lwd=2.5)

points(resid.extremes$annual.min$year, abs(resid.extremes$annual.min$resid.sig), 
       pch=16, cex=1)
abline(h=2*s.hf$sigma.hfb, lty=3, col="grey", lwd=2.5)