#Script for modeling width, depth by discharge relationships ##
##load source of Q_all script 
source("./analysis-scripts/Q_all_script.R")

##pull in data files for calculating Q-width relationships
#load data
#Q <- read.csv("./stream-data/Q_data_summary_working.csv", T)
LWAD <- read.csv("./stream-data/LWADS_summary.csv", T, stringsAsFactors = F)
LWAD = LWAD[which(LWAD$stream != "st7" & LWAD$stream != "OH2"),]
Hver.fix = which(LWAD$stream == "hver")
LWAD[Hver.fix, "stream"] = "Hver"
LWAD = data.frame(unclass(LWAD))
LWAD$Pd <- as.POSIXct(paste(LWAD$date), format = "%m/%d/%y", tz = "UTC")

Q$Pd_d = as.POSIXct(strftime(Q$Pd), format = "%Y-%m-%d", tz = "UTC")

##aggregating pres_allhr data to day so can line up with LWAD summary

pres_alld = aggregate(pres_allhr[c(2:dim(pres_allhr)[2])], 
                       list(Pd = cut(pres_allhr$Pd, breaks = "day")),
                       mean, na.rm = T)
pres_alld$Pd = as.POSIXct(pres_alld$Pd,format = "%Y-%m-%d", tz = "UTC")

## zoo objects of all streams to match with LWAD summary

st1_tempC <- with(pres_alld, zoo(st1_tempC, Pd))
st5_tempC <- with(pres_alld, zoo(st5_tempC, Pd))
st6_tempC <- with(pres_alld, zoo(st6_tempC, Pd))
st8_tempC <- with(pres_alld, zoo(st8_tempC, Pd))
st9_tempC <- with(pres_alld, zoo(st9_tempC, Pd))
st11L_tempC <- with(pres_alld, zoo(st11L_tempC, Pd))
st11U_tempC <- with(pres_alld, zoo(st11U_tempC, Pd))
st13_tempC <- with(pres_alld, zoo(st13_tempC, Pd))
st14_tempC <- with(pres_alld, zoo(st14_tempC, Pd))
st17_tempC <- with(pres_alld, zoo(st17_tempC, Pd))
Hver_tempC <- with(pres_alld, zoo(Hver_tempC, Pd))
lightmod <- with(pres_alld, zoo(cum.light, Pd))

st1_depthm <- with(pres_alld, zoo(st1_depthm, Pd))
st5_depthm <- with(pres_alld, zoo(st5_depthm, Pd))
st6_depthm <- with(pres_alld, zoo(st6_depthm, Pd))
st8_depthm <- with(pres_alld, zoo(st8_depthm, Pd))
st9_depthm <- with(pres_alld, zoo(st9_depthm, Pd))
st11L_depthm <- with(pres_alld, zoo(st11L_depthm, Pd))
st11U_depthm <- with(pres_alld, zoo(st11U_depthm, Pd))
st13_depthm <- with(pres_alld, zoo(st13_depthm, Pd))
st14_depthm <- with(pres_alld, zoo(st14_depthm, Pd))
st17_depthm <- with(pres_alld, zoo(st17_depthm, Pd))
Hver_depthm <- with(pres_alld, zoo(Hver_depthm, Pd))

st1_Q <- with(pres_alld, zoo(st1_Q, Pd))
st5_Q <- with(pres_alld, zoo(st5_Q, Pd))
st6_Q <- with(pres_alld, zoo(st6_Q, Pd))
st8_Q <- with(pres_alld, zoo(st8_Q, Pd))
st9_Q <- with(pres_alld, zoo(st9_Q, Pd))
st11L_Q <- with(pres_alld, zoo(st11L_Q, Pd))
st11U_Q <- with(pres_alld, zoo(st11U_Q, Pd))
st13_Q <- with(pres_alld, zoo(st13_Q, Pd))
st14_Q <- with(pres_alld, zoo(st14_Q, Pd))
st17_Q <- with(pres_alld, zoo(st17_Q, Pd))
Hver_Q <- with(pres_alld, zoo(Hver_Q, Pd))

st1_Q.mod <- with(Q[which(Q$Qstream == "st1"),], zoo(Q.mod, Pd_d))
st5_Q.mod <- with(Q[which(Q$Qstream == "st5"),], zoo(Q.mod, Pd_d))
st6_Q.mod <- with(Q[which(Q$Qstream == "st6"),], zoo(Q.mod, Pd_d))
st8_Q.mod <- with(Q[which(Q$Qstream == "st8"),], zoo(Q.mod, Pd_d))
st9_Q.mod <- with(Q[which(Q$Qstream == "st9"),], zoo(Q.mod, Pd_d))
st11L_Q.mod <- with(Q[which(Q$Qstream == "st11L"),], zoo(Q.mod, Pd_d))
st11U_Q.mod <- with(Q[which(Q$Qstream == "st11U"),], zoo(Q.mod, Pd_d))
st13_Q.mod <- with(Q[which(Q$Qstream == "st13"),], zoo(Q.mod, Pd_d))
st14_Q.mod <- with(Q[which(Q$Qstream == "st14"),], zoo(Q.mod, Pd_d))
st17_Q.mod <- with(Q[which(Q$Qstream == "st17"),], zoo(Q.mod, Pd_d))
Hver_Q.mod <- with(Q[which(Q$Qstream == "hver"),], zoo(Q.mod, Pd_d))

st1_tt.s <- with(Q[which(Q$Qstream == "st1"),], zoo(travel_time_secs, Pd_d))
st5_tt.s <- with(Q[which(Q$Qstream == "st5"),], zoo(travel_time_secs, Pd_d))
st6_tt.s <- with(Q[which(Q$Qstream == "st6"),], zoo(travel_time_secs, Pd_d))
st8_tt.s <- with(Q[which(Q$Qstream == "st8"),], zoo(travel_time_secs, Pd_d))
st9_tt.s <- with(Q[which(Q$Qstream == "st9"),], zoo(travel_time_secs, Pd_d))
st11L_tt.s <- with(Q[which(Q$Qstream == "st11L"),], zoo(travel_time_secs, Pd_d))
st11U_tt.s <- with(Q[which(Q$Qstream == "st11U"),], zoo(travel_time_secs, Pd_d))
st13_tt.s <- with(Q[which(Q$Qstream == "st13"),], zoo(travel_time_secs, Pd_d))
st14_tt.s <- with(Q[which(Q$Qstream == "st14"),], zoo(travel_time_secs, Pd_d))
st17_tt.s <- with(Q[which(Q$Qstream == "st17"),], zoo(travel_time_secs, Pd_d))
Hver_tt.s <- with(Q[which(Q$Qstream == "hver"),], zoo(travel_time_secs, Pd_d))

#now can add all these data to LWAD 
LWAD$temp = NA
LWAD$depth = NA
LWAD$Q = NA
LWAD$Q.mod = NA
LWAD$tt.s = NA

LWAD_full = c()

streams = unique(levels(LWAD$stream))
for(i in streams){
  LWADs = LWAD[which(LWAD$stream == i),]
  LWADs = LWADs[order(LWADs$Pd),]
  LWADz = with(LWADs, zoo(mean_width_cm, Pd))

  f <-  function(u) which.min(abs(as.numeric(index(get(paste0(i,"_tempC")))) - as.numeric(u)))
  ix <- vapply(index(LWADz), f, integer(1))
  LWADs$temp = coredata(get(paste0(i,"_tempC")))[ix]

  f1 <-  function(u) which.min(abs(as.numeric(index(get(paste0(i,"_depthm")))) - as.numeric(u)))
  ix <- vapply(index(LWADz), f1, integer(1))
  LWADs$depth = coredata(get(paste0(i,"_depthm")))[ix]
  
  f1 <-  function(u) which.min(abs(as.numeric(index(get(paste0(i,"_Q")))) - as.numeric(u)))
  ix <- vapply(index(LWADz), f1, integer(1))
  LWADs$Q = coredata(get(paste0(i,"_Q")))[ix]
  
  f1 <-  function(u) which.min(abs(as.numeric(index(get(paste0(i,"_Q.mod")))) - as.numeric(u)))
  ix <- vapply(index(LWADz), f1, integer(1))
  LWADs$Q.mod = coredata(get(paste0(i,"_Q.mod")))[ix]
  
  f1 <-  function(u) which.min(abs(as.numeric(index(get(paste0(i,"_tt.s")))) - as.numeric(u)))
  ix <- vapply(index(LWADz), f1, integer(1))
  LWADs$tt.s = coredata(get(paste0(i,"_tt.s")))[ix]
  
  LWAD_full = rbind(LWAD_full, LWADs)
}  

##convert discharge to m^3/s
LWAD_full$Qm3 <- LWAD_full$Q/1000

#change names to save typing
LWAD_full = LWAD_full[,c(12,2:6,10:11,13:18)]
colnames(LWAD_full) = c("Pd", "stream", "date", "width", "m_depth", "area", "lg_m", "lg_t",
                   "temp", "l_depth", "Q", "Q.mod", "tt.s" ,"Qm3")

## Plots of width, tt, depth, etc with Discharge
	###Width###
Q_width.plot <- ggplot(LWAD_full, aes(x = log(Q), y = width, colour = stream)) + geom_point(size = 5); Q_width.plot

#Q_width.plot + aes(colour = stream) + geom_smooth(method = "lm",se = F)
	###Travel time####
Q_tt.plot <- ggplot(LWAD_full, aes(x = log(Qm3), y = log(tt.s))) + geom_point(size = 5); Q_tt.plot

Q_tt.plot + aes(colour = stream)

Q_tt.plot + aes(colour = stream) + geom_line(aes(colour = stream))

	###depth###
Q_depth.plot <- ggplot(LWAD_full, aes(x = log(Qm3), y = m_depth)) + geom_point(aes(colour = stream),size = 5); Q_depth.plot

Q_depth.plot + geom_smooth(method = "lm", se = F)

##### Knock them out by stream

st1 <- LWAD_full[which(LWAD_full$stream == "st1"),]
st5 <- LWAD_full[which(LWAD_full$stream == "st5"),]
st6 <- LWAD_full[which(LWAD_full$stream == "st6"),]
st8 <- LWAD_full[which(LWAD_full$stream == "st8"),]
st9 <- LWAD_full[which(LWAD_full$stream == "st9"),]
st11L <- LWAD_full[which(LWAD_full$stream == "st11L"),]
st11U <- LWAD_full[which(LWAD_full$stream == "st11U"),]
st13 <- LWAD_full[which(LWAD_full$stream == "st13"),]
st14 <- LWAD_full[which(LWAD_full$stream == "st14"),]
st17 <- LWAD_full[which(LWAD_full$stream == "st17"),]
hver <- LWAD_full[which(LWAD_full$stream == "Hver"),]

############Modeling width-discharge for all streams########
st1_mod = st1[-6,]
st1.plot <- ggplot(st1_mod, aes(x = log(Qm3), y = log(width))) + geom_point(size = 5); st1.plot
st1.lm <- lm(log(width) ~ log(Qm3), data = st1_mod); summary(st1.lm)
st1.lm2 = lm(width~Q.mod, data = st1_mod);summary(st1.lm2)
#this is in meters m3/s 
#st5
st5.plot <- ggplot(st5, aes(x =log(Qm3), y = log(width))) + geom_point(size = 5) + stat_smooth(method = "lm", se = F); st5.plot
st5.lm <- lm(log(width) ~ log(Qm3), data = st5); summary(st5.lm)
##st5 log-log use regression for width

st6.plot <- ggplot(st6, aes(x = Q.mod/1000, y = width)) + geom_point(size = 2.5);st6.plot
st6.nls = nls(width ~ a*(Q.mod/1000)/(b+(Q.mod/1000)), data = st6, start = list(a = 140, b = 0.00268))
summary(st6.nls)
st6$w_est = 155.9*(st6$Q.mod/1000)/(0.00268+(st6$Q.mod/1000))##use this
plot(st6$Q.mod, st6$width)
points(st6$Q.mod, st6$w_est, col = "red")

### for st6 use saturating model unless fill in other dates from ICE2
st8.plot <- ggplot(st8, aes(x = Qm3, y = width)) + geom_point(size = 5) + stat_smooth(method = "lm", se = F); st8.plot
median(st8$width)
mean(st8$width)
## for st8 use average width for all discharge
st9.plot <- ggplot(st9, aes(x =Q.mod, y = width)) + geom_point(size = 2.5) + stat_smooth(method = "lm", se = F);st9.plot
mean(st9$width)
##for st9 use average width for all discharge until fill in other dates from ICE2

st11L.plot <- ggplot(st11L, aes(x = log(Q.mod/1000), y = log(width))) + geom_point() + stat_smooth(method = "lm", se = F); st11L.plot
st11L.lm <- lm(log(width)~log(Q.mod/1000), data= st11L); summary(st11L.lm)
##This is really shit!Don't really know what to do but use lm

st11U.plot = ggplot(st11U, aes(x = Q.mod/1000, y = width)) + 
  geom_point(size = 5) + geom_smooth(method = "lm", se = F) +
  ylim(limits = c(0,100)) + xlim(c(0,0.02));st11U.plot
st11U.nls = nls(width~a*(Q.mod/1000)/(b+(Q.mod/1000)), data = st11U, start = list(a = 110, b= .0001) )
summary(st11U.nls)
st11U$w_est = 94*(st11U$Q.mod/1000)/(0.000282+(st11U$Q.mod/1000))#use this for st11U
plot(st11U$Q.mod/1000, st11U$width)
points(st11U$Q.mod/1000, st11U$w_est, col = "red")

#for st11U use nls estimate
st13.plot <- ggplot(st13, aes(x = Q.mod, y = width)) + geom_point() + stat_smooth(method = "lm", se = F); st13.plot
mean(st13$width)
###st13 use average width for all discharge

st14.plot <- ggplot(st14, aes(x = Q.mod/1000, y = log(width))) + geom_point() + stat_smooth(method = "lm", se = F);st14.plot
st14.lm <- lm(log(width)~log(Q.mod/1000), data= st14);summary(st14.lm)

##st14 use regression for width-discharge

st17.plot <- ggplot(st17, aes(x = log(Q.mod/1000), y = log(width))) + geom_point() + stat_smooth(method = "lm",se = F);st17.plot
st17.plot <- ggplot(st17, aes(x = Q.mod/1000, y = width)) + geom_point() + stat_smooth(method = "lm",se = F);st17.plot
st17.lm <- lm(width~Q.mod, data= st17); summary(st17.lm)
st17.nls = nls(width~a+b^(Q.mod/1000), data = st17, start = list(a = 315, b = 1.2))
summary(st17.nls)
st17$w_est = 315+18220000^(st17$Q.mod/1000)#use this
plot((st17$Q.mod/1000),st17$width)
points(st17$Q.mod/1000, st17$w_est, col = "red")
##st17 use estimate from nls model for now 
hver_mod = hver[-which(hver$width >= 200),]
hver.plot <- ggplot(hver_mod, aes(x = Q.mod, y = width)) + geom_point() + stat_smooth(method = "lm", se = F); hver.plot
hver.lm <- lm(width~Q.mod, data=hver_mod);summary(hver.lm)
hver.nls = nls(width ~ a*(Q.mod/1000)/(b+(Q.mod/1000)), data = hver_mod, start = list(a = 140, b = 0.01))
summary(hver.nls)
hver_mod$w_est = 120*(hver_mod$Q.mod/1000)/(0.0018+(hver_mod$Q.mod/1000))#use this 
plot((hver_mod$Q.mod/1000),hver_mod$width, xlim = c(0,0.090), ylim = c(0,130))
points(hver_mod$Q.mod/1000, hver_mod$w_est, col = "red")
##hver nls estimations

######################Modeling travel time-discharge in all streams##########
st1tt.plot <- ggplot(st1, aes(x = log(Q.mod), y = log(tt.s))) + geom_point();st1tt.plot
st1tt.lm <- lm(log(tt.s)~log(Q.mod), data = st1);summary(st1tt.lm)
####use power regression for tt-discharge in st1
st5Q = Q[which(Q$Qstream == "st5"),]
st5Q = st5Q[-which(st5Q$travel_time_secs >= 450),]
st5Q= st5Q[-3,]
st5tt.plot <- ggplot(st5Q, aes(x = log(Q.mod/1000), y = log(travel_time_secs))) + geom_point();st5tt.plot
st5tt.lm <- lm(log(travel_time_secs)~log(Q.mod/1000), data = st5Q);summary(st5tt.lm)
##use regression tt-discharge of the data w/ outliers removed
st6Q = Q[which(Q$Qstream == "st6"),]
st6tt.plot <- ggplot(st6Q, aes(x = log(Q.mod/1000), y = log(travel_time_secs))) + geom_point() + stat_smooth(method = "lm", se = F);st6tt.plot
st6tt.lm <- lm(log(travel_time_secs)~log(Q.mod/1000), data = st6Q); summary(st6tt.lm)
###use regression for tt-Discharge
#taking out an  outlier that is waaaayy out there
st8Q = Q[which(Q$Qstream == "st8"),]
st8Q <- st8Q[-which(st8Q$Q.mod > 40 | st8Q$travel_time_secs >= 230),]

st8tt.plot <- ggplot(st8Q, aes(x = log(Q.mod/1000), y = log(travel_time_secs))) + geom_point() + stat_smooth(method = "lm", se = F);st8tt.plot
st8tt.lm <- lm(log(travel_time_secs)~log(Q.mod/1000), data= st8Q);summary(st8tt.lm)
##use regression with outliers removed for tt-discharge in st8
st9Q = Q[which(Q$Qstream == "st9"),]

st9tt.plot <- ggplot(st9Q, aes(x = log(Q.mod), y = log(travel_time_secs))) + geom_point();st9tt.plot
st9tt.lm <- lm(log(travel_time_secs)~log(Q.mod/1000), data= st9Q);summary(st9tt.lm)
##use regression of power lm for st9 tt-discharge
st11UQ = Q[which(Q$Qstream == "st11U"),]

st11Utt.plot <- ggplot(st11UQ, aes(x = log(Q.mod), y = log(travel_time_secs))) + geom_point();st11Utt.plot
st11Utt.lm = lm(log(travel_time_secs)~log(Q.mod/1000), data = st11UQ);summary(st11Utt.lm)
#use power regression for st11U

#pulling out 1 outlier
st11LQ = Q[which(Q$Qstream == "st11L"),]
st11LQ <- st11LQ[-which(st11LQ$Q.mod <= 5),]

st11Ltt.plot <- ggplot(st11LQ, aes(x = log(Q.mod), y = log(travel_time_secs))) + geom_point();st11Ltt.plot
st11Ltt.lm <- lm(log(travel_time_secs)~log(Q.mod/1000), data=st11LQ);summary(st11Ltt.lm)
##use regresssion with outlier removed
st13Q = Q[which(Q$Qstream == "st13"),]

st13Q <- st13Q[which(st13Q$Q.mod >= 12),]
st13tt.plot <- ggplot(st13Q, aes(x = log(Q.mod), y = log(travel_time_secs)))+geom_point()+stat_smooth(method = "lm", se = F);st13tt.plot

st13tt.lm <- lm(travel_time_secs~Q.mod, data = st13Q);summary(st13tt.lm)
mean(st13Q$travel_time_sec, na.rm = T)
#use regression with outlier removed for st13
st14Q = Q[which(Q$Qstream == "st14"),]

st14tt.plot <- ggplot(st14Q, aes(x = log(Q.mod), y = log(travel_time_secs))) + geom_point(); st14tt.plot
st14tt.lm <- lm(log(travel_time_secs)~log(Q.mod/1000), data=st14Q);summary(st14tt.lm)
##use power regression for tt-discharge in st14
st17Q = Q[which(Q$Qstream == "st17"),]

st17tt.plot <- ggplot(st17Q, aes(x = log(Q.mod), y = log(travel_time_secs))) + geom_point();st17tt.plot
st17tt.lm <- lm(log(travel_time_secs)~log(Q.mod/1000), data = st17Q);summary(st17tt.lm)
#use power regression for st17 tt-discharge relationships
hverQ = Q[which(Q$Qstream == "hver"),]

hvertt.plot <- ggplot(hverQ, aes(x = log(Q.mod), y = log(travel_time_secs))) + geom_point();hvertt.plot

hvertt.lm <- lm(log(travel_time_secs)~log(Q.mod/1000), data = hverQ);summary(hvertt.lm)

###take a look at the depths and discharge measures

st1d.plot <- ggplot(st1, aes(x = Q.mod, y = m_depth)) + geom_point();st1d.plot;summary(lm(log(m_depth)~log(Q.mod), data = st1))
st5d.plot <- ggplot(st5, aes(x = Q.mod, y = m_depth)) + geom_point();st5d.plot
st6d.plot <- ggplot(st6, aes(x = Q.mod, y = m_depth)) + geom_point();st6d.plot
st8d.plot <- ggplot(st8, aes(x = Q.mod, y = m_depth)) + geom_point();st8d.plot
st9d.plot <- ggplot(st9, aes(x = Q.mod, y = m_depth)) + geom_point();st9d.plot
st11Ld.plot <- ggplot(st11L, aes(x = Q.mod, y = m_depth)) + geom_point();st11Ld.plot
st13d.plot <- ggplot(st13, aes(x = Q.mod, y = m_depth)) + geom_point();st13d.plot
st14d.plot <- ggplot(st14, aes(x = log(Q.mod), y = log(m_depth))) + geom_point();st14d.plot
st17d.plot <- ggplot(st17, aes(x = Q.mod, y = m_depth)) + geom_point();st17d.plot
hverd.plot <- ggplot(hver, aes(x = Q.mod, y = m_depth)) + geom_point();hverd.plot
##These all suck!!! Not much data.

#Now take the estimated Widths and estimated travel times and add to discharge object: Q_all
##relisting all the regressions used for tt and widths
###Widths
w_rating1 <- lm(log(width) ~ log(Qm3), data = st1_mod); summary(st1.lm)
w_rating5 <- lm(log(width) ~ log(Q.mod/1000), data = st5); summary(st5.lm)
#w_rating6 = 155.9*(Q.mod/1000)/(0.002685+(Q.mod/1000))##use this
w_rating8 <- mean(st8$width, na.rm = T)
w_rating9 <- mean(st9$width, na.rm = T)
w_rating11L <- lm(log(width)~log(Q.mod/1000), data= st11L); summary(st11L.lm)
#w_rating11U = 94*(Q.mod/1000)/(0.000282+(Q.mod/1000))#use this for st11U
w_rating13 <- mean(st13$width, na.rm= T)
w_rating14 <- lm(log(width)~log(Q.mod/1000), data= st14);summary(st14.lm)
#w_rating17 = 315+18220000^(Q.mod/1000)#use this
#w_ratingHver = 120*(Q.mod/1000)/(0.0018+(Q.mod/1000))#use this 

###Travel Time
#Need to build models all in m3/s so can use length/width to get depth. 

tt_rating1 <- lm(log(tt.s)~log(Q.mod/1000), data = st1);summary(st1tt.lm)
st5Q = Q[which(Q$Qstream == "st5"),]
st5Q = st5Q[-which(st5Q$travel_time_secs >= 450),]
st5Q= st5Q[-3,]
tt_rating5 <- lm(log(travel_time_secs)~log(Q.mod/1000), data = st5Q);summary(st5tt.lm)
st6Q = Q[which(Q$Qstream == "st6"),]
tt_rating6 <- lm(log(travel_time_secs)~log(Q.mod/1000), data = st6Q); summary(st6tt.lm)
st8Q = Q[which(Q$Qstream == "st8"),]
st8Q <- st8Q[-which(st8Q$Q.mod > 40 | st8Q$travel_time_secs >= 230),]
tt_rating8<- lm(log(travel_time_secs)~log(Q.mod/1000), data= st8Q);summary(st8tt.lm)
st9Q = Q[which(Q$Qstream == "st9"),]
tt_rating9 <- lm(log(travel_time_secs)~log(Q.mod/1000), data= st9Q);summary(st9tt.lm)
st11LQ = Q[which(Q$Qstream == "st11L"),]
st11LQ <- st11LQ[-which(st11LQ$Q.mod <= 5),]
tt_rating11L <- lm(log(travel_time_secs)~log(Q.mod/1000), data=st11LQ);summary(st11Ltt.lm)
st11UQ = Q[which(Q$Qstream == "st11U"),]
tt_rating11U = lm(log(travel_time_secs)~log(Q.mod/1000), data = st11UQ);summary(st11Utt.lm)
st13Q = Q[which(Q$Qstream == "st13"),]
st13Q <- st13Q[which(st13Q$Q.mod >= 12),]
tt_rating13 <- mean(st13Q$travel_time_secs, na.rm = T)
st14Q = Q[which(Q$Qstream == "st14"),]
tt_rating14<- lm(log(travel_time_secs)~log(Q.mod/1000), data=st14Q);summary(st14tt.lm)
st17Q = Q[which(Q$Qstream == "st17"),]
tt_rating17 <- lm(log(travel_time_secs)~log(Q.mod/1000), data = st17Q);summary(st17tt.lm)
hverQ = Q[which(Q$Qstream == "hver"),]
tt_ratinghver<- lm(log(travel_time_secs)~log(Q.mod/1000), data = hverQ);summary(hvertt.lm)


###estimating width and tt.s with discharge
colnames(pres_allhr)[30] = "Qm3"
pres_allhr$Qm3 = pres_allhr$Qm3/1000
pres_allhr$st1_width <- exp(predict(w_rating1, pres_allhr));hist(pres_allhr$st1_width/100)
pres_allhr$Qm3 = pres_allhr$Qm3*1000
colnames(pres_allhr)[30]= "Q.mod"
pres_allhr$st1_tt.s <- exp(predict(tt_rating1, pres_allhr));hist(pres_allhr$st1_tt.s)
colnames(pres_allhr)[30] = "st1_Q"

colnames(pres_allhr)[31] = "Q.mod"
pres_allhr$st5_width <- exp(predict(w_rating5, pres_allhr));hist(pres_allhr$st5_width)
pres_allhr$st5_tt.s = exp(predict(tt_rating5, pres_allhr));hist(pres_allhr$st5_tt.s)
colnames(pres_allhr)[31] = "st5_Q"

colnames(pres_allhr)[32] = "Q.mod"
pres_allhr$st6_width = 155.9*(pres_allhr$Q.mod/1000)/(0.002685+(pres_allhr$Q.mod/1000));hist(pres_allhr$st6_width)
pres_allhr$st6_tt.s = exp(predict(tt_rating6, pres_allhr));hist(pres_allhr$st6_tt.s)
colnames(pres_allhr)[32] = "st6_Q"

colnames(pres_allhr)[33] = "Q.mod"
pres_allhr <- transform(pres_allhr, st8_width = mean(st8$width, na.rm = T))
pres_allhr$st8_tt.s = exp(predict(tt_rating8, pres_allhr));hist(pres_allhr$st8_tt.s)
colnames(pres_allhr)[33] = "st8_Q"

colnames(pres_allhr)[34] = "Q.mod"
pres_allhr <- transform(pres_allhr, st9_width = mean(st9$width, na.rm = T))
pres_allhr$st9_tt.s = exp(predict(tt_rating9, pres_allhr));hist(pres_allhr$st9_tt.s)
colnames(pres_allhr)[34] = "st9_Q"

colnames(pres_allhr)[35] = "Q.mod"
pres_allhr$st11L_width <- exp(predict(w_rating11L, pres_allhr));hist(pres_allhr$st11L_width)
pres_allhr$st11L_tt.s <- exp(predict(tt_rating11L, pres_allhr));hist(pres_allhr$st11L_tt.s)
colnames(pres_allhr)[35] = "st11L_Q"

colnames(pres_allhr)[36] = "Q.mod"
pres_allhr$st11U_width <- 94*(pres_allhr$Q.mod/1000)/(0.000282+(pres_allhr$Q.mod/1000));hist(pres_allhr$st11U_width)
pres_allhr$st11U_tt.s <- exp(predict(tt_rating11U, pres_allhr));hist(pres_allhr$st11U_tt.s)
colnames(pres_allhr)[36] = "st11U_Q"

colnames(pres_allhr)[37] = "Q.mod"
pres_allhr$Q.mod = 14.6
pres_allhr <- transform(pres_allhr, st13_width = mean(st13$width, na.rm = T))
pres_allhr = transform(pres_allhr, st13_tt.s = mean(st13Q$travel_time_secs, na.rm = T))
colnames(pres_allhr)[37] = "st13_Q"

colnames(pres_allhr)[38] = "Q.mod"
pres_allhr$st14_width <- exp(predict(w_rating14, pres_allhr));hist(pres_allhr$st14_width)
pres_allhr$st14_tt.s = exp(predict(tt_rating14, pres_allhr));hist(pres_allhr$st14_tt.s)
colnames(pres_allhr)[38] = "st14_Q"

colnames(pres_allhr)[39] = "Q.mod"
pres_allhr <- transform(pres_allhr, st17_width = mean(st17$width, na.rm = T))
pres_allhr$st17_tt.s = exp(predict(tt_rating17, pres_allhr));hist(pres_allhr$st17_tt.s)
colnames(pres_allhr)[39] = "st17_Q"

colnames(pres_allhr)[40] = "Q.mod"
pres_allhr$Hver_width <- 120*(pres_allhr$Q.mod/1000)/(0.0018+(pres_allhr$Q.mod/1000))
pres_allhr$Hver_tt.s = exp(predict(tt_ratinghver, pres_allhr))
colnames(pres_allhr)[40] = "Hver_Q"

##Getting the reach lengths for each read
pres_allhr <- transform(pres_allhr, st1_length = mean(st1$lg_m, na.rm = T))
pres_allhr <- transform(pres_allhr, st5_length = mean(st5$lg_m, na.rm = T))
pres_allhr <- transform(pres_allhr, st6_length = mean(st6$lg_m, na.rm = T))
pres_allhr <- transform(pres_allhr, st8_length = mean(st8$lg_m, na.rm = T))
pres_allhr <- transform(pres_allhr, st9_length = mean(st9$lg_m, na.rm = T))
pres_allhr <- transform(pres_allhr, st11L_length = mean(st11L$lg_m, na.rm = T))
pres_allhr <- transform(pres_allhr, st11U_length = mean(st11U$lg_m, na.rm = T))
pres_allhr <- transform(pres_allhr, st13_length = mean(st13$lg_m, na.rm = T))
pres_allhr <- transform(pres_allhr, st14_length = mean(st14$lg_m, na.rm = T))
pres_allhr <- transform(pres_allhr, st17_length = mean(st17$lg_m, na.rm = T))
pres_allhr <- transform(pres_allhr, Hver_length = mean(hver$lg_m, na.rm = T))

### modeling depth with discharge, travel time, width, and length 
#units = Depth(m) = (m3/s * s/1)/((cm/100) * m)
d_rating1 = lm(log(m_depth/100)~log(Q.mod), data = st1);colnames(pres_allhr)[30]= "Q.mod"
pres_allhr$st1_depthe = exp(predict(d_rating1, pres_allhr));colnames(pres_allhr)[30] = "st1_Q"
#pres_allhr$st1_depthe2 = ((pres_allhr$st1_Q/1000)*pres_allhr$st1_tt.s)/((pres_allhr$st1_width/100)*pres_allhr$st1_length)
#d_rating5 = lm(log(m_depth/100)~log(Q.mod), data = st5);colnames(pres_allhr)[31]="Q.mod"
#pres_allhr$st5_depthe = exp(predict(d_rating5, pres_allhr));colnames(pres_allhr)[31] = "st5_Q"
pres_allhr$st5_depthe = ((pres_allhr$st5_Q/1000)*pres_allhr$st5_tt.s)/((pres_allhr$st5_width/100)*pres_allhr$st5_length)
d_rating6 = lm(log(m_depth/100)~log(Q.mod), data = st6);colnames(pres_allhr)[32]="Q.mod"
pres_allhr$st6_depthe = exp(predict(d_rating6, pres_allhr));colnames(pres_allhr)[32] = "st6_Q"
#pres_allhr$st6_depthe = ((pres_allhr$st6_Q/1000)*pres_allhr$st6_tt.s)/((pres_allhr$st6_width/100)*pres_allhr$st6_length)
pres_allhr$st8_depthe = ((pres_allhr$st8_Q/1000)*pres_allhr$st8_tt.s)/((pres_allhr$st8_width/100)*pres_allhr$st8_length)
d_rating9 = lm(log(m_depth/100)~log(Q.mod), data = st9);colnames(pres_allhr)[34]="Q.mod"
pres_allhr$st9_depthe = exp(predict(d_rating9, pres_allhr));colnames(pres_allhr)[34] = "st9_Q"
#pres_allhr$st9_depthe = ((pres_allhr$st9_Q/1000)*pres_allhr$st9_tt.s)/((pres_allhr$st9_width/100)*pres_allhr$st9_length)
pres_allhr$st11L_depthe = ((pres_allhr$st11L_Q/1000)*pres_allhr$st11L_tt.s)/((pres_allhr$st11L_width/100)*pres_allhr$st11L_length)
pres_allhr$st11U_depthe = ((pres_allhr$st11U_Q/1000)*pres_allhr$st11U_tt.s)/((pres_allhr$st11U_width/100)*pres_allhr$st11U_length)
pres_allhr$st13_depthe = ((pres_allhr$st13_Q/1000)*pres_allhr$st13_tt.s)/((pres_allhr$st13_width/100)*pres_allhr$st13_length)
pres_allhr$st14_depthe = ((pres_allhr$st14_Q/1000)*pres_allhr$st14_tt.s)/((pres_allhr$st14_width/100)*pres_allhr$st14_length)
pres_allhr$st17_depthe = ((pres_allhr$st17_Q/1000)*pres_allhr$st17_tt.s)/((pres_allhr$st17_width/100)*pres_allhr$st17_length)
pres_allhr$Hver_depthe = ((pres_allhr$Hver_Q/1000)*pres_allhr$Hver_tt.s)/((pres_allhr$Hver_width/100)*pres_allhr$Hver_length)

#plotting all the estimated depths and the measured discharge
ggplot(pres_allhr, aes( x = st1_depthe*100)) + 
  geom_histogram(aes(y = ..density..), fill = "blue") +
  geom_histogram(data = st1, aes(x = m_depth, y = ..density..), fill = "red")

ggplot(pres_allhr, aes(x = st1_Q/1000, y = st1_depthe*100)) + geom_point(size = 3)
ggplot(pres_allhr, aes(x = st1_Q/1000, y = st1_width/100)) + geom_point(size = 3) +geom_vline(xintercept = median(st1_Q/1000,na.rm = T))#+ coord_cartesian(xlim = c(0,0.05), ylim = c(1,2.5))

ggplot(pres_allhr, aes( x = st5_depthe*100)) + 
  geom_histogram(aes(y = ..density..), fill = "blue") +
  geom_histogram(data = st5, aes(x = m_depth, y = ..density..), fill = "red")
ggplot(pres_allhr, aes(x = st5_Q/1000, y = st5_depthe*100)) + geom_point(size = 3)

ggplot(pres_allhr, aes( x = st6_depthe*100)) + 
  geom_histogram(aes(y = ..density..), fill = "blue") +
  geom_histogram(data = st6, aes(x = m_depth, y = ..density..), fill = "red")
ggplot(pres_allhr, aes(x = st6_Q/1000, y = st6_depthe*100)) + geom_point(size = 3)

ggplot(pres_allhr, aes( x = st8_depthe*100)) + 
  geom_histogram(aes(y = ..density..), fill = "blue") +
  geom_histogram(data = st8, aes(x = m_depth, y = ..density..), fill = "red")
ggplot(pres_allhr, aes(x = st8_Q/1000, y = st8_depthe*100)) + geom_point(size = 3)

ggplot(pres_allhr, aes( x = st9_depthe*100)) + 
  geom_histogram(aes(y = ..density..), fill = "blue") +
  geom_histogram(data = st9, aes(x = m_depth, y = ..density..), fill = "red")
ggplot(pres_allhr, aes(x = st9_Q/1000, y = st9_depthe*100)) + geom_point(size = 3)

ggplot(pres_allhr, aes( x = st11L_depthe*100)) + 
  geom_histogram(aes(y = ..density..), fill = "blue") +
  geom_histogram(data = st11L, aes(x = m_depth, y = ..density..), fill = "red")
ggplot(pres_allhr, aes(x = st11L_Q/1000, y = st11L_depthe*100)) + geom_point(size = 3)

ggplot(pres_allhr, aes( x = st11U_depthe*100)) + 
  geom_histogram(aes(y = ..density..), fill = "blue") +
  geom_histogram(data = st11U, aes(x = m_depth, y = ..density..), fill = "red") +
  xlim(c(0,10))
ggplot(pres_allhr, aes(x = st11U_Q/1000, y = st11U_depthe*100)) + geom_point(size = 3)

hist(pres_allhr$st11U_Q)
length(which(pres_allhr$st11U_Q >=100))
mean(pres_allhr$st11U_Q, na.rm = T)

ggplot(pres_allhr, aes( x = st13_depthe*100)) + 
  geom_histogram(aes(y = ..density..), fill = "blue") +
  geom_histogram(data = st13, aes(x = m_depth, y = ..density..), fill = "red")
mean(pres_allhr$st13_depthe*100, na.rm = T)
ggplot(pres_allhr, aes(x = st13_Q/1000, y = st13_depthe*100)) + geom_point(size = 3)

ggplot(pres_allhr, aes( x = st14_depthe*100)) +
  geom_histogram(aes(y = ..density..), fill = "blue") +
  geom_histogram(data = st14, aes(x = m_depth, y = ..density..), fill = "red")
ggplot(pres_allhr, aes(x = st14_Q/1000, y = st14_depthe*100)) + geom_point(size = 3)

ggplot(pres_allhr, aes( x = st17_depthe*100)) + 
  geom_histogram(aes(y = ..density..), fill = "blue") +
  geom_histogram(data = st17, aes(x = m_depth, y = ..density..), fill = "red")
ggplot(pres_allhr, aes(x = st17_Q/1000, y = st17_depthe*100)) + geom_point(size = 3)

ggplot(pres_allhr, aes( x = Hver_depthe*100)) + 
  geom_histogram(aes(y = ..density..), fill = "blue") +
  geom_histogram(data = hver, aes(x = m_depth, y = ..density..), fill = "red")
ggplot(pres_allhr, aes(x = Hver_Q/1000, y = Hver_depthe*100)) + geom_point(size = 3)

##save this file
write.csv(pres_allhr, file = "./output-files/Q-char_all_fin.csv", row.names = F)

rm(list = ls()[!ls() %in% c("pres_allhr", "Q")])

############ Old code repository ##############3
### Code to select just Q columns from pres_allhr
#Q_allhr = data.frame(Pd = pres_allhr$Pd, pres_allhr[str_detect(names(pres_allhr), "_Q")])




