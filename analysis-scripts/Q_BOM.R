#Script to create estimates of Q, CV, Q.ct, power, etc for BOM chapter 
library(tictoc)
tic()
source("./analysis-scripts/Qsubstrate.R")
toc()#about 3 mins
source("./analysis-scripts/Q-plots.R")

overKT = function(a){(1/(8.62e-5*(15+273.15)) - (1/(8.62e-5*(a+273.15))))}

####  Isolate all Q between 2011-Aug-14 and 2012-Aug-15 ####
Q_BOM <- subset(Q_allhr, Pd >= as.POSIXct('2011-08-14') & Pd <= as.POSIXct('2012-08-15'))
temp_BOM = subset(temp_allhr, Pd >= as.POSIXct('2011-08-14') & Pd <= as.POSIXct('2012-08-15'));temp_BOM[,c(5:6)] = NULL
tforce_BOM = subset(tforce_allhr, Pd >= as.POSIXct('2011-08-14') & Pd <= as.POSIXct('2012-08-15'))
RBS_BOM = subset(RBS_allhr, Pd >= as.POSIXct('2011-08-14') & Pd <= as.POSIXct('2012-08-15'))
depthe_BOM = subset(depth_allhr, Pd >= as.POSIXct('2011-08-14') & Pd <= as.POSIXct('2012-08-15'))
ins_BOM = subset(ins_allhr, Pd >= as.POSIXct('2011-08-14') & Pd <= as.POSIXct('2012-08-15'))
vel_BOM = subset(vel_allhr, Pd >= as.POSIXct('2011-08-14') & Pd <= as.POSIXct('2012-08-15'))

dfs = list(Q_BOM, temp_BOM, tforce_BOM, RBS_BOM, depthe_BOM, ins_BOM, vel_BOM)

dfs = lapply(dfs, setNames, nm = c("Pd","st1", "st5", "st6", "st8", "st9","st11L", "st11U",
                             "st13","st14","st17","hver"))

dfs_l = lapply(dfs, function(x) melt(x, id = c("Pd")))

Q_BOM.l = data.frame(dfs_l[1])
colnames(Q_BOM.l) <- c("Pd", "Stream", "Q")
head(Q_BOM.l)
temp_BOM.l = data.frame(dfs_l[2])
colnames(temp_BOM.l) <- c("Pd", "Stream", "temp")
head(temp_BOM.l);tail(temp_BOM.l)
tforce_BOM.l = data.frame(dfs_l[3])
colnames(tforce_BOM.l) = c("Pd", "Stream", "tforce")
RBS_BOM.l = data.frame(dfs_l[4])
colnames(RBS_BOM.l) = c("Pd", "Stream", "RBS")
depthe_BOM.l = data.frame(dfs_l[5])
colnames(depthe_BOM.l) = c("Pd", "Stream", "depth")
ins_BOM.l = data.frame(dfs_l[6])
colnames(ins_BOM.l) = c("Pd", "Stream", "instability")
vel_BOM.l = data.frame(dfs_l[7])
colnames(vel_BOM.l) = c("Pd", "Stream", "velocity")
Qcrit_BOM$Stream = factor(Qcrit_BOM$Stream, levels = levels(Q_BOM.l$Stream))
st_temps$Stream = factor(st_temps$Stream, levels = levels(Q_BOM.l$Stream))
rm("dfs","dfs_l");gc()

Q_BOM.sum <- Q_BOM.l %>%
  left_join(st_temps[which(st_temps$Date == "Jul"),]) %>%
  group_by(Stream) %>%
  mutate(power = 9800*(Q/1000)*(Slope/100)) %>%
	summarize(median_Q = median(Q, na.rm = T), max.power = max(power, na.rm = T),
	          max_Q = max(Q, na.rm = T), CV = (sd(Q,na.rm = T)/mean(Q,na.rm = T)),
	          QD = (quantile(Q, probs = 0.75)-quantile(Q,probs = 0.25))/(quantile(Q, probs = 0.75)+quantile(Q, probs = 0.25)))
st13_cv.fix = which(Q_BOM.sum$Stream == "st13");Q_BOM.sum[st13_cv.fix, "CV"] = (1.6/14.6)

temp_BOM.sum = temp_BOM.l %>%
  group_by(Stream) %>%
  mutate(kt = overKT(temp)) %>%
  summarize(mean_temp = mean(temp, na.rm = T), mean_kt = mean(kt, na.rm = T))

tforce_BOM.sum = tforce_BOM.l %>%
  group_by(Stream) %>%
  summarize(median_tforce = mean(tforce, na.rm = T))

RBS_BOM.sum= Q_BOM.l %>%
  left_join(RBS_BOM.l) %>%
  left_join(Qcrit_BOM) %>%
  group_by(Stream) %>%
  summarize_at(vars(RBS), funs(RBS = RBS[which.min(abs(as.numeric(Q) - unique(Qcrit)))]))
  
#RBS_BOM.sum = RBS_BOM.l %>%
#  group_by(Stream) %>%
#  summarize(RBS = median(RBS, na.rm = T))

sed_BOM.sum = sediment %>%
  group_by(Stream) %>%
  summarize(substrate = median(Size, na.rm = T))

depth_BOM.sum = depthe_BOM.l %>%
  group_by(Stream) %>%
  summarize(depth = mean(depth, na.rm = T))

ins_BOM.sum = Q_BOM.l %>%
  left_join(ins_BOM.l) %>%
  left_join(Qcrit_BOM) %>%
  group_by(Stream) %>%
  summarize_at(vars(instability), funs(instability = instability[which.min(abs(as.numeric(Q) - unique(Qcrit)))]))

#ins_BOM.sum = ins_BOM.l %>%
#  group_by(Stream) %>%
#  summarize(instability = median(instability, na.rm = T))

vel_BOM.sum = vel_BOM.l %>%
  group_by(Stream) %>%
  summarize(velocity = mean(velocity, na.rm = T))

#debugonce(f)

tcrit_BOM.sum = Q_BOM.l %>%
  left_join(tforce_BOM.l) %>%
  left_join(Qcrit_BOM) %>%
  group_by(Stream) %>%
  summarize_at(vars(tforce), funs(tforce_bf = tforce[which.min(abs(as.numeric(Q) - unique(Qcrit)))])) %>%
  mutate(dcrit = (tforce_bf/(0.08*9.807*1650)*1000)) %>%
  left_join(sediment) %>%
  group_by(Stream) %>%
  summarize(est_movement = length(which(Size <= unique(dcrit)))/length(Size))
  #summarize(est_movement = length(which(Size <= unique(tforce_bf)*0.97))/length(Size))
  
tcrit_BOM.sum[which(tcrit_BOM.sum$Stream == 'st8'),'est_movement'] = 0.4
tcrit_BOM.sum[which(tcrit_BOM.sum$Stream == 'hver'),'est_movement'] = 0.55
#Q1 = Q_BOM.l[which(Q_BOM.l$Stream == "st1"),]  
#Q1 %>% left_join(tforce_BOM.l) %>% left_join(Qcrit_BOM) %>% filter(Q > 310)


st_temps_pt1 = Reduce(function(...) merge(..., all = T), list(Q_BOM.sum, temp_BOM.sum, 
                                                tforce_BOM.sum, RBS_BOM.sum, sed_BOM.sum,
                                                depth_BOM.sum, ins_BOM.sum, vel_BOM.sum, tcrit_BOM.sum))

st_temps_pt2 = st_temps[which(st_temps$Date == "Jul"),c(1,4,12:13,17:18)]

st_temps.j = merge(st_temps_pt1, st_temps_pt2, by = "Stream")

########  
#code for office computer
#write.csv(st_temps.j, file = "C:/Users/Jim/Documents/Projects/Manuscripts/Iceland/EcosystemStoic/ch1_BOM/stream_temps.csv", row.names = F)

#code for Windows 10 file structure 
write.csv(st_temps.j, file = "C:/Users/Junker/Documents/Projects/ch1_BOM/stream_temps.csv", row.names = F)

################## Old Code ##################

#Q_BOM.l <- merge(Q_BOM.l, st_temps.feb, by = c("Stream"))

#Q_BOM.l <- Q_BOM.l[,c(1:5,7:11)]
#colnames(Q_BOM.l) <- c("Stream", "Date", "Time", "Pd", "Discharge", "Temp", "Slope", "CV", "substrate", "temp.mean")

#Q_BOM.l <- transform(Q_BOM.l, power = 9800 * (Q/1000) * (Slope/100))

##now merging the new measure of Q.ct and max power with st_temps to use in BOM analysis

#Q_BOM.l <- merge(Q_BOM.l, Q_BOM.sum, by = c("Stream"))

#Q_BOM.jul <- subset(Q_BOM.l, Pd >= as.POSIXct('2012-02-01') & Pd <= as.POSIXct('2012-08-15'))
#Q_BOM.feb <- subset(Q_BOM.l, Pd >= as.POSIXct('2011-09-01') & Pd <= as.POSIXct('2012-02-01'))

#Q.jul <- Q_BOM.jul %>%
#  group_by(Stream) %>%
#  mutate(max.power = max(power, na.rm = T))

#Q.jul <- Q.jul %>%
#  group_by(Stream) %>%
#  mutate(Q.ct = length(which(Discharge > 7 * median)))

#Q.feb <- Q_BOM.feb %>%
#  group_by(Stream) %>%
#  mutate(max.power = max(power, na.rm = T))

#Q.feb <- Q.feb %>%
#  group_by(Stream) %>%
#  mutate(Q.ct = length(which(Discharge > 7 * median)))

#Q_jul <- ddply(Q.jul, c("Stream"), summarize, mean(Q.ct))
#Q_jul1 <- ddply(Q.jul, c("Stream"), summarize, mean(max.power))

#Q_feb <- ddply(Q.feb, c("Stream"), summarize, mean(Q.ct))
#Q_feb1 <- ddply(Q.feb, c("Stream"), summarize, mean(max.power))

#Q_jul <- merge(Q_jul, Q_jul1, by = "Stream", all = T)
#Q_feb <- merge(Q_feb, Q_feb1, by = "Stream", all = T)

#Q_jul$Month = "Jul"
#Q_feb$Month = "Feb"

#Q_sum  <- rbind(Q_jul,Q_feb)
#colnames(Q_sum) <- c("Stream", "Q.ct", "max.power", "Date")
#st_temps <- merge(st_temps, Q_sum, by = c("Stream", "Date"))