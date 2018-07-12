###### Old code. Can clean once df is complete ######
mylist <- list(pres1, datetime)
pres1 <- do.call(rbind.fill, mylist)

mylist <- list(presL, datetime)
presL <- do.call(rbind.fill, mylist)

mylist <- list(presH, datetime)
presH <- do.call(rbind.fill, mylist)

mylist <- list(pres5, datetime)
pres5 <- do.call(rbind.fill, mylist)

mylist <- list(pres6, datetime)
pres6 <- do.call(rbind.fill, mylist)

mylist <- list(pres8, datetime)
pres8 <- do.call(rbind.fill, mylist)

mylist <- list(pres9, datetime)
pres9 <- do.call(rbind.fill, mylist)

mylist <- list(pres11D, datetime)
pres11D <- do.call(rbind.fill, mylist)

mylist <- list(pres11U, datetime)
pres11U <- do.call(rbind.fill, mylist)

mylist <- list(pres13, datetime)
pres13 <- do.call(rbind.fill, mylist)

mylist <- list(pres14, datetime)
pres14 <- do.call(rbind.fill, mylist)

mylist <- list(pres17, datetime)
pres17 <- do.call(rbind.fill, mylist)

mylist <- list(presHver, datetime)
presHver <- do.call(rbind.fill, mylist)

#Make hourly means


presLhr_d <- data.frame(presL$Pd, presL$Depthm, presL$TempC)
presHhr_d <- data.frame(presH$Pd, presH$Depthm, presH$TempC)
pres1hr_d <- data.frame(pres1$Pd, pres1$Depthm, pres1$TempC)
pres5hr_d <- data.frame(pres5$Pd, pres5$Depthm, pres5$TempC)
pres6hr_d <- data.frame(pres6$Pd, pres6$Depthm, pres6$TempC)
pres8hr_d <- data.frame(pres8$Pd, pres8$Depthm, pres8$TempC)
pres9hr_d <- data.frame(pres9$Pd, pres9$Depthm, pres9$TempC)
pres11Dhr_d <- data.frame(pres11D$Pd, pres11D$Depthm, pres11D$TempC)
pres11Uhr_d <- data.frame(pres11U$Pd, pres11U$Depthm, pres11U$TempC)
pres13hr_d <- data.frame(pres13$Pd, pres13$Depthm, pres13$TempC)
pres14hr_d <- data.frame(pres14$Pd, pres14$Depthm, pres14$TempC)
pres17hr_d <- data.frame(pres17$Pd, pres17$Depthm, pres17$TempC)
presHverhr_d <- data.frame(presHver$Pd, presHver$Depthm, presHver$TempC)


names(presLhr_d) <- c("time", "st7L_depthm", "st7L_tempC")
names(presHhr_d) <- c("time", "st7H_depthm", "st7H_tempC")
names(pres1hr_d) <- c("time", "st1_depthm", "st1_tempC")
names(pres5hr_d) <- c("time", "st5_depthm", "st5_tempC")
names(pres6hr_d) <- c("time", "st6_depthm", "st6_tempC")
names(pres8hr_d) <- c("time", "st8_depthm", "st8_tempC")
names(pres9hr_d) <- c("time", "st9_depthm", "st9_tempC")
names(pres11Dhr_d) <- c("time", "st11D_depthm", "st11D_tempC")
names(pres11Uhr_d) <- c("time", "st11U_depthm", "st11U_tempC")
names(pres13hr_d) <- c("time", "st13_depthm", "st13_tempC")
names(pres14hr_d) <- c("time", "st14_depthm", "st14_tempC")
names(pres17hr_d) <- c("time", "st17_depthm", "st17_tempC")
names(presHverhr_d) <- c("time", "Hver_depthm", "Hver_tempC")

##First merge all the depth data by time

presLhr <- aggregate(presLhr_d[c("st7L_depthm", "st7L_tempC")],
                     list(hour = cut(presLhr_d$time, breaks = "hour")),
                     mean, na.rm = TRUE)

presHhr <- aggregate(presHhr_d[c("st7H_depthm", "st7H_tempC")],
                     list(hour = cut(presHhr_d$time, breaks = "hour")),
                     mean, na.rm = TRUE)

pres1hr <- aggregate(pres1hr_d[c("st1_depthm", "st1_tempC")],
                     list(hour = cut(pres1hr_d$time, breaks = "hour")),
                     mean, na.rm = TRUE)

pres5hr <- aggregate(pres5hr_d[c("st5_depthm", "st5_tempC")],
                     list(hour = cut(pres5hr_d$time, breaks = "hour")),
                     mean, na.rm = TRUE)

pres6hr <- aggregate(pres6hr_d[c("st6_depthm", "st6_tempC")],
                     list(hour = cut(pres6hr_d$time, breaks = "hour")),
                     mean, na.rm = TRUE)

pres8hr <- aggregate(pres8hr_d[c("st8_depthm", "st8_tempC")],
                     list(hour = cut(pres8hr_d$time, breaks = "hour")),
                     mean, na.rm = TRUE)

pres9hr <- aggregate(pres9hr_d[c("st9_depthm", "st9_tempC")],
                     list(hour = cut(pres9hr_d$time, breaks = "hour")),
                     mean, na.rm = TRUE)

pres11Dhr <- aggregate(pres11Dhr_d[c("st11D_depthm", "st11D_tempC")],
                       list(hour = cut(pres11Dhr_d$time, breaks = "hour")),
                       mean, na.rm = TRUE)

pres11Uhr <- aggregate(pres11Uhr_d[c("st11U_depthm", "st11U_tempC")],
                       list(hour = cut(pres11Uhr_d$time, breaks = "hour")),
                       mean, na.rm = TRUE)

pres13hr <- aggregate(pres13hr_d[c("st13_depthm", "st13_tempC")],
                      list(hour = cut(pres13hr_d$time, breaks = "hour")),
                      mean, na.rm = TRUE)

pres14hr <- aggregate(pres14hr_d[c("st14_depthm", "st14_tempC")],
                      list(hour = cut(pres14hr_d$time, breaks = "hour")),
                      mean, na.rm = TRUE)

pres17hr <- aggregate(pres17hr_d[c("st17_depthm", "st17_tempC")],
                      list(hour = cut(pres17hr_d$time, breaks = "hour")),
                      mean, na.rm = TRUE)

presHverhr <- aggregate(presHverhr_d[c("Hver_depthm", "Hver_tempC")],
                        list(hour = cut(presHverhr_d$time, breaks = "hour")),
                        mean, na.rm = TRUE)
#convert to POSIXct
presLhr$time <- as.POSIXct(presLhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
presHhr$time <- as.POSIXct(presHhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
pres1hr$time <- as.POSIXct(pres1hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
pres5hr$time <- as.POSIXct(pres5hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
pres6hr$time <- as.POSIXct(pres6hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
pres8hr$time <- as.POSIXct(pres8hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
pres9hr$time <- as.POSIXct(pres9hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
pres11Dhr$time <- as.POSIXct(pres11Dhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
pres11Uhr$time <- as.POSIXct(pres11Uhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
pres13hr$time <- as.POSIXct(pres13hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
pres14hr$time <- as.POSIXct(pres14hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
pres17hr$time <- as.POSIXct(pres17hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
presHverhr$time <- as.POSIXct(presHverhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")

#

# RUNNING ONE TIME, THEN EXPORTING TO GET PT TEMPS
# #get temp at times of slugs
# #create zoo objects

temp_L <- with(presLhr, zoo(st7L_tempC, time))
temp_H <- with(presHhr, zoo(st7H_tempC, time))
temp_1 <- with(pres1hr, zoo(st1_tempC, time))
temp_5 <- with(pres5hr, zoo(st5_tempC, time))
temp_6 <- with(pres6hr, zoo(st6_tempC, time))
temp_8 <- with(pres8hr, zoo(st8_tempC, time))
temp_9 <- with(pres9hr, zoo(st9_tempC, time))
temp_11D <- with(pres11Dhr, zoo(st11D_tempC, time))
temp_11U <- with(pres11Uhr, zoo(st11U_tempC, time))
temp_13 <- with(pres13hr, zoo(st13_tempC, time))
temp_14 <- with(pres14hr, zoo(st14_tempC, time))
temp_17 <- with(pres17hr, zoo(st17_tempC, time))
temp_Hver <- with(presHverhr, zoo(Hver_tempC, time))
lightmod <- with(lightmodhr_d, zoo(cum.light, time))


d1 <- with(pres1hr, zoo(st1_depthm, time))
dL <- with(presLhr, zoo(st7L_depthm, time))
dH <- with(presHhr, zoo(st7H_depthm, time))
d5 <- with(pres5hr, zoo(st5_depthm, time))
d6 <- with(pres6hr, zoo(st6_depthm, time))
d8 <- with(pres8hr, zoo(st8_depthm, time))
d9 <- with(pres9hr, zoo(st9_depthm, time))
d11D <- with(pres11Dhr, zoo(st11D_depthm, time))
d11U <- with(pres11Uhr, zoo(st11U_depthm, time))
d13 <- with(pres13hr, zoo(st13_depthm, time))
d14 <- with(pres14hr, zoo(st14_depthm, time))
d17 <- with(pres17hr, zoo(st17_depthm, time))
dHver <- with(presHverhr, zoo(Hver_depthm, time))

depths <- merge(presLhr[,c(2,4)], presHhr[,c(2,4)], by = "time", all = TRUE)
depths <- merge(depths, pres1hr[,c(2,4)], by = "time", all = T)
names(depths) <- c("time","dL", "dH", "d1")
depths <- merge(depths, pres5hr[,c(2,4)], by = "time", all = T)
depths <- merge(depths, pres6hr[,c(2,4)], by = "time", all = T)
names(depths) <- c("time","dL", "dH", "d1", "d5", "d6")
depths <- merge(depths, pres8hr[,c(2:4)], by = "time", all = T)
depths <- merge(depths, pres9hr[,c(2,4)], by = "time", all = T)
names(depths) <- c("time","dL", "dH", "d1", "d5", "d6", "d8", "temp_8", "d9")
depths <- merge(depths, pres11Dhr[,c(2,4)], by = "time", all = T)
depths <- merge(depths, pres11Uhr[,c(2,4)], by = "time", all = T)
names(depths) <- c("time","dL", "dH", "d1", "d5", "d6", "d8", "temp_8", "d9", "d11D", "d11U")
depths <- merge(depths, pres13hr[,c(2,4)], by = "time", all = T)
depths <- merge(depths, pres14hr[,c(2,4)], by = "time", all = T)
names(depths) <- c("time","dL", "dH", "d1", "d5", "d6", "d8", "temp_8", "d9", "d11D", "d11U", 
                   "d13", "d14")
depths <- merge(depths, pres17hr[,c(2,4)], by = "time", all = T)
depths <- merge(depths, presHverhr[,c(2:4)], by = "time", all = T)
names(depths) <- c("time","dL", "dH", "d1", "d5", "d6", "d8", "temp_8", "d9", "d11D", "d11U", 
                   "d13", "d14", "d17", "dHver", "temp_Hver")
head(depths)