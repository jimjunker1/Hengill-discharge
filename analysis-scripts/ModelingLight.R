library(ggplot2)
library(scales)
library(chron)
library(mondate)


# SET SYSTEM TIME
	Sys.setenv(TZ = "UTC")

# Set working directory

# CODE FOR MODELING LIGHT

# From Bob HAll
# Here it is.?Enter date time as chron object, vector is fine (e.g. I do a year at a time).
# It is local time.  Lat and long are self explanatory.?Longstd is the standard longitude for the measurement.
#?So if mountain time, which is 7 time zones west of GMT, our standard longitude is 7*15 (each time zone is 15 deg)=105. 
# The sneak comes with daylight time, and I deal with that by adjusting the longstd to 90.
#?MDT is one hour ahead of MST, so it is like going one h closer to GMT.

# Iceland could not be easier:?no DST and on GMT so set longstd to 0.

# Be sure to set the "year" to 1 Jan of the year you need light.?Enter as I have it. Reading the code will show you why...

# Math came from Yard et al. 2005 Ecol. Model, but is certainly not unique to that paper.

# make a vector of timestamps
start.date <- "2010-01-01"
start.time <- "00:00:00"

x <- paste(start.date,start.time)

start.time.num <- as.numeric(as.chron(x))

# +1 means one month.  Use +12 if you want one year.
end.time.num <- as.numeric(as.chron(paste(mondate(x)+ 84, start.time)))

# 1/24 means one hour.  Change as needed.
mins <- as.chron(seq(start.time.num, end.time.num, 1/1440))

#convert degrees to radians
radi <- function(degrees){(degrees*pi/180)}

#light estimation function
lightest <- function (time, lat, longobs, longstd, year ) {

jday <- as.numeric(trunc(time)-as.numeric(as.Date(year)))


E <- 9.87*sin(radi((720*(jday-81))/365)) - 7.53*cos(radi((360*(jday-81))/365)) - 1.5*sin(radi((360*(jday-81))/365))

LST <- as.numeric(time-trunc(time))

ST <- LST+(3.989/1440)*(longstd-longobs)+E/1440

solardel <- 24.439*sin(radi(360*((283+jday)/365)))

hourangle <- (0.5-ST)*360

theta <- acos( sin(radi(solardel)) * sin(radi(lat)) + cos(radi(solardel)) * cos(radi(lat)) * cos(radi(hourangle)) )

suncos <- ifelse(cos(theta)<0, 0, cos(theta))

GI <- suncos*2326
GI 

}

light = lightest(time=mins, lat=64.05, longobs=-21.3, longstd= 0, year="2010-01-01")

lightdf = data.frame(mins, light)
lightdf$Pdt = as.POSIXct(lightdf$mins, format = "%m/%d/%y %H:%M:%S")

# works well
#ggplot(lightdf, aes(y = light, x = Pdt))+
#	geom_line(size = 1.25, color = "green") +
#  theme(axis.text.x = element_blank())#+
	#scale_x_datetime(labels = date_format("%d"), breaks = "5 days") 
	
lightesthr <- aggregate(lightdf["light"], list(hour = cut(lightdf$Pdt, breaks = "hour")),
                       mean, na.rm= TRUE)
lightesthr$hour = as.POSIXct(lightesthr$hour, format = "%Y-%m-%d %H:%M:%S")

#ggplot(lightesthr, aes(y = light, x = hour))+
#  geom_point(size = 1.25)

# Exporting file
write.csv(lightesthr,"./output-files/light-est_full.csv", row.names = F)

#save.image(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Lightdf.RData")

