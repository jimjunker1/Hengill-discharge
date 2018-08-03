#Q return interval plots
####  Function for plotting ecdf with line on ggplot  ####
f <-  function(u){
  which.min(abs(as.numeric(u$vals) - 0.5))
}
ecdf.plot <- function(DATA)
{
  DATA = DATA[!is.na(DATA)]
  x = sort(unique(DATA, na.rm = T))
  vals = cumsum(tabulate(match(DATA , unique(DATA))))/length(DATA)
  df = data.frame(x, vals)
  ggplot(df, aes(x = vals, y = log10(x))) + geom_point(size = 3, shape = 19, colour = "#999999") + geom_line(lwd = 1.2) +
    labs(x = "Cumulative Frequency", y = "log10(Q (L/s))") + scale_y_continuous(limits = c(NA,NA)) +
    geom_hline(yintercept = log10(as.numeric(df$x[f(df)]))) +
    annotate("text", -Inf, Inf, label = as.character(paste("Q50 (L/s)= ",round(df$x[f(df)],1), sep = "")), hjust = 0, vjust = 1)
}

Q_st1 = ecdf.plot(pres_allhr[,'st1_Q']);Q_st1
Q_st5 = ecdf.plot(pres_allhr[,'st5_Q']);Q_st5
Q_st6 = ecdf.plot(pres_allhr[,'st6_Q']);Q_st6
Q_st8 = ecdf.plot(pres_allhr[,'st8_Q']);Q_st8
Q_st9 = ecdf.plot(pres_allhr[,'st9_Q']);Q_st9
Q_st11L = ecdf.plot(pres_allhr[,'st11L_Q']);Q_st11L
Q_st11U = ecdf.plot(pres_allhr[,'st11U_Q']);Q_st11U
Q_st13 = ecdf.plot(pres_allhr[,'st13_Q']);Q_st13
Q_st14 = ecdf.plot(pres_allhr[,'st14_Q']);Q_st14
Q_st17 = ecdf.plot(pres_allhr[,'st17_Q']);Q_st17
Q_hver = ecdf.plot(pres_allhr[,'Hver_Q']);Q_hver


stream_list = list(sed_st14, sed_st13, sed_st17, sed_st1, sed_st5, sed_st6, sed_st9,
                   sed_st11l, sed_st11u)

png(file = "./stream-data/sediment-size-dist.png", res = 150, height = 15, width = 10, units = 'in')
gridExtra::grid.arrange(sed_st14, sed_st13, sed_st17, sed_st1, sed_st5, sed_st6, sed_st9,
                        sed_st11l, sed_st11u, ncol = 2)
dev.off()
#pull in file from Nick Bond on Github to determine recurrence interval
#library(gh)
#gh('/repos/nickbond/hydrostats/contents/R/partial.series.R',.send_headers = c(Accept = "application/vnd.github.v3.raw"),.destfile = "./analysis-scripts/parial.series.R")

##trying to get recurrence intervals to estimate bankfull Q = 1.5 y return interval
source("./analysis-scripts/partial.series.R")
Q1 = pres_allhr[,c("Pd","st1_Q")]
partial.series(flow.ts = Q1, ari = 1, plot = F)

Q5 = pres_allhr[,c("Pd","st5_Q")]
debugonce(partial.series)
partial.series(flow.ts = Q5, ari = 1, plot = F)





