#function for determining discharge at a certain return interval
#original code was written by Nick Bond and the hydrostats package 
#https://cran.r-project.org/web/packages/hydrostats/
#downloaded from Github repository at: 
#https://github.com/nickbond/hydrostats/blob/master/R/partial.series.R

partial.series <- function(flow.ts, ari = 2, ind.days = 7, duration = T, plot = F, volume = T, series = FALSE) {
    gauge <- deparse(substitute(flow.ts))
    
    record.year <- strftime(flow.ts[["Pd"]], format = "%Y")
    flow.ts <- data.frame(flow.ts, hydro.year = record.year)
    flow.ts.comp <- na.omit(flow.ts)
    n.hours <- tapply(flow.ts.comp[[2]], flow.ts.comp[["hydro.year"]], length)
    n.most.hours <- which(n.hours >= 1000)
    flow.ts.comp <- flow.ts.comp[which(flow.ts.comp[["hydro.year"]] %in% names(n.most.hours)), ]
    flow.ts.comp[["hydro.year"]] <- factor(flow.ts.comp[["hydro.year"]])
    record.year <- flow.ts.comp[["hydro.year"]]
    
    n.years <- length(n.most.hours)
    
    
    if (ari > n.years) {
        print("warning(time-series is shorter than ari. Please provide a smaller ari")
        if (volume == FALSE) {
            return(data.frame(ari = ari, n.years = n.years, n.events = NA, flow.threshold = NA, avg.duration = NA, max.duration = NA))
        } else {
            return(data.frame(ari = ari, n.years = n.years, n.events = NA, flow.threshold = NA, avg.duration = NA, max.duration = NA, med.spell.volume = NA))
        }
        
        
    }
    n.events <- data.frame(n.events = ceiling(n.years/ari))
    p.series <- vector("list", length = n.events$n.events)
   
    rising <- data.frame(rising = flow.ts[2:nrow(flow.ts), str_detect(names(flow.ts),"Q")] - flow.ts[1:nrow(flow.ts) - 1, str_detect(names(flow.ts),"Q")])
    falling <- data.frame(falling = flow.ts[3:nrow(flow.ts), str_detect(names(flow.ts),"Q")] - flow.ts[2:nrow(flow.ts) - 2, str_detect(names(flow.ts),"Q")])
    
    peak.search <- data.frame(flow.ts, rising = c(NA, rising[["rising"]]), falling = c(falling[["falling"]], NA, NA))
    peaks <- flow.ts[which(peak.search[["rising"]] > 0 & peak.search[["falling"]] < 0), ]
    
    peaks.ord <- peaks[order(peaks[[2]], decreasing = T), ]
    
    p.series[[1]] <- data.frame(peaks.ord[1, ])
    
    i = 1
    while (i < n.events$n.events) {
        i <- i + 1
        dif.time.test <- difftime(peaks.ord$Pd[1], peaks.ord$Pd)
        
        peaks.ord <- peaks.ord[which(abs(dif.time.test) > (ind.days * 24 * 60 * 60)), ]
        
        p.series[[i]] <- data.frame(peaks.ord[1, ])
        
        if (is.na(peaks.ord[1, str_detect(names(peaks.ord),"Q")]) == T) 
            NA
    }
    p.series <- do.call("rbind", p.series)
    #browser()
    
    p.series$event.rank <- seq(1:nrow(p.series))
    
    flow.threshold <- tail(p.series[[2]], 1)#select Q column
    
    if (plot == TRUE) {
        plot(flow.ts[["Pd"]], flow.ts[[2]], type = "l", main = gauge, xlab = "Date", ylab = "Q")
        
        points(p.series$Pd, paste('p.series$',names(p.series[str_detect(names(p.series),"Q")]),sep=""), col = "red", cex = 0.25)
        abline(h = (tail(p.series[2], 1) - 1))
    }
    
    high.flows <- ifelse(flow.ts[[2]] >= flow.threshold, 1, 0)
    high.flow.runs <- rle(high.flows)
    
    if (duration == TRUE) {
        avg.duration <- mean(high.flow.runs$lengths[which(high.flow.runs$values == 1)], na.rm = T)
        max.duration <- max(high.flow.runs$lengths[which(high.flow.runs$values == 1)], na.rm = T)
    }
    if (volume == TRUE) {
        spell.factor <- rep(seq_along(high.flow.runs$lengths), times = high.flow.runs$lengths)
        spells <- split(flow.ts[[2]], spell.factor)
        spell.volumes <- flow.ts[[2]]
        spell.volumes <- sapply(spells, sum)
        spell.volumes.below.threshold <- sapply(spells, length) * flow.threshold
        spell.volumes <- spell.volumes[which(high.flow.runs$values == 1)] - spell.volumes.below.threshold[which(high.flow.runs$values == 1)]
        
        
        if (series == TRUE) {
            return(list(p.series = p.series, ari = ari, n.years = n.years, n.events = n.events, flow.threshold = flow.threshold, avg.duration = avg.duration, max.duration = max.duration, med.spell.volume = median(spell.volumes)))
        } else {
            return(data.frame(ari = ari, n.years = n.years, n.events = n.events, flow.threshold = flow.threshold, avg.duration = avg.duration, max.duration = max.duration, med.spell.volume = median(spell.volumes)))
        }
    } else {
        if (series == TRUE) {
            return(list(p.series = p.series, ari = ari, n.years = n.years, n.events = n.events, flow.threshold = flow.threshold, avg.duration = avg.duration, max.duration = max.duration))
        } else {
            return(data.frame(ari = ari, n.years = n.years, n.events = n.events, flow.threshold = flow.threshold, avg.duration = avg.duration, max.duration = max.duration))
        }
    }
} 
