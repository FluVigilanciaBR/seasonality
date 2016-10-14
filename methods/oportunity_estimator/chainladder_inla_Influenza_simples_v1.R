# Notification delay modelling
# Leo Bastos
# 
#

# N_t - number of notified cases at time t
# Y_{t,d} - number of notified cases from time t with notification delay d
# D - maximum acceptable time delay

# N_t = Y_{t,0} + \sum_{d=1}^{D} Y_{t,d}

# Y_{0,t} is known forall t
# If T is today, Y_{t,d} is unknown for all (t,d) such that t+d > T

# Load auxiliary functions
source("../data_filter/episem.R")
source('./lastepiweek.R')
source('./generate.estimates.R')
source('./post.thresholds.R')
source('./post.sum.R')

# Set quantile target for delay distribution:
quantile.target <- .95

# Read data and filter columns
d <- droplevels(subset(read.csv("../data_filter/clean_data_epiweek.csv"),
                       select=c(SG_UF_NOT, DT_NOTIFIC_epiyearweek, DT_NOTIFIC_epiyear,
                                DT_NOTIFIC_epiweek, DT_DIGITA_epiyear, DT_DIGITA_epiweek)))

# Discard years before 2013:
d <- droplevels(subset(d, DT_NOTIFIC_epiyear >= 2013))

# Calculate opportunity between notification and upload:
d$DelayWeeks <- d$DT_DIGITA_epiweek - d$DT_NOTIFIC_epiweek +
  (d$DT_DIGITA_epiyear - d$DT_NOTIFIC_epiyear)*as.integer(sapply(d$DT_NOTIFIC_epiyear,lastepiweek))

# Discard notifications with delay greater than 6 months (> 26 weeks)
d <- na.exclude(d[d$DelayWeeks < 27, ])

# # Fill all epiweeks:
fyear <- min(d$DT_DIGITA_epiyear)
today <- episem(format(Sys.Date(), '%Y-%m-%d'))
lyear <- as.integer(strsplit(today, 'W')[[1]][1])
today.week <- as.integer(strsplit(today, 'W')[[1]][2])
years.list <- c(fyear:lyear)
df.epiweeks <- data.frame(DT_NOTIFIC_epiyearweek=character())
for (y in years.list){
  epiweeks <- c()
  lweek <- ifelse(y < lyear, as.integer(lastepiweek(y)), today.week)
  for (w in c(1:lweek)){
    epiweeks <- c(epiweeks, paste0(y,'W',sprintf('%02d', w)))
  }
  df.epiweeks <- rbind(df.epiweeks, data.frame(list(DT_NOTIFIC_epiyearweek=epiweeks)))
}
rownames(df.epiweeks) <- df.epiweeks$DT_NOTIFIC_epiyearweek

# Grab target quantile from delay distribution for each UF
delay.topquantile <- ceiling(with(d, tapply(DelayWeeks, SG_UF_NOT, FUN = function(x,...) max(8,quantile(x,...)), probs=quantile.target)))

# Check if plot folder exists
require(scales)
if (!dir.exists('./plots')) {
  dir.create(file.path('./plots'), showWarnings = FALSE)
}
# Load palette
require(RColorBrewer)
cores <- colorRampPalette((brewer.pal(9, 'Oranges')))(27)

# List of locations:
uf_list <- unique(d$SG_UF_NOT)

# Read activity thresholds:
df.thresholds <- read.csv('../mem/mem-data/clean_data4mem-mem-report-criterium-method.csv')
low.activity <- df.thresholds[is.na(df.thresholds$`se.típica.do.início.do.surto`),'UF']

for (uf in uf_list){
  if (!dir.exists(file.path('./plots',uf))) {
    dir.create(file.path('./plots',uf), showWarnings = FALSE)
  }
  
  # Plot UF's delay distribution
  qthreshold <- delay.topquantile[as.character(uf)]
  d.tmp <- droplevels(subset(d, SG_UF_NOT==uf))
  svg(paste0('./plots/',uf,'/delay_pattern.svg'))
  histo <- hist(d.tmp$DelayWeeks, breaks=c(0:27), plot=F)
  barplot.fig <- barplot(histo$density, xlab = "Delay (weeks)", ylab = "Notifications frequency",
                         xaxs='i', yaxs='i')
  abline(v=barplot.fig[qthreshold], col='gray')
  axis(1, at = barplot.fig, labels = c(1:length(barplot.fig)) )
  text(x=barplot.fig[qthreshold], y=.55*max(histo$density), 'Dmax', srt=90, pos=2)
  dev.off()
  
  # Prepare delay table
  aux <- tapply(d.tmp$DelayWeeks >= 0, INDEX = list(d.tmp$DT_NOTIFIC_epiyearweek), FUN = sum, na.rm = T)
  delay.tbl.tmp <- data.frame(Notifications = aux[order(rownames(aux))])
  
  for(k in 0:26){  
    aux <- tapply(d.tmp$DelayWeeks == k, INDEX = d.tmp$DT_NOTIFIC_epiyearweek, FUN = sum, na.rm = T)
    delay.tbl.tmp[paste("d",k, sep="")] <- aux[order(rownames(aux))]
  }
  
  delay.tbl.tmp <- merge(df.epiweeks, delay.tbl.tmp, by=0, all.x=T)
  delay.tbl.tmp[is.na(delay.tbl.tmp)] <- 0
  rownames(delay.tbl.tmp) <- delay.tbl.tmp$Row.names
  
  delay.tbl.uf <- delay.tbl.tmp[c("DT_NOTIFIC_epiyearweek", "Notifications")]
  delay.tbl.uf$UF <- uf
  
  # delay.tbl[[as.character(uf)]] <- delay.tbl.tmp

  # Plot UF's time series
  svg(paste0('./plots/',uf,'/timeseries.svg'))
  # # Time series
  fyear = min(d.tmp$DT_NOTIFIC_epiyear)
  lyear = max(d.tmp$DT_NOTIFIC_epiyear)
  plot(delay.tbl.tmp$Notifications , type = "l", axes=F, xlab="Time", ylab="")
  axis(2)
  axis(1, at = seq(0,52*(lyear-fyear+1),52) ,labels = fyear:(lyear+1))
  dev.off()

  # Plot time series with delay profile
  svg(paste0('./plots/',uf,'/delay_timeseries.svg'))
  delay.week <- paste("d",0:26, sep="")
  barplot.fig <- barplot(t(as.matrix(delay.tbl.tmp[,delay.week])), beside = F, col=cores, axisnames = F,
                         xlab  =  "Epi week", ylab = "Reports", border = NA)
  lines(x=barplot.fig,y=delay.tbl.tmp$d0, type = "l")
  axis(1, at = barplot.fig[seq(1,53*(lyear-fyear+1),52)] , labels = c(fyear:(lyear+1)) )
  #legend(x='topright', legend = c(seq(0,25,5)), fill=cores[seq(1,26,5)], pch = '.')
  dev.off()
  
  ##################################################################
  # Preparing the data to be modelled
  ##################################################################
  
  # Obtain location's thresholds
  uf.threshold <- as.numeric(df.thresholds[df.thresholds$UF == as.character(uf), c("limiar.pré.epidêmico",
                                                                        "intensidade.alta",
                                                                        "intensidade.muito.alta")])
  
  if (!(uf %in% low.activity)) {
    
    # Calculate estimates
    df.tbl.tmp.estimates <- generate.estimates(delay.tbl.tmp, Dmax=qthreshold)
    
    # Generate quantiles estimates
    aux2 <- t(apply(df.tbl.tmp.estimates$samples,1,FUN = post.sum))
    
    # Populate data frame with notified values
    delay.tbl.uf[colnames(aux2)] <- delay.tbl.uf$Notifications
    
    # For estimated region, update with obtained predictions
    delay.tbl.uf[index.time,colnames(aux2)] <- aux2
    
    # Calculate probability of falling in each activity region
    aux2 <- t(apply(delay.tbl.uf$Notifications,1,FUN = post.thresholds, lims = uf.threshold) )
    delay.tbl.uf[colnames(aux2)] <- aux2
    delay.tbl.uf[index.time,colnames(aux2)] <- t(apply(df.tbl.tmp.estimates$samples,1,FUN = post.thresholds, lims = uf.threshold ))
  
  } else {

    # Location does not have enough activity for drawing estimates
    aux2 <- t(apply(delay.tbl.uf$Notifications,1,FUN = post.thresholds, lims = uf.threshold) )
    delay.tbl.uf[colnames(aux2)] <- aux2

  }
  
  
  
}
