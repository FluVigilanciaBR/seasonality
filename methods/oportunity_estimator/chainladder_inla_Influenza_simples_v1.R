#!/usr/bin/env Rscript
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

# Contributtors
# Claudia T Codeço and Marcelo F C Gomes
# Load auxiliary functions
source("../data_filter/episem.R")
source('./lastepiweek.R')
source('./generate.estimates.R')
source('./post.thresholds.R')
source('./post.sum.R')

# Set quantile target for delay distribution:
quantile.target <- .95

# Read data and filter columns
d <- droplevels(subset(read.csv("../clean_data/clean_data_epiweek.csv", check.names = F),
                       select=c(SG_UF_NOT, DT_NOTIFIC_epiyearweek, DT_NOTIFIC_epiyear,
                                DT_NOTIFIC_epiweek, DT_DIGITA_epiyear, DT_DIGITA_epiweek)))

# Discard years before 2013:
d <- droplevels(subset(d, DT_NOTIFIC_epiyear >= 2013))

# Calculate opportunity between notification and upload:
d$DelayWeeks <- d$DT_DIGITA_epiweek - d$DT_NOTIFIC_epiweek +
  (d$DT_DIGITA_epiyear - d$DT_NOTIFIC_epiyear)*as.integer(sapply(d$DT_NOTIFIC_epiyear,lastepiweek))

# Discard notifications with delay greater than 6 months (> 26 weeks)
d <- na.exclude(d[d$DelayWeeks < 27, ])

# Latest week with closed counts on DT_DIGITA is actualy the previous one
today <- episem(format(Sys.Date(), '%Y-%m-%d'))
lyear <- as.integer(strsplit(today, 'W')[[1]][1])
today.week <- as.integer(strsplit(today, 'W')[[1]][2])
today.week <- ifelse(today.week > 1, today.week-1, as.integer(lastepiweek(lyear-1)))
today <- paste0(lyear,'W',today.week)
# Discar incomplete data from the current week
d <- d[d$DT_DIGITA_epiyear < lyear | (d$DT_DIGITA_epiyear==lyear & d$DT_DIGITA_epiweek<=today.week), ]

# Read population profile:
d_pop <- read.csv('../data/PROJECOES_2013_POPULACAO-simples_agebracket.csv', check.names = F)

# Create entries for regional aggregates:
d$Region <- mapply(function(x) as.character(unique(d_pop[d_pop$`Código`==as.character(x),'Região'])), d$SG_UF_NOT)
d$Country <- 'BR'

# Grab target quantile from delay distribution for each UF
delay.topquantile <- c(ceiling(with(d, tapply(DelayWeeks, SG_UF_NOT, FUN = function(x,...) max(8,quantile(x,...)),
                                            probs=quantile.target))),
                       ceiling(with(d, tapply(DelayWeeks, Region, FUN = function(x,...) max(8,quantile(x,...)),
                                              probs=quantile.target))),
                       ceiling(with(d, tapply(DelayWeeks, Country, FUN = function(x,...) max(8,quantile(x,...)),
                                              probs=quantile.target))))

# Read activity thresholds:
df.thresholds <- read.csv('../clean_data/mem-report.csv', check.names = F)
low.activity <- df.thresholds[is.na(df.thresholds$`se típica do início do surto`),'UF']

# Read weekly data:
d_weekly <- read.csv('../clean_data/clean_data_epiweek-weekly-incidence.csv', check.names = F)
d_weekly['DT_NOTIFIC_epiyearweek'] <- mapply(function(x,y) paste0(x,'W',sprintf("%02d",y)), d_weekly$epiyear,d_weekly$epiweek)
# # Fill all epiweeks:
fyear <- min(d_weekly$epiyear)
years.list <- c(fyear:lyear)
df.epiweeks <- data.frame(DT_NOTIFIC_epiyearweek=character(), UF=factor())
# List of locations:
uf_list <- unique(d_weekly$UF)
for (y in years.list){
  epiweeks <- c()
  lweek <- ifelse(y < lyear, as.integer(lastepiweek(y)), today.week)
  for (w in c(1:lweek)){
    epiweeks <- c(epiweeks, paste0(y,'W',sprintf('%02d', w)))
  }
  for (uf in uf_list){
    df.epiweeks <- rbind(df.epiweeks, data.frame(list(DT_NOTIFIC_epiyearweek=epiweeks, UF=uf)))
  }
}

d_weekly <- merge(df.epiweeks, d_weekly, by=c('DT_NOTIFIC_epiyearweek', 'UF'), all.x=T)
d_weekly[is.na(d_weekly$epiweek), 'epiweek'] <- mapply(function (x) as.integer(strsplit(as.character(x[[1]]), 'W')[[1]][2]), 
                                                       d_weekly[is.na(d_weekly$epiweek), 'DT_NOTIFIC_epiyearweek'])
d_weekly[is.na(d_weekly$epiyear), 'epiyear'] <- mapply(function (x) as.integer(strsplit(as.character(x[[1]]), 'W')[[1]][1]), 
                                                       d_weekly[is.na(d_weekly$epiyear), 'DT_NOTIFIC_epiyearweek'])

d_weekly[is.na(d_weekly)] <- 0
d_weekly$Situation <- 'stable'
d_weekly[,c("mean","50%","2.5%","97.5%")] <- d_weekly$SRAG

# Thresholds:
thres.cols <- c('limiar pré-epidêmico','intensidade alta','intensidade muito alta')
aux2 <- t(mapply(FUN=function(uf, inc) 
  post.thresholds(inc, lims=as.numeric(df.thresholds[df.thresholds$UF==as.character(uf),thres.cols])),
  d_weekly$UF, d_weekly$SRAG) )
thres.prob.cols <- colnames(aux2)
d_weekly[,thres.prob.cols] <- aux2

# Check if plot folder exists
require(scales)
if (!dir.exists('./plots')) {
  dir.create(file.path('./plots'), showWarnings = FALSE)
}
# Load palette
require(RColorBrewer)
cores <- colorRampPalette((brewer.pal(9, 'Oranges')))(27)

# Prepare filled epiweeks data frame:
# # Fill all epiweeks:
fyear <- min(d$DT_NOTIFIC_epiyear)
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

# List of locations:
uf_list <- unique(d$SG_UF_NOT)
reg_list <- unique(d$Region)
cntry_list <- unique(d$Country)

for (uf in c(uf_list, reg_list, cntry_list)){
  if (!dir.exists(file.path('./plots',uf))) {
    dir.create(file.path('./plots',uf), showWarnings = FALSE)
  }
  
  # Plot UF's delay distribution
  qthreshold <- delay.topquantile[as.character(uf)]
  if (uf %in% uf_list){
    d.tmp <- droplevels(subset(d, SG_UF_NOT==uf))
  } else if (uf %in% reg_list){
    d.tmp <- droplevels(subset(d, Region==uf))
  } else {
    d.tmp <- droplevels(subset(d, Country==uf))
  }
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
  
  # Plot UF's time series
  svg(paste0('./plots/',uf,'/timeseries.svg'))
  # # Time series
  fyear = min(d.tmp$DT_NOTIFIC_epiyear)
  lyear = max(d.tmp$DT_NOTIFIC_epiyear)
  plot(delay.tbl.tmp$Notifications , type = "l", axes=F, xlab="Time", ylab="Notifications")
  axis(2)
  axis(1, at = seq(0,52*(lyear-fyear+1),52) ,labels = fyear:(lyear+1))
  dev.off()

  # Plot time series with delay profile
  svg(paste0('./plots/',uf,'/delay_timeseries.svg'))
  delay.week <- paste("d",0:26, sep="")
  barplot.fig <- barplot(t(as.matrix(delay.tbl.tmp[,delay.week])), beside = F, col=cores, axisnames = F,
                         xlab  =  "Time", ylab = "Notifications", border = NA)
  lines(x=barplot.fig,y=delay.tbl.tmp$d0, type = "l")
  axis(1, at = barplot.fig[seq(1,53*(lyear-fyear+1),52)] , labels = c(fyear:(lyear+1)) )
  #legend(x='topright', legend = c(seq(0,25,5)), fill=cores[seq(1,26,5)], pch = '.')
  dev.off()
  
  ##################################################################
  # Preparing the data to be modelled
  ##################################################################
  
  # Time index of the unknown counts (Dmax+1,...,Tactual) 
  uf.indexes <- rownames(d_weekly[d_weekly$UF==as.character(uf),])
  Tactual <- length(uf.indexes)
  index.time <- uf.indexes[(Tactual-qthreshold+1):Tactual]
  
  if (!(uf %in% low.activity)) {
    
    # Calculate estimates
    df.tbl.tmp.estimates <- generate.estimates(delay.tbl.tmp, Dmax=qthreshold)
    
    # Generate quantiles estimates
    aux2 <- round(t(apply(df.tbl.tmp.estimates$samples,1,FUN = post.sum)))

    # Calculate corresponding incidence
    years <- d_weekly[index.time, 'epiyear']
    pop <- sapply(years, FUN=function(x) d_pop$Total[d_pop$`Código`==uf & d_pop$Ano==x])
    aux2 <- aux2*100000/pop
    
    # For estimated region, update with obtained predictions
    d_weekly[index.time, 'Situation'] <- 'estimated'
    d_weekly[index.time,colnames(aux2)] <- aux2
    
    # Calculate probability of falling in each activity region
    # Obtain location's thresholds
    uf.threshold <- as.numeric(df.thresholds[df.thresholds$UF == as.character(uf), c("limiar pré-epidêmico",
                                                                                     "intensidade alta",
                                                                                     "intensidade muito alta")])
    uf.threshold.absolute <- uf.threshold*d_pop[d_pop[,'Código']==uf & d_pop$Ano==lyear, 'Total']/100000
    d_weekly[index.time,thres.prob.cols] <- t(apply(df.tbl.tmp.estimates$samples,1,FUN = post.thresholds, lims = uf.threshold.absolute ))
  
  } else {
    d_weekly[index.time, 'Situation'] <- 'unknown'
  }
  
}

if (!dir.exists(file.path('../clean_data'))) {
  dir.create(file.path('../clean_data'), showWarnings = FALSE)
}

d_weekly[,'Run date'] <- Sys.Date()
write.csv(d_weekly, file=file.path('../clean_data/',paste0(today,'estimated_values.csv')), row.names = F)
write.csv(d_weekly, file='../clean_data/current_estimated_values.csv', row.names = F)
df.Dmax <- data.frame(list(UF=names(delay.topquantile), epiyearweek=today, Dmax=delay.topquantile, Execution=Sys.Date()))
write.csv(df.Dmax, file='../clean_data/Dmax.csv', row.names = F, append=T)