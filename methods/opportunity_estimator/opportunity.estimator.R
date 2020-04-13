#!/usr/bin/env Rscript --vanilla
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
source('./insert.na.triangle.R')
suppressPackageStartupMessages(library("tidyverse"))

# TODO: update to new data structure

## Read command line arguments
suppressPackageStartupMessages(library("argparse"))
# create parser object
parser <- ArgumentParser()
# specify our desired options
# by default ArgumentParser will add an help option
parser$add_argument("-t", "--type", type="character", default='srag',
                    help="Type of data input [srag, sragflu, obitoflu]. Default %(default)s")
parser$add_argument("-d", "--date", type="character", default=format(Sys.Date(), '%Y-%m-%d'),
                    help="Date to use as base, in format YYYY-MM-DD [default Sys.Date()]")
parser$add_argument("-g", "--graphs", type="character", default="F",
                    help="If graphs should be created [T] or not [F]. Default %(default)s")
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults,
args <- parser$parse_args()

# Normalize logical arg:
if (args$graphs %in% c('F', 'False', 'f', 'false')){
    args$graphs <- FALSE
} else {
    args$graphs <- TRUE
    require(ggplot2)
    source('theme.publication.R')
}

# Read data and filter columns
d.orig <- read.csv(paste0("../clean_data/",args$type,"_sinpri2digita_table_weekly.csv"), check.names = F,
              encoding='utf-8',
              stringsAsFactors=FALSE)

# Latest week with closed counts on DT_DIGITA is actualy the previous one
if (args$date == 'max'){
  today <- as.Date(as.character(max(d.orig$date)), format='%Y%m%d')
  print(paste0('Base date: ', today))
  today <- as.Date(today - 7,origin = '1970-01-01')
  today <- episem(today)
} else {
  today <- as.Date(args$date)
  print(paste0('Base date: ', today))
  today <- as.Date(today - 7,origin = '1970-01-01')
  today <- episem(today)
}
lyear <- as.integer(strsplit(today, 'W')[[1]][1])
today.week <- as.integer(strsplit(today, 'W')[[1]][2])
print(paste0('Database reference epiweek: ', today))


# Check if previous weeks have been processed yet:
fname <- file.path('../clean_data/', paste0(args$type, '_historical_estimated_incidence.csv'))
if (file.exists(fname)){
  historical <- read.csv(fname, stringsAsFactors=FALSE)
  histo.base_epiyearweek <- historical$base_epiyearweek %>%
    max()

  # Check if same base epiyear:
  if (histo.base_epiyearweek < today){
    histo.base_epiweek <- as.integer(strsplit(histo.base_epiyearweek, 'W')[[1]][2])
    histo.base_epiyear <- as.integer(strsplit(histo.base_epiyearweek, 'W')[[1]][1])

    # Check if base_epiweek is already the last epiweek of base_epiyear
    w.end <- as.integer(lastepiweek(histo.base_epiyear))
    if (histo.base_epiweek == w.end){
      histo.base_epiyear <- histo.base_epiyear +1
      histo.base_epiweek <- 0
    }
    
    # Fill missing epiweeks:
    for (y in seq(histo.base_epiyear+1, lyear)){
      w.start <- ifelse(y>histo.base_epiyear, 1, histo.base_epiweek+1)
      w.end <- ifelse(y<lyear, as.integer(lastepiweek(histo.base_epiyear)), today.week)
      epiweek.list <-sapply(seq.int(w.start, w.end),
                            FUN=function(x) paste0(histo.base_epiyear, 'W', sprintf('%02d', x)))
    }
    
  } else {
    print('Epiweek already processed')
    quit()
  }

} else {
  epiweek.list <- c(today)
}

# Read population profile:
d_pop <- read.csv('../data/PROJECOES_2013_POPULACAO-simples_agebracket.csv', check.names = F, encoding='utf-8',
                  stringsAsFactors = FALSE)
# Read dquantile file:
dquantile.tbl <- read.csv('../clean_data/sinpri2digita_quantiles.csv', encoding='utf-8', stringsAsFactors = FALSE)

### Read activity thresholds:
df.thresholds <- read.csv(paste0('../clean_data/', args$type, '_mem-report.csv'), check.names = F, encoding='utf-8',
                          stringsAsFactors = FALSE)
low.activity <- df.thresholds[df.thresholds$`região de baixa atividade típica` == 1,'UF']

for (today in epiweek.list){
  lyear <- as.integer(strsplit(today, 'W')[[1]][1])
  today.week <- as.integer(strsplit(today, 'W')[[1]][2])
  print(paste0('Database reference epiweek: ', today))
  
  # Discard incomplete data from the current week and older than 2 years
  d <- d.orig[d.orig$epiyearweek <= today, ]
  
  #### Discard runoff triangle (important for retroactive calculations):
  uf_list <- unique(d$UF)
  uf_list <- uf_list[!(uf_list %in% c('99','RegNI', 'RNI'))]
  delay.week <- paste("d",0:26, sep="")
  for (uf in uf_list){
    d[d$UF == uf, delay.week] <- d %>%
      filter(UF == uf) %>%
      dplyr::select(delay.week) %>%
      insert.na.triangle()
  }
  d[is.na(d)] <- 0
  d$Notifications_within_26w <- d[,delay.week] %>%
    rowSums()
  
  ### Grab target quantile from delay distribution for each UF
  dquantile <- dquantile.tbl[dquantile.tbl$dado == args$type & dquantile.tbl$epiyearweek == min(today,max(dquantile.tbl$epiyearweek)), c('UF', 'epiyearweek', 'delayweeks')]
  
  # Read weekly data:
  d_weekly <- d %>%
    select(UF, epiyear, epiweek, Notifications) %>%
    rename(SRAG=Notifications) %>%
    dplyr::filter(!(UF %in% c('99','RegNI', 'RNI')))
  
  # convert to incidence:
  d_weekly$SRAG <- 100000*apply(d_weekly[,c('UF', 'epiyear', 'SRAG')], MARGIN=1,
                                FUN = function(y) as.numeric(y[3])/d_pop$Total[d_pop$`Código`==y[1] & d_pop$Ano==y[2]])
  new.vars <- c("mean","50%","2.5%","97.5%", "25%", "75%", "5%", "95%")
  d_weekly[, new.vars] <- d_weekly$SRAG
  d_weekly$Situation <- 'stable'
  
  # Thresholds:
  thres.cols <- c('limiar pré-epidêmico','intensidade alta','intensidade muito alta')
  aux2 <- t(mapply(FUN=function(uf, inc) 
    post.thresholds(inc, lims=as.numeric(df.thresholds[df.thresholds$UF==as.character(uf),thres.cols])),
    d_weekly$UF, d_weekly$SRAG) )
  thres.prob.cols <- colnames(aux2)
  d_weekly[,thres.prob.cols] <- aux2
  
  
  if (args$graphs){
    # Check if plot folder exists
    require(scales)
    if (!dir.exists('./plots')) {
      dir.create(file.path('./plots'), showWarnings = FALSE)
    }
    # Load palette
    require(RColorBrewer)
    cores <- colorRampPalette((brewer.pal(9, 'Oranges')))(27)
    
    if (!dir.exists(file.path('./plots',args$type))) {
      dir.create(file.path('./plots',args$type), showWarnings = FALSE)
    }
  }
  
  start.epiweek <- paste0(lyear-2,'W', min(lastepiweek(lyear-2), sprintf('%02d',today.week)))
  for (uf in uf_list){
    qthreshold <- dquantile$delayweeks[dquantile$UF == as.character(uf)]
    delay.tbl.tmp <- droplevels(d[d$UF==uf,]) %>%
      dplyr::select(-dado, -Notifications) %>%
      rename(Notifications=Notifications_within_26w) %>%
      filter(epiyearweek >= start.epiweek)
    
    rownames(delay.tbl.tmp) <- delay.tbl.tmp$epiyearweek
    
    
    if (args$graphs){
      
      # Check if folder exists
      if (!dir.exists(file.path('./plots',args$type,uf))) {
        dir.create(file.path('./plots',args$type,uf), showWarnings = FALSE)
      }
      # Plot UF's delay distribution
      svg(paste0('./plots/',args$type, '/',uf,'/delay_pattern.svg'))
      histo <- delay.tbl.tmp %>%
        dplyr::select(-UF, -starts_with("epi"), -Notifications, -date) %>%
        colSums() %>%
        data.frame() %>%
        rownames_to_column() %>%
        rename(Delay = "rowname", Frequency = ".") %>%
        mutate(Delay=as.integer(gsub("d", "", Delay)), Frequency=Frequency/sum(Frequency))
      histo %>%
        ggplot(aes(x=Delay, y=Frequency)) +
        geom_col() +
        geom_vline(xintercept = qthreshold, linetype=2) +
        annotate("text", label="Dmax", x=qthreshold, y=.55*max(histo$Frequency), vjust=-0.5, angle=90) +
        scale_x_continuous(name="Delay (weeks)", breaks=seq(0,26,2))+
        scale_y_continuous(expand=c(0,0)) +
        labs(title=paste0("Delay distribution from ", min(delay.tbl.tmp$epiyearweek), " to ", today)) +
        theme_Publication()
      dev.off()
      
      # Plot UF's time series
      svg(paste0('./plots/',args$type, '/', uf,'/timeseries.svg'))
      # # Time series
      delay.tbl.tmp %>%
        dplyr::select(date, Notifications) %>%
        ggplot(aes(x=as.Date(strptime(date, format='%Y%m%d')), y=Notifications)) +
        scale_x_date(date_labels="%Y-%m-%d", date_breaks = "2 months") +
        geom_line() +
        xlab("Time (weeks)") +
        theme_Publication() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      dev.off()
      
      # Plot time series with delay profile
      svg(paste0('./plots/',args$type, '/', uf,'/delay_timeseries.svg'))
      delay.week <- paste("d",0:26, sep="")
      barplot.fig <- barplot(t(as.matrix(delay.tbl.tmp[,delay.week])), beside = F, col=cores, axisnames = F,
                             xlab  =  "Time", ylab = "Notifications", border = NA)
      lines(x=barplot.fig,y=delay.tbl.tmp$d0, type = "l")
      m1 <- as.integer(lastepiweek(lyear-2))-today.week
      m2 <- m1 + as.integer(lastepiweek(lyear-1))
      if (today.week > 26){
        x.breaks <- c(1,
                      m1+1,
                      m1+26,
                      m2+1,
                      m2+26,
                      m2+today.week+1
        )
        x.labels <- c(start.epiweek,
                      paste0(lyear-1,'W01'),
                      paste0(lyear-1,'W26'),
                      paste0(lyear,'W01'),
                      paste0(lyear,'W26'),
                      today)
      } else {
        x.breaks <- c(1,
                      26-today.week+1,
                      m1+1,
                      m1+26,
                      m2+1,
                      m2+today.week+1
        )
        x.labels <- c(start.epiweek,
                      paste0(lyear-2,'W26'),
                      paste0(lyear-1,'W01'),
                      paste0(lyear-1,'W26'),
                      paste0(lyear,'W01'),
                      today)
      }
      axis(1, at = barplot.fig[x.breaks] ,
           labels = x.labels, las=2 )
      legend(x='topright', legend = c(seq(0,25,5)), fill=cores[seq(1,26,5)], pch = '.')
      dev.off()
    }
    
    ##################################################################
    # Preparing the data to be modelled
    ##################################################################
    
    # Time index of the unknown counts (Dmax+1,...,Tactual)
    
    qthreshold <- max(4, qthreshold)
    uf.indexes <- rownames(d_weekly[d_weekly$UF==as.character(uf),])
    Tactual <- length(uf.indexes)
    index.time <- uf.indexes[(Tactual-min(8,qthreshold)+1):Tactual]
    
    if (!(uf %in% low.activity)) {
      print(uf)
      # Calculate estimates
      df.tbl.tmp.estimates <- generate.estimates(delay.tbl.tmp, Dmax=qthreshold, do.plots=args$graphs, uf=paste0(args$type,'/', uf))
      
      # Keep only up to 8 weeks:
      i.start <- max(0, qthreshold-8) + 1
      df.tbl.tmp.estimates$samples <- df.tbl.tmp.estimates$samples[i.start:qthreshold,]
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
  
  d_weekly$Tipo <- NA
  # Consolidate column order:
  d_weekly <- d_weekly[,c('UF', 'epiyear', 'epiweek', 'Tipo', 'SRAG', 'Situation',
                          new.vars, 'L0', 'L1', 'L2', 'L3')]
  d_weekly[,'Run date'] <- Sys.Date()
  con<-file(file.path('../clean_data/',paste0(args$type,'_', today, 'estimated_incidence.csv')), encoding="UTF-8")
  write.csv(d_weekly, file=con, na='', row.names = F)
  
  con<-file(file.path(paste0('../clean_data/', args$type, '_current_estimated_incidence.csv')), encoding="UTF-8")
  write.csv(d_weekly, file=con, na='', row.names = F)
  
  df.Dmax <- dquantile %>%
    rename(Dmax = delayweeks)
  df.Dmax$Execution <- Sys.Date()
  fname <- file.path('../clean_data/', paste0(args$type, '_Dmax.csv'))
  ifelse(file.exists(fname), print.col.names <- FALSE, print.col.names <- TRUE)
  write.table(df.Dmax, file=fname, sep=',', quote=F, na='', row.names = F, col.names = print.col.names,
              append=T, fileEncoding='UTF-8')
  
  fname <- file.path('../clean_data/', paste0(args$type, '_historical_estimated_incidence.csv'))
  if (file.exists(fname)){
    print.col.names <- FALSE
    file.copy(from=fname, to=paste0(fname, '.', today, '.bkp'))
  } else {
    print.col.names <- TRUE
  }
  d_weekly['base_epiyearweek'] <- today
  d_weekly['base_epiyear'] <- strsplit(today, 'W')[[1]][1]
  d_weekly['base_epiweek'] <- strsplit(today, 'W')[[1]][2]
  write.table(d_weekly[d_weekly$Situation != 'stable', ], file=fname, sep=',', quote=F, na='', row.names = F,
              col.names = print.col.names, append=T, fileEncoding='UTF-8')
}