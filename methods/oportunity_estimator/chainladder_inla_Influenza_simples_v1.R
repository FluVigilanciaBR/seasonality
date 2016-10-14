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

# Load epidemiological week function
source("../data_filter/episem.R")
# Load auxiliary function
source('./lastepiweek.R')

# Set quantile target for delay distribution:
quantile.target <- .975

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
years.list <- unique(d$DT_DIGITA_epiyear)
years.list <- years.list[order(years.list)]
# df.epiweeks <- data.frame(DT_NOTIFIC_epiyearweek=character(), SG_UF_NOT=numeric())
df.epiweeks <- data.frame(DT_NOTIFIC_epiyearweek=character())
for (y in years.list){
  epiweeks <- c()
  lweek <- as.integer(lastepiweek(y))
  for (w in c(1:lweek)){
    epiweeks <- c(epiweeks, paste0(y,'W',sprintf('%02d', w)))
  }
  df.epiweeks <- rbind(df.epiweeks, data.frame(list(DT_NOTIFIC_epiyearweek=epiweeks)))
  # for (uf in uf_list){
  #   df.epiweeks <- rbind(df.epiweeks, data.frame(list(DT_NOTIFIC_epiyearweek=epiweeks, SG_UF_NOT=rep(uf, lweek))))
  # }
}

# dtest <- merge(df.epiweeks, d, all.x=T)  

# Grab target quantile from delay distribution for each UF
delay.topquantile <- ceiling(with(d, tapply(DelayWeeks, SG_UF_NOT, quantile, probs=quantile.target)))

# Plot delay distribution
require(scales)
if (!dir.exists('./plots')) {
  dir.create(file.path('./plots'), showWarnings = FALSE)
}

uf_list <- unique(d$SG_UF_NOT)
delay.tbl <- c()
delay.tbl.uf <- c()
counter <- 1
require(RColorBrewer)
cores <- colorRampPalette((brewer.pal(9, 'Oranges')))(27)
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
  text(x=barplot.fig[qthreshold], y=.55*max(histo$density), percent(quantile.target), srt=90, pos=2)
  dev.off()
  
  # Prepare delay table
  aux <- tapply(d.tmp$DelayWeeks >= 0, INDEX = list(d.tmp$DT_NOTIFIC_epiyearweek), FUN = sum, na.rm = T)
  delay.tbl.tmp <- data.frame(Notifications = aux[order(rownames(aux))])
  
  for(k in 0:26){  
    aux <- tapply(d.tmp$DelayWeeks == k, INDEX = d.tmp$DT_NOTIFIC_epiyearweek, FUN = sum, na.rm = T)
    delay.tbl.tmp[paste("d",k, sep="")] <- aux[order(rownames(aux))]
  }
  delay.tbl[[counter]] <- delay.tbl.tmp
  delay.tbl.uf <- c(delay.tbl.uf, uf)
  counter <- counter + 1

  # Plot UF's time series
  svg(paste0('./plots/',uf,'/timeseries.svg'))
  # # Time series
  fyear = min(d.tmp$DT_NOTIFIC_epiyear)
  lyear = max(d.tmp$DT_NOTIFIC_epiyear)
  plot(delay.tbl.tmp$Notifications , type = "l", axes=F, xlab="Time", ylab="")
  axis(2)
  axis(1, at = seq(0,52*(lyear-fyear),52) ,labels = fyear:(lyear))
  dev.off()

  # Plot time series with delay profile
  svg(paste0('./plots/',uf,'/delay_timeseries.svg'))
  barplot.fig <- barplot(t(as.matrix(delay.tbl.tmp[delay.week] )), beside = F, col=cores, axisnames = F,
                         xlab  =  "Epi week", ylab = "Reports", border = NA)
  lines(x=barplot.fig,y=delay.tbl.tmp$d0, type = "l")
  axis(1, at = barplot.fig[seq(1,53*(lyear-fyear+1),52)] , labels = c(fyear:(lyear+1)) )
  legend(x='topright', legend = c(seq(0,25,5)), fill=cores[seq(1,26,5)], pch = '.')
  dev.off()
  
}

delay.tbl <- setNames(delay.tbl, delay.tbl.uf)

delay.week <- paste("d",0:26, sep="")


##################################################################
# Preparing the data to be modelled
##################################################################

delay.data <- delay.tbl[delay.week]

# tempo máximo do banco
Tmax <- nrow(delay.data)

# Número máximo de semanas com delay
#Dmax <- ncol(delay.data.obs)-1
Dmax <- delay.topquantile

#View(delay.tbl)

# Semana escolhida arbitrariamente
Tactual <- dim(delay.data)[1] - 10

# Taking the 20th week of 2012 (peak of a epidemic) as a test week
#Tactual <- 124

delay.data.obs <- delay.data[1:Tactual,(0:Dmax)+1]

# Time index of the unknown counts (Dmax+1,...,Tactual) 
index.time <- (Tactual-Dmax+1):Tactual


delay.data.obs.trian <- delay.data.obs

# Creating the run-off triangle data frame
delay.data.obs.trian[outer(1:Tactual, 0:Dmax, FUN = "+") > Tactual] <- NA

# This function creates a data frame from the run-off triangle matrix to be used in INLA
make.df.trian <- function(M){
  Time <- nrow(M)
  Delay <- ncol(M)
  aux.df <- data.frame(Y = as.vector(as.matrix(M)), 
                       Time = rep(x = 1:Time, times = Delay),
                       Delay = rep(x = 0:(Delay-1), each=Time)
  )
  aux.df
}

# A <- data.frame(matrix(1:12, nrow=3))
# as.vector(as.matrix(A))
# make.df.trian(A)

# Creating a data frame for INLA
delay.inla.trian <- make.df.trian(delay.data.obs.trian)

# Find the missing values
index.missing <- which(is.na(delay.inla.trian$Y))

require(INLA)

# Equacao do modelo: intercepto + efeito_de_tempo + efeito_de_oportunidade!!!
model <- Y ~ 1 + 
  f(Time, model = "rw1", hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001)))) + 
  f(Delay, model = "rw1", hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001))))


# model.ar <- Y ~ 1 + 
#   f(Time, model = "ar1", hyper = list(
#     "prec" = list(prior = "loggamma", param = c(0.001, 0.001)),
#     "rho" = list(prior = "normal", param = c(0, 0.2)))
#    ) + 
#   f(Delay, model = "rw1", hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001))))

# ajuste, verossimilhanca binomial negativa 
output <- inla(model, family = "nbinomial", data = delay.inla.trian,
               control.predictor = list(link = 1, compute = T),
               control.compute = list( config = T, waic=TRUE, dic=TRUE),
               control.family = list( 
                 hyper = list("theta" = list(prior = "loggamma", param = c(0.1, 0.1)))
               )
)


# criterios de comparacao de modelo, só são uteis se estivermos comparando modelos!
# c(WAIC = output$waic$waic, DIC = output$dic$dic)

# Resumo dos hiperparametros
#output$summary.hyperpar

plot.inla.re = function(outputRE, x = outputRE$ID){
  plot( x, y = outputRE$mean, type = "n", ylim = range(outputRE[,c(4,6)]), ylab="", xlab="" )
  polygon(x = c(x, rev(x)),
          y = c(outputRE$'0.025quant', rev(outputRE$'0.975quant')),
          border = "black", col = "gray")
  lines(x, outputRE$mean, lty=1, lwd=2)
  lines(x = range(x), y = rep(0,2), lty=2)
}

plot.inla.re(output$summary.random$Time)
plot.inla.re(output$summary.random$Delay)


# Gerando amostras da posteriori dos parâmetros do modelo ajustado no INLA
delay.samples.list <- inla.posterior.sample(n = 250, output)



# Sampling the missing triangule from inla output in vector format from the model likelihood
aaa <- lapply(X = delay.samples.list, FUN = function(x, idx = index.missing) rnbinom(n = idx, mu = exp(x$latent[idx]), size = x$hyperpar[1])) 


# Creating a vectorized version of the triangle matrix
delay.vec.trian <- inla.matrix2vector(as.matrix(delay.data.obs.trian[index.time,]))

# Transforming back from the vector form to the matrix form
bbb <- lapply(aaa, FUN = function(xxx, data = delay.vec.trian){
  data[which(is.na(data))] <- xxx
  inla.vector2matrix(data, ncol = Dmax+1) } )


# Samples of {N_t : t=Tactual-Dmax+1,...Tactual}
ccc <- sapply(bbb, FUN = function(x) rowSums(x) )
#ccc[,250]

# Serie verdadeira consolidada
Nt.true <- rowSums(delay.data.obs[index.time,])

# Serie observada no intante Tactual
Nt.obs <- rowSums(delay.data.obs.trian[index.time,], na.rm=T)

# Nt.forecast <- rowSums(matrix(output$summary.fitted.values$mean, ncol=Dmax+1)[index.time,])

plot(index.time, Nt.true, ylim=range(Nt.true, Nt.obs), ylab="", xlab="")
points(index.time, Nt.obs, pch=3)
#lines(index.time, Nt.forecast, col=2)
lines(index.time, rowMeans(ccc), col=2)
lines(index.time, apply(ccc,1,quantile,probs = 0.025), col=2, lty=2)
lines(index.time, apply(ccc,1,quantile,probs = 0.975), col=2, lty=2)

legend("topleft", c("Observed counts", "Real counts", "Posterior prediction", "95% CI limits"), pch=c(3,1,NA,NA), lty=c(NA,NA,1,2), col=c(1,1,2,2))


Nt.true[Dmax]
Nt.obs[Dmax]

# Descritavas da amostra a posteriori
post.sum = function(x,probs = c(0.5, 0.025,0.975)) c(mean = mean(x), quantile(x,probs))

# Descritivas para as Dmax semanas de atraso
apply(ccc,1,FUN = post.sum)

# Descritivas para a ultima semana apenas
apply(ccc,1,FUN = post.sum)[,Dmax]

# Probabilidades a posteriori de pertencer a cada regiao limiar (0,25,50,75,Infty)
apply(ccc,1,FUN = function(x) c(L0=mean(x<25), L1=mean(x>=25 & x< 50), 
                                L2 = mean(x>=50 & x< 75), L3 = mean(x>=75) ) )

# para a ultima semana apenas
apply(ccc,1,FUN = function(x) c(L0=mean(x<25), L1=mean(x>=25 & x< 50), 
                                L2 = mean(x>=50 & x< 75), L3 = mean(x>=75) ) )[,Dmax]

