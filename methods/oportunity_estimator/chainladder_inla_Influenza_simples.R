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

# Reading dengue data
#load("~/Dropbox/Research/Surveillance/Dengue/Data/denguesinan.RData") 
d <- read.csv("../data_filter/clean_data_filtro_sintomas.csv")

# Claudia's data manipulation
require(chron)
d$ano <- years(as.Date(d$DT_DIGITA))
d <- subset(d, ano >= 2013)
d <- subset(d, SG_UF_NOT==41) # parana
d$DT_NOTIFIC <- d$DT_SIN_PRI

# Calculating delay time
d$DelayDays <- difftime(as.Date(as.character(d$DT_DIGITA),format="%Y-%m-%d"), 
                        as.Date(as.character(d$DT_NOTIFIC),format="%Y-%m-%d"),
                        units = "days")

# Notification period
summary(d$DT_NOTIFIC)
summary(d$DT_DIGITA)

# Total of dengue notification 
length(d$DelayDays)


# Number of notifications greater than 6 months (>= 182 days)
sum(d$DelayDays > 183, na.rm = T) 

d <- na.exclude(d[d$DelayDays < 183, ])

sum(d$DelayDays > 183, na.rm = T) 

# Delay in weeks
d$DelayWeeks <- floor( d$DelayDays / 7 )

plot(table(d$DelayWeeks), xlab = "Delay in weeks", ylab = "Notifications")


aux <- tapply(d$DelayWeeks >= 0 , INDEX = d$SEM_NOT, FUN = sum, na.rm = T)
delay.tbl <- data.frame(Notifications = aux[order(rownames(aux))])
#View(delay.tbl)

head(delay.tbl)

for(k in 0:26){  
  aux <- tapply(d$DelayWeeks == k, INDEX = d$SEM_NOT, FUN = sum, na.rm = T)
  delay.tbl[paste("d",k, sep="")] <- aux[order(rownames(aux))]
}

tail(delay.tbl)



# Barplot

# # Time series
plot(delay.tbl$Notifications , type = "l", axes=F, xlab="Time", ylab="")
axis(2)
axis(1)#, at = seq(0,261-52,length.out = 5) ,labels = 2010:2014, )

# View(delay.tbl)




# # Time series
# plot(delay.tbl$Notifications, type = "l", axes=F, xlab="Time", ylab="")
# axis(2)
# axis(1, at = c(0, seq(25,261-25,length.out = 5), 261) ,labels = c(NA,2010:2014,NA) )


delay.week <- paste("d",0:26, sep="")
#cores <- heat.colors(n = 27, alpha = 0.8)

# # Criando o grafico bonito
# cores <- rainbow(n = 26, alpha = 0.8)
# barplot.fig <- barplot(t(as.matrix(delay.tbl[delay.week] )), beside = F, col=cores, axisnames = F,
#                        xlab  =  "Epi week", ylab = "Reports")
# #axis(1, at = c(0, 53, 105, 157, 209, 261) , labels = c(2010:2014,NA) )
# axis(1, at = barplot.fig[c(1, 53, 105, 157, 209, 261)] , labels = c(2010:2014,NA) )
#legend.col(col = cores, lev = 0:25)

# plot(delay.tbl[delay.week[1]], ylim=range(delay.tbl[delay.week]), type = "l", axes=F, xlab="Time", ylab="")
# axis(2)
# axis(1, at = c(0, seq(25,261-25,length.out = 5), 261) ,labels = c(NA,2010:2014,NA) )
# for(i in 1:28)
#   lines(delay.tbl[delay.week[i]], col=cores[i], lwd=2)
#  
# legend.col(col = cores, lev = 0:25)






##################################################################
# Preparing the data to be modelled
##################################################################

delay.data <- delay.tbl[delay.week]

# tempo máximo do banco
Tmax <- nrow(delay.data)

# Número máximo de semanas com delay
#Dmax <- ncol(delay.data.obs)-1
Dmax <- 10

#View(delay.tbl)

# Semana escolhida arbitrariamente
Tactual <- 80

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
#View(delay.inla.trian)
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

