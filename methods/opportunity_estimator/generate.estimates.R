generate.estimates <- function(delay.tbl.tmp.ori, Dmax, do.plots=F, uf='tmp'){
  
  # Generate etimates for the previous Dmax weeks based on notification oportunity profile
  # found in delay.tbl.tmp
  delay.week <- paste0("d",0:Dmax)
  delay.tbl.tmp <- delay.tbl.tmp.ori[delay.week]
  delay.tbl.tmp[, Dmax+1] <- delay.tbl.tmp[,Dmax+1] + delay.tbl.tmp.ori$Notifications - rowSums(delay.tbl.tmp)
  
  # tempo máximo do banco
  Tmax <- nrow(delay.tbl.tmp)
  
  # Última semana no banco
  Tactual <- dim(delay.tbl.tmp)[1]
  
  delay.tbl.tmp.obs.trian <- delay.tbl.tmp[1:Tactual,(0:Dmax)+1]
  
  # Time index of the unknown counts (Dmax+1,...,Tactual) 
  index.time <- (Tactual-Dmax+1):Tactual
  
  # Creating the run-off triangle data frame
  delay.tbl.tmp.obs.trian[outer(1:Tactual, 0:Dmax, FUN = "+") > Tactual] <- NA
  
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
  delay.inla.trian <- make.df.trian(delay.tbl.tmp.obs.trian) %>%
    mutate(TimeDelay = paste(Time, Delay))
  
  # Find the missing values
  index.missing <- which(is.na(delay.inla.trian$Y))
  
  require(INLA)
  
  # Equacao do modelo: intercepto + efeito_de_tempo + efeito_de_oportunidade!!!
  model <- Y ~ 1 + 
    f(Time, model = "rw2", hyper = list("prec" = list(prior = "loggamma", param = c(0.01, 0.01)))) +
    f(Delay, model = "rw1", hyper = list("prec" = list(prior = "loggamma", param = c(0.01, 0.01)))) #+
    # Efeito tempo-atraso
    #f(TimeDelay, model = "iid", constr = T)
  
  
  # model.ar <- Y ~ 1 + 
  #   f(Time, model = "ar1", hyper = list(
  #     "prec" = list(prior = "loggamma", param = c(0.001, 0.001)),
  #     "rho" = list(prior = "normal", param = c(0, 0.2)))
  #    ) + 
  #   f(Delay, model = "rw1", hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001))))
  
  # ajuste, verossimilhanca binomial negativa 
  hess.min <- -1
  h.value <- 0.0001
  trials <- 0
  while (hess.min <= 0 & trials < 50){
    # output <- inla(model, family = "nbinomial", data = delay.inla.trian,
    #                control.predictor = list(link = 1, compute = T),
    #                control.compute = list( config = T, waic=TRUE, dic=TRUE),
    #                control.family = list( 
    #                  hyper = list("theta" = list(prior = "loggamma", param = c(0.1, 0.1)))
    #                ),
    #                control.inla = list(h = h.value)
    # )
    output <- inla(model, family = "zeroinflatednbinomial1", data = delay.inla.trian,
                   control.predictor = list(link = 1, compute = T),
                   control.compute = list( config = T, waic=TRUE, dic=TRUE),
                   control.family = list( 
                     hyper = list("theta1" = list(prior = "loggamma", param = c(0.1, 0.1)),
                                  "theta2" = list(prior = "gaussian", param = c(0, 0.4)))
                   ),
                   control.inla = list(h = h.value)
    )
    hess.start <- which(output$logfile == 'Eigenvalues of the Hessian')
    hess.min <- min(as.numeric(output$logfile[(hess.start+1):(hess.start+3)]))
    h.value <- h.value + 0.0001
    trials <- trials + 1
  }
  print(paste('Hessian trials:',trials))
  
  # criterios de comparacao de modelo, só são uteis se estivermos comparando modelos!
  # c(WAIC = output$waic$waic, DIC = output$dic$dic)
  
  # Resumo dos hiperparametros
  #output$summary.hyperpar
  
  if (do.plots == T){
    plot.inla.re = function(outputRE, x = outputRE$ID){
      plot( x, y = outputRE$mean, type = "n", ylim = range(outputRE[,c(4,6)]), ylab="", xlab="" )
      polygon(x = c(x, rev(x)),
              y = c(outputRE$'0.025quant', rev(outputRE$'0.975quant')),
              border = "black", col = "gray")
      lines(x, outputRE$mean, lty=1, lwd=2)
      lines(x = range(x), y = rep(0,2), lty=2)
    }

    svg(paste0('./plots/',uf,'/time_effect.svg'))
    plot.inla.re(output$summary.random$Time)
    dev.off()
    
    svg(paste0('./plots/',uf,'/delay_effect.svg'))
    plot.inla.re(output$summary.random$Delay)
    dev.off()
    
  }
  
  
  # Gerando amostras da posteriori dos parâmetros do modelo ajustado no INLA
  delay.samples.list <- inla.posterior.sample(n = 500, output)
  
  
  
  # Sampling the missing triangule from inla output in vector format from the model likelihood
  aaa <- lapply(X = delay.samples.list,
                FUN = function(x, idx = index.missing){
                  aux <- rnbinom(n = idx, mu = exp(x$latent[idx]), size = x$hyperpar[1])
                  aux.unif <- rbinom(n = length(idx), 1, prob = 1-x$hyperpar[2])
                  return(aux * aux.unif)
                }
  ) 
  
  
  # Creating a vectorized version of the triangle matrix
  delay.vec.trian <- inla.matrix2vector(as.matrix(delay.tbl.tmp.obs.trian[index.time,]))
  
  # Transforming back from the vector form to the matrix form
  bbb <- lapply(aaa, FUN = function(xxx, data = delay.vec.trian){
    data[which(is.na(data))] <- ifelse(is.na(xxx), 0, xxx)
    inla.vector2matrix(data, ncol = Dmax+1) } )
  
  
  # Samples of {N_t : t=Tactual-Dmax+1,...Tactual}
  ccc <- sapply(bbb, FUN = function(x) rowSums(x) )

  return(list(samples=ccc))
}