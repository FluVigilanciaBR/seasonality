# Auxiliar functions for nowcasting

# Auxiliar function, sampling from a negative binomial likelihood
ff <- function(x, idx){
  rnbinom(n = idx, mu = exp(x$latent[idx]), size = x$hyperpar[1])
}

# Auxiliar function selecionando um pedaco do dataset
gg <- function(x, dados, idx, Fim.sat, Dmax){
  data.aux <- dados
  data.aux$Casos[idx] <- x
  data.aggregated <- data.aux %>%
    # Selecionando apenas os dias faltantes a partir
    # do domingo da respectiva ultima epiweek
    # com dados faltantes
    filter(Date >= Fim.sat - Dmax  ) %>%
    group_by(Date) %>% 
    dplyr::summarise( 
      Casos = sum(Casos) 
    )
  data.aggregated
}


# Algorithm to get samples for the predictive distribution for the number of cases

nowcasting <- function(output.day, dadosRio.ag, 
                       Fim = today.week, Dm = Dmax){
  
  index.missing = which(is.na(dadosRio.ag$Casos))
  
  
  # Step 1: Sampling from the approximate posterior distribution using INLA
  srag.samples.list <- inla.posterior.sample(n = 1000, output.day)
  
  # Step 2: Sampling the missing triangle (in vector form) from the likelihood using INLA estimates
  vector.samples <- lapply(X = srag.samples.list, 
                           FUN = ff,
                           idx = index.missing
  )
  
  # Step 3: Calculate N_t for each triangle sample {N_t : t=Tactual-Dmax+1,...Tactual}
  tibble.samples <- lapply( X = vector.samples,
                            FUN = gg,
                            dados = dadosRio.ag, 
                            idx = index.missing,
                            Fim.sat = Fim, 
                            Dmax = Dm
  )
  
  # Nowcasting
  srag.pred <- bind_rows(tibble.samples, .id = "sample")
  
  srag.pred
}


#######################
# hess.min <- -1
# h.value <- 0.01
# trials <- 0
# while (hess.min <= 0 & trials < 50){
#   output <- inla(model, family = "nbinomial", data = delay.inla.trian,
#                  control.predictor = list(link = 1, compute = T),
#                  control.compute = list( config = T, waic=TRUE, dic=TRUE),
#                  control.family = list( 
#                    hyper = list("theta" = list(prior = "loggamma", param = c(0.1, 0.1)))
#                  ),
#                  control.inla = list(h = h.value)
#   )
#   hess.start <- which(output$logfile == 'Eigenvalues of the Hessian')
#   hess.min <- min(as.numeric(output$logfile[(hess.start+1):(hess.start+3)]))
#   h.value <- h.value + 0.01
#   trials <- trials + 1
# }
# print(paste('Hessian trials:',trials))
# 
######################


# Running INLA for the nowcasting model
nowcast.INLA <- function(dados.ag, model.day,...){
  hess.min <- -1
  h.value <- 0.01
  trials <- 0
  while (hess.min <= 0 & trials < 50){
    output <- inla(formula = model.day, 
                 family = "nbinomial", 
                 data = dados.ag,
                 num.threads = 4,
                 control.predictor = list(link = 1, compute = T),
                 control.compute = list( config = T),
                 control.inla = list(h = h.value),
                 ...
                 # control.family = list( 
                 # hyper = list("theta" = list(
                 #   prior = "loggamma", param = c(1, 0.1)))
                 #   )
    )
    hess.start <- which(output$logfile == 'Eigenvalues of the Hessian')
    hess.min <- min(as.numeric(output$logfile[(hess.start+1):(hess.start+3)]))
    h.value <- h.value + 0.01
    trials <- trials + 1
  }
  output
}


# Plot nowcasting
plot.nowcast <- function(pred.summy, Fim, nowcast = T){
  
  if(!nowcast){
    # Time series
    p0.day <- pred.summy %>% 
      ggplot(aes(x =  Date, y = Casos, 
                 color = "Casos notificados", 
                 linetype = "Casos notificados")) + 
      geom_line(size = 1, na.rm = T) 
  } else {
    p0.day <- pred.summy %>% 
      ggplot(aes(x =  Date, y = Casos.cut, 
                 color = "Casos notificados", 
                 linetype = "Casos notificados")) + 
      geom_line(size = 1, na.rm = T) +
      geom_ribbon( aes( ymin=IC90I, ymax=IC90S), fill = 'gray', 
                   color = 'gray', alpha = 0.5, 
                   show.legend = F) + 
      geom_line(aes(x = Date, y = Median, 
                    colour = "Casos estimados", 
                    linetype = "Casos estimados"), 
                size = 1, na.rm = T) +
      geom_line(aes(x=Date, y=rolling_average,
                    colour='Média móvel',
                    linetype='Média móvel'), size=1) +
      scale_colour_manual(name = "", 
                          values = c("black", "black", 'blue'), 
                          guide = guide_legend(reverse=F)) +
      scale_linetype_manual(name = "", 
                            values = c("dotted", "solid", 'solid'), 
                            guide = guide_legend(reverse=F))
  }
  
  p0.day <- p0.day + 
    #ylab("Casos hospitalização de SRAG") + 
    #xlab("Tempo") +
    theme_bw( base_size = 14) +
    theme( legend.position = c(0.2, 0.8), legend.title = element_blank()) 
  
  p0.day
}
