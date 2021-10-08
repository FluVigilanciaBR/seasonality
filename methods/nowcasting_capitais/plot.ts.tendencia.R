require(tidyverse)
suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))
source('../report/theme.publication.R')

# Function for trend plot
plot.ts.tendencia <- function(df, xbreaks=c(1, seq(4, 52, 4)), xlbls=c(1, seq(4, 52, 4)), xlimits=c(1, 53)){
  plt <- df %>%
    select(Date, tendencia.3s, tendencia.6s) %>%
    mutate(tendencia.3s = case_when(
      Date < today.week - 2 ~ NA_real_,
      TRUE ~ tendencia.3s
    )) %>%
    rename('curto prazo'=tendencia.3s, 'longo prazo'=tendencia.6s) %>%
    pivot_longer(-Date, names_to = 'janela', values_to = 'tendencia') %>%
    arrange(desc(janela)) %>%
    ggplot(aes(x=Date, y=tendencia, color=janela)) +
    geom_hline(yintercept = 0, color='grey', size=1.5, linetype=2) +
    geom_line() +
    geom_point() +
    scale_y_continuous(breaks = seq(-1,1,.5),
                       labels=c('Prob. queda\n> 95%', 'Prob. queda\n> 75%', 'Estabilidade./\noscilação', 'Prob. cresc.\n> 75%', 'Prob. cresc.\n> 95%'),
                       limits = c(-1,1),
                       name=NULL) +
    scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits, name=NULL) +
    scale_color_discrete(name=NULL) +
    theme_Publication() +
    theme(plot.margin=unit(c(1,0,5,5), units='pt'),
          axis.text.y = element_text(size = rel(.8), angle=30),
          legend.margin = margin(0,0,0,0, unit='pt'),
          legend.justification=c(0,1), 
          legend.position=c(0.015, 1.05),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.key.size = unit(14, 'pt'),
          legend.text = element_text(family = 'Roboto', size = rel(.8))
    )
  
  return(plt)
}
