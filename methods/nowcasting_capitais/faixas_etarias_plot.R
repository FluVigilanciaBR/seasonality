require(ggplot2)
require(cowplot)
library(tidyverse)
source('../report/theme.publication.R')

plt.age.prop.pointrange <- function(df, uf=0, sigla='BR'){
  colorcount=length(unique(df$age_cat))
  getPalette = colorRampPalette(colorblind_pal()(min(8, colorcount)))
  
  plt <- df %>% 
    filter(is.finite(age_cat), SG_UF == uf) %>%
    ggplot(aes(x = DT_SIN_PRI_epiweek, fill = age_cat, color=age_cat)) + 
    geom_ribbon(aes(ymax=q90s, ymin=q90i), color='grey', fill='grey', linetype=0, alpha=0.5) + 
    geom_ribbon(aes(ymax=q50s, ymin=q50i), color='grey', fill='grey', linetype=0, alpha=0.75) + 
    # geom_line(aes(y=q90i), color='darkgrey', linetype=1) + 
    # geom_line(aes(y=q50s), color='darkgrey', linetype=2) + 
    # geom_line(aes(y=q50i), color='darkgrey', linetype=2) + 
    geom_pointrange(aes(y=prop_median, ymin=prop_li, ymax=prop_ls), fatten=.75) + 
    geom_vline(xintercept = as.Date.character('2020-12-01'), linetype='dashed', size=1.25) + 
    scale_fill_manual(values=getPalette(colorcount), guide=F) +
    scale_color_manual(values=getPalette(colorcount), guide=F) +
    scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits, name=NULL) +
    scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
    labs(x='Semana de primeiros sintomas', y='Proporção', color='Faixa\netária', fill='Faixa\netária') +
    theme_Publication() + 
    ggtitle(sigla, subtitle='Proporção dos casos semanais entre as faixas etárias de interesse.') +
    facet_grid(rows='age_cat', scale='free_y')
  return(plt)
}

plt.age.inc <- function(df, facet_cols=c('inc', 'inc.obitos', 'let'), facet_labs=c('Casos', 'Óbitos', 'Letalidade'),
                        ylabs='letalidade (%) e incidências (por 100mil hab.)'){
  dado.labs <- facet_labs
  names(dado.labs) <- facet_cols
  
  plt <- df %>%
    select(-n, -obitos) %>%
    pivot_longer(cols=all_of(facet_cols),
                 names_to='dado', values_to = 'valor') %>%
    ggplot(aes(x=DT_SIN_PRI_epiweek,
               y=valor,
               color=age_cat,
               linetype=age_cat
    )) +
    geom_line() +
    scale_color_manual(values=c(colorblind_pal()(4), colorblind_pal()(7))) +
    scale_linetype_manual(values=c(rep(2, 4), rep(1, 7))) +
    scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits, name=NULL) +
    labs(y=ylabs,
         color='Faixa\netária',
         linetype='Faixa\netária') +
    theme_Publication() +
    facet_wrap(~dado, ncol=1, scales='free_y',
               labeller=labeller(dado=dado.labs))

  return(plt)  
}

plot.nowcast <- function(pred.summy, Fim, nowcast = T, xlimits){
  
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
      geom_ribbon( aes( ymin=Q1, ymax=Q3), fill = 'gray', 
                   color = 'gray', alpha = 0.75, 
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
    ylab("Incidência\n(por 100mil hab.)") +
    xlab("Semana de primeiros sintomas") +
    scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits) +
    theme_Publication() +
    theme(plot.margin=unit(c(1,0,5,5), units='pt'),
          legend.margin = margin(0,0,0,0, unit='pt'),
          legend.justification=c(0,1),
          legend.position=c(0.015, 1.052),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.key.size = unit(14, 'pt'),
          legend.text = element_text(family = 'Roboto'))
  
  p0.day
}

plt.age.inc.rows <- function(df, xlabs='Semana de primeiros sintomas',
                        ylabs='Incidência de SRAG por COVID-19 (por 100mil hab.)',
                        title,
                        subtitle){
  colorcount=length(unique(df$age_cat))
  getPalette = colorRampPalette(colorblind_pal()(min(8, colorcount)))
  
  plt <- df %>%
    ggplot(aes(x=DT_SIN_PRI_epiweek,
               y=valor,
               color=age_cat)) +
    geom_line() +
    scale_color_manual(palette=getPalette, guide="none") +
    scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits, name=NULL) +
    labs(y=ylabs,
         x=xlabs) +
    theme_Publication() +
    facet_grid(rows='age_cat', scales='free_y') +
    ggtitle(title, subtitle=subtitle)
  
  return(plt)  
}
