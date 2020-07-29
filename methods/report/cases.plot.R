library(tidyverse)
library(ggplot2)
library(lemon)
library(grid)
library(gridExtra)
library(tools)
library(reticulate)
library(stringi)
library(magick)
source('theme.publication.R')
use_python('~/miniconda/env/fludashboard-development/bin/python')
source_python('../data_filter/episem.py')
suppressPackageStartupMessages(library("argparse"))

logo <- image_read('Figs/infogripe.png')
# create parser object
parser <- ArgumentParser()
# specify our desired options
# by default ArgumentParser will add an help option
parser$add_argument("-y", "--epiyear", type="integer",
                    help="Epidemiological year")
parser$add_argument("-w", "--epiweek", type="integer",
                    help="Epidemiological week")
parser$add_argument("-p", "--plot", action='store_true',
                    help="Update plots?")
parser$add_argument("-f", "--filtertype", type="character", default='srag',
                    help="Type of filter [srag, sragnofever, hospdeath]. Default %(default)s")
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults,
args <- parser$parse_args()
epiweek.sunday <- epiweek2date(as.integer(args$epiyear), as.integer(args$epiweek)) %>%
  as.Date(format='%Y%m%d') + 6

# Base folders
data.folder <- '../../data/data/'
plots.folder <- './Figs'

# ID conversions:
region.indeces <- c('NI', 'BR', 'RegN', 'RegL', 'RegC', 'RegS', 'RegNI',
                    'N', 'NE', 'SE', 'S', 'CO', 'RNI')
region.id <- list('NI'=99,
                  'BR'= 0,
                  'RegN'= 1001,
                  'RegL'= 1002,
                  'RegC'= 1003,
                  'RegS'= 1004,
                  'RegNI'= 9999,
                  'N'= 1,
                  'NE'= 2,
                  'SE'= 3,
                  'S'= 4,
                  'CO'= 5,
                  'RNI'= 9)

suff_list <- list('srag' = '', 'sragnofever' = '_sragnofever', 'hospdeath' = '_hospdeath')
suff_out <- suff_list[args$filtertype]
suff <- c('incidencia', 'casos')

df.territories <- read.csv('territorios.csv', stringsAsFactors = FALSE)

plot.timeseries <- function(df, scale_val=1){
  # Function for timeseries plot with endemic channels, activity thresholds and estimates
  # Generate plot with 3 datasets if Brazil (territory_id == 0) or else plot only SRAG data (dataset_id == 1)
  territory_id <- unique(df$territory_id)
  
  df <- df %>%
    filter(scale_id == scale_val)
  df.ts <- df %>%
    select(dataset_id, epiweek, rolling_average, X50., X2.5., X97.5., SRAG, limiar.pré.epidêmico, intensidade.alta, intensidade.muito.alta) %>%
    mutate(SRAG = case_when(
      epiweek >= args$epiweek[1] - 1 ~ NA_real_,
      TRUE ~ SRAG
    ),
    rolling_average = case_when(
      epiweek >= args$epiweek[1] - 2 & dataset_id != 1 ~ NA_real_,
      TRUE ~ rolling_average
    )) %>%
    mutate_at(c('X2.5.', 'X97.5.', 'X50.'), function(x) {
      case_when(.$epiweek >= args$epiweek[1] - 1 & .$dataset_id != 1 ~ NA_real_,
                TRUE ~ x)
    }) %>%
    gather(ts.name, measure, c(X50., rolling_average, X2.5., X97.5., SRAG, limiar.pré.epidêmico, intensidade.alta, intensidade.muito.alta))
  
  df.ts$ts.name <- df.ts$ts.name %>%
    factor(levels = c('SRAG', 'X2.5.', 'X97.5.', 'X50.', 'rolling_average', 'limiar.pré.epidêmico', 'intensidade.alta', 'intensidade.muito.alta'))
  
  plot.dataset <- function(d){
    # Internal function plot based on dataset_id
    y.upper <- df %>%
      filter(dataset_id == d) %>%
      select(SRAG) %>%
      max(na.rm=T)*1.1
    y.upper.veryhigh <- df %>%
      filter(dataset_id == d) %>%
      select(intensidade.muito.alta) %>%
      unique(na.rm=T)*1.1
    y.upper.ic <- df %>%
      filter(dataset_id == d) %>%
      select(X97.5.) %>%
      max(na.rm=T)*1.1
    y.upper <- max(y.upper, y.upper.veryhigh$intensidade.muito.alta, y.upper.ic)
    
    acumulado <- df %>% 
      filter(dataset_id == d) %>%
      transmute(acum = case_when(
        is.na(X50.) ~ SRAG,
        !is.na(X50.) ~ X50.
      ), lim.inf = case_when(
        is.na(X2.5.) ~ SRAG,
        !is.na(X2.5.) ~ X2.5.
      ), lim.sup = case_when(
        is.na(X97.5.) ~ SRAG,
        !is.na(X97.5.) ~ X97.5.
      )) %>%
      colSums(na.rm=T) %>%
      t() %>%
      as.data.frame()
    
    if (scale_val == 1){
      fmt='%.02f'
      acumulado$acum <- sprintf(fmt=fmt, acumulado$acum)
      acumulado$lim.inf <- sprintf(fmt=fmt, acumulado$lim.inf)
      acumulado$lim.sup <- sprintf(fmt=fmt, acumulado$lim.sup)
    } else {
      fmt='%d'
      acumulado$acum <- sprintf(fmt=fmt, round(acumulado$acum))
      acumulado$lim.inf <- sprintf(fmt=fmt, round(acumulado$lim.inf))
      acumulado$lim.sup <- sprintf(fmt=fmt, round(acumulado$lim.sup))
    }
    
    if (sum(df$Situation[df$dataset_id == d] == 'estimated', na.rm=T) > 0){
      min.epiweek <- df %>%
        filter(dataset_id == d & Situation == 'estimated') %>%
        select(epiweek) %>%
        min()
      min.epiweek <- max(1, min.epiweek-1)
      df.ts$measure[df.ts$dataset_id == d & df.ts$epiweek == min.epiweek & df.ts$ts.name %in% c('X2.5.', 'X50.', 'X97.5.')] <- df$SRAG[df$dataset_id == d & df$epiweek == min.epiweek]
      total.titl <- paste0('Total estimado: ', acumulado$acum, ' [', acumulado$lim.inf, '-', acumulado$lim.sup, ']')
    } else {
      total.titl <- paste0('Total notificado (dado incompleto): ', acumulado$acum)
    }
    
    
    leg.pt <- ifelse(territory_id == 0, .8, .7)
    leg.nrow <- ifelse(territory_id == 0, 2, 3)
    leg.pos <- ifelse(territory_id == 0, 'bottom', 'right')
    # leg.pt <- ifelse(territory_id == 0, .8, .7)
    # leg.nrow <- ifelse(territory_id == 0, 2, 2)
    # leg.pos <- ifelse(territory_id == 0, 'bottom', 'bottom')
    ylabs <- c('Incidência (por 100mil hab.)', 'Casos')
    title <- c('SRAG', 'SRAG por Influenza', 'Óbitos de SRAG por Influenza', 'SRAG por COVID-19', 'Óbitos de SRAG por COVID-19', 'Óbitos de SRAG')
    p <- df %>%
      filter(dataset_id == d) %>%
      ggplot(aes(x = epiweek)) +
      geom_ribbon(aes(ymin = 0, ymax = corredor.baixo, fill = 'Zona de êxito'), alpha=.5) +
      geom_ribbon(aes(ymin = corredor.baixo, ymax = corredor.mediano, fill = 'Zona de segurança'), alpha=.5) +
      geom_ribbon(aes(ymin = corredor.mediano, ymax = corredor.alto, fill = 'Zona de alerta'), alpha=.5) +
      geom_ribbon(aes(ymin = corredor.alto, ymax = Inf, fill = 'Zona de risco'), alpha=.5) +
      geom_line(data = df.ts %>% filter(dataset_id == d), aes(y = measure, color = ts.name, linetype = ts.name)) +
      scale_fill_manual(name = NULL, values = c('Zona de êxito' = 'green', 'Zona de segurança' = 'yellow', 'Zona de alerta' = 'orange', 'Zona de risco' = 'red'),
                        breaks = c('Zona de êxito', 'Zona de segurança', 'Zona de alerta', 'Zona de risco')) +
      scale_x_continuous(expand = c(0,0), breaks = c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52)) +
      scale_y_continuous(expand = c(0,0), limits = c(0, as.double(y.upper))) +
      scale_linetype_manual(name = NULL, values = c('SRAG' = 1, 'X50.' = 1, 'rolling_average' = 1, 'X2.5.' = 2, 'X97.5.' = 2, 'limiar.pré.epidêmico' = 2, 'intensidade.alta' = 2, 'intensidade.muito.alta' = 2),
                            breaks = c('SRAG', 'X50.', 'rolling_average', 'X2.5.', 'limiar.pré.epidêmico', 'intensidade.alta', 'intensidade.muito.alta'),
                            labels = c('Casos notificados', 'Casos estimados', 'Média móvel', 'Intervalo de confiança', 'Limiar pré-epidêmico', 'Intensidade alta', 'Intensidade muito alta')) +
      scale_color_manual(name = NULL, values = c('SRAG' = 'black', 'X50.' = 'red', 'rolling_average' = 'blue', 'X2.5.' = 'black', 'X97.5.' = 'black', 'limiar.pré.epidêmico' = 'green', 'intensidade.alta' = 'blue', 'intensidade.muito.alta' = 'red'),
                         breaks = c('SRAG', 'X50.', 'rolling_average', 'X2.5.', 'limiar.pré.epidêmico', 'intensidade.alta', 'intensidade.muito.alta'),
                         labels = c('Casos notificados', 'Casos estimados', 'Média móvel', 'Intervalo de confiança', 'Limiar pré-epidêmico', 'Intensidade alta', 'Intensidade muito alta')) +
      labs(x = 'Semana epidemiológica', y = ylabs[scale_val], title=title[d], subtitle = total.titl) +
      theme_Publication() + theme(legend.key.size = unit(15, 'pt'), legend.text=element_text(size = rel(leg.pt)), legend.position = leg.pos, plot.margin=unit(c(2, 10, 5, 5), 'pt'),
                                  plot.title = element_text(size = rel(.8)), plot.subtitle = element_text(size = rel(.8), hjust=0.5))
    if (territory_id == 0){
      p <- p +
        guides(fill=guide_legend(nrow = leg.nrow, byrow = TRUE, title = NULL), color=guide_legend(nrow = leg.nrow, byrow = TRUE, title = NULL), linetype=guide_legend(nrow = leg.nrow, byrow = TRUE, title = NULL))
    }
    return(p)
  }
  
  plot.title <- df.territories$Unidade.da.Federação[df.territories$territory_id == territory_id]
  grid.title <- plot.title %>%
    textGrob(gp=gpar(fontsize=15))

  if (territory_id == 0){
    p.grid <- lapply(c(1, 2, 3, 4, 5, 6), plot.dataset)
    pdf(paste0('Figs/', 'Brazil_timeseries_', suff[scale_val], suff_out, '.pdf'), width = 12, height = 8, onefile = FALSE)
    p.grid[[2]] <- p.grid[[2]] + ylab(" ")
    p.grid[[4]] <- p.grid[[4]] + ylab(" ")
    p.grid[[3]] <- p.grid[[3]] + ylab(" ")
    p.grid[[5]] <- p.grid[[5]] + ylab(" ")
    
    p.grid[[1]] <- p.grid[[1]] + xlab(" ")
    p.grid[[2]] <- p.grid[[2]] + xlab(" ")
    p.grid[[4]] <- p.grid[[4]] + xlab(" ")
    
    grid_arrange_shared_legend(p.grid[[1]], p.grid[[2]], p.grid[[4]], p.grid[[6]], p.grid[[3]], p.grid[[5]], ncol = 3, nrow = 2, position = 'bottom', top = grid.title)
    grid::grid.raster(logo, x = 0.999, y = 0.001, just = c('right', 'bottom'), width = unit(.8, 'inches'))
    dev.off()
  } else {
    p.grid <- lapply(c(1, 6), plot.dataset)
    pdf(paste0('Figs/', 'Territory_', territory_id, '_dataset_1_6_timeseries_', suff[scale_val], suff_out, '.pdf'), width = 10, height = 3, onefile = FALSE)
    p.grid[[2]] <- p.grid[[2]] + ylab(" ")
    
    grid_arrange_shared_legend(p.grid[[1]], p.grid[[2]], ncol = 2, nrow = 1, position = 'right', top = grid.title)
    grid::grid.raster(logo, x = 0.999, y = 0.001, just = c('right', 'bottom'), width = unit(.6, 'inches'))
    dev.off()
    
    pdf(paste0('Figs/', 'Territory_', territory_id, '_dataset_1_timeseries', suff_out, '.pdf'), width = 6, height = 3, onefile = FALSE)
    print(p.grid[[1]] + ggtitle(paste0('SRAG em ', plot.title)))
    grid::grid.raster(logo, x = 0.999, y = 0.001, just = c('right', 'bottom'), width = unit(.65, 'inches'))
    dev.off()
    
    # for (d in c(1, 6)){
    #   p <- plot.dataset(d=d)
    #   pdf(paste0('Figs/', 'Territory_', territory_id, '_dataset_', d, '_timeseries.pdf'), width = 6, height = 3)
    #   print(p)
    #   dev.off()
    # }
  }
  return()
}


#### Prepare data for timeseries and tables ####
df.current <- read.csv(paste0(data.folder, 'current_estimated_values', suff_out, '.csv'), stringsAsFactors = F)
df.typical <- read.csv(paste0(data.folder, 'mem-typical', suff_out, '.csv'), stringsAsFactors = F)
df.report <- read.csv(paste0(data.folder, 'mem-report', suff_out, '.csv'), stringsAsFactors = F)

df.report$territory_id <- df.report$UF
df.report$territory_id[df.report$UF %in% region.indeces] <- as.integer(unlist(region.id[df.report$UF[df.report$UF %in% region.indeces]]))
df.report$territory_id <- as.integer(df.report$territory_id)
df.report <- df.report %>%
  mutate(
    dataset_id = case_when(
      dado == 'srag' ~ 1,
      dado == 'sragflu' ~ 2,
      dado == 'obitoflu' ~ 3,
      dado == 'sragcovid' ~ 4,
      dado == 'obitocovid' ~ 5,
      dado == 'obito' ~ 6
    ),
    scale_id = case_when(
      escala == 'incidência' ~ 1,
      escala == 'casos' ~ 2
    )
  )

df.typical$territory_id <- df.typical$UF
df.typical$territory_id[df.typical$UF %in% region.indeces] <- as.integer(unlist(region.id[df.typical$UF[df.typical$UF %in% region.indeces]]))
df.typical$territory_id <- as.integer(df.typical$territory_id)
df.typical <- df.typical %>%
  mutate(
    dataset_id = case_when(
      dado == 'srag' ~ 1,
      dado == 'sragflu' ~ 2,
      dado == 'obitoflu' ~ 3,
      dado == 'sragcovid' ~ 4,
      dado == 'obitocovid' ~ 5,
      dado == 'obito' ~ 6
    ),
    scale_id = case_when(
      escala == 'incidência' ~ 1,
      escala == 'casos' ~ 2
    )
  )


last.epiyear.flag = (args$epiweek - 2 <= 0)
if (last.epiyear.flag){
  last.year.maxweek <- df.current %>%
    dplyr::filter(epiyear == args$epiyear - 1) %>%
    select(epiweek) %>%
    max()
  current.minus.4.epiweek <- last.year.maxweek - (2 - args$epiwee)
  current.minus.4.epiyear <- args$epiyear - 1  
} else {
  current.minus.4.epiweek <- args$epiweek - 2
  current.minus.4.epiyear <- args$epiyear  
}

df.current$territory_id <- df.current$UF
df.current$territory_id[df.current$UF %in% region.indeces] <- as.integer(unlist(region.id[df.current$UF[df.current$UF %in% region.indeces]]))
df.current$territory_id <- as.integer(df.current$territory_id)
df.current <- df.current %>%
  mutate(
    dataset_id = case_when(
      dado == 'srag' ~ 1,
      dado == 'sragflu' ~ 2,
      dado == 'obitoflu' ~ 3,
      dado == 'sragcovid' ~ 4,
      dado == 'obitocovid' ~ 5,
      dado == 'obito' ~ 6
    ),
    scale_id = case_when(
      escala == 'incidência' ~ 1,
      escala == 'casos' ~ 2
    )
  )

df.current2plot <- df.current
df.current2plot$X2.5.[df.current2plot$Situation != 'estimated'] <- NA
df.current2plot$X50.[df.current2plot$Situation != 'estimated'] <- NA
df.current2plot$X97.5.[df.current2plot$Situation != 'estimated'] <- NA

#### Plot timeseries ####
for (t.id in unique(df.current2plot$territory_id)){
  print(t.id)
  df.plot.ts <- df.current2plot %>%
    select(-Tipo) %>%
    filter(territory_id == t.id, epiyear == args$epiyear) %>%
    right_join(df.typical %>% filter(territory_id == t.id))
  df.plot.ts <- df.report %>%
    select(territory_id, dataset_id, scale_id, limiar.pré.epidêmico, intensidade.alta, intensidade.muito.alta) %>%
    right_join(df.plot.ts)
  plot.timeseries(df.plot.ts, scale_val = 2)
}
