suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))
suppressWarnings(suppressPackageStartupMessages(library(lemon)))
suppressWarnings(suppressPackageStartupMessages(library(grid)))
suppressWarnings(suppressPackageStartupMessages(library(gridExtra)))
suppressWarnings(suppressPackageStartupMessages(library(tools)))
suppressWarnings(suppressPackageStartupMessages(library(reticulate)))
suppressWarnings(suppressPackageStartupMessages(library(stringi)))
suppressWarnings(suppressPackageStartupMessages(library(sf)))
suppressWarnings(suppressPackageStartupMessages(library(magick)))
source('theme.publication.R')
use_python('~/miniconda/env/fludashboard-development/bin/python')
source_python('../data_filter/episem.py')
suppressWarnings(suppressPackageStartupMessages(library("argparse")))
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

logo <- image_read('Figs/infogripe.png')

# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults,
args <- parser$parse_args()
epiweek.sunday <- epiweek2date(as.integer(args$epiyear), as.integer(args$epiweek)) %>%
  as.Date(format='%Y%m%d') + 6

if (!(args$filtertype %in% c('srag', 'sragnofever', 'hospdeath'))){
  stop(paste0('Invalid filter type', args$filtertype))
}

suff_list <- list('srag' = '', 'sragnofever' = '_sragnofever', 'hospdeath' = '_hospdeath')
suff_out <- suff_list[args$filtertype]

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
df.territories <- read.csv('territorios.csv', stringsAsFactors = FALSE)

if (args$plot){
  # Load shapefiles
  brazil.shp <- sf::st_read(dsn=plots.folder, layer='Brasil', stringsAsFactors = F)
  brazil.shp$CD_GEOCODU <- as.integer(brazil.shp$CD_GEOCODU)
  brazil.reggeo.shp <- st_read(dsn=plots.folder, layer='Brasil-regiao', stringsAsFactors = F)
  brazil.regperf.shp <- st_read(dsn=plots.folder, layer='Brasil-regionais', stringsAsFactors = F)
  
  plot.map <- function(brazil.shp, brazil.reggeo.shp, brazil.regperf.shp, plot.colors, plot.breaks, plot.labels, plot.title, file.name){
    # Plot maps with different territorial aggregations
    p1 <- ggplot(brazil.reggeo.shp) +
      geom_sf(aes(fill = alert.level), show.legend=F) +
      scale_fill_manual(values = plot.colors, breaks = plot.breaks, label = plot.labels, drop=F) +
      theme_void() +
      labs(title='Regiões geopolíticas') +
      theme(plot.margin = unit(c(0,0,0,0), 'pt'))
    p2 <- ggplot(brazil.regperf.shp) +
      geom_sf(aes(fill = alert.level), show.legend=T) +
      scale_fill_manual(values = plot.colors, breaks = plot.breaks, label = plot.labels, drop=F) +
      theme_void() +
      labs(title='Regionais por perfil de atividade') +
      theme(legend.direction = 'horizontal',
            legend.position ='bottom',
            legend.title=element_blank(),
            plot.margin = unit(c(0,0,0,0), 'pt'))
    p3 <- ggplot(brazil.shp) +
      geom_sf(aes(fill = alert.level), show.legend=F) +
      scale_fill_manual(values = plot.colors, breaks = plot.breaks, label = plot.labels, drop=F) +
      theme_void() +
      labs(title='Unidades Federativas') +
      theme(plot.margin = unit(c(0,0,0,0), 'pt'))
    
    # pdf(paste0('Figs/', file.name), width = 12, height = 4)
    # grid.arrange(p1, p2, p3, nrow=1, top=plot.title)
    # dev.off()
    # 
    # pdf(paste0('Figs/', file.name), width = 12, height = 4)
    # grid.arrange(p1, p2, p3, nrow=1, top=plot.title)
    # dev.off()
    # 
    # pdf(paste0('Figs/', file.name), width = 12, height = 4)
    # grid.arrange(p1, p2, p3, nrow=1, top=plot.title)
    # dev.off()
    
    png(paste0('Figs/', file.name), width = 12, height = 4, units='in', res=72)
    grid.arrange(p1, p2, p3, nrow=1, top=plot.title)
    grid::grid.raster(logo, x = 0.99, y = 0.01, just = c('right', 'bottom'), width = unit(1, 'inches'))
    dev.off()
    
    
    return()
  }
  
  contingency.maps <- function(df.contingency){
    # Prepare data and generate maps with contingency level
    brazil.shp <- brazil.shp %>%
      left_join(df.contingency %>%
                  select(territory_id, contingency), by=c('CD_GEOCODU' = 'territory_id')) %>%
      mutate(alert.level = factor(contingency, levels=c(1, 2, 3, 4)))
    
    brazil.reggeo.shp <- brazil.reggeo.shp %>%
      left_join(df.contingency %>%
                  select(territory_id, contingency), by=c('regiao' = 'territory_id')) %>%
      mutate(alert.level = factor(contingency, levels=c(1, 2, 3, 4)))
    
    brazil.regperf.shp <- brazil.regperf.shp %>%
      left_join(df.contingency %>%
                  select(territory_id, contingency), by=c('regionais' = 'territory_id'))  %>%
      mutate(alert.level = factor(contingency, levels=c(1, 2, 3, 4)))
    
    plot.colors <- c('#ffffcc', '#a1dab4', '#41b6c4', '#225ea8')
    plot.breaks <- c(1, 2, 3, 4)
    plot.labels <- c('Nível Basal', 'Nível 0', 'Nível 1', 'Nível 2')
    plot.map(brazil.shp, brazil.reggeo.shp, brazil.regperf.shp, plot.colors, plot.breaks, plot.labels, plot.title = 'Níveis do plano de contingência', paste0('contingency', suff_out, '.png'))
    return()
  }
  
  activity.maps <- function(df.weekly.alert, dataset){
    # Prepare data and generate maps with weekly activity level
    brazil.shp <- brazil.shp %>%
      left_join(df.weekly.alert %>%
                  select(territory_id, alert), by=c('CD_GEOCODU' = 'territory_id')) %>%
      mutate(alert.level = factor(alert, levels=c(1, 2, 3, 4)))
    
    brazil.reggeo.shp <- brazil.reggeo.shp %>%
      left_join(df.weekly.alert %>%
                  select(territory_id, alert), by=c('regiao' = 'territory_id')) %>%
      mutate(alert.level = factor(alert, levels=c(1, 2, 3, 4)))
    
    brazil.regperf.shp <- brazil.regperf.shp %>%
      left_join(df.weekly.alert %>%
                  select(territory_id, alert), by=c('regionais' = 'territory_id'))  %>%
      mutate(alert.level = factor(alert, levels=c(1, 2, 3, 4)))
    
    plot.colors <- c('#edf8fb', '#b3cde3', '#9f8cc6', '#88419d')
    plot.breaks <- c(1, 2, 3, 4)
    plot.labels <- c('Baixa', 'Epidêmica', 'Alta', 'Muito alta')
    plot.title <- list(
      'srag' = 'Nível de atividade de SRAG',
      'obito' = 'Nível de atividade de óbitos de SRAG',
      'sragflu' = 'Nível de atividade de SRAG por influenza',
      'obitoflu' = 'Nível de atividade de óbitos de SRAG por influenza',
      'sragcovid' = 'Nível de atividade de SRAG por COVID-19',
      'obitocovid' = 'Nível de atividade de óbitos de SRAG por COVID-19'
    )
    plot.map(brazil.shp, brazil.reggeo.shp, brazil.regperf.shp, plot.colors, plot.breaks, plot.labels, as.character(plot.title[dataset]), paste0(dataset, '-activity', suff_out, '.png'))
    return()
  }
  
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
        gather(ts.name, measure, c(rolling_average, X50., X2.5., X97.5., SRAG, limiar.pré.epidêmico, intensidade.alta, intensidade.muito.alta))

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
      png(paste0('Figs/', 'Brazil_timeseries', suff_out, '.png'), width = 12, height = 8, units='in', res=72)
      p.grid[[2]] <- p.grid[[2]] + ylab(" ")
      p.grid[[4]] <- p.grid[[4]] + ylab(" ")
      p.grid[[3]] <- p.grid[[3]] + ylab(" ")
      p.grid[[5]] <- p.grid[[5]] + ylab(" ")
      
      p.grid[[1]] <- p.grid[[1]] + xlab(" ")
      p.grid[[2]] <- p.grid[[2]] + xlab(" ")
      p.grid[[4]] <- p.grid[[4]] + xlab(" ")

      grid_arrange_shared_legend(p.grid[[1]], p.grid[[2]], p.grid[[4]], p.grid[[6]], p.grid[[3]], p.grid[[5]], ncol = 3, nrow = 2, position = 'bottom')
      grid::grid.raster(logo, x = 0.999, y = 0.001, just = c('right', 'bottom'), width = unit(.8, 'inches'))
      dev.off()
    } else {
      p.grid <- lapply(c(1, 6), plot.dataset)
      png(paste0('Figs/', 'Territory_', territory_id, '_dataset_1_6_timeseries', suff_out, '.png'), width = 10, height = 3, units='in', res=72)
      p.grid[[2]] <- p.grid[[2]] + ylab(" ")

      grid_arrange_shared_legend(p.grid[[1]], p.grid[[2]], ncol = 2, nrow = 1, position = 'right')
      grid::grid.raster(logo, x = 0.999, y = 0.001, just = c('right', 'bottom'), width = unit(.6, 'inches'))
      dev.off()
      
      png(paste0('Figs/', 'Territory_', territory_id, '_dataset_1_timeseries', suff_out, '.png'), width = 6, height = 3, units='in', res=72)
      print(p.grid[[1]] + ggtitle(paste0('SRAG - UF ', territory_id, ': ', plot.title)))
      grid::grid.raster(logo, x = 0.999, y = 0.001, just = c('right', 'bottom'), width = unit(.65, 'inches'))
      dev.off()
      
      # for (d in c(1, 6)){
      #   p <- plot.dataset(d=d)
      #   png(paste0('Figs/', 'Territory_', territory_id, '_dataset_', d, '_timeseries.png'), width = 6, height = 3)
      #   print(p)
      #   dev.off()
      # }
    }
    return()
  }
  
}

#### Generate contingency map: ####
print('Loading contingency data')
df.contingency <- read.csv(paste0(data.folder, 'contingency_level', suff_out, '.csv'), stringsAsFactors = F) %>%
  filter(epiyear == args$epiyear)
if (args$plot){
  print('Generating contingency map')
  contingency.maps(df.contingency)
}

##### Generate activity maps: ####
df.weekly.alert <- read.csv(paste0(data.folder, 'weekly_alert', suff_out, '.csv'), stringsAsFactors = F)
df.weekly.alert <- df.weekly.alert %>%
  filter(epiyear == args$epiyear,
         epiweek == args$epiweek)

if (args$plot){
  datasets = list('srag' = 1, 'sragflu' = 2, 'obitoflu' = 3, 'sragcovid' = 4, 'obitocovid' = 5, 'obito' = 6)
  for (dataset in names(datasets)){
    print(paste0('Generating activity map: ', dataset))
    df.weekly.alert %>%
      filter(dataset_id == datasets[dataset]) %>%
      activity.maps(dataset = dataset)
  }
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
  current.minus.4.epiweek <- last.year.maxweek - (2 - args$epiweek)
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
if (args$plot){
  for (t.id in unique(df.current2plot$territory_id)){
    print(t.id)
    df.plot.ts <- df.current2plot %>%
      select(-Tipo) %>%
      filter(territory_id == t.id, epiyear == args$epiyear) %>%
      right_join(df.typical %>% filter(territory_id == t.id))
    df.plot.ts <- df.report %>%
      select(territory_id, dataset_id, scale_id, limiar.pré.epidêmico, intensidade.alta, intensidade.muito.alta) %>%
      right_join(df.plot.ts)
    plot.timeseries(df.plot.ts, ifelse(t.id == 0, 2, 1))
  }
}

#### Tables for report ####
estimated <- list(1, 2, 3, 4, 5, 6)
for (d in c(1, 2, 3, 4, 5, 6)){
  estimated[d] <- df.current %>%
    filter(epiyear == args$epiyear, epiweek == args$epiweek, Situation == 'estimated', dataset_id == d, scale_id == 1) %>%
    select(territory_id) %>%
    as.vector()
}
     
df.current <- df.current %>%
  filter(scale_id == 1) %>%
  dplyr::filter(
    (dataset_id == 1 &
       ((territory_id %in% estimated[[1]] & epiyear == args$epiyear & epiweek == args$epiweek) |
          (!(territory_id %in% estimated[[1]]) & epiyear == current.minus.4.epiyear & epiweek == current.minus.4.epiweek))
    ) |
      (dataset_id == 2 &
         ((territory_id %in% estimated[[2]] & epiyear == args$epiyear & epiweek == args$epiweek) |
            (!(territory_id %in% estimated[[2]]) & epiyear == current.minus.4.epiyear & epiweek == current.minus.4.epiweek))
      ) |
      (dataset_id == 3 &
         ((territory_id %in% estimated[[3]] & epiyear == args$epiyear & epiweek == args$epiweek) |
            (!(territory_id %in% estimated[[3]]) & epiyear == current.minus.4.epiyear & epiweek == current.minus.4.epiweek))
      ) |
      (dataset_id == 4 &
         ((territory_id %in% estimated[[4]] & epiyear == args$epiyear & epiweek == args$epiweek) |
            (!(territory_id %in% estimated[[4]]) & epiyear == current.minus.4.epiyear & epiweek == current.minus.4.epiweek))
      ) |
      (dataset_id == 5 &
         ((territory_id %in% estimated[[5]] & epiyear == args$epiyear & epiweek == args$epiweek) |
            (!(territory_id %in% estimated[[5]]) & epiyear == current.minus.4.epiyear & epiweek == current.minus.4.epiweek))
      ) |
      (dataset_id == 6 &
         ((territory_id %in% estimated[[6]] & epiyear == args$epiyear & epiweek == args$epiweek) |
            (!(territory_id %in% estimated[[6]]) & epiyear == current.minus.4.epiyear & epiweek == current.minus.4.epiweek))
      )
  ) %>%
  select(territory_id, epiyear, epiweek, dataset_id, scale_id, Situation, X50.) %>%
  left_join(df.typical) %>%
  mutate(canal.endemico = case_when(
    X50. > corredor.alto ~ 4,
    X50. <= corredor.alto & X50. > corredor.mediano ~ 3,
    X50. <= corredor.mediano & X50. > corredor.baixo ~ 2,
    TRUE ~ 1
  ))

df.siglas <- read.csv('territorios.csv', stringsAsFactors = F)
df.siglas <- df.current %>%
  right_join(df.siglas) %>%
  left_join(df.weekly.alert %>% select(territory_id, dataset_id, alert, low_level, epidemic_level, high_level, very_high_level)) %>%
  mutate(alert.probability = case_when(
    Situation != 'estimated' ~ low_level,
    Situation == 'estimated' & alert == 1 ~ low_level,
    Situation == 'estimated' & alert == 2 ~ epidemic_level,
    Situation == 'estimated' & alert == 3 ~ high_level,
    Situation == 'estimated' & alert == 4 ~ very_high_level
  ))

df.table <- expand.grid(list(dado = c(1, 2, 3, 4, 5, 6),
                             territorio = c('geopolítico', 'perfil de atividade', 'UF'),
                             indicador = c('canal endêmico', 'atividade semanal'),
                             nivel = c(1, 2, 3, 4)),
                        stringsAsFactors = F)
df.table <- df.siglas %>%
  filter(Tipo != 'País') %>%
  mutate(sigla = case_when(
    Situation != 'estimated' ~ paste0(sigla, '*'),
    TRUE ~ sigla
  )) %>%
  group_by(Tipo, dataset_id, canal.endemico) %>%
  summarise(lista = stri_paste(sigla, collapse= ', ')) %>%
  ungroup() %>%
  transmute(dado = dataset_id,
            territorio = case_when(
              Tipo == 'Estado' ~ 'UF',
              Tipo == 'Região' ~ 'geopolítico',
              Tipo == 'Regional' ~ 'perfil de atividade'),
            indicador = 'canal endêmico',
            nivel = canal.endemico,
            lista = case_when(
              !is.na(lista) ~ lista,
              TRUE ~ ' ')) %>%
  right_join(df.table)

df.table <- df.siglas %>%
  filter(Tipo != 'País') %>%
  mutate(sigla.prob = case_when(
    !is.na(alert.probability) ~ paste0(sigla, '(', 100*alert.probability, '\\%)'),
    is.na(alert.probability) ~ paste0(sigla, '*'))) %>%
  group_by(Tipo, dataset_id, alert) %>%
  summarise(lista = stri_paste(sigla.prob, collapse= ', ')) %>%
  ungroup() %>%
  transmute(dado = dataset_id,
            territorio = case_when(
              Tipo == 'Estado' ~ 'UF',
              Tipo == 'Região' ~ 'geopolítico',
              Tipo == 'Regional' ~ 'perfil de atividade'),
            indicador = 'atividade semanal',
            nivel = alert,
            lista = lista) %>%
  right_join(df.table, by = c('dado', 'territorio', 'indicador', 'nivel')) %>%
  mutate(lista = case_when(
    !is.na(lista.x) ~ lista.x,
    is.na(lista.x) ~ lista.y),
    lista = case_when(
      !is.na(lista) ~ lista,
      TRUE ~ ' ')) %>%
  select(-lista.x, -lista.y)

df.table <- df.siglas %>%
  filter(Tipo == 'País') %>%
  transmute(dado = dataset_id,
            territorio = Tipo,
            indicador = 'canal endêmico',
            nivel = canal.endemico,
            lista = 'BR') %>%
  union(df.table)

df.table <- df.siglas %>%
  filter(Tipo == 'País') %>%
  transmute(dado = dataset_id,
            territorio = Tipo,
            indicador = 'atividade semanal',
            nivel = alert,
            lista = paste0('BR', ' (', 100*alert.probability, '\\%)' )) %>%
  union(df.table)

df.table$ano.epi <- args$epiyear
df.table$se.epi <- args$epiweek
df.table$data.fim <- epiweek.sunday
write.csv(df.table, paste0('tabela_de_alertas', suff_out, '.csv'), na = ' ', row.names = F)

tmp <- as.data.frame(list(indicador = c(rep('canal endêmico', 4), rep('atividade semanal', 4)),
                          nivel = rep(c(1, 2, 3, 4), 2),
                          texto = c('zona de êxito', 'zona de segurança', 'zona de alerta', 'zona de risco', 'baixa', 'epidêmica', 'alta', 'muito alta')
                          )) %>%
  mutate(indicador = as.character(indicador), texto = as.character(texto))

br.report <- df.table %>%
  filter(territorio == 'País') %>%
  left_join(tmp)

df.lab.orig <- read.csv(paste0(data.folder, 'clean_data_epiweek-weekly-incidence_w_situation', suff_out, '.csv'), stringsAsFactors = F) %>%
  filter(epiyear == args$epiyear & escala == 'casos' & sexo == 'Total' & UF == 'BR') %>%
  select(dado, FLU_A, FLU_B, VSR, SARS2, DELAYED, POSITIVE_CASES, NEGATIVE, SRAG, epiweek)

df.lab <- df.lab.orig %>%
  filter(dado == 'srag') %>%
  select(-dado) %>%
  colSums(na.rm = T) %>%
  t() %>%
  as.data.frame() %>%
  mutate(FLU_A = 100*FLU_A/POSITIVE_CASES,
         FLU_B = 100*FLU_B/POSITIVE_CASES,
         VSR = 100*VSR/POSITIVE_CASES,
         SARS2 = 100*SARS2/POSITIVE_CASES,
         dado = 'srag')
df.lab <- df.lab.orig %>%
  filter(dado == 'obito') %>%
  select(-dado) %>%
  colSums(na.rm = T) %>%
  t() %>%
  as.data.frame() %>%
  mutate(FLU_A = 100*FLU_A/POSITIVE_CASES,
         FLU_B = 100*FLU_B/POSITIVE_CASES,
         VSR = 100*VSR/POSITIVE_CASES,
         SARS2 = 100*SARS2/POSITIVE_CASES,
         dado = 'obito') %>%
  rbind(df.lab)

Sweave(paste0('Boletim_InfoGripe_template', suff_out, '.Rnw'))
texi2dvi(file = paste0('Boletim_InfoGripe_template', suff_out, '.tex'), pdf = TRUE, quiet=TRUE, clean=TRUE)
suff_out_ptbr <- ''
if (args$filtertype == 'sragnofever'){
  suff_out_ptbr <- '_sem_filtro_febre'
} else if (args$filtertype == 'hospdeath'){
  suff_out_ptbr <- '_sem_filtro_sintomas'
}
system(paste("cp", paste0('Boletim_InfoGripe_template', suff_out, '.pdf'), paste0('Boletim_InfoGripe_SE',
args$epiyear, sprintf('%02d', args$epiweek), suff_out_ptbr, '.pdf')))
system(paste("mv", '--force', paste0('Boletim_InfoGripe_template', suff_out, '.pdf'), paste0('Boletim_InfoGripe_atual',
suff_out_ptbr, '.pdf')))
