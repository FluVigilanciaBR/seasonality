suppressWarnings(suppressPackageStartupMessages(library("tidyverse")))
suppressWarnings(suppressPackageStartupMessages(library(magick)))
suppressWarnings(suppressPackageStartupMessages(library(grid)))
suppressWarnings(suppressPackageStartupMessages(library(cowplot)))
suppressWarnings(suppressPackageStartupMessages(library(geofacet)))

options(dplyr.summarise.inform=F) 

int_breaks_rounded <- function(x, n = 5)  pretty(x, n)[round(pretty(x, n),1) %% 1 == 0]

logo <- image_read('../report/Figs/infogripe.png')
source('../report/theme.publication.R')


plot.compar <- function(mun, df, seilbl, seflbl, current=F){
  epiweekmax <- 53
  epilbls <- c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52)
  xbreaks <- c(epilbls, epilbls + epiweekmax)
  xlbls <- c(epilbls, epilbls)
  xlimits <- c(1, as.integer(seflbl))
  uf <- df %>%
    filter(CO_MUN_RES_nome == mun) %>%
    select(DS_UF_SIGLA) %>%
    unique()
  
  p0.day <- df %>%
    filter(CO_MUN_RES_nome == mun, grupo_jur == 0) %>%
    ggplot(aes(x =  Date,
               color = base,
               fill = base)) +
    geom_line(aes(y = Median), linetype=3) +
    geom_line(aes(y = Casos.cut), linetype=1) +
    geom_ribbon( aes( ymin=IC90I, ymax=IC90S),
                 alpha = 0.5,
                 show.legend = F, colour=NA) +
    scale_color_colorblind(name=element_blank()) +
    scale_fill_colorblind(name=element_blank()) +
    theme_Publication()
  p.now.srag <- p0.day +
    ylab("Incidência de SRAG (por 100mil hab.)") +
    xlab("Semana de primeiros sintomas") +
    scale_x_continuous(breaks = xbreaks,
                       labels = xlbls,
                       limits = xlimits) +
    theme_Publication(base_size = 16, base_family = 'Roboto') +
    ggtitle(paste0(uf, ' : ', mun)) +
    theme(plot.margin=unit(c(1,0,5,5), units='pt'),
          axis.text = element_text(size = rel(1)),
          legend.margin = margin(0,0,0,0, unit='pt'),
          legend.justification=c(0,1),
          legend.position=c(0.015, 1.05),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.key.size = unit(14, 'pt'),
          legend.text = element_text(family = 'Roboto', size = rel(1))
    )
  png(filename = paste0('./Figs/comparacao/', uf, '_', mun, '_', seilbl, '_', seflbl, '.png'),
      width=8, height=6, units='in', res=200)
  print(p.now.srag)
  grid::grid.raster(logo, x = 0.999, y = 0.95, just = c('right', 'top'), width = unit(1, 'inches'))
  dev.off()
  if (current){
    png(filename = paste0('./Figs/comparacao/', uf, '_', mun, '_comparacao_recente.png'),
        width=8, height=6, units='in', res=200)
    print(p.now.srag)
    grid::grid.raster(logo, x = 0.999, y = 0.95, just = c('right', 'top'), width = unit(1, 'inches'))
    dev.off()
  }
}

plot.geo.facet <- function(seilbl, seflbl, pred.capitais, current=F){
  epiweekmax <- 53
  epilbls <- c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52)
  xbreaks <- c(epilbls, epilbls + epiweekmax)
  xlbls <- c(epilbls, epilbls)
  xlimits <- c(1, as.integer(seflbl))

  p0.day <- pred.capitais %>%
    filter(grupo_jur == 0) %>%
    ggplot(aes(x =  Date,
               color = base,
               fill = base)) +
    geom_line(aes(y = Median), linetype=3) +
    geom_line(aes(y = Casos.cut), linetype=1) +
    geom_ribbon( aes( ymin=IC90I, ymax=IC90S),
                 alpha = 0.5,
                 show.legend = F, colour=NA) +
    scale_color_colorblind(name=element_blank()) +
    scale_fill_colorblind(name=element_blank()) +
    theme_Publication()
  p.now.srag <- p0.day +
    ylab("Incidência de SRAG (por 100mil hab.)") +
    xlab("Semana de primeiros sintomas") +
    scale_x_continuous(breaks = xbreaks,
                       labels = xlbls,
                       limits = xlimits) +
    theme_Publication(base_size = 16, base_family = 'Roboto') +
    labs(title='Comparação entre atualizações das estimativas de SRAG com base no\nSIVEP-Gripe para residentes das capitais') +
    theme(plot.margin=unit(c(1,0,5,5), units='pt'),
          axis.text = element_text(size = rel(1)),
          axis.text.x = element_text(size=rel(.6), angle=45),
          legend.margin = margin(0,0,0,0, unit='pt'),
          legend.justification=c(0,1),
          legend.position=c(0.015, 1.05),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.key.size = unit(14, 'pt'),
          legend.text = element_text(family = 'Roboto', size = rel(1))
    ) +
    facet_geo(~ DS_UF_SIGLA, grid='br_states_grid1', scale='free_y')
  png(filename = paste0('./Figs/comparacao/Capitais_', seilbl, '_', seflbl, '.png'),
      width=20, height=18, units='in', res=300)
  print(p.now.srag)
  grid::grid.raster(logo, x = 0.999, y = 0.95, just = c('right', 'top'), width = unit(1, 'inches'))
  dev.off()
  if (current){
    png(filename = paste0('./Figs/comparacao/Capitais_comparacao_recente.png'),
        width=20, height=18, units='in', res=300)
    print(p.now.srag)
    grid::grid.raster(logo, x = 0.999, y = 0.95, just = c('right', 'top'), width = unit(1, 'inches'))
    dev.off()
  }
}

compara.semanas <- function(sei, sef, current=F){
  
  if (sei <= 53){
    anoi <- 2020
    seilbl <- sprintf('%02d', sei)
  } else {
    anoi <- 2021
    seilbl <- sprintf('%02d', sei - 53)
  }
  if (sef <= 53){
    anof <- 2020
    seflbl <- sprintf('%02d', sef)
  } else {
    anof <- 2021
    seflbl <- sprintf('%02d', sef - 53)
  }
  baseseilbl <- paste('semana', seilbl)
  baseseflbl <- paste('semana', seflbl)
  
  pred.capitais <- readRDS(paste0('capitais_', anoi, '_', sei, '.rds')) %>%
    filter(grupo_jur == 0) %>%
    mutate(base = baseseilbl)
  pred.capitais <- readRDS(paste0('capitais_', anof, '_', sef, '.rds')) %>%
    filter(grupo_jur == 0) %>%
    mutate(base = baseseflbl) %>%
    rbind(pred.capitais) %>%
    mutate(base = factor(base, levels = c(baseseflbl, baseseilbl)))
  
  mun.list <- pred.capitais %>%
    filter(grupo_jur == 0) %>%
    select(CO_MUN_RES_nome) %>%
    unique()
  
  for (mun in mun.list$CO_MUN_RES_nome){
    plot.compar(mun=mun, df=pred.capitais, seilbl = sei, seflbl = sef, current = current)
  }
  
  plot.geo.facet(sei, sef, pred.capitais, current = current)
}
