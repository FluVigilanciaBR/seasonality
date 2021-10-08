library(tidyverse)
library(ggplot2)
library(colorRamps)
library(sf)
library(grid)
library(lemon)
library(magick)
library(RColorBrewer)
source('../report/theme.publication.R')

data_folder <- '../data/'
info.logo <- image_read('../report/Figs/infogripe.png')

geomacsaud <- st_read('../data/mapa_MRS_indicadores.gpkg')
df <- read.csv(paste0(data_folder, 'municip_macsaud_regmetr.csv')) %>%
  filter(!is.na(CO_MACSAUD) & CO_UF != 0) %>%
  select(CO_MACSAUD, DS_NOMEPAD_macsaud, DS_ABREV_macsaud, CO_UF, DS_UF_SIGLA) %>%
  mutate(CO_MACSAUD = as.character(CO_MACSAUD),
         CO_UF = as.character(CO_UF)) %>%
  unique()
geomacsaud <- geomacsaud %>%
  left_join(df %>% select(-CO_UF), by = c('CO_MACSAUD' = 'CO_MACSAUD'))

num.macsaud <- df %>%
  group_by(CO_UF) %>%
  summarise(num = n(), sigla = unique(DS_UF_SIGLA))

plot.macsaud <- function(uf){
  n <- num.macsaud %>%
    filter(CO_UF == uf) %>%
    select(num) %>%
    as.integer()
  sigla <- num.macsaud %>%
    filter(CO_UF == uf) %>%
    select(sigla) %>%
    as.character()
  if (n > 8){
    getPalette <- colorRampPalette(colorblind_pal()(8))
  } else {
    getPalette <- colorblind_pal()
  }
  p <-  geomacsaud %>%
    filter(!is.na(DS_ABREV_macsaud) & UFCOD == uf) %>%
    arrange(DS_ABREV_macsaud) %>%
    ggplot() +
    geom_sf(aes(fill=DS_ABREV_macsaud),size=0.1) +
    scale_x_continuous(expand=c(0.01,0)) +  
    scale_y_continuous(expand=c(0.01,0)) +  
    scale_fill_manual(values = str_replace(getPalette(n), '#000000', '#353a42'), name=NULL) +
    theme_map() +
    theme_Publication() +
    theme(plot.margin=unit(c(1,0,15,0), units='pt'),
          axis.text.x=element_text(angle=30, size=rel(.8)),
          axis.text.y=element_text(angle=30, size=rel(.8)),
          legend.margin = margin(1,1,0,0, unit='pt'),
          legend.key.size = unit(12, 'pt'),
          legend.text = element_text(family = 'Roboto', size = rel(.95)))
  png(paste0('./Figs/MACSAUD/Mapa_macrorregioes_saude_', sigla, '.png'),
      height = 4, width = 6, units = 'in', res = 100)
  print(p)
  grid::grid.raster(info.logo, x = 0.001, y = 0.001, just = c('left', 'bottom'), width = unit(.8, 'inches'))
  dev.off()
}

# for (uf in num.macsaud$CO_UF){
#   plot.macsaud(uf)
# }


plot.tendencia <- function(i, geom.tendencia, subtitle=element_blank()){
  fill.var <- c('tendencia.3s', 'tendencia.6s')
  plot.title <- c('curto prazo\n(últimas 3 semanas)', 'longo prazo\n(últimas 6 semanas)')
  plot.subtitle <- subtitle
  p <-  geom.tendencia %>%
    ggplot() +
    geom_sf(aes(fill=factor(get(fill.var[i]), levels=c(-1, -.5, 0, .5, 1))),
            size=0.1) +
    scale_x_continuous(expand=c(0.01,0)) +
    scale_y_continuous(expand=c(0.01,0)) +
    scale_fill_brewer(palette='BrBG',
                      direction=-1,
                      breaks=c(-1, -.5, 0., .5, 1),
                      labels=c('Prob. queda\n> 95%',
                               'Prob. queda\n> 75%',
                               'Estabilidade./\noscilação',
                               'Prob. cresc.\n> 75%',
                               'Prob. cresc.\n> 95%'),
                      drop=F,
                      name=NULL) +
    guides(fill = guide_legend(reverse=T)) +
    ggtitle(plot.title[i], subtitle=plot.subtitle) +
    theme_Publication() +
    theme(plot.margin=unit(c(1,0,15,0), units='pt'),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          legend.margin = margin(1,1,0,0, unit='pt'),
          legend.key.size = unit(12, 'pt'),
          legend.text = element_text(family = 'Roboto', size = rel(.95)),
          plot.title = element_text(size = rel(.95)))
  
  return(p)
}

plot.transmission.threshold <- function(geom.thres, subtitle=element_blank()){
  fill.lbl <- c('Pré-epidêmica', 'Epidêmica', 'Alta', 'Muito Alta', 'Extremamente Alta')
  plot.title <- 'Transmissão comunitária de vírus respiratórios segundo SRAG'
  plot.subtitle <- subtitle
  p <-  geom.thres %>%
    ggplot() +
    geom_sf(aes(fill=factor(nivel, levels=c(0, 1, 2, 3, 4), labels=fill.lbl)),
            size=0.1) +
    scale_x_continuous(expand=c(0.01,0)) +
    scale_y_continuous(expand=c(0.01,0)) +
    scale_fill_manual(values=c('#e0f3f8','#ffffbf','#fee090','#fc8d59', '#d73027'),
                      drop=F,
                      name=NULL) +
    ggtitle(plot.title, subtitle=plot.subtitle) +
    theme_Publication() +
    theme(plot.margin=unit(c(0,0,0,0), units='pt'),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          legend.margin = margin(0,0,0,0, unit='pt'),
          legend.position = 'bottom',
          legend.direction = 'horizontal',
          legend.key.size = unit(12, 'pt'),
          legend.text = element_text(family = 'Roboto', size = rel(.8)),
          plot.title = element_text(size = rel(.95)),
          strip.background = element_blank(),
          strip.text = element_text(face='plain'))
  
  return(p)
  
}

brazil.shp <- sf::st_read(dsn='../report/Figs', layer='Brasil', stringsAsFactors = F)
brazil.shp$CD_GEOCODU <- as.integer(brazil.shp$CD_GEOCODU)
plot.macsaude.tendencia <- function(uf, df, orientation='landscape'){
  
  plt.date <- df %>%
    select(Date) %>%
    unique() %>%
    as.integer()
  
  fill.var <- c('tendencia.3s', 'tendencia.6s')
  plot.title <- c('curto prazo\n(últimas 3 semanas)', 'longo prazo\n(últimas 6 semanas)')
  if (orientation != 'landscape'){
    plt.height = 10
    plt.width = 8
    nrow = 2
    ncol = 1
  } else {
    plt.height = 4
    plt.width = 10
    nrow = 1
    ncol = 2
  }
  
  suffix <- ''
  if (uf %in% c('BR', 0)){
    sigla <- 'BR'
    
    geomacsaud.tendencia <- geomacsaud %>%
      left_join(df %>% mutate(CO_MACSAUD = as.character(CO_MACSAUD), by='CO_MACSAUD'))
    
    if (orientation == 'landscape'){
      suffix <- '_horizontal'
    }
  } else {
    sigla <- geomacsaud %>%
      st_drop_geometry() %>%
      filter(UFCOD == as.character(uf)) %>%
      select(DS_UF_SIGLA) %>%
      unique()
    
    geomacsaud.tendencia <- geomacsaud %>%
      filter(!is.na(DS_ABREV_macsaud) & UFCOD == as.character(uf)) %>%
      left_join(df %>% mutate(CO_MACSAUD = as.character(CO_MACSAUD), by='CO_MACSAUD'))
  }
  
  p <- lapply(c(1,2), plot.tendencia, geom.tendencia = geomacsaud.tendencia)
  
  if (sigla == 'BR'){
    p[[1]] <- p[[1]] +
      geom_sf(data=brazil.shp, size=.4, aes(fill=NA))
    p[[2]] <- p[[2]] +
      geom_sf(data=brazil.shp, size=.4, aes(fill=NA))
  }
  png(paste0('./Figs/MACSAUD/Mapa_macrorregioes_saude_', sigla, '_tendencia', suffix,'.png'),
      height = plt.height, width = plt.width, units = 'in', res = 100)
  grid_arrange_shared_legend(p[[1]], p[[2]], ncol = ncol, nrow = nrow, position = 'right')
  grid::grid.raster(info.logo, x = 0.001, y = 0.001, just = c('left', 'bottom'), width = unit(.8, 'inches'))
  dev.off()
  
  png(paste0('./Figs/MACSAUD/Mapa_macrorregioes_saude_', sigla, '_tendencia', suffix, '_se', plt.date,'.png'),
      height = plt.height, width = plt.width, units = 'in', res = 100)
  grid_arrange_shared_legend(p[[1]], p[[2]], ncol = ncol, nrow = nrow, position = 'right')
  grid::grid.raster(info.logo, x = 0.001, y = 0.001, just = c('left', 'bottom'), width = unit(.8, 'inches'))
  dev.off()
  
}

plot.ufs.tendencia <- function(df, tgt.col='CO_UF', fpath = './Figs/Capitais/Mapa_capitais_tendencia.png'){
  fill.var <- c('tendencia.3s', 'tendencia.6s')
  plot.title <- c('curto prazo\n(últimas 3 semanas)', 'longo prazo\n(últimas 6 semanas)')
  geom.shp <- brazil.shp %>%
    left_join(df, by=c('CD_GEOCODU' = tgt.col))
  
  p <- lapply(c(1,2), plot.tendencia, geom.tendencia=geom.shp)
  png(fpath,
      height = 4, width = 10, units = 'in', res = 100)
  grid_arrange_shared_legend(p[[1]], p[[2]], ncol = 2, nrow = 1, position = 'right')
  grid::grid.raster(info.logo, x = 0.001, y = 0.001, just = c('left', 'bottom'), width = unit(.8, 'inches'))
  dev.off()
}


plot.macsaude.nivel <- function(df){
  
  geomacsaud.tendencia <- geomacsaud %>%
    left_join(df %>% 
                filter(Date > max(df$Date) - 30) %>%
                mutate(CO_MACSAUD = as.character(CO_MACSAUD), by='CO_MACSAUD'))
  maxdate <- max(df$Date)
  epi.week <- df$epiweek[df$Date == maxdate]
  epi.year <- df$epiyear[df$Date == maxdate]
  
  plt.height = 6
  plt.width = 6
  p <- plot.transmission.threshold(geomacsaud.tendencia %>% filter(Date == maxdate), subtitle=paste('Semana epidemiológica', epi.week, epi.year, sep=' ')) + geom_sf(data=brazil.shp, size=.4, alpha=0)
  png('./Figs/MACSAUD/Mapa_transmissao.png', height = plt.height, width = plt.width, units='in', res=100)
  print(p)
  grid::grid.raster(info.logo, x = 0.999, y = 0.04, just = c('right', 'bottom'), width = unit(.8, 'inches'))
  dev.off()
  
  plt.height = 10
  plt.width = 8
  p <- plot.transmission.threshold(geomacsaud.tendencia %>% filter(Date >= maxdate-30), subtitle=paste('Dados até a semana epi.', epi.week, epi.year, sep=' ')) + facet_wrap(~sabado, nrow=5)
  png('./Figs/MACSAUD/Mapa_transmissao_serie.png', height = plt.height, width = plt.width, units='in', res=100)
  print(p)
  grid::grid.raster(info.logo, x = 0.999, y = 0.001, just = c('right', 'bottom'), width = unit(.8, 'inches'))
  dev.off()
  
  
}
