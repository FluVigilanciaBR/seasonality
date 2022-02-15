require(data.table)
require(tidyverse)
require(geofacet)
require(purrr)
require(ggplot2)
require(magick)
require(grid)
source("../data_filter/episem.R")
source('../report/theme.publication.R')
logo <- image_read('../report/Figs/infogripe.png')

tblCADMUN <- read.csv("../data/municip_macsaud_regmetr.csv", stringsAsFactors = F) %>%
  filter(IN_CAPITAL %in% (c('E', 'F'))) %>%
  select(CO_UF, CO_MUNICIP, DS_UF_SIGLA) %>% distinct()

bsb_ras <- c(
  530005,
  530080,
  530050,
  539931,
  539925,
  530015,
  530090
)

## Read command line arguments
suppressPackageStartupMessages(library("argparse"))
# create parser object
parser <- ArgumentParser()
# specify our desired options
# by default ArgumentParser will add an help option
parser$add_argument("-d", "--date", type="character", default=format(Sys.Date(), '%Y-%m-%d'),
                    help="Date to use as base, in format YYYY-MM-DD [default Sys.Date()]")
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults,
args <- parser$parse_args()

# Latest week with closed counts on DT_DIGITA is actualy the previous one
if (args$date == 'max'){
  today <- as.Date(as.character(max(d.orig$date)), format='%Y%m%d')
  print(paste0('Base date: ', today))
  today <- as.Date(today - 7,origin = '1970-01-01')
  today <- episem(today)
} else {
  today <- as.Date(args$date)
  print(paste0('Base date: ', today))
  today <- as.Date(today - 7,origin = '1970-01-01')
  today <- episem(today)
}
lyear <- as.integer(strsplit(today, 'W')[[1]][1])
today.week <- as.integer(strsplit(today, 'W')[[1]][2])

epiweekmax <- as.integer(lastepiweek(lyear-1))
today.week.ori <- today.week
today.week <- today.week + epiweekmax
epiweek.start <- today.week - 40

dados_full <- fread('../clean_data/clean_data_srag_hospdeath_epiweek.csv.gz', data.table=F) %>%
  filter(DT_SIN_PRI_epiyear >= ifelse(today.week.ori<41, lyear-1, lyear)) %>%
  select(SG_UF_NOT,
         CO_MUN_NOT,
         DT_SIN_PRI_epiweek,
         DT_SIN_PRI_epiyear,
         DT_INTERNA_epiweek,
         DT_INTERNA_epiyear,
         DT_DIGITA_epiweek,
         DT_DIGITA_epiyear,
         SinPri2Digita_DelayWeeks,
         SinPri2Interna_DelayWeeks,
         NU_NOTIFIC,
         DT_NOTIFIC
  ) %>%
  distinct(NU_NOTIFIC, DT_NOTIFIC, CO_MUN_NOT, .keep_all = T) %>%
  mutate(
    DT_SIN_PRI_epiweek = case_when(
      DT_SIN_PRI_epiyear == lyear ~ as.numeric(DT_SIN_PRI_epiweek) + epiweekmax,
      TRUE ~ as.numeric(DT_SIN_PRI_epiweek)
    ),
    DT_INTERNA_epiweek = case_when(
      DT_INTERNA_epiyear == lyear ~ as.numeric(DT_INTERNA_epiweek) + epiweekmax,
      TRUE ~ as.numeric(DT_INTERNA_epiweek)
    ),
    DT_DIGITA_epiweek = case_when(
      DT_DIGITA_epiyear == lyear ~ as.numeric(DT_DIGITA_epiweek) + epiweekmax,
      TRUE ~ as.numeric(DT_DIGITA_epiweek)
    ),
    Interna2Digita_DelayWeeks = DT_DIGITA_epiweek - DT_INTERNA_epiweek,
    CO_MUN_NOT = case_when(
      as.integer(SG_UF_NOT) == 53 ~ as.numeric(530010),
      TRUE ~ as.numeric(CO_MUN_NOT)
    )
  )
gc()

plot.oportunidade <- function(dados_full,
                              p=c(.8, .9, .95),
                              xmin=1,
                              xmax=52,
                              save.folder='./Figs/Oportunidades/',
                              subtitle=NULL){
  p_names <- map_chr(p, ~paste0("q", .x*100))
  p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
    set_names(nm = p_names)
  
  qdf <- dados_full %>%
    select(SG_UF_NOT, DT_SIN_PRI_epiweek, SinPri2Digita_DelayWeeks) %>%
    group_by(SG_UF_NOT, DT_SIN_PRI_epiweek) %>%
    summarize_at(vars(SinPri2Digita_DelayWeeks), funs(!!!p_funs)) %>%
    mutate(diferenca = 'sinpri2digita')
  qdf <- dados_full %>%
    select(SG_UF_NOT, DT_SIN_PRI_epiweek, SinPri2Interna_DelayWeeks) %>%
    group_by(SG_UF_NOT, DT_SIN_PRI_epiweek) %>%
    summarize_at(vars(SinPri2Interna_DelayWeeks), funs(!!!p_funs)) %>%
    mutate(diferenca = 'sinpri2interna') %>%
    rbind(qdf)
  qdf <- dados_full %>%
    select(SG_UF_NOT, DT_INTERNA_epiweek, Interna2Digita_DelayWeeks) %>%
    group_by(SG_UF_NOT, DT_INTERNA_epiweek) %>%
    summarize_at(vars(Interna2Digita_DelayWeeks), funs(!!!p_funs)) %>%
    mutate(diferenca = 'interna2digita', DT_SIN_PRI_epiweek = DT_INTERNA_epiweek) %>%
    rbind(qdf)
  
  qdf <- qdf %>%
    left_join(tblCADMUN , by=c('SG_UF_NOT'='CO_UF'))
  
  qdf <- qdf %>%
    pivot_longer(cols=c('q80', 'q90', 'q95'), names_to='quantil', values_to='oportunidade')
  
  epilbls <- c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52)
  xbreaks <- c(epilbls, epilbls + 53)
  xlbls <- c(epilbls, epilbls)
  p.opor <- function(qdf,
                     col_x='DT_INTERNA_epiweek',
                     opor='interna2digita',
                     xmin=20,
                     xmax=64,
                     xtitle='Semana de internação',
                     ytitle='Oportunidade de digitação em relação à internação (semanas epidemiológicas)',
                     title=NULL,
                     subtitle=subtitle
                     ){

    # col_y = col_y = 'oportunidade'
    # col_color = 'quantil'
    p.opor <- qdf %>%
      filter(!!as.name(col_x) >= xmin,
             diferenca == opor) %>%
      ggplot(aes(x=!!as.name(col_x), y=oportunidade, color=quantil)) +
      geom_line() +
      geom_abline(slope=-1, intercept=eval(xmax), linetype=2, color='darkgrey') +
      scale_color_colorblind() +
      ylab(ytitle) +
      xlab(xtitle) +
      scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = c(xmin, xmax)) +
      theme_Publication(base_size = 22, base_family = 'Roboto') +
      geofacet::facet_geo(~DS_UF_SIGLA, grid='br_states_grid1', scale='free_y') +
      theme(legend.position = c(.9, .1),
            legend.text=element_text(family='Roboto', size=rel(1)),
            legend.title = element_text(face='bold'),
            legend.key.width = unit(22, units = 'pt'),
            axis.text.x = element_text(angle=45, hjust=1))
    if (is.null(subtitle)){
      p.opor <- p.opor +
        ggtitle(title)
    } else {
      p.opor <- p.opor +
        ggtitle(title, subtitle = subtitle)
    }
  }
  
  p <- p.opor(qdf %>% filter(oportunidade >= 0),
              col_x='DT_INTERNA_epiweek',
              opor='interna2digita',
              xmin=xmin,
              xmax=xmax,
              xtitle='Semana de internação',
              ytitle='Oportunidade (semanas epidemiológicas)',
              title='Oportunidade de digitação em relação à internação',
              subtitle=subtitle)
  png(filename = file.path(save.folder, 'ufs_interna2digita.png'),
      width=27, height=18, units='in', res=100)
  print(p)
  grid::grid.raster(logo, x = 0.999, y = 0.99, just = c('right', 'top'), width = unit(1.5, 'inches'))
  dev.off()

  p <- p.opor(qdf %>% filter(oportunidade >= 0),
              col_x='DT_SIN_PRI_epiweek',
              opor='sinpri2interna',
              xmin=xmin,
              xmax=xmax,
              xtitle='Semana de primeiros sintomas',
              ytitle='Tempo até internação (semanas epidemiológicas)',
              title='Tempo até internação em relação à 1os sintomas',
              subtitle = subtitle)
  png(filename = file.path(save.folder, 'ufs_sinpri2interna.png'),
      width=27, height=18, units='in', res=100)
  print(p)
  grid::grid.raster(logo, x = 0.999, y = 0.99, just = c('right', 'top'), width = unit(1.5, 'inches'))
  dev.off()

  p <- p.opor(qdf %>% filter(oportunidade >= 0),
              col_x='DT_SIN_PRI_epiweek',
              opor='sinpri2digita',
              xmin=xmin,
              xmax=xmax,
              xtitle='Semana de primeiros sintomas',
              ytitle='Oportunidade (semanas epidemiológicas)',
              title='Oportunidade de digitação em relação à 1os sintomas',
              subtitle = subtitle)
  png(filename = file.path(save.folder, 'ufs_sinpri2digita.png'),
      width=27, height=18, units='in', res=100)
  print(p)
  grid::grid.raster(logo, x = 0.999, y = 0.99, just = c('right', 'top'), width = unit(1.5, 'inches'))
  dev.off()

}

dados_full %>%
  filter(DT_DIGITA_epiweek <= today.week) %>%
  plot.oportunidade(p=c(.8, .9, .95),
                    xmin=epiweek.start,
                    xmax=today.week,
                    subtitle=paste('Dados digitados até a semana epidemiológica', lyear, today.week.ori))

dados_full %>%
  filter(DT_DIGITA_epiweek <= today.week,
         CO_MUN_NOT %in% tblCADMUN$CO_MUNICIP) %>%
  plot.oportunidade(p=c(.8, .9, .95),
                    xmin=epiweek.start,
                    xmax=today.week,
                    subtitle=paste('Dados notificados na capital, digitados até a semana epidemiológica',
                                   lyear,
                                   today.week.ori),
                    save.folder='./Figs/Oportunidades/Capitais')
