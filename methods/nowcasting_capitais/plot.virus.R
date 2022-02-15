suppressWarnings(suppressPackageStartupMessages(library(lubridate)))
suppressWarnings(suppressPackageStartupMessages(library(foreign)))
suppressWarnings(suppressPackageStartupMessages(library(data.table)))
suppressWarnings(suppressPackageStartupMessages(library("tidyverse")))
suppressWarnings(suppressPackageStartupMessages(library(magick)))
suppressWarnings(suppressPackageStartupMessages(library(grid)))
suppressWarnings(suppressPackageStartupMessages(library(cowplot)))
suppressWarnings(suppressPackageStartupMessages(library(geofacet)))
options(dplyr.summarise.inform=F) 
source("../data_filter/episem.R")
source('plot.ts.tendencia.R')

int_breaks_rounded <- function(n = 5, min.n=2, ...){
  fxn <- function(x){
    breaks <- round(pretty(x, n, min.n, ...),0)
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

logo <- image_read('../report/Figs/infogripe.png')

# Codes for DF's central health region
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
parser$add_argument("-t", "--type", type="character", default='srag',
                    help="Type of data input [srag, sragflu, obitoflu]. Default %(default)s")
parser$add_argument("-f", "--filtertype", type="character", default='sragnofever',
                    help="Type of filter [srag, sragnofever, hospdeath]. Default %(default)s")
parser$add_argument("-d", "--date", type="character", default=format(Sys.Date(), '%Y-%m-%d'),
                    help="Date to use as base, in format YYYY-MM-DD [default Sys.Date()]")
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults,
args <- parser$parse_args()

if (!(args$filtertype %in% c('srag', 'sragnofever', 'hospdeath'))){
  stop(paste0('Invalid filter type', args$filtertype))
}


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

# Tabela cadmun no git
tblCADMUN <- read.csv("../data/municip_macsaud_regmetr.csv", stringsAsFactors = F)
tblmumpop <- read.csv("../data/tab_datasus_pop_faixateraria.csv", stringsAsFactors = F)
tblmumpop[,4:ncol(tblmumpop)] <- tblmumpop %>%
  select(-codmun, -municipio, -ano) %>%
  mutate_all(~ as.character(.)) %>%
  mutate_all(~ gsub('\\.', '', .)) %>%
  mutate_all(~ as.numeric(.))

tblCADMUN <- tblmumpop %>%
  filter(ano == 2019) %>%
  transmute(CO_MUNICIP = codmun, Populacao = Total) %>%
  right_join(tblCADMUN)

uf.list <- tblCADMUN %>% 
  filter(CO_STATUS_municip == 'ATIVO') %>%
  select(CO_UF, DS_UF_SIGLA, Populacao) %>%
  group_by(CO_UF, DS_UF_SIGLA) %>%
  summarise(Populacao = sum(Populacao)) %>%
  rbind(data.frame(list(CO_UF = 0, DS_UF_SIGLA = 'BR', Populacao = 0)))
uf.list$Populacao[uf.list$CO_UF == 0] <- sum(uf.list$Populacao)
uf.list <- read.csv("../data/regioesclimaticas.csv", stringsAsFactors = F) %>%
  transmute(DS_UF_NOME=UF, DS_UF_SIGLA=Sigla) %>%
  right_join(uf.list, by='DS_UF_SIGLA')

rm(tblCADMUN)
rm(tblmumpop)
gc(verbose=FALSE)

# Suffix filter based on filtertype
suff_list <- list('srag' = '', 'sragnofever' = '_sragnofever', 'hospdeath' = '_hospdeath')
suff <- suff_list[args$filtertype]
preff_list <- list(srag = 'srag', sragnofever = '.', hospdeath = 'hospdeath')
preff <- as.character(preff_list[args$filtertype])
fx.breaks=c(0, 5, 12, 18, seq(30, 80, 10), 140)
fx.labels=c('0-4', '5-11', '12-17','18-29', '30-39', '40-49', '50-59',
            '60-69', '70-79',
            '80+')

path_file <- paste0("../clean_data/clean_data_", args$type, suff, "_epiweek.csv.gz")
dados_full <- fread( path_file, stringsAsFactors = F, data.table=F) %>%
  filter(DT_SIN_PRI_epiyear >= 2020) %>%
  select(NU_NOTIFIC,
         CO_MUN_NOT,
         SG_UF_NOT,
         idade_em_anos,
         DT_SIN_PRI,
         DT_SIN_PRI_epiweek,
         DT_SIN_PRI_epiyear,
         DT_NOTIFIC,
         SARS2,
         RINO,
         BOCA,
         METAP,
         PARA1,
         PARA2,
         PARA3,
         PARA4,
         ADNO,
         VSR,
         OTHERS,
         FLU_A,
         FLU_B,
         FLU_LAB,
         FLU_CLINIC,
         TP_FLU_PCR,
         TP_FLU_AN,
         TESTED,
         POSITIVE)
dados_full <- dados_full %>%
  distinct(NU_NOTIFIC, CO_MUN_NOT, DT_NOTIFIC, .keep_all = T) %>%
  mutate(DT_SIN_PRI = ymd(DT_SIN_PRI),
         fx_etaria = as.character(cut(idade_em_anos,
                                      breaks=fx.breaks,
                                      right=F,
                                      labels=fx.labels
         )
         )
  )

epiweekmax <- ifelse(lyear==2020, 0, as.integer(lastepiweek(lyear-1)))
l.addepiweek <- list('2020'=0, '2021'=53, '2022'=52)                        
epiweek.shift <- sum(unlist(l.addepiweek[as.character(seq(2020, lyear-1))]))
today.week.ori <- today.week
if (lyear > 2020){
  today.week.ori <- today.week
  today.week <- today.week + epiweekmax + epiweek.shift
}
dados_full <- dados_full %>%
  mutate(epiyear = DT_SIN_PRI_epiyear,
         epiweek = DT_SIN_PRI_epiweek,
         DT_SIN_PRI_epiweek = case_when(
           epiyear == 2020 ~ as.integer(epiweek),
           epiyear == 2021 ~ as.integer(epiweek) + as.integer(l.addepiweek['2021']),
           epiyear == 2022 ~ as.integer(epiweek) + as.integer(l.addepiweek['2021']) + as.integer(l.addepiweek['2022'])
         )
  ) %>%
  filter(DT_SIN_PRI_epiweek <= today.week) %>%
  mutate(postv = case_when(
    if_any(c(SARS2,
             RINO,
             BOCA,
             METAP,
             PARA1,
             PARA2,
             PARA3,
             PARA4,
             ADNO,
             VSR,
             OTHERS,
             FLU_A,
             FLU_B,
             FLU_LAB),
           ~ . == 1) ~ 1,
    TRUE ~ 0),
    FLU_LAB = case_when(
      if_any(c(FLU_A, FLU_B), ~ . == 1) ~ 1,
      TRUE ~ 0)
  ) 
epiweek.table <- dados_full %>%
  select(DT_SIN_PRI_epiweek, epiweek, epiyear) %>%
  unique() %>%
  arrange(DT_SIN_PRI_epiweek)

epilbls <- c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52)
xbreaks <- c(epilbls + epiweek.shift, epilbls + epiweekmax + epiweek.shift)
xlbls <- c(epilbls, epilbls)
xlimits <- c(today.week-15, today.week)
  
dados.ag <- dados_full %>%
  group_by(DT_SIN_PRI_epiweek) %>%
  summarise(SRAG=n(),
            SARS2=sum(SARS2, na.rm=T),
            VSR=sum(VSR, na.rm=T),
            FLU=sum(FLU_LAB, na.rm=T),
            FLU_A=sum(FLU_A, na.rm=T),
            FLU_B=sum(FLU_B, na.rm=T),
            RINO=sum(RINO, na.rm=T),
            ADNO=sum(ADNO, na.rm=T),
            BOCA=sum(BOCA, na.rm=T),
            METAP=sum(METAP, na.rm=T),
            PARA1=sum(PARA1, na.rm=T),
            PARA2=sum(PARA2, na.rm=T),
            PARA3=sum(PARA3, na.rm=T),
            PARA4=sum(PARA4, na.rm=T),
            OUTROS=sum(OTHERS, na.rm=T),
            positivos=sum(postv, na.rm=T)) %>%
  ungroup() %>%
  mutate(SG_UF_NOT = 0, fx_etaria='Total')
dados.ag <- dados_full %>%
  filter(!is.na(fx_etaria)) %>%
  group_by(DT_SIN_PRI_epiweek, fx_etaria) %>%
  summarise(SRAG=n(),
            SARS2=sum(SARS2, na.rm=T),
            VSR=sum(VSR, na.rm=T),
            FLU=sum(FLU_LAB, na.rm=T),
            FLU_A=sum(FLU_A, na.rm=T),
            FLU_B=sum(FLU_B, na.rm=T),
            RINO=sum(RINO, na.rm=T),
            ADNO=sum(ADNO, na.rm=T),
            BOCA=sum(BOCA, na.rm=T),
            METAP=sum(METAP, na.rm=T),
            PARA1=sum(PARA1, na.rm=T),
            PARA2=sum(PARA2, na.rm=T),
            PARA3=sum(PARA3, na.rm=T),
            PARA4=sum(PARA4, na.rm=T),
            OUTROS=sum(OTHERS, na.rm=T),
            positivos=sum(postv, na.rm=T)) %>%
  ungroup() %>%
  mutate(SG_UF_NOT=0) %>%
  bind_rows(dados.ag)
dados.ag <- dados_full %>%
  group_by(DT_SIN_PRI_epiweek, SG_UF_NOT) %>%
  summarise(SRAG=n(),
            SARS2=sum(SARS2, na.rm=T),
            VSR=sum(VSR, na.rm=T),
            FLU=sum(FLU_LAB, na.rm=T),
            FLU_A=sum(FLU_A, na.rm=T),
            FLU_B=sum(FLU_B, na.rm=T),
            RINO=sum(RINO, na.rm=T),
            ADNO=sum(ADNO, na.rm=T),
            BOCA=sum(BOCA, na.rm=T),
            METAP=sum(METAP, na.rm=T),
            PARA1=sum(PARA1, na.rm=T),
            PARA2=sum(PARA2, na.rm=T),
            PARA3=sum(PARA3, na.rm=T),
            PARA4=sum(PARA4, na.rm=T),
            OUTROS=sum(OTHERS, na.rm=T),
            positivos=sum(postv, na.rm=T)) %>%
  ungroup() %>%
  mutate(fx_etaria='Total') %>%
  bind_rows(dados.ag)
dados.ag <- dados_full %>%
  filter(!is.na(fx_etaria)) %>%
  group_by(DT_SIN_PRI_epiweek, SG_UF_NOT, fx_etaria) %>%
  summarise(SRAG=n(),
            SARS2=sum(SARS2, na.rm=T),
            VSR=sum(VSR, na.rm=T),
            FLU=sum(FLU_LAB, na.rm=T),
            FLU_A=sum(FLU_A, na.rm=T),
            FLU_B=sum(FLU_B, na.rm=T),
            RINO=sum(RINO, na.rm=T),
            ADNO=sum(ADNO, na.rm=T),
            BOCA=sum(BOCA, na.rm=T),
            METAP=sum(METAP, na.rm=T),
            PARA1=sum(PARA1, na.rm=T),
            PARA2=sum(PARA2, na.rm=T),
            PARA3=sum(PARA3, na.rm=T),
            PARA4=sum(PARA4, na.rm=T),
            OUTROS=sum(OTHERS, na.rm=T),
            positivos=sum(postv, na.rm=T)) %>%
  ungroup() %>%
  bind_rows(dados.ag) %>%
  complete(SG_UF_NOT, DT_SIN_PRI_epiweek=1:today.week, fx_etaria,
           fill=list(SRAG=0,
                     SARS2=0,
                     VSR=0,
                     FLU=0,
                     FLU_A=0,
                     FLU_B=0,
                     RINO=0,
                     ADNO=0,
                     BOCA=0,
                     METAP=0,
                     PARA1=0,
                     PARA2=0,
                     PARA3=0,
                     PARA4=0,
                     OUTROS=0,
                     positivos=0))
dados.ag <- dados.ag %>%
  mutate(fx_etaria=factor(fx_etaria,
                          levels = c(fx.labels, 'Total')
                          )
         ) %>%
  left_join(uf.list %>% select(CO_UF, DS_UF_SIGLA),
            by=c('SG_UF_NOT'='CO_UF'))
rm(dados_full)
gc()

dados.ag %>%
  left_join(epiweek.table, by=c('DT_SIN_PRI_epiweek')) %>%
  select(-DT_SIN_PRI_epiweek) %>%
  mutate(`Semana epidemiológica` = epiweek,
         `Ano epidemiológico` = epiyear) %>%
  write_csv2('casos_semanais_fx_etaria_virus_sem_filtro_febre.csv')

plt.age.virus <- function(uf, df=dados.ag){
  uf.props <- uf.list %>%
    filter(CO_UF == uf) %>%
    select(DS_UF_NOME, DS_UF_SIGLA)
  plt <- df %>%
    filter(SG_UF_NOT==uf, fx_etaria!='Total', DT_SIN_PRI_epiweek > today.week-16) %>%
    mutate(OUTROS=BOCA+
             METAP+
             PARA1+
             PARA2+
             PARA3+
             PARA4+
             OUTROS
    ) %>%
    pivot_longer(cols=c(SRAG, SARS2, VSR, FLU_A, FLU_B, RINO, ADNO, OUTROS),
                 names_to='dado',
                 values_to='Casos') %>%
    mutate(dado=factor(dado, levels=c('SRAG',
                                      'SARS2',
                                      'VSR',
                                      'FLU_A',
                                      'FLU_B',
                                      'RINO',
                                      'ADNO',
                                      'OUTROS'),
                       labels=c('SRAG em geral',
                                'SARS-CoV-2',
                                'VSR',
                                'FLU A',
                                'FLU B',
                                'RINO',
                                'ADENO',
                                'OUTROS'))) %>%
    ggplot(aes(x=DT_SIN_PRI_epiweek, y=Casos, color=dado)) +
    geom_rect(aes(xmin=today.week-4, xmax=today.week, ymin=0, ymax=Inf), fill='lightgray', size=0, alpha=.5, inherit.aes=F) +
    geom_line(size=1.2) +
    scale_color_colorblind(name='') +
    scale_x_continuous(breaks = xbreaks,
                       labels = xlbls,
                       minor_breaks = waiver(),
                       limits = xlimits) +
    scale_y_continuous(breaks = int_breaks_rounded()) +
    theme_Publication(base_size = 14, base_family='Roboto') +
    facet_wrap(~fx_etaria, scales = "free_y") +
    labs(
      x = "Semana de primeiros sintomas",
      y = "Casos de SRAG",
      title = uf.props$DS_UF_NOME,
      subtitle = paste0("Novos casos semanais por faixa etária. Dados até a semana ",
                        today.week.ori, ' ',
                        lyear,
                        '.\nPara semanas recentes os dados são parciais (área cinza).')) +
    theme(panel.grid.minor.x = element_line(colour="#f0f0f0",
                                            linetype=2),
          axis.text.x = element_text(angle=45, hjust = 1),
          legend.position='bottom',
          legend.direction = 'horizontal',
          legend.key.width = unit(10, units = 'pt')
    )
  png(filename = paste0(preff,"/Figs/UF/fig_", uf.props$DS_UF_SIGLA, "_virus_lab.png"),
      width=9, height=9, units='in', res=200)
  print(plt)
  grid::grid.raster(logo, x = 0.999, y = 0.99, just = c('right', 'top'), width = unit(1, 'inches'))
  dev.off()
}

map(uf.list$CO_UF, plt.age.virus)

plt <- dados.ag %>%
  filter(SG_UF_NOT!=0, fx_etaria=='0-4', DT_SIN_PRI_epiweek > today.week-16) %>%
  mutate(OUTROS=BOCA+
           METAP+
           PARA1+
           PARA2+
           PARA3+
           PARA4+
           OUTROS
  ) %>%
  pivot_longer(cols=c(SRAG, SARS2, VSR, FLU_A, FLU_B, RINO, ADNO, OUTROS),
               names_to='dado',
               values_to='Casos') %>%
  mutate(dado=factor(dado, levels=c('SRAG',
                                    'SARS2',
                                    'VSR',
                                    'FLU_A',
                                    'FLU_B',
                                    'RINO',
                                    'ADNO',
                                    'OUTROS'),
                     labels=c('SRAG em geral',
                              'SARS-CoV-2',
                              'VSR',
                              'FLU A',
                              'FLU B',
                              'RINO',
                              'ADENO',
                              'OUTROS'))) %>%
  ggplot(aes(x=DT_SIN_PRI_epiweek, y=Casos, color=dado)) +
  geom_rect(aes(xmin=today.week-4, xmax=today.week, ymin=0, ymax=Inf), fill='lightgray', size=0, alpha=.5, inherit.aes=F) +
  geom_line(size=1.2) +
  scale_color_colorblind(name='') +
  scale_x_continuous(breaks = xbreaks,
                     labels = xlbls,
                     minor_breaks = waiver(),
                     limits = xlimits) +
  scale_y_continuous(breaks=int_breaks_rounded()) +
  theme_Publication(base_size = 22, base_family='Roboto') +
  facet_geo(~DS_UF_SIGLA, grid='br_states_grid1', scales = "free_y") +
  labs(
    x = "Semana de primeiros sintomas",
    y = "Casos de SRAG",
    subtitle = paste0("Novos casos semanais em crianças 0-4 anos. Dados até a semana ",
                      today.week.ori,
                      ' ',
                      lyear,
                      '.\nPara semanas recentes os dados são parciais (área cinza).')) +
  theme(panel.grid.minor.x = element_line(colour="#f0f0f0",
                                          linetype=2),
        legend.position = c(.92,.2),
        legend.key.width = unit(22, units = 'pt'),
        legend.text=element_text(family='Roboto', size=rel(1)),
        axis.text.x = element_text(angle=45, hjust=1)
  )
png(filename = paste0(preff,"/Figs/UF/fig_UFs_virus_lab_0_4.png"),
    width=18, height=26, units='in', res=100)
print(plt)
grid::grid.raster(logo, x = 0.999, y = 0.99, just = c('right', 'top'), width = unit(1.5, 'inches'))
dev.off()

plt <- dados.ag %>%
  filter(SG_UF_NOT!=0, fx_etaria=='Total', DT_SIN_PRI_epiweek>today.week-16) %>%
  mutate(OUTROS=BOCA+
           METAP+
           PARA1+
           PARA2+
           PARA3+
           PARA4+
           OUTROS
  ) %>%
  pivot_longer(cols=c(SRAG, SARS2, VSR, FLU_A, FLU_B, RINO, ADNO, OUTROS),
               names_to='dado',
               values_to='Casos') %>%
  mutate(dado=factor(dado, levels=c('SRAG',
                                    'SARS2',
                                    'VSR',
                                    'FLU_A',
                                    'FLU_B',
                                    'RINO',
                                    'ADNO',
                                    'OUTROS'),
                     labels=c('SRAG em geral',
                              'SARS-CoV-2',
                              'VSR',
                              'FLU A',
                              'FLU B',
                              'RINO',
                              'ADENO',
                              'OUTROS'))) %>%
  ggplot(aes(x=DT_SIN_PRI_epiweek, y=Casos, color=dado)) +
  geom_rect(aes(xmin=today.week-4, xmax=today.week, ymin=0, ymax=Inf), fill='lightgray', size=0, alpha=.5, inherit.aes=F) +
  geom_line(size=1.2) +
  scale_color_colorblind(name='') +
  scale_x_continuous(breaks = xbreaks,
                     labels = xlbls,
                     minor_breaks = waiver(),
                     limits=xlimits) +
  theme_Publication(base_size = 24, base_family='Roboto') +
  facet_geo(~DS_UF_SIGLA, grid='br_states_grid1', scales = "free_y") +
  labs(
    x = "Semana de primeiros sintomas",
    y = "Casos de SRAG",
    subtitle = paste0("Novos casos semanais na população em geral. Dados até a semana ",
                      today.week.ori,
                      ' ',
                      lyear,
                      '.\nPara semanas recentes os dados são parciais (área cinza).')) +
  theme(panel.grid.minor.x = element_line(colour="#f0f0f0",
                                          linetype=2),
        legend.position = c(.92,.2),
        legend.key.width = unit(22, units = 'pt'),
        legend.text=element_text(family='Roboto', size=rel(1)),
        axis.text.x = element_text(angle=45, hjust=1)
  )
png(filename = paste0(preff,"/Figs/UF/fig_UFs_virus_lab.png"),
    width=18, height=26, units='in', res=100)
print(plt)
grid::grid.raster(logo, x = 0.999, y = 0.99, just = c('right', 'top'), width = unit(1.5, 'inches'))
dev.off()

