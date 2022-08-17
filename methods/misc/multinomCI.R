library(tidyverse)
library(lubridate)
require(DescTools)
source("../data_filter/episem.R")
source('faixas_etarias_plot.R')

# pt_br <- locale(encoding = "latin1", asciify = FALSE)
# 
# colunas <- cols(.default = "c")

# Tabela descritiva dos municípios
tblCADMUN <- read.csv("../data/municip_macsaud_regmetr.csv", stringsAsFactors = F) %>%
  select(CO_MUNICIP, CO_UF, DS_UF_SIGLA) %>%
  filter(CO_UF > 1, CO_UF != 20) %>%
  distinct() %>%
  mutate(DS_UF_SIGLA = case_when(
    CO_UF == 0 ~ 'BR',
    TRUE ~ DS_UF_SIGLA
  ))

# Tabela de população municipal por faixa etária:
dfpop <- read.csv('~/codes/covid-19/csv/populacao_datasus_faixateraria_TOTAL.csv', dec=',')
dfpop[5:ncol(dfpop)] <- lapply(dfpop[5:ncol(dfpop)], function(x) as.numeric(gsub("\\.", "", x)))
tblCADMUN <- tblCADMUN %>%
  left_join(dfpop %>% filter(ano == 2020) %>% select(-sexo, -municipio),
            by=c('CO_MUNICIP' = 'codmun'))
col.idade <- dfpop %>%
  select(-codmun, -municipio, -ano, -sexo) %>%
  names()

# Calculando pop de cada UF a partir das estimativas de cada município
tbl.ufs <- tblCADMUN %>%
  select(CO_UF, DS_UF_SIGLA, all_of(col.idade)) %>%
  group_by(CO_UF, DS_UF_SIGLA) %>%
  summarise_each(funs(sum(., na.rm=TRUE)))

# Calculando pop nacional por faixa etária a partir dos dados de cada UF
# e agregando à tabela de UFs
tbl.ufs <- tbl.ufs %>%
  ungroup() %>%
  select(-CO_UF, -DS_UF_SIGLA) %>%
  group_by() %>%
  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  mutate(CO_UF = 0, DS_UF_SIGLA = 'BR') %>%
  rbind(tbl.ufs)

# Dados de notificação
dadosBR <- read.csv("../clean_data/clean_data_srag_hospdeath_epiweek.csv") %>%  
  filter(DT_SIN_PRI_epiyear >= 2020) %>%
  select(SG_UF,
         SG_UF_NOT,
         DT_SIN_PRI,
         DT_SIN_PRI_epiweek,
         DT_SIN_PRI_epiyear,
         DT_DIGITA,
         DT_DIGITA_epiweek,
         DT_DIGITA_epiyear,
         HOSPITAL,
         EVOLUCAO,
         CLASSI_FIN,
         PCR_SARS2,
         idade_em_anos,
         NU_NOTIFIC,
         DT_NOTIFIC,
         CO_MUN_NOT) %>%
  distinct(NU_NOTIFIC, CO_MUN_NOT, DT_NOTIFIC, .keep_all=T)

# Semana de ínicio
epiweek_start = 12
# Quantidade de semanas a serem descartadas do final da série
epiweek_stop = 3

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

# Transformar a coluna de semana epidemiológica em valor corrido para facilitar
# a construção dos gráficos
epiweekmax <- as.integer(lastepiweek(2020))
today.week.ori <- today.week
if (lyear > 2020){
  today.week.ori <- today.week
  today.week <- today.week + epiweekmax
}
dadosBR <- dadosBR %>%
  mutate(epiyear = DT_SIN_PRI_epiyear,
         epiweek = DT_SIN_PRI_epiweek,
         DT_SIN_PRI_epiweek = case_when(
           DT_SIN_PRI_epiyear > 2020 ~ DT_SIN_PRI_epiweek + epiweekmax,
           TRUE ~ DT_SIN_PRI_epiweek)
         ) %>%
  filter(DT_DIGITA_epiweek <= today.week)

epiweek.table <- dadosBR %>%
  select(DT_SIN_PRI_epiweek, epiweek, epiyear) %>%
  unique() %>%
  arrange(DT_SIN_PRI_epiweek)

epilbls <- c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52)
xbreaks <- c(epilbls, epilbls + epiweekmax)
xlbls <- c(epilbls, epilbls)
xlimits <- c(epiweek_start, today.week - epiweek_stop)

calc.props.and.ic <- function(df,
                              epiweek_start=12,
                              epiweek_end=53,
                              epiweek_ic_stop=48,
                              breaks=c(0,20,40,60,80,150),
                              labels=c("0-19","20-39", "40-59", "60-79", "80+")){
  # Construir tabela de total de casos por semana e faixa etária, para o BR
  dadosBR.ag <- df %>% 
    filter(
      HOSPITAL == 1,
      # CO_MUN == MUN,
      between(DT_SIN_PRI_epiweek, epiweek_start, epiweek_end),
    ) %>% 
    mutate(
      age_cat = cut(idade_em_anos, breaks = breaks, 
                    labels =  labels,
                    right = F)
    ) %>% 
    drop_na(age_cat) %>%
    group_by( DT_SIN_PRI_epiweek, age_cat) %>% 
    #group_by( DT_SIN_PRI_sun, SinPri2Digita_DelayWeeks, age_cat) %>% 
    tally() %>%
    mutate(SG_UF = 0)
  # Construir tabela de total de casos por UF, semana e faixa etária, e agregar à tabela do BR
  dadosBR.ag <- df %>% 
    filter(
      HOSPITAL == 1,
      between(DT_SIN_PRI_epiweek, epiweek_start, epiweek_end),
      between(SG_UF, 10, 60)
    ) %>% 
    mutate(
      age_cat = cut(idade_em_anos, breaks = breaks, 
                    labels =  labels,
                    right = F)
    ) %>% 
    drop_na(age_cat) %>%
    group_by(SG_UF, DT_SIN_PRI_epiweek, age_cat) %>% 
    #group_by( DT_SIN_PRI_sun, SinPri2Digita_DelayWeeks, age_cat) %>% 
    tally() %>%
    rbind(dadosBR.ag) %>%
    ungroup()
  
  # Calcular a incerteza associada a cada observação, retornando os casos observados,
  # e mediana e IC 95% do modelo multinomial
  tmp <- dadosBR.ag %>%
    group_by(SG_UF, DT_SIN_PRI_epiweek) %>%
    mutate(MultinomCI(n)) %>%
    transmute(SG_UF = SG_UF,
              age_cat = age_cat,
              cases = n,
              prop_median = `MultinomCI(n)`[,1],
              prop_li = `MultinomCI(n)`[,2],
              prop_ls = `MultinomCI(n)`[,3]) %>%
    ungroup()
  
  # Transformar a escala em proporções dentro de cada semana e UF
  tmp <- dadosBR.ag %>% 
    group_by(SG_UF, DT_SIN_PRI_epiweek) %>% 
    mutate(Prop = n / sum(n)) %>% ungroup() %>%
    rename(cases = n) %>%
    left_join(tmp, by=c('SG_UF','DT_SIN_PRI_epiweek', 'age_cat', 'cases')) %>%
    complete(SG_UF, DT_SIN_PRI_epiweek, age_cat, fill = list(cases = 0,
                                                             prop_median = 0,
                                                             prop_li = 0,
                                                             prop_ls = 0,
                                                             Prop = 0
                                                             ))
               
  # Agregar banda de referência com base nas proporções observadas ao longo da janela
  # temporal [epiweek_start, epiweek_ic_stop] para construir a banda sombreada no gráfico
  # Não é a incerteza, mas sim intervalos de referência com base nas observações dentro da janela
  # de base
  tmp <- tmp %>%
    filter(DT_SIN_PRI_epiweek <= epiweek_ic_stop) %>%
    select(SG_UF, age_cat, Prop) %>%
    group_by(SG_UF, age_cat) %>%
    summarize(q90s = quantile(Prop, probs=.95),
              q90i = quantile(Prop, probs=.05),
              q50s = quantile(Prop, probs=.75),
              q50i = quantile(Prop, probs=.25)) %>%
    left_join(tmp, by=c('SG_UF', 'age_cat')) %>%
    left_join(tbl.ufs, by=c('SG_UF'='CO_UF'))
  return(tmp)
}

tmp <- calc.props.and.ic(df=dadosBR,
                         epiweek_start=epiweek_start,
                         epiweek_end=today.week - epiweek_stop,
                         epiweek_ic_stop=53,
                         breaks=c(0,20,40,60,80,150),
                         labels=c("0-19","20-39", "40-59", "60-79", "80+"))
tmp.sragcovid <- calc.props.and.ic(df=dadosBR %>% filter(CLASSI_FIN==5),
                                   epiweek_start=epiweek_start,
                                   epiweek_end=today.week - epiweek_stop,
                                   epiweek_ic_stop=53,
                                   breaks=c(0,20,40,60,80,150),
                                   labels=c("0-19","20-39", "40-59", "60-79", "80+"))

tmp %>%
  left_join(epiweek.table, by='DT_SIN_PRI_epiweek') %>%
  saveRDS(paste0('~/codes/covid19br/Sivep_status/hosp_manaus/Multinomial/Data/infogripe.srag.bin20.2021', today.week.ori, '.rds'))
tmp.sragcovid %>%
  left_join(epiweek.table, by='DT_SIN_PRI_epiweek') %>%
  saveRDS(paste0('~/codes/covid19br/Sivep_status/hosp_manaus/Multinomial/Data/infogripe.sragcovid.bin20.2021', today.week.ori, '.rds'))

srag.uf <- readRDS('ufs_current.rds') %>%
  filter(Date >= epiweek_start)


for (uf in unique(tbl.ufs$CO_UF)){
  sigla <- as.character(tbl.ufs$DS_UF_SIGLA[tbl.ufs$CO_UF == uf])
  
  plt <- plt.age.prop.pointrange(tmp, uf, paste0(sigla, ': SRAG'))
  p.now.srag <- plot.nowcast(srag.uf %>% filter(CO_UF == uf), Fim=today.week - epiweek_stop )
  p <- align_plots(plt, p.now.srag, align='v', axis='lr')

  png(paste0('./Figs/Faixa_etaria/', sigla, '_prop_etaria_srag.png'), height=6, width=9, units='in', res=100)
  print(plot_grid(p[[1]], p[[2]], ncol=1, rel_heights = c(1, .4)))
  dev.off()
  
  plt <- plt.age.prop.pointrange(tmp.sragcovid, uf, paste0(sigla, ': SRAGCOVID'))
  p <- align_plots(plt, p.now.srag, align='v', axis='lr')
  png(paste0('./Figs/Faixa_etaria/', sigla, '_prop_etaria_sragcovid.png'), height=6, width=9, units='in', res=100)
  print(plot_grid(p[[1]], p[[2]], ncol=1, rel_heights = c(1, .4)))
  dev.off()
}

# Age40+ --------------
# Age groups 40+, by 10 up to 80+

tmp <- calc.props.and.ic(df=dadosBR,
                         epiweek_start=epiweek_start,
                         epiweek_end=today.week - epiweek_stop,
                         epiweek_ic_stop=53,
                         breaks=c(40,50, 60, 70, 80, 150),
                         labels=c("40-49","50-59", "60-69", "70-79", "80+"))
tmp.sragcovid <- calc.props.and.ic(df=dadosBR %>% filter(CLASSI_FIN == 5),
                                   epiweek_start=epiweek_start,
                                   epiweek_end=today.week - epiweek_stop,
                                   epiweek_ic_stop=53,
                                   breaks=c(40,50, 60, 70, 80, 150),
                                   labels=c("40-49","50-59", "60-69", "70-79", "80+"))
tmp %>%
  left_join(epiweek.table, by='DT_SIN_PRI_epiweek') %>%
  saveRDS(paste0('~/codes/covid19br/Sivep_status/hosp_manaus/Multinomial/Data/infogripe.srag.40mais.bin10.2021', today.week.ori, '.rds'))
tmp.sragcovid %>%
  left_join(epiweek.table, by='DT_SIN_PRI_epiweek') %>%
  saveRDS(paste0('~/codes/covid19br/Sivep_status/hosp_manaus/Multinomial/Data/infogripe.sragcovid.40mais.bin10.2021', today.week.ori, '.rds'))

for (uf in unique(tbl.ufs$CO_UF)){
  sigla <- as.character(tbl.ufs$DS_UF_SIGLA[tbl.ufs$CO_UF == uf])
  
  plt <- plt.age.prop.pointrange(tmp, uf, paste0(sigla, ': SRAG')) +
    labs(subtitle='Proporção restrita a pacientes com 40 anos ou mais')
  p.now.srag <- plot.nowcast(srag.uf %>% filter(CO_UF == uf), Fim=today.week )
  p <- align_plots(plt, p.now.srag, align='v', axis='lr')
  png(paste0('./Figs/Faixa_etaria/', sigla, '_prop_etaria_srag_40mais.png'), height=6, width=9, units='in', res=100)
  print(plot_grid(p[[1]], p[[2]], ncol=1, rel_heights = c(1, .4)))
  dev.off()
  
  plt <- plt.age.prop.pointrange(tmp.sragcovid, uf, paste0(sigla, ': SRAGCOVID')) +
    labs(subtitle='Proporção restrita a pacientes com 40 anos ou mais')
  p <- align_plots(plt, p.now.srag, align='v', axis='lr')
  png(paste0('./Figs/Faixa_etaria/', sigla, '_prop_etaria_sragcovid_40mais.png'), height=6, width=9, units='in', res=100)
  print(plot_grid(p[[1]], p[[2]], ncol=1, rel_heights = c(1, .4)))
  dev.off()
}

# Age60+ ---------------
# Age groups 60+, by 5 up to 85+

tmp <- calc.props.and.ic(df=dadosBR,
                         epiweek_start=epiweek_start,
                         epiweek_end=today.week - epiweek_stop,
                         epiweek_ic_stop=53,
                         breaks=c(60, 65, 70, 75, 80, 85, 150),
                         labels=c("60-64", "65-69", "70-74", "75-79", "80-84", "85+"))
tmp.sragcovid <- calc.props.and.ic(df=dadosBR %>% filter(PCR_SARS2 == 1),
                                   epiweek_start=epiweek_start,
                                   epiweek_end=today.week - epiweek_stop,
                                   epiweek_ic_stop=53,
                                   breaks=c(60, 65, 70, 75, 80, 85, 150),
                                   labels=c("60-64", "65-69", "70-74", "75-79", "80-84", "85+"))
tmp %>%
  left_join(epiweek.table, by='DT_SIN_PRI_epiweek') %>%
  saveRDS(paste0('~/codes/covid19br/Sivep_status/hosp_manaus/Multinomial/Data/infogripe.srag.60mais.bin5.2021', today.week.ori, '.rds'))
tmp.sragcovid %>%
  left_join(epiweek.table, by='DT_SIN_PRI_epiweek') %>%
  saveRDS(paste0('~/codes/covid19br/Sivep_status/hosp_manaus/Multinomial/Data/infogripe.sragcovid.60mais.bin5.2021', today.week.ori, '.rds'))

for (uf in unique(tbl.ufs$CO_UF)){
  sigla <- as.character(tbl.ufs$DS_UF_SIGLA[tbl.ufs$CO_UF == uf])
  
  plt <- plt.age.prop.pointrange(tmp, uf, paste0(sigla, ': SRAG')) +
    labs(subtitle='Proporção restrita a pacientes com 60 anos ou mais')
  p.now.srag <- plot.nowcast(srag.uf %>% filter(CO_UF == uf), Fim=today.week )
  p <- align_plots(plt, p.now.srag, align='v', axis='lr')
  png(paste0('./Figs/Faixa_etaria/', sigla, '_prop_etaria_srag_60mais.png'), height=6, width=9, units='in', res=100)
  print(plot_grid(p[[1]], p[[2]], ncol=1, rel_heights = c(1, .4)))
  dev.off()

  plt <- plt.age.prop.pointrange(tmp.sragcovid, uf, paste0(sigla, ': SRAGCOVID')) +
    labs(subtitle='Proporção restrita a pacientes com 60 anos ou mais')
  p <- align_plots(plt, p.now.srag, align='v', axis='lr')
  png(paste0('./Figs/Faixa_etaria/', sigla, '_prop_etaria_sragcovid_60mais.png'), height=6, width=9, units='in', res=100)
  print(plot_grid(p[[1]], p[[2]], ncol=1, rel_heights = c(1, .4)))
  dev.off()
}
