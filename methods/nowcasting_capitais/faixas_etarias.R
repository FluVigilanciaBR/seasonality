library(tidyverse)
library(lubridate)
require(DescTools)
source("../data_filter/episem.R")
source('faixas_etarias_plot.R')

# pt_br <- locale(encoding = "latin1", asciify = FALSE)
# 
# colunas <- cols(.default = "c")


tblCADMUN <- read.csv("../data/municip_macsaud_regmetr.csv", stringsAsFactors = F) %>%
  select(CO_MUNICIP, CO_UF, DS_UF_SIGLA) %>%
  filter(CO_UF > 1, CO_UF != 20) %>%
  distinct() %>%
  mutate(DS_UF_SIGLA = case_when(
    CO_UF == 0 ~ 'BR',
    TRUE ~ DS_UF_SIGLA
  ))

dfpop <- read.csv('~/codes/covid-19/csv/populacao_datasus_faixateraria_TOTAL.csv', dec=',')
dfpop[5:ncol(dfpop)] <- lapply(dfpop[5:ncol(dfpop)], function(x) as.numeric(gsub("\\.", "", x)))
tblCADMUN <- tblCADMUN %>%
  left_join(dfpop %>% filter(ano == 2020) %>% select(-sexo, -municipio),
            by=c('CO_MUNICIP' = 'codmun'))
col.idade <- dfpop %>%
  select(-codmun, -municipio, -ano, -sexo) %>%
  names()

tbl.ufs <- tblCADMUN %>%
  select(CO_UF, DS_UF_SIGLA, all_of(col.idade)) %>%
  group_by(CO_UF, DS_UF_SIGLA) %>%
  summarise_each(funs(sum(., na.rm=TRUE)))

tbl.ufs <- tbl.ufs %>%
  ungroup() %>%
  select(-CO_UF, -DS_UF_SIGLA) %>%
  group_by() %>%
  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  mutate(CO_UF = 0, DS_UF_SIGLA = 'BR') %>%
  rbind(tbl.ufs)


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

epiweek_start = 12
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

calc.age.cat <- function(df,
                         epiweek_start=12,
                         epiweek_end=53,
                         breaks=c(0,20,40,60,80,150),
                         labels=c("0-19","20-39", "40-59", "60-79", "80+")){
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
    count(.drop=F) %>%
    mutate(SG_UF = 0)
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
    drop_na(age_cat, SG_UF) %>%
    group_by(SG_UF, DT_SIN_PRI_epiweek, age_cat) %>% 
    #group_by( DT_SIN_PRI_sun, SinPri2Digita_DelayWeeks, age_cat) %>% 
    count(.drop=F) %>%
    rbind(dadosBR.ag) %>%
    ungroup() %>%
    complete(SG_UF, DT_SIN_PRI_epiweek, age_cat, fill = list(n = 0))
  
  return(dadosBR.ag)
}

calc.props.and.ic <- function(df,
                              epiweek_start=12,
                              epiweek_end=53,
                              epiweek_ic_stop=48,
                              breaks=c(0,20,40,60,80,150),
                              labels=c("0-19","20-39", "40-59", "60-79", "80+")){
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

df <- calc.age.cat(dadosBR %>% filter(PCR_SARS2==1), epiweek_end = today.week - 5,
                   breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 200),
                   labels = col.idade[1:11])
df <- calc.age.cat(dadosBR %>% filter(PCR_SARS2==1, EVOLUCAO==2), epiweek_end = today.week - 5,
                   breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 200),
                   labels = col.idade[1:11]) %>%
  rename(obitos=n) %>%
  right_join(df, by=c('SG_UF', 'DT_SIN_PRI_epiweek', 'age_cat'))

lbl.idade <- c('0-4', '5-9', '10-14', '15-19',
               '20-29', '30-39', '40-49',
               '50-59', '60-69', '70-79',
               '80+')
df <- tbl.ufs %>%
  pivot_longer(cols=all_of(col.idade), names_to='age_cat', values_to='pop') %>%
  mutate(CO_UF = as.integer(CO_UF)) %>%
  right_join(df, by=c('CO_UF'='SG_UF', 'age_cat')) %>%
  mutate(inc = 100000*n/pop,
         inc.obitos = 100000*obitos/pop,
         let = case_when(
           n > 0 ~ 100*obitos/n,
           n == 0 ~ 0)) %>%
  replace_na(list(obitos=0, inc.obitos=0, let=0)) %>%
  mutate(age_cat=factor(age_cat, levels=col.idade[1:11], labels=lbl.idade))
df %>%
  left_join(epiweek.table, by='DT_SIN_PRI_epiweek') %>%
  saveRDS(paste0('./infogripe.sragcovid.hosp.obito.idade.2021', today.week.ori, '.rds'))

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
  
  p.now.srag <- plot.nowcast(srag.uf %>% filter(CO_UF == uf), Fim=today.week-5) +
    ggtitle('SRAG em geral')
  if (uf != 0){
    plt <- plt.age.inc(df %>% filter(CO_UF == uf),
                       facet_cols=c('inc', 'inc.obitos'),
                       facet_labs=c('Casos', 'Óbitos'),
                       ylabs='Incidência (por 100mil hab.)')
  } else {
    plt <- plt.age.inc(df %>% filter(CO_UF == uf))
  }
  plt <- plt +
    geom_vline(xintercept=8+53, color='darkgrey', linetype=1) + 
    geom_vline(xintercept=53+10, color='darkgrey', linetype=2) + 
    geom_vline(xintercept=11+53, color='darkgrey', linetype=1) + 
    geom_vline(xintercept=14+53, color='darkgrey', linetype=2) + 
    geom_vline(xintercept=15+53, color='darkgrey', linetype=1) +
    annotate('text', x=8+53, y=Inf, label='80+', size=3, angle=90, vjust=0, hjust=1) +
    annotate('text', x=11+53, y=Inf, label='70+', size=3, angle=90, vjust=0, hjust=1) +
    annotate('text', x=15+53, y=Inf, label='60+', size=3, angle=90, vjust=0, hjust=1) +
    theme(legend.key.size=unit(14, 'pt')) +
    ggtitle(paste0(sigla, ': SRAGCOVID'), subtitle=paste('Dados até a semana', today.week.ori, lyear))
  p <- align_plots(plt, p.now.srag, align='v', axis='lr')
  png(paste0('./Figs/Faixa_etaria/', sigla, '_etaria_sragcovid.png'), height=12, width=10, units='in', res=100)
  print(plot_grid(p[[1]], p[[2]], ncol=1, rel_heights = c(1, .25)))
  dev.off()
  
  plt <- plt.age.inc(df %>% filter(CO_UF == uf, age_cat %in% c('0-4', '5-9', '10-14', '15-19', '20-29', '30-39')),
                     facet_cols=c('inc', 'inc.obitos'), facet_labs=c('Casos', 'Óbitos')) +
    theme(legend.key.size=unit(14, 'pt')) +
    ggtitle(paste0(sigla, ': SRAGCOVID'), subtitle=paste('Dados até a semana', today.week.ori, lyear))
  p <- align_plots(plt, p.now.srag, align='v', axis='lr')
  png(paste0('./Figs/Faixa_etaria/', sigla, '_etaria_menor40_sragcovid.png'), height=12, width=10, units='in', res=100)
  print(plot_grid(p[[1]], p[[2]], ncol=1, rel_heights = c(1, .25)))
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
tmp %>%
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
