require(tidyverse)
require(lubridate)
require(geofacet)
require(RcppRoll)
source('../report/theme.publication.R')
source("../data_filter/episem.R")
source('vac.hosp.plots.R')
source('load.pop.data.R')
source('load.vac.data.R')
source('load.vac.srag.data.R')
source('sr.rr.vac.hosp.R')

Sys.setlocale(locale='pt_BR.UTF-8')
plot.folder <- './Figs/vac/'

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
# Semana epi de banco atual:
epiweek <- as.integer(strsplit(today, 'W')[[1]][2])

# Semana limite para análise:
epiweek.num = epiweek-2
epiyear.num = lyear
fx.breaks=c(0, 5, 12, 18, seq(30, 80, 10), 140)
fx.labels=c('0-4', '5-11', '12-17','18-29', '30-39', '40-49', '50-59',
            '60-69', '70-79',
            '80+')

tbl.ufs <- load.pop.data(year=lyear) %>%
  filter(fx_etaria %in% c(fx.labels, 'Total', 'Total(5+)', 'Total(12+)', 'Total(18+)'))

dados.vac <- load.vac.serie(fpath='~/codes/covid19br/dados-vacinas/doses_cobertura_proporcao_semana.csv') %>%
  filter((epiyear==2021 & epiweek >= 10) | epiyear > 2021,
         fx_etaria != 'Ign')
dados.vac <- dados.vac %>%
  left_join(tbl.ufs,
            by=c('DS_UF_SIGLA', 'fx_etaria')) %>%
  mutate(fx_etaria = factor(fx_etaria,
                            levels=c(fx.labels,
                                     'Total',
                                     'Total(5+)',
                                     'Total(12+)',
                                     'Total(18+)')),
         vac.frac=n/pop)

dados.vac.comp <- dados.vac %>%
  rename(SG_UF_NOT=CO_UF, frac=vac.frac, eventos=n, vac_completa=doses)

# dados.vac.comp <- dados.vac %>%
#   filter(doses %in% c('D', 'D+REF')) %>%
#   group_by(CO_UF, DS_UF_SIGLA, DS_NOME_uf, epiyear, epiweek, fx_etaria, pop) %>%
#   summarise(n=sum(n, na.rm=T), vac.frac=sum(vac.frac, na.rm=T)) %>%
#   ungroup() %>%
#   rename(SG_UF_NOT=CO_UF, frac=vac.frac, eventos=n) %>%
#   mutate(vac_completa='D+') %>%
#   bind_rows(dados.vac.comp)
# 
# dados.vac.comp <- dados.vac %>%
#   filter(doses %in% c('D2', 'D2+REF')) %>%
#   group_by(CO_UF, DS_UF_SIGLA, DS_NOME_uf, epiyear, epiweek, fx_etaria, pop) %>%
#   summarise(n=sum(n, na.rm=T), vac.frac=sum(vac.frac, na.rm=T)) %>%
#   ungroup() %>%
#   rename(SG_UF_NOT=CO_UF, frac=vac.frac, eventos=n) %>%
#   mutate(vac_completa='D2+') %>%
#   bind_rows(dados.vac.comp)

dados.vac.comp <- dados.vac %>%
  filter(doses %in% c('D+', 'D2+')) %>%
  group_by(CO_UF, DS_UF_SIGLA, DS_NOME_uf, epiyear, epiweek, fx_etaria, pop) %>%
  summarise(n=sum(n, na.rm=T), vac.frac=sum(vac.frac, na.rm=T)) %>%
  ungroup() %>%
  rename(SG_UF_NOT=CO_UF, frac=vac.frac, eventos=n) %>%
  mutate(vac_completa='D2/D+') %>%
  bind_rows(dados.vac.comp)

# dados.vac.comp <- dados.vac %>%
#   filter(doses %in% c('D+REF', 'D2+REF')) %>%
#   group_by(CO_UF, DS_UF_SIGLA, DS_NOME_uf, epiyear, epiweek, fx_etaria, pop) %>%
#   summarise(n=sum(n, na.rm=T), vac.frac=sum(vac.frac, na.rm=T)) %>%
#   ungroup() %>%
#   rename(SG_UF_NOT=CO_UF, frac=vac.frac, eventos=n) %>%
#   mutate(vac_completa='REF') %>%
#   bind_rows(dados.vac.comp)

dados.vac.comp <- dados.vac.comp %>%
  filter(vac_completa %in% c('D2+', 'D1')) %>%
  group_by(SG_UF_NOT, DS_UF_SIGLA, DS_NOME_uf, epiyear, epiweek, fx_etaria, pop) %>%
  summarise(eventos=sum(eventos, na.rm=T), frac=sum(frac, na.rm=T)) %>%
  ungroup() %>%
  mutate(vac_completa='D1+') %>%
  bind_rows(dados.vac.comp)

dados.vac.comp <- dados.vac.comp %>%
  filter(vac_completa %in% c('D+', 'D1+')) %>%
  group_by(SG_UF_NOT, DS_UF_SIGLA, DS_NOME_uf, epiyear, epiweek, fx_etaria, pop) %>%
  summarise(eventos=sum(eventos, na.rm=T), frac=sum(frac, na.rm=T)) %>%
  ungroup() %>%
  mutate(vac_completa='D1/D+') %>%
  bind_rows(dados.vac.comp)

dados.vac.comp <- normalize.vac.data(dados.vac.comp, max.cov = 0.95)
tbl.ufs <- tbl.ufs %>%
  rename(pop.ori=pop) %>%
  left_join(dados.vac.comp %>%
              select(SG_UF_NOT, fx_etaria, pop) %>%
              distinct() %>%
              mutate(fx_etaria=as.character(fx_etaria)),
            by=c('CO_UF'='SG_UF_NOT', 'fx_etaria'))

dados.vac.comp <- dados.vac.comp %>%
  filter(vac_completa %in% c('D1/D+')) %>%
  mutate(frac=1-frac, eventos=pop-eventos, vac_completa='não') %>%
  bind_rows(dados.vac.comp) %>%
  mutate(dado='pop')
dados.vac.comp <- dados.vac.comp %>%
  select(-vac_completa, -eventos, -frac) %>%
  distinct() %>%
  mutate(vac_completa='ignorado', eventos=0, frac=0) %>%
  bind_rows(dados.vac.comp) %>%
  arrange(SG_UF_NOT, fx_etaria, vac_completa, epiyear, epiweek) %>%
  group_by(SG_UF_NOT, fx_etaria, vac_completa) %>%
  mutate(eventos.wdw2=round(roll_meanr(eventos, n=2)),
         eventos.wdw4=round(roll_meanr(eventos, n=4))) %>%
  group_by(SG_UF_NOT, fx_etaria, vac_completa) %>%
  mutate(frac.wdw2 = eventos.wdw2/pop,
         frac.wdw4 = eventos.wdw4/pop) %>%
  ungroup()
  
dadosBR.vac.hosp <- load.vac.srag.data(epiweek=epiweek, lyear=lyear, overwrite=F)

dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
  filter(!(epiyear == lyear & epiweek > epiweek.num),
         !(epiyear > lyear)) %>%
  left_join(tbl.ufs %>% distinct(CO_UF, DS_UF_SIGLA),
            by=c('SG_UF_NOT'='CO_UF')) %>%
  inner_join(dados.vac.comp %>%
              select(-pop, -starts_with('frac'), -dado) %>%
              rename(pop=eventos,
                     pop.wdw2=eventos.wdw2,
                     pop.wdw4=eventos.wdw4),
            by=c('SG_UF_NOT', 'DS_UF_SIGLA', 'epiyear', 'epiweek', 'fx_etaria', 'vac_completa')) %>%
  mutate(time.index = case_when(
    epiyear == 2020 ~ as.integer(epiweek),
    epiyear == 2021 ~ as.integer(epiweek + 53),
    epiyear == 2022 ~ as.integer(epiweek + 53 + 52))
  ) %>%
  mutate(time.index = time.index-min(time.index)+1) %>%
  arrange(SG_UF_NOT, desc(dado), fx_etaria, vac_completa, time.index)

#### Janelas móveis:
dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
  group_by(SG_UF_NOT, dado, fx_etaria, vac_completa) %>%
  mutate(eventos.wdw2=roll_sumr(eventos, n=2),
         eventos.wdw4=roll_sumr(eventos, n=4))

dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
  filter(vac_completa %in% c('ignorado', 'não', 'D1',
                             'D+', 'D2+')) %>%
  group_by(SG_UF_NOT, dado, fx_etaria, time.index) %>%
  mutate(frac=eventos/(sum(eventos, na.rm=T)),
         frac.wdw2=eventos.wdw2/(sum(eventos.wdw2, na.rm=T)),
         frac.wdw4=eventos.wdw4/(sum(eventos.wdw4, na.rm=T))) %>%
  ungroup() %>%
  bind_rows(
    dadosBR.vac.hosp %>%
      filter(vac_completa %in% c('ignorado', 'não', 'D1',
                                 'D', 'D2',
                                 'D+REF', 'D2+REF')) %>%
      group_by(SG_UF_NOT, dado, fx_etaria, time.index) %>%
      mutate(frac=eventos/(sum(eventos, na.rm=T)),
             frac.wdw2=eventos.wdw2/(sum(eventos.wdw2, na.rm=T)),
             frac.wdw4=eventos.wdw4/(sum(eventos.wdw4, na.rm=T))) %>%
      ungroup() %>%
      filter(vac_completa %in% c('D2', 'D2+REF'))
  ) %>%
  bind_rows(
    dadosBR.vac.hosp %>%
      filter(vac_completa %in% c('ignorado', 'não', 'D1',
                                 'D', 'D2+',
                                 'D+REF')) %>%
      group_by(SG_UF_NOT, dado, fx_etaria, time.index) %>%
      mutate(frac=eventos/(sum(eventos, na.rm=T)),
             frac.wdw2=eventos.wdw2/(sum(eventos.wdw2, na.rm=T)),
             frac.wdw4=eventos.wdw4/(sum(eventos.wdw4, na.rm=T))) %>%
      ungroup() %>%
      filter(vac_completa %in% c('D', 'D+REF'))
  ) %>%
  bind_rows(
    dadosBR.vac.hosp %>%
      filter(vac_completa %in% c('ignorado', 'não', 'D1/D+')) %>%
      group_by(SG_UF_NOT, dado, fx_etaria, time.index) %>%
      mutate(frac=eventos/(sum(eventos, na.rm=T)),
             frac.wdw2=eventos.wdw2/(sum(eventos.wdw2, na.rm=T)),
             frac.wdw4=eventos.wdw4/(sum(eventos.wdw4, na.rm=T))) %>%
      ungroup() %>%
      filter(vac_completa %in% c('D1/D+'))
  ) %>%
  bind_rows(
    dadosBR.vac.hosp %>%
      filter(vac_completa %in% c('ignorado', 'não', 'D1',
                                 'D2/D+')) %>%
      group_by(SG_UF_NOT, dado, fx_etaria, time.index) %>%
      mutate(frac=eventos/(sum(eventos, na.rm=T)),
             frac.wdw2=eventos.wdw2/(sum(eventos.wdw2, na.rm=T)),
             frac.wdw4=eventos.wdw4/(sum(eventos.wdw4, na.rm=T))) %>%
      ungroup() %>%
      filter(vac_completa %in% c('D2/D+'))
  ) %>%
  bind_rows(
    dadosBR.vac.hosp %>%
      filter(vac_completa %in% c('ignorado', 'não', 'D1+',
                                 'D+')) %>%
      group_by(SG_UF_NOT, dado, fx_etaria, time.index) %>%
      mutate(frac=eventos/(sum(eventos, na.rm=T)),
             frac.wdw2=eventos.wdw2/(sum(eventos.wdw2, na.rm=T)),
             frac.wdw4=eventos.wdw4/(sum(eventos.wdw4, na.rm=T))) %>%
      ungroup() %>%
      filter(vac_completa %in% c('D1+'))
  ) %>%
  bind_rows(
    dadosBR.vac.hosp %>%
      filter(vac_completa %in% c('ignorado', 'não', 'D1',
                                 'D', 'D2',
                                 'REF')) %>%
      group_by(SG_UF_NOT, dado, fx_etaria, time.index) %>%
      mutate(frac=eventos/(sum(eventos, na.rm=T)),
             frac.wdw2=eventos.wdw2/(sum(eventos.wdw2, na.rm=T)),
             frac.wdw4=eventos.wdw4/(sum(eventos.wdw4, na.rm=T))) %>%
      ungroup() %>%
      filter(vac_completa %in% c('REF'))
  )
  
dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
  mutate(inc = eventos/pop*100000,
         inc.wdw2=eventos.wdw2/pop.wdw2*100000,
         inc.wdw4=eventos.wdw4/pop.wdw4*100000) %>%
  select(SG_UF_NOT, DS_UF_SIGLA, DS_NOME_uf, dado, fx_etaria, vac_completa, time.index, epiyear, epiweek,
         eventos, frac, inc, pop, reescale,
         eventos.wdw2, frac.wdw2, inc.wdw2, pop.wdw2,
         eventos.wdw4, frac.wdw4, inc.wdw4, pop.wdw4)

epiweek.list <- dadosBR.vac.hosp %>%
  ungroup() %>%
  select(epiyear, epiweek, time.index) %>%
  distinct()

dadosBR.vac.hosp.2 <- dadosBR.vac.hosp %>%
  bind_rows(dados.vac.comp %>%
              right_join(epiweek.list, by=c('epiyear', 'epiweek'))) %>%
  mutate(vac_completa=factor(vac_completa,
                             levels=c('ignorado', 'não', 'D1',
                                      'D', 'D1+', 'D+', 'D1/D+', 'D2',
                                      'D2+', 'D2/D+', 'D+REF', 'D2+REF',
                                      'REF')),
         fx_etaria = factor(fx_etaria,
                            levels=c(fx.labels,
                                     'Total',
                                     'Total(5+)',
                                     'Total(12+)',
                                     'Total(18+)'))) %>%
  arrange(SG_UF_NOT, epiyear, epiweek, fx_etaria, vac_completa, dado)

for (wdw in c(2,4)){
  epiweek.lbl = epiweek2date(epiyear.num, epiweek.num)
  epiweek.lbl <- paste0('SE ', epiweek.num-(wdw-1), '-', epiweek.num, ' (', epiweek.lbl-7*wdw+1, ' a ', epiweek.lbl, ')')
  gsbttl=paste0('Período ', epiweek.lbl, ', com base em dados digitados até a semana ', epiweek, ' de ', lyear)
  plt <- plot.mes(df=dadosBR.vac.hosp.2 %>%
                    select(-frac) %>%
                    rename_with(~ str_remove(., paste0('.wdw', wdw)), ends_with(paste0('frac.wdw', wdw))),
                  epiyear.num=epiyear.num,
                  epiweek.num=epiweek.num,
                  vac_levels = c('ignorado', 'não', 'D1',
                                 'D+', 'D2+'),
                  vac_lbls=c('ignorado', 'não vacinado', 'apenas D1', 'ao menos D', 'ao menos D2'),
                  list.dado=c('pop', 'sragcovid'),
                  lbl.dado=c('População geral', 'Casos de SRAG\npor COVID-19'),
                  gsbttl=gsbttl)
  ggsave(paste0(plot.folder,'hosp.classifin5.vac.br.se', epiweek, lyear,'.janela.', wdw, '.png'),
         plot=plt,
         width=10.80,
         height = 10.80,
         units='in',
         dpi=100)
  
  plt <- plot.uf.epiweek(df=dadosBR.vac.hosp.2 %>%
                           select(-frac) %>%
                           rename_with(~ str_remove(., paste0('.wdw', wdw)), starts_with(paste0('frac.wdw', wdw))),
                         epiyear.num=epiyear.num,
                         epiweek.num=epiweek.num,
                         vac_levels = c('ignorado', 'não', 'D1',
                                        'D+', 'D2+'),
                         vac_lbls=c('ignorado', 'não vacinado', 'apenas D1', 'ao menos D', 'ao menos D2'),
                         gsbttl=gsbttl)
  ggsave(paste0(plot.folder,'hosp.classifin5.vac.uf.se', epiweek, lyear,'.janela.', wdw, '.png'),
         plot=plt,
         width=10.80, height = 10.80, units='in', dpi=100)
  
  for (sigla in c('BR', 'SP')){
    gsbttl=paste0('Incidência com base na média de casos e da cobertura vacinal a cada ', wdw , ' semanas.\n',
                  'Dados digitados até a semana ', epiweek, ' de ', lyear, '.')
    plt <- plot.inc.serie(df=dadosBR.vac.hosp.2 %>%
                            filter(DS_UF_SIGLA==sigla,
                                   dado=='sragcovid',
                                   !(fx_etaria %in% c('0-4', '5-11'))) %>%
                            select(-inc) %>%
                            rename_with(~ str_remove(., paste0('.wdw', wdw)), starts_with(paste0('inc.wdw', wdw))) %>%
                            mutate(data=epiweek2date(epiyear, epiweek)-(wdw-1)*7/2,
                                   inc=inc/wdw),
                          vac.lvls = c('não', 'D2+'),
                          vac.lbls=c('não vacinado', 'ao menos D2'),
                          ttl=paste0(sigla, ': SRAG por COVID-19'),
                          subttl=gsbttl)
    ggsave(paste0(plot.folder,'serie.inc.informada.janelas', sigla, '.se', epiweek, lyear,
                  '.janela.', wdw,'.png'),
           plot=plt, width=13, height = 8, dpi=100)
    plt <- plot.inc.serie(df=dadosBR.vac.hosp.2 %>%
                            filter(DS_UF_SIGLA==sigla,
                                   dado=='obitocovid',
                                   !(fx_etaria %in% c('0-4', '5-11'))) %>%
                            select(-inc) %>%
                            rename_with(~ str_remove(., paste0('.wdw', wdw)), starts_with(paste0('inc.wdw', wdw))) %>%
                            mutate(data=epiweek2date(epiyear, epiweek)-(wdw-1)*7/2,
                                   inc=inc/wdw),
                          vac.lvls = c('não', 'D2+'),
                          vac.lbls=c('não vacinado', 'ao menos D2'),
                          ttl=paste0(sigla, ': óbitos de SRAG por COVID-19'),
                          subttl=gsbttl)
    ggsave(paste0(plot.folder,'serie.inc.obito.informada.janelas', sigla, '.se', epiweek, lyear,
                  '.janela.', wdw,'.png'),
           plot=plt, width=13, height = 8, dpi=100)
  }
}

saveRDS(dadosBR.vac.hosp, paste0('infogripe.vac.hosp.adultos.bisemanal.se', epiweek, lyear, '.rds'))
saveRDS(dadosBR.vac.hosp.2, paste0('infogripe.vac.hosp.pop.adultos.bisemanal.se', epiweek, lyear, '.rds'))
saveRDS(dadosBR.vac.hosp, paste0('infogripe.vac.hosp.adultos.bisemanal.rds'))
saveRDS(dadosBR.vac.hosp.2, paste0('infogripe.vac.hosp.pop.adultos.bisemanal.rds'))

dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
  rename(eventos.wdw1=eventos,
         frac.wdw1=frac,
         pop.wdw1=pop,
         inc.wdw1=inc) %>%
  pivot_longer(cols=c(ends_with('.wdw1'),
                      ends_with('.wdw2'),
                      ends_with('.wdw4')),
               names_to=c('variavel', 'janela'),
               names_pattern = "(.*).wdw(.)",
               values_to='valor') %>%
  pivot_wider(names_from=variavel, values_from=valor)
saveRDS(dadosBR.vac.hosp, paste0('infogripe.vac.janelas.se', epiweek, lyear, '.rds'))

dadosBR.vac.hosp.2 <- vac.srag.clean.ignored(df=dadosBR.vac.hosp, complete=F)

dadosBR.vac.hosp.2 <- dadosBR.vac.hosp.2 %>%
  bind_rows(dados.vac.comp %>%
              filter(vac_completa != 'ignorado') %>%
              droplevels() %>%
              right_join(epiweek.list, by=c('epiyear', 'epiweek')) %>%
              rename(eventos.wdw1=eventos,
                     frac.wdw1=frac,
                     pop.wdw1=pop) %>%
              mutate(pop.wdw2=pop.wdw1,
                     pop.wdw4=pop.wdw1) %>%
              pivot_longer(cols=c(ends_with('.wdw1'),
                                  ends_with('.wdw2'),
                                  ends_with('.wdw4')),
                           names_to=c('variavel', 'janela'),
                           names_pattern = "(.*).wdw(.)",
                           values_to='valor') %>%
              pivot_wider(names_from=variavel, values_from=valor))
  
rr <- tibble()

for (wdw in c(1, 2, 4)){
  epiweek.lbl = epiweek2date(epiyear.num, epiweek.num)
  epiweek.lbl <- paste0('SE ', epiweek.num-wdw+1, '-', epiweek.num, ' (', epiweek.lbl-7*wdw+1, ' a ', epiweek.lbl, ')')
  gsbttl=paste0('Período ', epiweek.lbl, ', com base em dados digitados\naté a semana ', epiweek, ' de ', lyear,
                '.\nRestrito a notificações de SRAG com infomação vacinal.')
  plt <- plot.mes(df=dadosBR.vac.hosp.2 %>%
                    filter(janela==wdw),
                  epiyear.num=epiyear.num,
                  epiweek.num=epiweek.num,
                  vac_levels = c('ignorado', 'não', 'D1',
                                 'D+', 'D2+'),
                  vac_lbls=c('ignorado', 'não vacinado', 'apenas D1', 'ao menos D', 'ao menos D2'),
                  gsbttl=gsbttl)
  ggsave(paste0(plot.folder,'hosp.classifin5.vac.informada.br.se', epiweek, lyear, '.janela.', wdw, '.png'), plot=plt, width=10.8, height = 10.8, dpi=100)
  
  plt <- plot.uf.epiweek(df=dadosBR.vac.hosp.2 %>%
                           filter(janela==wdw),
                         epiyear.num=epiyear.num,
                         epiweek.num=epiweek.num,
                         vac_levels = c('ignorado', 'não', 'D1',
                                        'D+', 'D2+'),
                         vac_lbls=c('ignorado', 'não vacinado', 'apenas D1', 'ao menos D', 'ao menos D2'),
                         gsbttl=gsbttl)
  ggsave(paste0(plot.folder,'hosp.classifin5.vac.informada.uf.se', epiweek, lyear, '.janela.', wdw, '.png'), plot=plt, width=10.8, height = 10.8, dpi=100)
  
  for (sigla in c('BR', 'SP')){
    plt <- dadosBR.vac.hosp.2 %>%
      filter(dado=='sragcovid', janela==wdw,
             DS_UF_SIGLA==sigla,
             !(fx_etaria %in% c('0-4', '5-11')) & (vac_completa %in% c('não', 'D2+')),
             fx_etaria %in% c('5-11', '12-17', '18-29',
                              '30-39', '40-49', '50-59',
                              '60-69', '70-79', '80+')
             ) %>%
      mutate(fx_etaria=factor(fx_etaria, levels=c('5-11', '12-17', '18-29',
                                                  '30-39', '40-49', '50-59',
                                                  '60-69', '70-79', '80+'),
                              labels=c('5 a 11 anos', '12 a 17 anos', '18 a 29 anos',
                                       '30 a 39 anos', '40 a 49 anos', '50 a 59 anos',
                                       '60 a 69 anos', '70 a 79 anos', '80 anos ou mais')),
             inc=inc.ori,
             eventos=eventos.ori) %>%
      plot.incidencia(epiyear.num=epiyear.num,
                      epiweek.num=epiweek.num,
                      title=paste0(sigla, ': impacto da vacinação nos casos e óbitos de SRAG por COVID-19'),
                      vac_levels=c('não', 'D2+'),
                      vac_lbls=c('não vacinado', 'ao menos D2'),
                      xlabels=c('Casos graves de COVID-19'),
                      subtitle=gsbttl)
    ggsave(paste0(plot.folder,'incidencia.vac.', tolower(sigla), '.d2.se', epiweek, lyear, '.janela.', wdw, '.png'),
           plot=plt, width=12, height = 9, dpi=100)
    
    plt <- dadosBR.vac.hosp.2 %>%
      filter(dado=='sragcovid',
             janela==wdw,
             DS_UF_SIGLA==sigla,
             fx_etaria %in% c('5-11', '12-17', '18-29',
                              '30-39', '40-49', '50-59',
                              '60-69', '70-79', '80+')) %>%
      mutate(fx_etaria=factor(fx_etaria, levels=c('5-11', '12-17', '18-29',
                                                  '30-39', '40-49', '50-59',
                                                  '60-69', '70-79', '80+'),
                              labels=c('5 a 11 anos', '12 a 17 anos', '18 a 29 anos',
                                       '30 a 39 anos', '40 a 49 anos', '50 a 59 anos',
                                       '60 a 69 anos', '70 a 79 anos', '80 anos ou mais')),
             inc=inc.ori,
             eventos=eventos.ori) %>%
      plot.incidencia(epiyear.num=epiyear.num,
                      epiweek.num=epiweek.num,
                      title=paste0(sigla, ': impacto da vacinação nos casos e óbitos de SRAG por COVID-19'),
                      vac_levels=c('não', 'D1', 'D+', 'D2+'),
                      vac_lbls=c('não vacinado', 'apenas D1', 'ao menos D', 'ao menos D2'),
                      xlabels=c('Casos graves de COVID-19'),
                      subtitle=gsbttl)
    ggsave(paste0(plot.folder,'incidencia.vac.', tolower(sigla), '.d1dd2.se', epiweek, lyear, '.jaenla.', wdw, '.png'),
           plot=plt, width=12, height = 9, dpi=100)
  }
  
  rr <- rr.vac(dadosBR.vac.hosp.2 %>%
                 filter(vac_completa %in% c('não', 'D2/D+'),
                        dado != 'pop',
                        !(epiyear==lyear & epiweek > epiweek.num),
                        janela==wdw) %>%
                 select(-janela, -time.index) %>%
                 mutate(vac_completa = factor(vac_completa,
                                              levels=c('não', 'D2/D+'),
                                              labels=c('não', 'sim')))
  ) %>%
    mutate(compara='não vacinado vs\nao menos D2 ou D',
           janela=wdw) %>%
    bind_rows(rr)
  rr <- rr.vac(dadosBR.vac.hosp.2 %>%
                 filter(vac_completa %in% c('não', 'D2+'),
                        dado != 'pop',
                        !(epiyear==lyear & epiweek > epiweek.num),
                        janela==wdw) %>%
                 select(-janela, -time.index) %>%
                 mutate(vac_completa = factor(vac_completa,
                                              levels=c('não', 'D2+'),
                                              labels=c('não', 'sim'))
                        )
  ) %>%
    mutate(compara='não vacinado vs\nao menos D2',
           janela=wdw) %>%
    bind_rows(rr)
  rr <- rr.vac(dadosBR.vac.hosp.2 %>%
                 filter(vac_completa %in% c('não', 'D1+'),
                        dado != 'pop',
                        !(epiyear==lyear & epiweek > epiweek.num),
                        janela==wdw) %>%
                 select(-janela, -time.index) %>%
                 mutate(vac_completa = factor(vac_completa,
                                              levels=c('não', 'D1+'),
                                              labels=c('não', 'sim'))
                 )
  ) %>%
    mutate(compara='não vacinado vs\nao menos D1',
           janela=wdw) %>%
    bind_rows(rr)
  rr <- rr.vac(dadosBR.vac.hosp.2 %>%
                 filter(vac_completa %in% c('não', 'D+'),
                        dado != 'pop',
                        !(epiyear==lyear & epiweek > epiweek.num),
                        janela==wdw) %>%
                 select(-janela, -time.index) %>%
                 mutate(vac_completa = factor(vac_completa,
                                              levels=c('não', 'D+'),
                                              labels=c('não', 'sim')))
  ) %>%
    mutate(compara='não vacinado vs\nao menos D',
           janela=wdw) %>%
    bind_rows(rr)
}

rr <- rr %>%
  mutate(compara=factor(compara,
                        levels=c('não vacinado vs\nao menos D2 ou D',
                                 'não vacinado vs\nao menos D2',
                                 'não vacinado vs\nao menos D1',
                                 'não vacinado vs\nao menos D'),
                        ordered=T)
         )

rr <- rr %>%
  mutate(rr = case_when(
    fx_etaria %in% c('0-4') ~ NA_real_,
    fx_etaria %in% c('5-11') & (epiyear==2021 & epiweek < 48) ~ NA_real_,
    fx_etaria %in% c('12-17') & (epiyear==2021 & epiweek < 40) ~ NA_real_,
    fx_etaria %in% c('18-29') & (epiyear==2021 & epiweek < 36) ~ NA_real_,
    fx_etaria %in% c('30-39') & (epiyear==2021 & epiweek < 32) ~ NA_real_,
    rr == Inf ~ NA_real_,
    TRUE ~ rr))

for (wdw in unique(dadosBR.vac.hosp.2$janela)){
  
  ggsbttl = paste0('Dados do SIVEP-Gripe digitados até a semana ', epiweek, ' de ', lyear,
                   '.\nRisco relativo a partir da incidência a cada ', wdw, ' semanas epidemiológicas.')
  compara.id = 2
  for (sigla in c('BR', 'SP')){
    plt <- plot.rr.vac(rr %>% filter(!(epiyear==2021 & epiweek < 18),
                                     !grepl(pattern='T', x=fx_etaria),
                                     !(fx_etaria %in% c('0-4', '5-11', NA)),
                                     dado=='sragcovid',
                                     janela==wdw,
                                     as.integer(compara)==compara.id) %>%
                         select(-dado) %>%
                         droplevels() %>%
                         rename(dado=compara) %>%
                         mutate(dt_sinpri = epiweek2date(epiyear, epiweek) - (janela-1)*7/2,
                                rr=1-1/rr),
                       color_lbls = str_replace(levels(rr$compara)[compara.id], '\n', ' '),
                       sigla=sigla,
                       title = paste0(sigla, ': efetividade das vacinas contra SRAG por COVID-19'),
                       subtitle=ggsbttl) +
      labs(y='Efetividade') +
      scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
      coord_cartesian(ylim=c(0,1)) +
      theme(axis.text.x = element_text(angle=30, hjust=1))
    ggsave(paste0(plot.folder,'rr.vac.informada.', sigla, '.se', epiweek, lyear, '.janela.', wdw, '.png'), plot=plt, width=13, height = 9, dpi=100)
    
    plt <- plot.rr.vac(rr %>% filter(!(epiyear==2021 & epiweek < 36),
                                     !grepl(pattern='T', x=fx_etaria),
                                     !(fx_etaria %in% c('0-4', '5-11', NA)),
                                     dado=='sragcovid',
                                     janela==wdw,
                                     as.integer(compara)==compara.id) %>%
                         select(-dado) %>%
                         droplevels() %>%
                         rename(dado=compara) %>%
                         mutate(dt_sinpri = epiweek2date(epiyear, epiweek) - (janela-1)*7/2,
                                rr=1-1/rr),
                       color_lbls = str_replace(levels(rr$compara)[compara.id], '\n', ' '),
                       sigla=sigla,
                       title = paste0(sigla, ': efetividade das vacinas contra SRAG por COVID-19'),
                       subtitle=ggsbttl) +
      labs(y='Efetividade') +
      scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
      coord_cartesian(ylim=c(0,1)) +
      theme(axis.text.x = element_text(angle=30, hjust=1))
    ggsave(paste0(plot.folder,'rr.vac.informada.', sigla, '.set.se', epiweek, lyear, '.janela.', wdw, '.png'),
           plot=plt, width=13, height = 9, dpi=100)
    
  }
}
rr %>% write_csv('sragcovid.sr.rr.fx.etaria.csv', na='')
