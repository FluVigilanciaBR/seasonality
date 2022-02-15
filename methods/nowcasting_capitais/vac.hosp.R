library(tidyverse)
library(lubridate)
require(geofacet)
require(ggpattern)
source('../report/theme.publication.R')
source("../data_filter/episem.R")
source('vac.hosp.plots.R')
source('load.pop.data.R')
source('load.vac.data.R')
source('load.vac.srag.data.R')
source('sr.rr.vac.hosp.R')

Sys.setlocale(locale='pt_BR.UTF-8')

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

# Mês de interesse:
mes.num = 1
ano.num = 2022
mes.lbl = month(mes.num, label = T, abbr=F, locale = 'pt_BR.UTF-8')

fx.breaks=c(0, 5, 12, 18, seq(30, 80, 10), 140)
fx.labels=c('0-4', '5-11', '12-17','18-29', '30-39', '40-49', '50-59',
            '60-69', '70-79',
            '80+')

tbl.ufs <- load.pop.data(year=lyear) %>%
  filter(fx_etaria %in% c(fx.labels, 'Total', 'Total(5+)', 'Total(12+)', 'Total(18+)'))

dados.vac <- load.vac.data()

dados.vac.comp <- dados.vac %>%
  filter(doses %in% c('D2', 'DU')) %>%
  group_by(CO_UF, DS_UF_SIGLA, ano, mes, fx_etaria, pop) %>%
  summarise(n=sum(n, na.rm=T), vac.frac=sum(vac.frac, na.rm=T)) %>%
  ungroup() %>%
  rename(SG_UF_NOT=CO_UF, frac=vac.frac, eventos=n) %>%
  mutate(vac_completa='D2/DU+')

dados.vac.comp <- dados.vac %>%
  filter(doses %in% c('D2', 'D1')) %>%
  pivot_wider(names_from=doses, values_from = c(n, vac.frac)) %>%
  mutate(n = n_D1 - n_D2,
         vac.frac = vac.frac_D1 - vac.frac_D2,
         vac_completa='D1') %>%
  select(-matches('_D1$|_D2$')) %>%
  rename(SG_UF_NOT=CO_UF, frac=vac.frac, eventos=n) %>%
  bind_rows(dados.vac.comp)

dados.vac.comp <- dados.vac %>%
  filter(doses %in% c('DU', 'D1')) %>%
  pivot_wider(names_from=doses, values_from = c(n, vac.frac)) %>%
  mutate(n = pop - (n_D1 + n_DU),
         vac.frac = n/pop,
         vac_completa='não') %>%
  select(-matches('_D1$|_DU$')) %>%
  rename(SG_UF_NOT=CO_UF, frac=vac.frac, eventos=n) %>%
  bind_rows(dados.vac.comp) %>%
  mutate(dado='pop')

dados.vac.comp <- dados.vac.comp %>%
  filter(vac_completa=='D2/DU+') %>%
  mutate(eventos = pop - eventos,
         frac = 1 - frac,
         vac_completa='não/incompleta') %>%
  bind_rows(dados.vac.comp)

dadosBR.vac.hosp <- load.vac.srag.data(epiweek=epiweek, lyear=lyear)

dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
  left_join(tbl.ufs %>% distinct(CO_UF, DS_UF_SIGLA),
            by=c('SG_UF_NOT'='CO_UF')) %>%
  left_join(dados.vac.comp %>%
              select(-pop, -frac, -dado) %>%
              rename(pop=eventos),
            by=c('SG_UF_NOT', 'DS_UF_SIGLA', 'ano', 'mes', 'fx_etaria', 'vac_completa')) %>%
  mutate(inc = eventos/pop*100000) %>%
  arrange(SG_UF_NOT, ano, mes, fx_etaria, dado, vac_completa)

dadosBR.vac.hosp.2 <- dadosBR.vac.hosp %>%
  bind_rows(dados.vac.comp) %>%
  mutate(vac_completa=factor(vac_completa,
                             levels=c('ignorado', 'não', 'não/incompleta', 'D1', 'D2/DU+')),
         fx_etaria = factor(fx_etaria,
                            levels=c(fx.labels,
                                     'Total',
                                     'Total(5+)',
                                     'Total(12+)',
                                     'Total(18+)'))) %>%
  arrange(SG_UF_NOT, ano, mes, fx_etaria, vac_completa, dado)

plt <- plot.mes(list.dado=c('pop', 'sragcovid'),
                lbl.dado=c('População geral', 'Casos de SRAG\npor COVID-19'))
ggsave(paste0('hosp.classifin5.vac.br.', mes.lbl, '.se', epiweek, lyear,'.png'),
       plot=plt,
       width=10.80,
       height = 10.80,
       units='in',
       dpi=100)
plt <- plot.mes(list.dado=c('pop', 'obitocovid'),
                lbl.dado=c('População geral', 'Óbitos de SRAG\npor COVID-19'),
                gttl='Cobertura vacinal: população vs. óbitos de SRAG por COVID-19')
ggsave(paste0('obito.classifin5.vac.br.', mes.lbl, '.se', epiweek, lyear,'.png'),
       plot=plt,
       width=10.80,
       height = 10.80,
       units='in',
       dpi=100)

plt <- plot.mensal()
ggsave(paste0('hosp.classifin5.vac.br.mensal.se', epiweek, lyear,'.png'),
       plot=plt,
       width=12, height = 9, dpi=100)
plt <- plot.mensal(list.dado=c('pop', 'obitocovid'),
                   lbl.dado=c('População geral', 'Óbitos de SRAG\npor COVID-19'),
                   gttl='Cobertura vacinal: população vs. óbitos de SRAG por COVID-19')
ggsave(paste0('obito.classifin5.vac.br.mensal.se', epiweek, lyear, '.png'),
       plot=plt,
       width=12, height = 9, dpi=100)


plt <- plot.uf.mes()
ggsave(paste0('hosp.classifin5.vac.uf.', mes.lbl, '.se', epiweek, '2021.png'), width=10.80, height = 10.80, units='in', dpi=100)
plt <- plot.uf.mes(list.dado=c('obitocovid'),
                   gttl='Cobertura vacinal nos óbitos de SRAG por COVID-19')
ggsave(paste0('obito.classifin5.vac.uf.', mes.lbl, '.se', epiweek, '2021.png'), width=10.80, height = 10.80, units='in', dpi=100)

saveRDS(dadosBR.vac.hosp, paste0('infogripe.vac.hosp.adultos.mensal.se', epiweek, lyear, '.rds'))
saveRDS(dadosBR.vac.hosp.2, paste0('infogripe.vac.hosp.pop.adultos.mensal.se', epiweek, lyear, '.rds'))

dadosBR.vac.hosp %>%
  complete(SG_UF_NOT, mes, vac_completa, fx_etaria, fill=list(eventos=0, frac=0)) %>%
  filter(vac_completa == 'ignorado') %>%
  write_csv2(file=paste0('infogripe.vac.hosp.adultos.mensal.vacignorado.se', epiweek, lyear, '.csv'))


dadosBR.vac.hosp.2 <- dadosBR.vac.hosp %>%
  filter(vac_completa %in% c('não/incompleta', 'D2/DU+')) %>%
  droplevels() %>%
  group_by(SG_UF_NOT, DS_UF_SIGLA, dado, ano, mes, fx_etaria) %>%
  summarise(vac_completa=vac_completa, eventos=eventos, pop=pop, frac=frac/sum(frac, na.rm=T)) %>%
  ungroup() %>%
  bind_rows(dados.vac.comp %>%
              filter(vac_completa %in% c('não/incompleta', 'D2/DU+')) %>%
              droplevels()) %>%
  mutate(vac_completa=factor(vac_completa,
                             levels=c('ignorado', 'não/incompleta', 'D2/DU+')),
         fx_etaria = factor(fx_etaria,
                            levels=c(fx.labels, 
                                     'Total',
                                     'Total(5+)',
                                     'Total(12+)',
                                     'Total(18+)')
                            )
         ) %>%
  arrange(SG_UF_NOT, ano, mes, fx_etaria, vac_completa, dado)

gsbttl <- paste0('Distribuição mensal, com base em dados digitados até a semana ', epiweek, ' de 2021.\nRestrito a notificações de SRAG com infomação vacinal.')
plt <- plot.mes(gsbttl=gsbttl)
ggsave(paste0('hosp.classifin5.vac.informada.br.', mes.lbl, '.se', epiweek, lyear, '.png'), plot=plt, width=10.8, height = 10.8, dpi=100)
plt <- plot.mes(list.dado=c('pop', 'obitocovid'),
                lbl.dado=c('População geral', 'Óbitos de SRAG\npor COVID-19'),
                gttl='Cobertura vacinal: população vs. óbitos de SRAG por COVID-19',
                gsbttl=gsbttl)
ggsave(paste0('obito.classifin5.vac.informada.br.', mes.lbl, '.se', epiweek, lyear, '.png'), plot=plt, width=10.8, height = 10.8, dpi=100)


plt <- plot.mensal(gsbttl=gsbttl)
ggsave(paste0('hosp.classifin5.vac.informada.br.mensal.se', epiweek, '2021.png'), plot=plt, width=12, height = 9, dpi=100)
plt <- plot.mensal(list.dado=c('pop', 'obitocovid'),
                   lbl.dado=c('População geral', 'Óbitos de SRAG\npor COVID-19'),
                   gttl='Cobertura vacinal: população vs. óbitos de SRAG por COVID-19',
                   gsbttl=gsbttl)
ggsave(paste0('obito.classifin5.vac.informada.br.mensal.se', epiweek, '2021.png'), plot=plt, width=12, height = 9, dpi=100)

gsbttl <- paste0('Mês de ',
                 mes.lbl,
                 ', com base em dados digitados até a semana ',
                 epiweek,
                 ' de 2021.\nRestrito a notificações de SRAG com infomação vacinal.')

plt <- plot.uf.mes(gsbttl=gsbttl)
ggsave(paste0('hosp.classifin5.vac.informada.uf.', mes.lbl, '.se', epiweek, lyear, '.png'), plot=plt, width=10.8, height = 10.8, dpi=100)
plt <- plot.uf.mes(list.dado=c('obitocovid'),
                   gttl='Cobertura vacinal: população vs. óbitos de SRAG por COVID-19',
                   gsbttl=gsbttl)
ggsave(paste0('obito.classifin5.vac.informada.uf.', mes.lbl, '.se', epiweek, lyear, '.png'), plot=plt, width=10.8, height = 10.8, dpi=100)

plt <- plot.incidencia()
ggsave(paste0('incidencia.vac.br.', mes.lbl, '.se', epiweek, lyear, '.png'), plot=plt, width=10, height = 8, dpi=100)

rr <- sr.rr.vac(dadosBR.vac.hosp.2 %>%
                  filter(vac_completa %in% c('não/incompleta', 'D2/DU+')) %>%
                  mutate(vac_completa = factor(vac_completa,
                                               levels=c('não/incompleta', 'D2/DU+'),
                                               labels=c('não', 'sim')))
                )
rr <- rr %>%
  mutate(rr = case_when(
    fx_etaria %in% c('0-4', '12-17') & (ano==2021 & mes < 10) ~ NA_real_,
    TRUE ~ rr),
         sr_não = case_when(
           fx_etaria %in% c('0-4', '12-17') & (ano==2021 & mes < 10) ~ NA_real_,
           TRUE ~ sr_não),
         sr_sim = case_when(
           fx_etaria %in% c('0-4', '12-17') & (ano==2021 & mes < 10) ~ NA_real_,
           TRUE ~ sr_sim)
    )

plt <- plot.rr.vac(rr %>% filter(!(ano==2021 & mes <=4),
                                 !grepl(pattern='T', x=fx_etaria))
                   ) +
  theme(axis.text.x = element_text(angle=30, hjust=1))
ggsave(paste0('rr.vac.informada.br.se', epiweek, lyear, '.png'), plot=plt, width=12, height = 9, dpi=100)
plt <- plot.rr.vac(rr %>% filter(!(ano==2021 & mes <=4),
                                 !grepl(pattern='T', x=fx_etaria)),
                   scale='fixed') +
  theme(axis.text.x = element_text(angle=30, hjust=1))
ggsave(paste0('rr.vac.informada.br.se', epiweek, lyear, '_fixed.png'), plot=plt, width=12, height = 9, dpi=100)
plt <- plot.sr.vac(rr %>% filter(!grepl(pattern='T', x=fx_etaria))) +
  theme(axis.text.x = element_text(angle=30, hjust=1))
ggsave(paste0('sr.vac.informada.br.se', epiweek, lyear, '.png'), plot=plt, width=12, height = 9, dpi=100)
plt <- plot.sr.vac(rr %>% filter(!grepl(pattern='T', x=fx_etaria)), scale='fixed') +
  theme(axis.text.x = element_text(angle=30, hjust=1))
ggsave(paste0('sr.vac.informada.br.se', epiweek, lyear, '_fixed.png'), plot=plt, width=12, height = 9, dpi=100)

rr %>% write_csv('sragcovid.sr.rr.fx.etaria.csv', na='')
