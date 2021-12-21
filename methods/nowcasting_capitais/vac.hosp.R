library(tidyverse)
library(lubridate)
require(geofacet)
require(ggpattern)
source('../report/theme.publication.R')
source("../data_filter/episem.R")

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


tblufpop <- read_csv("~/codes/mave/covid19-pni/Data/pop_age_uf_2021.csv") %>%
  select(-`0-4`, -`5-9`, -`10-14`, -`15-19`)

tblufpop <- tblufpop %>%
  pivot_longer(cols=3:ncol(tblufpop), names_to = 'fx_etaria', values_to = 'pop') %>%
  complete(CO_UF, ano, fx_etaria, fill=(list(pop=0)))

tbl.ufs <- read.csv("../data/municip_macsaud_regmetr.csv", stringsAsFactors = F) %>%
  select(CO_UF, DS_UF_SIGLA) %>%
  filter(CO_UF != 20, CO_UF > 0) %>%
  distinct() %>%
  bind_rows(data.frame(list('CO_UF'=0, 'DS_UF_SIGLA'='BR'))) %>%
  left_join(tblufpop, by='CO_UF')

dados.vac <- tibble()
for (uf in unique(tbl.ufs$DS_UF_SIGLA[tbl.ufs$DS_UF_SIGLA != 'BR'])){
  tmp <- read_csv(
    paste0('~/codes/covid19br/dados-vacinas/doses_estados/doses_aplicadas_',
           uf,
           '.csv')
  )
  dados.vac <- tmp %>%
    mutate(DS_UF_SIGLA=uf) %>%
    bind_rows(dados.vac)
}

dados.vac <- dados.vac %>%
  mutate(agegroup=replace_na(agegroup, 'Ign')) %>%
  filter(!is.na(data)) %>%
  mutate(mes=month(data)) %>%
  group_by(DS_UF_SIGLA, mes, agegroup, doses) %>%
  summarise(n=sum(n)) %>%
  ungroup() %>%
  mutate(fx_etaria=as.character(
    factor(agegroup,
           levels = as.character(c(seq(1,10), 'Ign')),
           labels=c('0-9',
                    '10-19',
                    '20-29',
                    '30-39',
                    '40-49',
                    '50-59',
                    '60-69',
                    '70-79',
                    '80-89',
                    '90+',
                    'Ign')
           )
  )) %>%
  select(-agegroup) %>%
  complete(DS_UF_SIGLA, mes, fx_etaria, doses, fill=list(n=0))

dados.vac <- dados.vac %>%
  filter(fx_etaria %in% c('80-89', '90+')) %>%
  group_by(DS_UF_SIGLA, mes, doses) %>%
  summarise(n=sum(n)) %>%
  ungroup() %>%
  mutate(fx_etaria='80+') %>%
  bind_rows(dados.vac %>% filter(!(fx_etaria %in% c('80-89', '90+'))))

dados.vac <- dados.vac %>%
  group_by(DS_UF_SIGLA, mes, doses) %>%
  summarise(n=sum(n)) %>%
  ungroup() %>%
  mutate(fx_etaria='Total') %>%
  bind_rows(dados.vac)

dados.vac <- dados.vac %>%
  group_by(mes, fx_etaria, doses) %>%
  summarise(n=sum(n, na.rm = T)) %>%
  ungroup() %>%
  mutate(DS_UF_SIGLA='BR') %>%
  bind_rows(dados.vac)

dados.vac <- dados.vac %>%
  filter(!(fx_etaria %in% c('0-9', 'Total'))) %>%
  group_by(DS_UF_SIGLA, mes, doses) %>%
  summarise(n=sum(n)) %>%
  ungroup() %>%
  mutate(fx_etaria='Total(10+)') %>%
  bind_rows(dados.vac) %>%
  left_join(tbl.ufs %>% select(-CO_UF, -ano),
            by=c('DS_UF_SIGLA', 'fx_etaria')) %>%
  mutate(n=replace_na(n,0)) %>%
  left_join(tbl.ufs %>% select(CO_UF, DS_UF_SIGLA) %>% distinct(),
            by='DS_UF_SIGLA') %>%
  arrange(CO_UF, mes, doses, fx_etaria) %>%
  group_by(CO_UF, doses, fx_etaria) %>%
  summarise(DS_UF_SIGLA=DS_UF_SIGLA, mes=mes, n=cumsum(n), pop=pop, vac.frac=n/pop) %>%
  ungroup()

dadosBR <- vroom::vroom("../clean_data/clean_data_srag_hospdeath_epiweek.csv", col_types = cols(
  'SG_UF' = col_double(),
  'SG_UF_NOT' = col_double(),
  'DT_SIN_PRI' = col_date(),
  'DT_SIN_PRI_epiyear' = col_integer(),
  'HOSPITAL' = col_integer(),
  'EVOLUCAO' = col_integer(),
  'CS_GESTANT' = col_integer(),
  'PUERPERA' = col_integer(),
  'CS_SEXO' = col_character(),
  'CLASSI_FIN' = col_integer(),
  'PCR_SARS2' = col_integer(),
  'idade_em_anos' = col_double(),
  'NU_NOTIFIC' = col_character(),
  'DT_NOTIFIC' = col_date(),
  'CO_MUN_NOT' = col_double(),
  'VACINA_COV' = col_integer(),
  'DOSE_1_COV' = col_date(format='%d/%m/%Y'),
  'DOSE_2_COV' = col_date(format='%d/%m/%Y'),
  'FAB_COV' = col_character(),
  .default = col_character()
)) %>%
  filter(as.integer(DT_SIN_PRI_epiyear) >= 2021) %>%
  select(SG_UF,
         SG_UF_NOT,
         DT_SIN_PRI,
         HOSPITAL,
         EVOLUCAO,
         CLASSI_FIN,
         CS_SEXO,
         CS_GESTANT,
         PUERPERA,
         PCR_SARS2,
         idade_em_anos,
         NU_NOTIFIC,
         DT_NOTIFIC,
         CO_MUN_NOT,
         VACINA_COV,
         DOSE_1_COV,
         DOSE_2_COV,
         FAB_COV) %>%
  distinct(NU_NOTIFIC, CO_MUN_NOT, DT_NOTIFIC, .keep_all=T)
gc()

janssen.lbl <- 'JHAN|JAS|JANS|JANH|JAHS|JAHN|JANNS|JHAS|JOHN|JOHS|JHON|JSNS|JON|JOHO|JENS|JESEN|JAUSEN|SSEN'

dadosBR <- dadosBR %>%
  mutate(
    SG_UF_NOT = as.integer(SG_UF_NOT),
    idade_em_anos = as.integer(idade_em_anos),
    janssen = grepl(janssen.lbl, FAB_COV),
    DT_SIN_PRI = as.Date(DT_SIN_PRI),
    DOSE_1_COV = as.Date(DOSE_1_COV, format='%d/%m/%Y'),
    DOSE_2_COV = as.Date(DOSE_2_COV, format='%d/%m/%Y'),
    gest_puer = case_when(
      PUERPERA == 1 ~ 1,
      between(CS_GESTANT, 1, 4) ~ 1,
      TRUE ~ 0),
    vac_completa = case_when(
      (VACINA_COV == 1 & janssen & (DT_SIN_PRI > DOSE_1_COV + 14)) |
      ((VACINA_COV == 1) & (DT_SIN_PRI > DOSE_2_COV + 14)) ~ 1,
      (VACINA_COV == 2 |
         ((VACINA_COV == 1) & 
            (
              (is.finite(DOSE_1_COV) & !is.finite(DOSE_2_COV)) |
              (is.finite(DOSE_1_COV) & !janssen & (DT_SIN_PRI <= DOSE_2_COV + 14))
            )
         )
         ) ~ 0,
      TRUE ~ 9
    ),
    vac_completa = factor(vac_completa, levels=c(9, 0, 1), labels=c('ignorado', 'não', 'sim')),
    fx_etaria = cut(as.integer(idade_em_anos),
                    breaks=c(seq(0, 80, 10), 140),
                    labels=c('0-9', '10-19','20-29', '30-39', '40-49', '50-59',
                             '60-69', '70-79',
                             '80+'),
                    right=F),
    mes = month(DT_SIN_PRI)
  )
gc()

dadosBR.vac.hosp <- dadosBR %>%
  filter(HOSPITAL == 1,
         CLASSI_FIN == 5,
         mes >= 3) %>%
  group_by(mes, fx_etaria, vac_completa) %>%
  summarise(hosp=n()) %>%
  group_by(mes, fx_etaria) %>%
  summarise(vac_completa=vac_completa, hosp=hosp, frac=hosp/sum(hosp)) %>%
  mutate(SG_UF_NOT = 0) %>%
  ungroup()

dadosBR.vac.hosp <- dadosBR %>%
  filter(HOSPITAL == 1,
         CLASSI_FIN == 5,
         mes >= 3,
         !is.na(SG_UF_NOT)) %>%
  group_by(SG_UF_NOT, mes, fx_etaria, vac_completa) %>%
  summarise(hosp=n()) %>%
  group_by(SG_UF_NOT, mes, fx_etaria) %>%
  summarise(vac_completa=vac_completa, hosp=hosp, frac=hosp/sum(hosp)) %>%
  ungroup() %>%
  bind_rows(dadosBR.vac.hosp)

dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
  group_by(SG_UF_NOT, mes, vac_completa) %>%
  summarise(hosp=sum(hosp)) %>%
  group_by(SG_UF_NOT, mes) %>%
  summarise(vac_completa=vac_completa, hosp=hosp, frac=hosp/sum(hosp)) %>%
  mutate(fx_etaria='Total') %>%
  ungroup() %>%
  bind_rows(dadosBR.vac.hosp)

dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
  filter(!fx_etaria %in% c('0-9', 'Total')) %>%
  group_by(SG_UF_NOT, mes, vac_completa) %>%
  summarise(hosp=sum(hosp)) %>%
  group_by(SG_UF_NOT, mes) %>%
  summarise(vac_completa=vac_completa, hosp=hosp, frac=hosp/sum(hosp)) %>%
  ungroup() %>%
  mutate(fx_etaria='Total(10+)') %>%
  bind_rows(dadosBR.vac.hosp) %>%
  arrange(SG_UF_NOT, mes, fx_etaria, vac_completa)

dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
  left_join(tbl.ufs, by=c('SG_UF_NOT'='CO_UF', 'fx_etaria'))

dados.vac.comp <- dados.vac %>%
  filter(doses %in% c('D2', 'DU')) %>%
  group_by(CO_UF, DS_UF_SIGLA, mes, fx_etaria, pop) %>%
  summarise(n=sum(n), vac.frac=sum(vac.frac)) %>%
  ungroup() %>%
  rename(SG_UF_NOT=CO_UF, frac=vac.frac, eventos=n) %>%
  mutate(vac_completa='sim')

dados.vac.comp <- dados.vac.comp %>%
  mutate(vac_completa='não', frac=1-frac, eventos=pop-eventos) %>%
  bind_rows(dados.vac.comp) %>%
  mutate(dado='pop')

dadosBR.vac.hosp.2 <- dadosBR.vac.hosp %>%
  mutate(dado='sragcovid') %>%
  rename(eventos=hosp) %>%
  bind_rows(dados.vac.comp) %>%
  mutate(vac_completa=factor(vac_completa,
                             levels=c('sim', 'não', 'ignorado'),
                             labels=c('sim', 'não', 'ignorado'))) %>%
  arrange(SG_UF_NOT, mes, fx_etaria, vac_completa, dado)
dadosBR.vac.hosp.2 <- dadosBR.vac.hosp.2 %>%
  group_by(SG_UF_NOT, mes, fx_etaria, dado) %>%
  summarise(vac_completa=vac_completa, frac=cumsum(frac))

# Mês de interesse:
mes.num = 11
mes.lbl = month(mes.num, label = T, abbr=F, locale = 'pt_BR.UTF-8')


gttl <- 'Cobertura vacinal: população vs. SRAG por COVID-19'
gsbttl <- paste0('Mês de ', mes.lbl, ', com base em dados digitados até a semana ', epiweek, ' de 2021.')

plt <- dadosBR.vac.hosp.2 %>%
  filter(mes==mes.num, SG_UF_NOT==0, !(fx_etaria %in% c('Ign', 'Total', 'Total(10+)'))) %>%
  complete(mes, SG_UF_NOT, fx_etaria, dado, vac_completa, fill=list(frac=0)) %>%
  mutate(dado=factor(dado, levels=c('sragcovid', 'pop'),
                     labels=c('Casos de SRAG\npor COVID-19', 'População geral'))) %>%
  ggplot(aes(x=fx_etaria, y=frac, pattern_density=dado, pattern=dado, pattern_spacing=dado)) +
  geom_col_pattern(data=. %>% filter(vac_completa=='ignorado'), position=position_dodge(width=.9),
                   aes(fill=vac_completa),
                   pattern_angle=45) +
  geom_col_pattern(data=. %>% filter(vac_completa=='não'), position=position_dodge(width=.9),
                   aes(fill=vac_completa),
                   pattern_angle=45) +
  geom_col_pattern(data=. %>% filter(vac_completa=='sim'), position=position_dodge(width=.9),
                   aes(fill=vac_completa),
                   pattern_angle=45) +
  scale_fill_manual(values=colorblind_pal()(3), name='2a dose ou dose única') +
  scale_pattern_density_manual(values=c(0,.05), name='') +
  scale_pattern_manual(values=c('crosshatch', 'stripe'), name='') + 
  scale_pattern_spacing_manual(values=c(1,.05), name='') +
  labs(x='Faixa etária', y='Proporção') +
  ggtitle(gttl,
          subtitle=gsbttl) +
  theme_Publication(base_size=16, base_family = 'Roboto') +
  theme(legend.position='bottom',
        legend.direction='horizontal')
ggsave(paste0('hosp.classifin5.vac.br.', mes.lbl, '.se', epiweek, '2021.png'),
       plot=plt,
       width=10.80,
       height = 10.80,
       units='in',
       dpi=100)

gsbttl <- paste0('Distribuição mensal, com base em dados digitados até a semana ', epiweek, ' de 2021.')
plt <- dadosBR.vac.hosp.2 %>%
  filter(between(mes, 3, mes.num), SG_UF_NOT==0, !(fx_etaria %in% c('Ign', 'Total', 'Total(10+)'))) %>%
  complete(mes, SG_UF_NOT, fx_etaria, dado, vac_completa, fill=list(frac=0)) %>%
  mutate(dado=factor(dado, levels=c('sragcovid', 'pop'),
                     labels=c('Casos de SRAG\npor COVID-19', 'População geral')),
         mes=month(mes, label = T, abbr=F, locale = 'pt_BR.UTF-8')) %>%
  ggplot(aes(x=fx_etaria, y=frac, pattern_density=dado, pattern=dado, pattern_spacing)) +
  geom_col_pattern(data=. %>% filter(vac_completa=='ignorado'), position=position_dodge(width=.8),
                   aes(fill=vac_completa),
                   pattern_angle=45) +
  geom_col_pattern(data=. %>% filter(vac_completa=='não'), position=position_dodge(width=.8),
                   aes(fill=vac_completa),
                   pattern_angle=45) +
  geom_col_pattern(data=. %>% filter(vac_completa=='sim'), position=position_dodge(width=.8),
                   aes(fill=vac_completa),
                   pattern_angle=45) +
  scale_fill_manual(values=colorblind_pal()(3), name='2a dose ou dose única') +
  scale_pattern_density_manual(values=c(0,.05), name='') +
  scale_pattern_manual(values=c('crosshatch', 'stripe'), name='') + 
  scale_pattern_spacing_manual(values=c(1,.05), name='') +
  labs(x='Faixa etária', y='Proporção') +
  ggtitle(gttl,
          subtitle=gsbttl) +
  theme_Publication(base_size=16, base_family = 'Roboto') +
  theme(legend.position='bottom',
        legend.direction='horizontal',
        axis.text.x = element_text(angle=35, hjust=1)) +
  facet_wrap(~mes)
ggsave(paste0('hosp.classifin5.vac.br.mensal.se', epiweek, '2021.png'),
       plot=plt,
       width=10.8, height = 10.8, dpi=100)


gsbttl <- paste0('Mês de ', mes.lbl, ', com base em dados digitados até a semana ', epiweek, ' de 2021.')
dadosBR.vac.hosp %>%
  filter(SG_UF_NOT != 0, mes==mes.num, !(fx_etaria %in% c('Ign', 'Total', 'Total(10+)'))) %>%
  ggplot(aes(x=fx_etaria, y=frac, fill=vac_completa)) +
  geom_col() +
  scale_fill_colorblind(name='2a dose ou dose única') +
  labs(x='Faixa etária', y='Proporção') +
  theme_Publication(base_size=14, base_family = 'Roboto') +
  facet_geo(~DS_UF_SIGLA, grid='br_states_grid1') +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        axis.text.x = element_text(angle=45, hjust=1)) +
  ggtitle('Cobertura vacinal nas hospitalizações de SRAG por COVID-19',
          subtitle=gsbttl)
ggsave(paste0('hosp.classifin5.vac.uf.', mes.lbl, '.se', epiweek, '2021.png'), width=10.80, height = 10.80, units='in', dpi=100)

# gsbttl <- paste0('Mês de ', mes.lbl, ', com base em dados digitados até a semana ', epiweek, ' de 2021.')
# dadosBR.vac.hosp %>%
#   filter(SG_UF_NOT == 0, mes==mes.num, !(fx_etaria %in% c('Ign', 'Total', 'Total(10+)'))) %>%
#   ggplot(aes(x=fx_etaria, y=frac, fill=vac_completa)) +
#   geom_col() +
#   scale_fill_colorblind(name='2a dose ou dose única') +
#   scale_y_continuous(breaks=seq(0,1,.2)) +
#   labs(x='Faixa etária', y='Proporção') +
#   theme_Publication(base_size=16, base_family = 'Roboto') +
#   theme(legend.position = 'bottom',
#         legend.direction = 'horizontal',
#         axis.text.x = element_text(angle=45, hjust=1)) +
#   ggtitle(gttl,
#           subtitle=gsbttl)
# ggsave(paste0('hosp.classifin5.vac.br.', mes.lbl, '.se', epiweek, '2021.png'), width=10.8, height = 10.8, dpi=100)

# gsbttl <- paste0('Distribuição mensal, com base em dados digitados até a semana ', epiweek, ' de 2021.')
# dadosBR.vac.hosp %>%
#   filter(SG_UF_NOT == 0, !(fx_etaria %in% c('Ign', 'Total', 'Total(10+)'))) %>%
#   mutate(mes=month(mes, label = T, abbr=F, locale = 'pt_BR.UTF-8')) %>%
#   ggplot(aes(x=fx_etaria, y=frac, fill=vac_completa)) +
#   geom_col() +
#   scale_fill_colorblind(name='2a dose ou dose única') +
#   scale_y_continuous(breaks=seq(0,1,.2)) +
#   labs(x='Faixa etária', y='Proporção') +
#   theme_Publication(base_size=16, base_family = 'Roboto') +
#   theme(legend.position = 'bottom',
#         legend.direction = 'horizontal',
#         axis.text.x = element_text(angle=45, hjust=1)) +
#   ggtitle(gttl,
#           subtitle=gsbttl) +
#   facet_wrap(~mes)
# ggsave(paste0('hosp.classifin5.vac.br.mensal.se', epiweek, '2021.png'), width=10.8, height = 10.8, dpi=100)

saveRDS(dadosBR, paste0('infogripe.vac.hosp.se', epiweek, '2021.rds'))
saveRDS(dadosBR.vac.hosp, paste0('infogripe.vac.hosp.adultos.mensal.se', epiweek, '2021.rds'))

dadosBR.vac.hosp %>%
  complete(SG_UF_NOT, mes, vac_completa, fx_etaria, fill=list(hosp=0, frac=0)) %>%
  filter(vac_completa == 'ignorado') %>%
write_csv2(file=paste0('infogripe.vac.hosp.adultos.mensal.julho.vacignorado.se', epiweek, '2021.csv'))


dadosBR.vac.hosp.2 <- dadosBR.vac.hosp %>%
  filter(vac_completa != 'ignorado') %>%
  group_by(SG_UF_NOT, DS_UF_SIGLA, mes, fx_etaria) %>%
  summarise(vac_completa=vac_completa, hosp=hosp, pop=pop, frac=frac/sum(frac)) %>%
  ungroup() %>%
  mutate(dado='sragcovid') %>%
  rename(eventos=hosp) %>%
  bind_rows(dados.vac.comp) %>%
  mutate(vac_completa=factor(vac_completa,
                             levels=c('sim', 'não'),
                             labels=c('sim', 'não'))) %>%
  arrange(SG_UF_NOT, mes, fx_etaria, vac_completa, dado)
dadosBR.vac.hosp.2 <- dadosBR.vac.hosp.2 %>%
  group_by(SG_UF_NOT, mes, fx_etaria, dado) %>%
  summarise(vac_completa=vac_completa, frac=cumsum(frac))

gsbttl <- paste0('Distribuição mensal, com base em dados digitados até a semana ', epiweek, ' de 2021.')
plt <- dadosBR.vac.hosp.2 %>%
  filter(between(mes, 3, mes.num), SG_UF_NOT==0, !(fx_etaria %in% c('Ign', 'Total', 'Total(10+)'))) %>%
  complete(mes, SG_UF_NOT, fx_etaria, dado, vac_completa, fill=list(frac=0)) %>%
  mutate(dado=factor(dado, levels=c('sragcovid', 'pop'),
                     labels=c('Casos de SRAG\npor COVID-19', 'População geral')),
         mes=month(mes, label = T, abbr=F, locale = 'pt_BR.UTF-8')) %>%
  ggplot(aes(x=fx_etaria, y=frac, pattern_density=dado, pattern=dado, pattern_spacing)) +
  geom_col_pattern(data=. %>% filter(vac_completa=='não'), position=position_dodge(width=.8),
                   aes(fill=vac_completa),
                   pattern_angle=45) +
  geom_col_pattern(data=. %>% filter(vac_completa=='sim'), position=position_dodge(width=.8),
                   aes(fill=vac_completa),
                   pattern_angle=45) +
  scale_fill_manual(values=colorblind_pal()(3)[2:3], name='2a dose ou dose única') +
  scale_pattern_density_manual(values=c(0,.1), name='') +
  scale_pattern_manual(values=c('crosshatch', 'stripe'), name='') +
  scale_pattern_spacing_manual(values=c(1,.06), name='') +
  labs(x='Faixa etária', y='Proporção') +
  scale_y_continuous(breaks=seq(0,1,.2)) +
  ggtitle('Cobertura vacinal: população vs. SRAG por COVID-19',
          subtitle=paste0(gsbttl,'\nRestrito a notificações de SRAG com infomação vacinal.')) +
  theme_Publication(base_size=16, base_family = 'Roboto') +
  theme(legend.position='bottom',
        legend.direction='horizontal',
        axis.text.x = element_text(angle=35, hjust=1)) +
  facet_wrap(~mes)
ggsave(paste0('hosp.classifin5.vac.informada.br.mensal.se', epiweek, '2021.png'), plot=plt, width=10.8, height = 10.8, dpi=100)

gsbttl <- paste0('Mês de ',
                 mes.lbl,
                 ', com base em dados digitados até a semana ',
                 epiweek,
                 ' de 2021.\nRestrito a notificações de SRAG com infomação vacinal.')
plt <- dadosBR.vac.hosp %>%
  filter(SG_UF_NOT != 0,
         mes==mes.num,
         !(fx_etaria %in% c('Ign', 'Total', 'Total(10+)')),
         vac_completa!='ignorado') %>%
  group_by(SG_UF_NOT, DS_UF_SIGLA, mes, fx_etaria, ano, pop) %>%
  summarise(vac_completa=vac_completa, hosp=hosp, frac=frac/sum(frac)) %>%
  ggplot(aes(x=fx_etaria, y=frac, fill=vac_completa)) +
  geom_col() +
  scale_fill_manual(values=colorblind_pal()(3)[2:3], name='2a dose ou dose única') +
  labs(x='Faixa etária', y='Proporção') +
  theme_Publication(base_size=14, base_family = 'Roboto') +
  facet_geo(~DS_UF_SIGLA, grid='br_states_grid1') +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        axis.text.x = element_text(angle=45, hjust=1)) +
  ggtitle('Cobertura vacinal nas hospitalizações de SRAG por COVID-19',
          subtitle=gsbttl)
ggsave(paste0('hosp.classifin5.vac.informada.uf.', mes.lbl, '.se', epiweek, '2021.png'), width=10.80, height = 10.80, units='in', dpi=100)
