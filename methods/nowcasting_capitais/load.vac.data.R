require(tidyverse)
require(lubridate)
source('load.pop.data.R')

load.vac.data <- function(fx.labels=c('0-4', '5-11', '12-17','18-29', '30-39', '40-49', '50-59',
                                      '60-69', '70-79', '80+')
){

  dados.vac <- tibble()
  for (uf in unique(tbl.ufs$DS_UF_SIGLA[tbl.ufs$DS_UF_SIGLA != 'BR'])){
    tmp <- read_csv(
      paste0('~/codes/covid19br/dados-vacinas/doses_estados/doses_aplicadas_',
             uf,
             '.csv')
    ) %>%
      select(agegroup, data, doses, ag_child) %>%
      rename(n=ag_child)
    dados.vac <- tmp %>%
      mutate(DS_UF_SIGLA=uf) %>%
      bind_rows(dados.vac)
  }
  
  dados.vac <- dados.vac %>%
    mutate(agegroup=replace_na(agegroup, 'Ign')) %>%
    filter(!is.na(data)) %>%
    mutate(mes=month(data), epiyear=year(data)) %>%
    group_by(DS_UF_SIGLA, epiyear, mes, agegroup, doses) %>%
    summarise(n=sum(n, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria=as.character(
      factor(agegroup,
             levels = as.character(c(seq(1,11), 'Ign')),
             labels=c('0-4',
                      '5-11',
                      '12-17',
                      '18-29',
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
    complete(DS_UF_SIGLA, nesting(epiyear, mes), fx_etaria, doses, fill=list(n=0))
  
  dados.vac <- dados.vac %>%
    filter(fx_etaria %in% c('80-89', '90+')) %>%
    group_by(DS_UF_SIGLA, epiyear, mes, doses) %>%
    summarise(n=sum(n, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='80+') %>%
    bind_rows(dados.vac %>% filter(!(fx_etaria %in% c('80-89', '90+'))))
  
  dados.vac <- dados.vac %>%
    group_by(DS_UF_SIGLA, epiyear, mes, doses) %>%
    summarise(n=sum(n, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total') %>%
    bind_rows(dados.vac)
  
  dados.vac <- dados.vac %>%
    group_by(epiyear, mes, fx_etaria, doses) %>%
    summarise(n=sum(n, na.rm = T)) %>%
    ungroup() %>%
    mutate(DS_UF_SIGLA='BR') %>%
    bind_rows(dados.vac)
  
  dados.vac <- dados.vac %>%
    filter(!(fx_etaria %in% c('0-4', 'Total'))) %>%
    group_by(DS_UF_SIGLA, epiyear, mes, doses) %>%
    summarise(n=sum(n, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total(5+)') %>%
    bind_rows(dados.vac)
  dados.vac <- dados.vac %>%
    filter(!(fx_etaria %in% c('0-4', '5-11','Total(5+)', 'Total'))) %>%
    group_by(DS_UF_SIGLA, epiyear, mes, doses) %>%
    summarise(n=sum(n, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total(12+)') %>%
    bind_rows(dados.vac)
  dados.vac <- dados.vac %>%
    filter(!(fx_etaria %in% c('0-4', '5-11', '12-17', 'Total(5+)', 'Total(12+)', 'Total'))) %>%
    group_by(DS_UF_SIGLA, epiyear, mes, doses) %>%
    summarise(n=sum(n, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total(18+)') %>%
    bind_rows(dados.vac)
  
  dados.vac <- dados.vac %>%
    mutate(n=replace_na(n,0)) %>%
    mutate(fx_etaria = factor(fx_etaria,
                              levels=c(fx.labels,
                                       'Total',
                                       'Total(5+)',
                                       'Total(12+)',
                                       'Total(18+)'))) %>%
    arrange(DS_UF_SIGLA, epiyear, mes, doses, fx_etaria) %>%
    group_by(DS_UF_SIGLA, doses, fx_etaria) %>%
    summarise(DS_UF_SIGLA=DS_UF_SIGLA, epiyear=epiyear, mes=mes, n=cumsum(n)) %>%
    ungroup()
  
  return(dados.vac)
}

load.vac.serie <- function(fpath='~/codes/covid19br/dados-vacinas/doses_cobertura_proporcao_semana.csv',
                           fx.labels=c('0-4', '5-11', '12-17','18-29', '30-39', '40-49', '50-59',
                                       '60-69', '70-79', '80+')
){
  #' Status vacinal de população, por faixa etária, ao início de cada epiweek epidemiológica
  #' Dados orindos do SI-PNI/RNDS, extraídos do opendatasus, e processados pela equipe
  #' do Observatório COVID-19 BR.
  #' Disponíveis em: https://github.com/covid19br/dados-vacinas
  #' D1 significa indivíduos com apenas uma dose de vacina que não seja Janssen.
  #' D2 significa indivíduos com ao menos 2 doses. Inclui indivíduos com DA e R.
  #' D significa indivíduos com ao menos a 1a dose sendo Janssen, e não computados entre os D2. Podem ter DA e R.
  dados.vac <- read_csv(fpath)
  
  dados.vac <- dados.vac %>%
    filter(!is.na(week),
           grepl('cum', dose)) %>%
    mutate(agegroup=as.character(agegroup),
           agegroup=replace_na(agegroup, 'Ign'),
           week = week+14) %>%  # Deslocando em 15 dias para levar em conta o tempo até proteção
    rename(data=week,
           DS_UF_SIGLA=UF,
           doses=dose) %>%
    mutate(doses = str_replace(doses, 'cum', ''),
      doses=case_when(
      doses == 'DU' ~ 'D+',
      doses == 'D' ~ 'D+',
      doses == 'D2' ~ 'D2+',
      TRUE ~ doses
      )
    ) %>%
    mutate(epiweek=epiweek(data), epiyear=epiyear(data)) %>%
    mutate(fx_etaria=as.character(
      factor(agegroup,
             levels = as.character(c(seq(1,11), 'Ign')),
             labels=c('0-4',
                      '5-11',
                      '12-17',
                      '18-29',
                      '30-39',
                      '40-49',
                      '50-59',
                      '60-69',
                      '70-79',
                      '80-89',
                      '90+',
                      'Ign'),
             ordered = T
      )
    )) %>%
    select(-agegroup) %>%
    complete(DS_UF_SIGLA, nesting(epiyear, epiweek, data), fx_etaria=c('0-4',
                                                         '5-11',
                                                         '12-17',
                                                         '18-29',
                                                         '30-39',
                                                         '40-49',
                                                         '50-59',
                                                         '60-69',
                                                         '70-79',
                                                         '80-89',
                                                         '90+',
                                                         'Ign'),
             doses, fill=list(n=NA_integer_)) %>%
    mutate(n = case_when(
      data==min(data) & is.na(n) ~ as.integer(0),
      data>min(data) & n==0 ~ NA_integer_,
      TRUE ~ as.integer(n)
    )) %>%
    select(-data)
  
  dados.vac <- dados.vac %>%
    arrange(DS_UF_SIGLA, fx_etaria, doses, epiyear, epiweek) %>%
    group_by(DS_UF_SIGLA, fx_etaria, doses) %>%
    fill(n, .direction='down') %>%
    filter(epiyear >= 2021)
    
  dados.vac <- dados.vac %>%
    filter(fx_etaria %in% c('80-89', '90+')) %>%
    group_by(DS_UF_SIGLA, epiyear, epiweek, doses) %>%
    summarise(n=sum(n, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='80+') %>%
    bind_rows(dados.vac %>% filter(!(fx_etaria %in% c('80-89', '90+'))))
  
  dados.vac <- dados.vac %>%
    group_by(DS_UF_SIGLA, epiyear, epiweek, doses) %>%
    summarise(n=sum(n, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total') %>%
    bind_rows(dados.vac)
  
  dados.vac <- dados.vac %>%
    group_by(epiyear, epiweek, fx_etaria, doses) %>%
    summarise(n=sum(n, na.rm = T)) %>%
    ungroup() %>%
    mutate(DS_UF_SIGLA='BR') %>%
    bind_rows(dados.vac)
  
  dados.vac <- dados.vac %>%
    filter(!(fx_etaria %in% c('0-4', 'Total'))) %>%
    group_by(DS_UF_SIGLA, epiyear, epiweek, doses) %>%
    summarise(n=sum(n, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total(5+)') %>%
    bind_rows(dados.vac)
  dados.vac <- dados.vac %>%
    filter(!(fx_etaria %in% c('0-4', '5-11','Total(5+)', 'Total'))) %>%
    group_by(DS_UF_SIGLA, epiyear, epiweek, doses) %>%
    summarise(n=sum(n, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total(12+)') %>%
    bind_rows(dados.vac)
  dados.vac <- dados.vac %>%
    filter(!(fx_etaria %in% c('0-4', '5-11', '12-17', 'Total(5+)', 'Total(12+)', 'Total'))) %>%
    group_by(DS_UF_SIGLA, epiyear, epiweek, doses) %>%
    summarise(n=sum(n, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total(18+)') %>%
    bind_rows(dados.vac)
  
  return(dados.vac)
  
}

normalize.vac.data <- function(df, max.cov=.95){
  #' Estabelece max.cov como cobertura máxima possível em cada faixa etária,
  #' em termos de fração da população.
  #' Importante para cálculo de risco relativo e incidência
  
  max.coverage <- df %>%
    filter(vac_completa != 'não') %>%
    select(SG_UF_NOT, fx_etaria, frac) %>%
    group_by(SG_UF_NOT, fx_etaria) %>%
    summarise(frac=max(frac)) %>%
    mutate(reescale=case_when(
      frac > max.cov ~ frac/max.cov,
      TRUE ~ 1
    )) %>%
    select(-frac)
  
  df %>%
    left_join(max.coverage, by=c('SG_UF_NOT', 'fx_etaria')) %>%
    mutate(pop=floor(pop*reescale),
           frac=eventos/pop) %>%
  return()
}
