load.vac.data <- function(){
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
    mutate(mes=month(data), ano=year(data)) %>%
    group_by(DS_UF_SIGLA, ano, mes, agegroup, doses) %>%
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
    complete(DS_UF_SIGLA, nesting(ano, mes), fx_etaria, doses, fill=list(n=0))
  
  dados.vac <- dados.vac %>%
    filter(fx_etaria %in% c('80-89', '90+')) %>%
    group_by(DS_UF_SIGLA, ano, mes, doses) %>%
    summarise(n=sum(n, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='80+') %>%
    bind_rows(dados.vac %>% filter(!(fx_etaria %in% c('80-89', '90+'))))
  
  dados.vac <- dados.vac %>%
    group_by(DS_UF_SIGLA, ano, mes, doses) %>%
    summarise(n=sum(n, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total') %>%
    bind_rows(dados.vac)
  
  dados.vac <- dados.vac %>%
    group_by(ano, mes, fx_etaria, doses) %>%
    summarise(n=sum(n, na.rm = T)) %>%
    ungroup() %>%
    mutate(DS_UF_SIGLA='BR') %>%
    bind_rows(dados.vac)
  
  dados.vac <- dados.vac %>%
    filter(!(fx_etaria %in% c('0-4', 'Total'))) %>%
    group_by(DS_UF_SIGLA, ano, mes, doses) %>%
    summarise(n=sum(n, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total(5+)') %>%
    bind_rows(dados.vac)
  dados.vac <- dados.vac %>%
    filter(!(fx_etaria %in% c('0-4', '5-11','Total(5+)', 'Total'))) %>%
    group_by(DS_UF_SIGLA, ano, mes, doses) %>%
    summarise(n=sum(n, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total(12+)') %>%
    bind_rows(dados.vac)
  dados.vac <- dados.vac %>%
    filter(!(fx_etaria %in% c('0-4', '5-11', '12-17', 'Total(5+)', 'Total(12+)', 'Total'))) %>%
    group_by(DS_UF_SIGLA, ano, mes, doses) %>%
    summarise(n=sum(n, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total(18+)') %>%
    bind_rows(dados.vac)
  
  dados.vac <- dados.vac %>%
    left_join(tbl.ufs,
              by=c('DS_UF_SIGLA', 'fx_etaria')) %>%
    mutate(n=replace_na(n,0)) %>%
    mutate(fx_etaria = factor(fx_etaria,
                              levels=c(fx.labels,
                                       'Total',
                                       'Total(5+)',
                                       'Total(12+)',
                                       'Total(18+)'))) %>%
    arrange(CO_UF, ano, mes, doses, fx_etaria) %>%
    group_by(CO_UF, doses, fx_etaria) %>%
    summarise(DS_UF_SIGLA=DS_UF_SIGLA, ano=ano, mes=mes, n=cumsum(n), pop=pop, vac.frac=n/pop) %>%
    ungroup()
  
  return(dados.vac)
}