load.pop.data <- function(year=2021){
  fx.labels=c('0-4', '5-11', '12-17', '18-29', '30-39', '40-49', '50-59',
              '60-69', '70-79', '80+')
  
  tblufpop <- read_csv('~/ownCloud/Fiocruz/Influenza/Projetos/pni-covid/efetividade/PROJ POR MICRORREGIÃO/ProjMicro-2010_2030.csv') %>%
    mutate(CO_UF = trunc(microcode/1000)) %>%
    select(-microcode, -microname, -Sexo) %>%
    rename(DS_NOME_uf = ARmaior,
           ano=Ano) %>%
    group_by(CO_UF, DS_NOME_uf, ano) %>%
    summarise_all(~sum(.)) %>%
    mutate(across(starts_with('X'), ~ round(.)),
           `0-4` = X0.a.5,
           `5-11` = round(X5.a.10 + .4*X10.a.15),
           `12-17` = round(.6*X10.a.15 + .6*X15.a.20),
           `18-29` = round(.4*X15.a.20 + X20.a.25 + X25.a.30),
           `18-24` = round(.4*X15.a.20 + X20.a.25),
           `30-39` = X30.a.35 + X35.a.40,
           `40-49` = X40.a.45 + X45.a.50,
           `50-59` = X50.a.55 + X55.a.60,
           `60-69` = X60.a.65 + X65.a.70,
           `70-79` = X70.a.75 + X75.a.80,
           `80+` = X80.a.85 + X85.a.90 + X90.,
           `50+` = `50-59` + `60-69` + `70-79` + `80+`,
           Total = rowSums(across(all_of(fx.labels))),
           `Total(5+)` = Total - `0-4`,
           `Total(12+)` = `Total(5+)` - `5-11`,
           `Total(18+)` = `Total(12+)` - `12-17`
    ) %>%
    pivot_longer(cols=c(all_of(fx.labels), starts_with('T'), starts_with('X'), `18-24`, `50+`), names_to = 'fx_etaria', values_to = 'pop')
  tblufpop <- tblufpop %>%
    group_by(ano, fx_etaria) %>%
    summarise(pop=sum(pop)) %>%
    mutate(CO_UF=0, DS_NOME_uf='BRASIL') %>%
    ungroup() %>%
    bind_rows(tblufpop)
  
  tbl.ufs <- read_csv("../data/tb_uf.csv") %>%
    select(CO_UF, DS_SIGLA) %>%
    rename(DS_UF_SIGLA = DS_SIGLA) %>%
    left_join(tblufpop %>%
                filter(ano==year) %>%
                select(-ano),
              by='CO_UF')
  
  rm(tblufpop)
  
  return(tbl.ufs)
}