require(tidyverse)
require(lubridate)

load.vac.srag.data <- function(fpath="../clean_data/clean_data_srag_hospdeath_epiweek.csv.gz",
                               epiweek,
                               lyear,
                               fx.breaks=c(0, 5, 12, 18, seq(30, 80, 10), 140),
                               fx.labels=c('0-4', '5-11', '12-17','18-29', '30-39', '40-49', '50-59',
                                           '60-69', '70-79',
                                           '80+'),
                               overwrite=FALSE){
  #' Retorna dado de SRAG por faixa etária e status vacinal, para casos e óbitos,
  #' agregados a cada duas semanas epidemiológicas, mantendo as semanas pares.
  #' Salva em disco o dado base original e verifica se há necessidade de sobreescrever a partir
  #' do dado bruto, com base na semana epidemiológica de requisição (lyear, epiweek)
  
  vac.lvls=c(9, 0, 1,
             4, 3, 5,
             2, 6, 7,
             8)
  vac.lbls=c('ignorado', 'não', 'D1',
             'D', 'D1+', 'D2',
             'D2/D+', 'D+REF', 'D2+REF',
             'REF')
  fout = file.path('.', paste0('infogripe.vac.hosp.se', epiweek, lyear, '.rds'))
  if (file.exists(fout) & !(overwrite)){
    print('Dados de casos já pré processados, carregando...')
    dadosBR <- readRDS(fout)
  } else {
    dadosBR <- vroom::vroom(fpath, col_types = cols(
      'SG_UF' = col_double(),
      'SG_UF_NOT' = col_double(),
      'DT_SIN_PRI' = col_date(),
      'DT_SIN_PRI_epiweek' = col_integer(),
      'DT_SIN_PRI_epiyear' = col_integer(),
      'HOSPITAL' = col_integer(),
      'EVOLUCAO' = col_integer(),
      'CS_GESTANT' = col_integer(),
      'PUERPERA' = col_integer(),
      'CS_SEXO' = col_character(),
      'CLASSI_FIN' = col_integer(),
      'PCR_SARS2' = col_integer(),
      'SARS2' = col_integer(),
      'idade_em_anos' = col_double(),
      'NU_NOTIFIC' = col_character(),
      'DT_NOTIFIC' = col_date(),
      'CO_MUN_NOT' = col_double(),
      'VACINA_COV' = col_integer(),
      'DOSE_1_COV' = col_date(format='%d/%m/%Y'),
      'DOSE_2_COV' = col_date(format='%d/%m/%Y'),
      'DOSE_REF' = col_date(format='%d/%m/%Y'),
      'FAB_COV' = col_character(),
      'FAB_COV_1' = col_character(),
      'FAB_COV_2' = col_character(),
      'FAB_COVREF' = col_character(),
      'FATOR_RISC' = col_integer(),
      'CARDIOPATI' = col_integer(),
      'HEMATOLOGI' = col_integer(),
      'SIND_DOWN' = col_integer(),
      'HEPATICA' = col_integer(),
      'ASMA' = col_integer(),
      'DIABETES' = col_integer(),
      'NEUROLOGIC' = col_integer(),
      'PNEUMOPATI' = col_integer(),
      'IMUNODEPRE' = col_integer(),
      'RENAL' = col_integer(),
      'OBESIDADE' = col_integer(),
      'OUT_MORBI' = col_integer(),
      .default = col_character()
    ),
    col_select=c(SG_UF,
                 SG_UF_NOT,
                 DT_SIN_PRI,
                 DT_SIN_PRI_epiweek,
                 DT_SIN_PRI_epiyear,
                 HOSPITAL,
                 EVOLUCAO,
                 CLASSI_FIN,
                 CS_SEXO,
                 CS_GESTANT,
                 PUERPERA,
                 PCR_SARS2,
                 SARS2,
                 idade_em_anos,
                 NU_NOTIFIC,
                 DT_NOTIFIC,
                 CO_MUN_NOT,
                 VACINA_COV,
                 DOSE_1_COV,
                 DOSE_2_COV,
                 DOSE_REF,
                 FAB_COV,
                 FAB_COV_1,
                 FAB_COV_2,
                 FAB_COVREF,
                 FATOR_RISC,
                 HEMATOLOGI,
                 SIND_DOWN,
                 HEPATICA,
                 ASMA,
                 DIABETES,
                 NEUROLOGIC,
                 PNEUMOPATI,
                 IMUNODEPRE,
                 RENAL,
                 OBESIDADE,
                 OUT_MORBI,
                 MORB_DESC)
    ) %>%
      filter(as.integer(DT_SIN_PRI_epiyear) >= 2021,
             HOSPITAL == 1 | EVOLUCAO == 2) %>%
      distinct(NU_NOTIFIC, CO_MUN_NOT, DT_NOTIFIC, .keep_all=T) %>%
      as_tibble()
    gc()
    
    janssen.lbl <- 'JHAN|JAS|JANS|JANH|JAHS|JAHN|JANNS|JHAS|JOHN|JOHS|JHON|JSNS|JON|JOHO|JENS|JESEN|JAUSEN|SSEN'
    comorbs <- c('gest_puer',
                 'HEMATOLOGI',
                 'SIND_DOWN',
                 'HEPATICA',
                 'ASMA',
                 'DIABETES',
                 'NEUROLOGIC',
                 'PNEUMOPATI',
                 'IMUNODEPRE',
                 'RENAL',
                 'OBESIDADE',
                 'OUT_MORBI',
                 'MORB_DESC_2M')
    
    if (any(!is.na(dadosBR$FAB_COV1))){
      dadosBR$FAB_COV <- NA
    }
    
    dadosBR <- dadosBR %>%
      mutate(
        SG_UF_NOT = as.integer(SG_UF_NOT),
        idade_em_anos = as.integer(idade_em_anos),
        janssen = (grepl(janssen.lbl, FAB_COV_1) | grepl(janssen.lbl, FAB_COV_1)) & !grepl('D3 JANS|3ª JANS', FAB_COV_1),
        DOSE_1_COV_tmp = DOSE_1_COV,
        DOSE_1_COV_tmp = case_when(
          DOSE_1_COV_tmp < '2020-05-01' ~ NA_Date_,
          TRUE ~ DOSE_1_COV_tmp), # 1a dose anterior ao início dos estudos no BR
        DOSE_2_COV =case_when(
          DOSE_2_COV < '2020-05-01' ~ NA_Date_,
          TRUE ~ DOSE_2_COV), # 1a dose anterior ao início dos estudos no BR
        DOSE_REF = case_when(
          DOSE_REF < '2020-05-01' ~ NA_Date_,
          TRUE ~ DOSE_REF), # 1a dose anterior ao início dos estudos no BR
        DOSE_1_COV = case_when(
          DOSE_1_COV_tmp > DOSE_2_COV ~ DOSE_2_COV,
          TRUE ~ DOSE_1_COV_tmp
        ),
        DOSE_2_COV = case_when(
          DOSE_1_COV_tmp > DOSE_2_COV + 14 ~ DOSE_1_COV_tmp,
          DOSE_2_COV < DOSE_1_COV_tmp + 14 ~ NA_Date_,
          TRUE ~ DOSE_2_COV
        ),
        DOSE_1_2_COV_swap = (DOSE_1_COV_tmp != DOSE_1_COV),
        VACINA_COV = case_when(
          idade_em_anos < 18 & VACINA_COV ==1 & 
            (janssen | year(DOSE_1_COV) < 2021 | (year(DOSE_1_COV) == 2021 & epiweek(DOSE_1_COV) < 20)) ~ as.integer(2),
          idade_em_anos < 12 & 
            ((year(DOSE_1_COV) == 2021 & epiweek(DOSE_1_COV) < 48) | year(DOSE_2_COV) < 2022) ~ as.integer(2),
          idade_em_anos < 5 & VACINA_COV == 1 ~ as.integer(2),
          TRUE ~ as.integer(VACINA_COV)
        ),
        gest_puer = case_when(
          PUERPERA == 1 ~ 1,
          between(CS_GESTANT, 1, 4) ~ 1,
          TRUE ~ 0),
        vac_completa = case_when(
          (VACINA_COV == 1 & !janssen & (DT_SIN_PRI > DOSE_REF + 14)) ~ 7, #D2+REF
          (VACINA_COV == 1 & janssen & (
            (DT_SIN_PRI > DOSE_REF + 14) | (DT_SIN_PRI > DOSE_2_COV + 14))
           ) ~ 6, # D+REF
          (VACINA_COV == 1 & (DT_SIN_PRI > DOSE_2_COV + 14)) ~ 5, # D2
          (VACINA_COV == 1 & janssen & (DT_SIN_PRI > DOSE_1_COV + 14)) ~ 4, # D
          (VACINA_COV == 1 & !janssen & (DT_SIN_PRI > DOSE_1_COV + 14)) ~ 1, # D1 apenas
          VACINA_COV == 2 | (
            VACINA_COV == 1 & (DT_SIN_PRI <= DOSE_1_COV + 14)
          ) ~ 0, # Sem vacinação
          TRUE ~ 9 # Sem informação
        ),
        vac_completa = as.character(factor(vac_completa,
                                           levels=c(9, 0, 1,
                                                    4, 3, 5,
                                                    2, 6, 7,
                                                    8),
                                           labels=c('ignorado', 'não', 'D1',
                                                    'D', 'D1+', 'D2',
                                                    'D2/D+', 'D+REF', 'D2+REF',
                                                    'REF'))),
        fx_etaria = cut(as.integer(idade_em_anos),
                        breaks=fx.breaks,
                        labels=fx.labels,
                        right=F),
        epiweek = DT_SIN_PRI_epiweek,
        epiyear = DT_SIN_PRI_epiyear,
        FATOR_RISC = ifelse(gest_puer == 1, 1, FATOR_RISC),
        # Hipertensao entrando como doenca cardiovascular cronica! 
        MORB_DESC_2M = 
          ifelse(test = str_detect( string = MORB_DESC, pattern = ",") |
                   str_detect( string = MORB_DESC, pattern = "/"),
                 yes = 1,
                 no = 0)
      ) %>%
      mutate(qnt.comorbs = rowSums(across(all_of(comorbs), ~ . == 1), na.rm=T),
             qnt.comorbs = ifelse(FATOR_RISC==1 & qnt.comorbs < 1, 1, qnt.comorbs))
    gc()
    
    saveRDS(dadosBR, fout)
  }

  dadosBR.vac.hosp <- tibble()
  dadosBR <- dadosBR %>%
    filter(HOSPITAL == 1 | EVOLUCAO == 2,
           CLASSI_FIN == 5 | SARS2 == 1,
           !(DT_SIN_PRI_epiyear == 2021 & DT_SIN_PRI_epiweek < 10))
  
  dadosBR.vac.hosp <- dadosBR %>%
    group_by(epiyear, epiweek, fx_etaria, vac_completa) %>%
    summarise(eventos=n()) %>%
    group_by(epiyear, epiweek, fx_etaria) %>%
    summarise(vac_completa=vac_completa, eventos=eventos, frac=eventos/sum(eventos, na.rm=T)) %>%
    mutate(SG_UF_NOT = 0, dado='sragcovid') %>%
    ungroup() %>%
    bind_rows(dadosBR.vac.hosp)
  
  dadosBR.vac.hosp <- dadosBR %>%
    group_by(SG_UF_NOT, epiyear, epiweek, fx_etaria, vac_completa) %>%
    summarise(eventos=n()) %>%
    group_by(SG_UF_NOT, epiyear, epiweek, fx_etaria) %>%
    summarise(vac_completa=vac_completa, eventos=eventos, frac=eventos/sum(eventos, na.rm=T)) %>%
    mutate(dado='sragcovid') %>%
    ungroup() %>%
    bind_rows(dadosBR.vac.hosp)
  
  dadosBR.vac.hosp <- dadosBR %>%
    filter(EVOLUCAO == 2) %>%
    group_by(epiyear, epiweek, fx_etaria, vac_completa) %>%
    summarise(eventos=n()) %>%
    group_by(epiyear, epiweek, fx_etaria) %>%
    summarise(vac_completa=vac_completa, eventos=eventos, frac=eventos/sum(eventos, na.rm=T)) %>%
    mutate(SG_UF_NOT = 0, dado='obitocovid') %>%
    ungroup() %>%
    bind_rows(dadosBR.vac.hosp)
  
  dadosBR.vac.hosp <- dadosBR %>%
    filter(EVOLUCAO == 2) %>%
    group_by(SG_UF_NOT, epiyear, epiweek, fx_etaria, vac_completa) %>%
    summarise(eventos=n()) %>%
    group_by(SG_UF_NOT, epiyear, epiweek, fx_etaria) %>%
    summarise(vac_completa=vac_completa, eventos=eventos, frac=eventos/sum(eventos, na.rm=T)) %>%
    ungroup() %>%
    mutate(dado='obitocovid') %>%
    bind_rows(dadosBR.vac.hosp)
  
  rm(dadosBR)
  gc()
  
  dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
    group_by(SG_UF_NOT, dado, epiyear, epiweek, vac_completa) %>%
    summarise(eventos=sum(eventos, na.rm=T)) %>%
    group_by(SG_UF_NOT, dado, epiyear, epiweek) %>%
    summarise(vac_completa=vac_completa, eventos=eventos, frac=eventos/sum(eventos, na.rm=T)) %>%
    mutate(fx_etaria='Total') %>%
    ungroup() %>%
    bind_rows(dadosBR.vac.hosp)
  
  dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
    filter(!fx_etaria %in% c('0-4', 'Total')) %>%
    group_by(SG_UF_NOT, dado, epiyear, epiweek, vac_completa) %>%
    summarise(eventos=sum(eventos, na.rm=T)) %>%
    group_by(SG_UF_NOT, dado, epiyear, epiweek) %>%
    summarise(vac_completa=vac_completa, eventos=eventos, frac=eventos/sum(eventos, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total(5+)') %>%
    bind_rows(dadosBR.vac.hosp)
  dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
    filter(!fx_etaria %in% c('0-4', '5-11', 'Total(5+)', 'Total')) %>%
    group_by(SG_UF_NOT, dado, epiyear, epiweek, vac_completa) %>%
    summarise(eventos=sum(eventos, na.rm=T)) %>%
    group_by(SG_UF_NOT, dado, epiyear, epiweek) %>%
    summarise(vac_completa=vac_completa, eventos=eventos, frac=eventos/sum(eventos, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total(12+)') %>%
    bind_rows(dadosBR.vac.hosp)
  dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
    filter(!fx_etaria %in% c('0-4', '5-11', '12-17', 'Total(5+)', 'Total(12+)', 'Total')) %>%
    group_by(SG_UF_NOT, dado, epiyear, epiweek, vac_completa) %>%
    summarise(eventos=sum(eventos, na.rm=T)) %>%
    group_by(SG_UF_NOT, dado, epiyear, epiweek) %>%
    summarise(vac_completa=vac_completa, eventos=eventos, frac=eventos/sum(eventos, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total(18+)') %>%
    bind_rows(dadosBR.vac.hosp)
  
  dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
    mutate(fx_etaria = factor(fx_etaria,
                              levels=c(fx.labels,
                                       'Total',
                                       'Total(5+)',
                                       'Total(12+)',
                                       'Total(18+)'))) %>%
    arrange(SG_UF_NOT, dado, epiyear, epiweek, fx_etaria, vac_completa)
  
  dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
    filter(vac_completa %in% c('D1', 'D2', 'D2+REF')) %>%
    select(-vac_completa) %>%
    group_by(SG_UF_NOT, dado, epiyear, epiweek, fx_etaria) %>%
    summarise(eventos = sum(eventos, na.rm=T), frac=sum(frac, na.rm=T)) %>%
    mutate(vac_completa = 'D1+') %>%
    ungroup() %>%
    bind_rows(dadosBR.vac.hosp)
  
  dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
    filter(vac_completa %in% c('D+REF', 'D')) %>%
    select(-vac_completa) %>%
    group_by(SG_UF_NOT, dado, epiyear, epiweek, fx_etaria) %>%
    summarise(eventos = sum(eventos, na.rm=T), frac=sum(frac, na.rm=T)) %>%
    mutate(vac_completa = 'D+') %>%
    ungroup() %>%
    bind_rows(dadosBR.vac.hosp)
  
  dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
    filter(vac_completa %in% c('D1+', 'D+')) %>%
    select(-vac_completa) %>%
    group_by(SG_UF_NOT, dado, epiyear, epiweek, fx_etaria) %>%
    summarise(eventos = sum(eventos, na.rm=T), frac=sum(frac, na.rm=T)) %>%
    mutate(vac_completa = 'D1/D+') %>%
    ungroup() %>%
    bind_rows(dadosBR.vac.hosp)
  
  dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
    filter(vac_completa %in% c('D2+REF', 'D2')) %>%
    select(-vac_completa) %>%
    group_by(SG_UF_NOT, dado, epiyear, epiweek, fx_etaria) %>%
    summarise(eventos = sum(eventos, na.rm=T), frac=sum(frac, na.rm=T)) %>%
    mutate(vac_completa = 'D2+') %>%
    ungroup() %>%
    bind_rows(dadosBR.vac.hosp)
  
  dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
    filter(vac_completa %in% c('D2+', 'D+')) %>%
    select(-vac_completa) %>%
    group_by(SG_UF_NOT, dado, epiyear, epiweek, fx_etaria) %>%
    summarise(eventos = sum(eventos, na.rm=T), frac=sum(frac, na.rm=T)) %>%
    mutate(vac_completa = 'D2/D+') %>%
    ungroup() %>%
    bind_rows(dadosBR.vac.hosp)
  
  dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
    filter(vac_completa %in% c('D2+REF', 'D+REF')) %>%
    select(-vac_completa) %>%
    group_by(SG_UF_NOT, dado, epiyear, epiweek, fx_etaria) %>%
    summarise(eventos = sum(eventos, na.rm=T), frac=sum(frac, na.rm=T)) %>%
    mutate(vac_completa = 'REF') %>%
    ungroup() %>%
    bind_rows(dadosBR.vac.hosp) %>%
    complete(SG_UF_NOT, dado, nesting(epiyear, epiweek), fx_etaria, vac_completa=c('ignorado', 'não', 'D1',
                                                                                   'D', 'D1+', 'D+', 'D1/D+', 'D2',
                                                                                   'D2+', 'D2/D+', 'D+REF', 'D2+REF',
                                                                                   'REF'),
             fill=list(eventos=0, frac=0))
  
  gc()
  
  return(dadosBR.vac.hosp)
}

vac.srag.clean.ignored <- function(df=dadosBR.vac.hosp, complete=T){
  if (complete){
    dadosBR.vac.hosp.2 <- df %>%
      filter(vac_completa %in% c('não', 'D1', 'D', 'D2', 'D+REF', 'D2+REF')) %>%
      droplevels() %>%
      group_by(SG_UF_NOT, DS_UF_SIGLA, DS_NOME_uf, dado, epiyear, epiweek, fx_etaria, janela) %>%
      summarise(vac_completa=vac_completa, eventos.ori=eventos, eventos=round(eventos/sum(frac, na.rm=T)), pop=pop, frac=frac/sum(frac, na.rm=T)) %>%
      ungroup()
    dadosBR.vac.hosp.2 <- dadosBR.vac.hosp.2 %>%
      filter(vac_completa %in% c('D1', 'D2', 'D2+REF')) %>%
      droplevels() %>%
      group_by(SG_UF_NOT, DS_UF_SIGLA, DS_NOME_uf, dado, epiyear, epiweek, fx_etaria, janela) %>%
      summarise(vac_completa='D1+', eventos.ori=sum(eventos.ori, na.rm=T), eventos=sum(eventos, na.rm=T), pop=sum(pop, na.rm=T), frac=sum(frac, na.rm=T)) %>%
      ungroup() %>%
      bind_rows(dadosBR.vac.hosp.2)
    dadosBR.vac.hosp.2 <- dadosBR.vac.hosp.2 %>%
      filter(vac_completa %in% c('D', 'D+REF')) %>%
      droplevels() %>%
      group_by(SG_UF_NOT, DS_UF_SIGLA, DS_NOME_uf, dado, epiyear, epiweek, fx_etaria, janela) %>%
      summarise(vac_completa='D+', eventos.ori=sum(eventos.ori, na.rm=T), eventos=sum(eventos, na.rm=T), pop=sum(pop, na.rm=T), frac=sum(frac, na.rm=T)) %>%
      ungroup() %>%
      bind_rows(dadosBR.vac.hosp.2)
    dadosBR.vac.hosp.2 <- dadosBR.vac.hosp.2 %>%
      filter(vac_completa %in% c('D+', 'D1+')) %>%
      droplevels() %>%
      group_by(SG_UF_NOT, DS_UF_SIGLA, DS_NOME_uf, dado, epiyear, epiweek, fx_etaria, janela) %>%
      summarise(vac_completa='D1/D+', eventos.ori=sum(eventos.ori, na.rm=T), eventos=sum(eventos, na.rm=T), pop=sum(pop, na.rm=T), frac=sum(frac, na.rm=T)) %>%
      ungroup() %>%
      bind_rows(dadosBR.vac.hosp.2)
    dadosBR.vac.hosp.2 <- dadosBR.vac.hosp.2 %>%
      filter(vac_completa %in% c('D2', 'D2+REF')) %>%
      droplevels() %>%
      group_by(SG_UF_NOT, DS_UF_SIGLA, DS_NOME_uf, dado, epiyear, epiweek, fx_etaria, janela) %>%
      summarise(vac_completa='D2+', eventos.ori=sum(eventos.ori, na.rm=T), eventos=sum(eventos, na.rm=T), pop=sum(pop, na.rm=T), frac=sum(frac, na.rm=T)) %>%
      ungroup() %>%
      bind_rows(dadosBR.vac.hosp.2)
    dadosBR.vac.hosp.2 <- dadosBR.vac.hosp.2 %>%
      filter(vac_completa %in% c('D+', 'D2+')) %>%
      droplevels() %>%
      group_by(SG_UF_NOT, DS_UF_SIGLA, DS_NOME_uf, dado, epiyear, epiweek, fx_etaria, janela) %>%
      summarise(vac_completa='D2/D+', eventos.ori=sum(eventos.ori, na.rm=T), eventos=sum(eventos, na.rm=T), pop=sum(pop, na.rm=T), frac=sum(frac, na.rm=T)) %>%
      ungroup() %>%
      bind_rows(dadosBR.vac.hosp.2)
    dadosBR.vac.hosp.2 <- dadosBR.vac.hosp.2 %>%
      filter(vac_completa %in% c('D+REF', 'D2+REF')) %>%
      droplevels() %>%
      group_by(SG_UF_NOT, DS_UF_SIGLA, DS_NOME_uf, dado, epiyear, epiweek, fx_etaria, janela) %>%
      summarise(vac_completa='REF', eventos.ori=sum(eventos.ori, na.rm=T), eventos=sum(eventos, na.rm=T), pop=sum(pop, na.rm=T), frac=sum(frac, na.rm=T)) %>%
      ungroup() %>%
      bind_rows(dadosBR.vac.hosp.2) %>%
      mutate(inc=100000*eventos/pop,
             inc.ori=100000*eventos.ori/pop) %>%
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
                                         'Total(18+)')
             )
      ) %>%
      arrange(SG_UF_NOT, janela, epiyear, epiweek, fx_etaria, vac_completa, dado)
    
  } else {
    dadosBR.vac.hosp.2 <- df %>%
      filter(vac_completa %in% c('não', 'D1', 'D+', 'D2+')) %>%
      droplevels() %>%
      group_by(SG_UF_NOT, DS_UF_SIGLA, DS_NOME_uf, dado, epiyear, epiweek, fx_etaria, janela) %>%
      mutate(vac_completa=vac_completa, eventos.ori=eventos, eventos=round(eventos/sum(frac, na.rm=T)), pop=pop, frac=frac/sum(frac, na.rm=T)) %>%
      ungroup()
    dadosBR.vac.hosp.2 <- dadosBR.vac.hosp.2 %>%
      filter(vac_completa %in% c('D1', 'D2+')) %>%
      droplevels() %>%
      group_by(SG_UF_NOT, DS_UF_SIGLA, DS_NOME_uf, dado, epiyear, epiweek, fx_etaria, janela) %>%
      summarise(vac_completa='D1+', eventos.ori=sum(eventos.ori, na.rm=T), eventos=sum(eventos, na.rm=T), pop=sum(pop, na.rm=T), frac=sum(frac, na.rm=T)) %>%
      ungroup() %>%
      bind_rows(dadosBR.vac.hosp.2)
    dadosBR.vac.hosp.2 <- dadosBR.vac.hosp.2 %>%
      filter(vac_completa %in% c('D+', 'D1+')) %>%
      droplevels() %>%
      group_by(SG_UF_NOT, DS_UF_SIGLA, DS_NOME_uf, dado, epiyear, epiweek, fx_etaria, janela) %>%
      summarise(vac_completa='D1/D+', eventos.ori=sum(eventos.ori, na.rm=T), eventos=sum(eventos, na.rm=T), pop=sum(pop, na.rm=T), frac=sum(frac, na.rm=T)) %>%
      ungroup() %>%
      bind_rows(dadosBR.vac.hosp.2)
    dadosBR.vac.hosp.2 <- dadosBR.vac.hosp.2 %>%
      filter(vac_completa %in% c('D+', 'D2+')) %>%
      droplevels() %>%
      group_by(SG_UF_NOT, DS_UF_SIGLA, DS_NOME_uf, dado, epiyear, epiweek, fx_etaria, janela) %>%
      summarise(vac_completa='D2/D+', eventos.ori=sum(eventos.ori, na.rm=T), eventos=sum(eventos, na.rm=T), pop=sum(pop, na.rm=T), frac=sum(frac, na.rm=T)) %>%
      ungroup() %>%
      bind_rows(dadosBR.vac.hosp.2) %>%
      mutate(inc=100000*eventos/pop,
             inc.ori=100000*eventos.ori/pop) %>%
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
                                         'Total(18+)')
             )
      ) %>%
      arrange(SG_UF_NOT, janela, epiyear, epiweek, fx_etaria, vac_completa, dado)
  }
  return(dadosBR.vac.hosp.2)
}
