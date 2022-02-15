require(tidyverse)
require(lubridate)

load.vac.srag.data <- function(fpath="../clean_data/clean_data_srag_hospdeath_epiweek.csv.gz",
                               epiweek,
                               lyear,
                               fx.breaks=c(0, 5, 12, 18, seq(30, 80, 10), 140),
                               fx.labels=c('0-4', '5-11', '12-17','18-29', '30-39', '40-49', '50-59',
                                           '60-69', '70-79',
                                           '80+')){
  dadosBR <- vroom::vroom(fpath, col_types = cols(
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
    'SARS2' = col_integer(),
    'idade_em_anos' = col_double(),
    'NU_NOTIFIC' = col_character(),
    'DT_NOTIFIC' = col_date(),
    'CO_MUN_NOT' = col_double(),
    'VACINA_COV' = col_integer(),
    'DOSE_1_COV' = col_date(format='%d/%m/%Y'),
    'DOSE_2_COV' = col_date(format='%d/%m/%Y'),
    'FAB_COV' = col_character(),
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
               FAB_COV,
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
    distinct(NU_NOTIFIC, CO_MUN_NOT, DT_NOTIFIC, .keep_all=T)
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
          ((VACINA_COV == 1) & (DT_SIN_PRI > DOSE_2_COV + 14)) ~ 2, # D2/DU ou mais
        (VACINA_COV == 1 & !janssen) & (
          (DT_SIN_PRI > DOSE_1_COV + 14) &
            (!is.finite(DOSE_2_COV) | (DT_SIN_PRI <= DOSE_2_COV + 14))
        ) ~ 1, # D1 apenas
        VACINA_COV == 2 |
          (VACINA_COV == 1 & 
             is.finite(DOSE_1_COV) & DT_SIN_PRI <= DOSE_1_COV + 14
          ) ~ 0, # Sem vacinação
        TRUE ~ 9 # Sem informação
      ),
      vac_completa = as.character(factor(vac_completa,
                                         levels=c(9, 0, 3, 1, 2),
                                         labels=c('ignorado', 'não', 'não/incompleta', 'D1', 'D2/DU+'))),
      fx_etaria = cut(as.integer(idade_em_anos),
                      breaks=fx.breaks,
                      labels=fx.labels,
                      right=F),
      mes = month(DT_SIN_PRI),
      ano = year(DT_SIN_PRI),
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
  
  saveRDS(dadosBR, paste0('infogripe.vac.hosp.se', epiweek, lyear, '.rds'))
  
  dadosBR.vac.hosp <- dadosBR %>%
    filter(HOSPITAL == 1 | EVOLUCAO == 2,
           CLASSI_FIN == 5 | SARS2 == 1,
           (ano==2021 & mes >= 3) | ano > 2021) %>%
    group_by(ano, mes, fx_etaria, vac_completa) %>%
    summarise(eventos=n()) %>%
    group_by(ano, mes, fx_etaria) %>%
    summarise(vac_completa=vac_completa, eventos=eventos, frac=eventos/sum(eventos, na.rm=T)) %>%
    mutate(SG_UF_NOT = 0, dado='sragcovid') %>%
    ungroup()
  
  dadosBR.vac.hosp <- dadosBR %>%
    filter(HOSPITAL == 1 | EVOLUCAO == 2,
           CLASSI_FIN == 5 | SARS2 == 1,
           (ano==2021 & mes >= 3) | ano > 2021,
           !is.na(SG_UF_NOT)) %>%
    group_by(SG_UF_NOT, ano, mes, fx_etaria, vac_completa) %>%
    summarise(eventos=n()) %>%
    group_by(SG_UF_NOT, ano, mes, fx_etaria) %>%
    summarise(vac_completa=vac_completa, eventos=eventos, frac=eventos/sum(eventos, na.rm=T)) %>%
    mutate(dado='sragcovid') %>%
    ungroup() %>%
    bind_rows(dadosBR.vac.hosp)
  
  dadosBR.vac.hosp <- dadosBR %>%
    filter(CLASSI_FIN == 5 | SARS2 == 1,
           EVOLUCAO == 2,
           (ano==2021 & mes >= 3) | ano > 2021) %>%
    group_by(ano, mes, fx_etaria, vac_completa) %>%
    summarise(eventos=n()) %>%
    group_by(ano, mes, fx_etaria) %>%
    summarise(vac_completa=vac_completa, eventos=eventos, frac=eventos/sum(eventos, na.rm=T)) %>%
    mutate(SG_UF_NOT = 0, dado='obitocovid') %>%
    ungroup() %>%
    bind_rows(dadosBR.vac.hosp)
  
  dadosBR.vac.hosp <- dadosBR %>%
    filter(CLASSI_FIN == 5 | SARS2 == 1,
           EVOLUCAO == 2,
           (ano==2021 & mes >= 3) | ano > 2021,
           !is.na(SG_UF_NOT)) %>%
    group_by(SG_UF_NOT, ano, mes, fx_etaria, vac_completa) %>%
    summarise(eventos=n()) %>%
    group_by(SG_UF_NOT, ano, mes, fx_etaria) %>%
    summarise(vac_completa=vac_completa, eventos=eventos, frac=eventos/sum(eventos, na.rm=T)) %>%
    ungroup() %>%
    mutate(dado='obitocovid') %>%
    bind_rows(dadosBR.vac.hosp)
  
  rm(dadosBR)
  gc()
  
  dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
    group_by(SG_UF_NOT, dado, ano, mes, vac_completa) %>%
    summarise(eventos=sum(eventos, na.rm=T)) %>%
    group_by(SG_UF_NOT, dado, ano, mes) %>%
    summarise(vac_completa=vac_completa, eventos=eventos, frac=eventos/sum(eventos, na.rm=T)) %>%
    mutate(fx_etaria='Total') %>%
    ungroup() %>%
    bind_rows(dadosBR.vac.hosp)
  
  dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
    filter(!fx_etaria %in% c('0-4', 'Total')) %>%
    group_by(SG_UF_NOT, dado, ano, mes, vac_completa) %>%
    summarise(eventos=sum(eventos, na.rm=T)) %>%
    group_by(SG_UF_NOT, dado, ano, mes) %>%
    summarise(vac_completa=vac_completa, eventos=eventos, frac=eventos/sum(eventos, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total(5+)') %>%
    bind_rows(dadosBR.vac.hosp)
  dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
    filter(!fx_etaria %in% c('0-4', '5-11', 'Total(5+)', 'Total')) %>%
    group_by(SG_UF_NOT, dado, ano, mes, vac_completa) %>%
    summarise(eventos=sum(eventos, na.rm=T)) %>%
    group_by(SG_UF_NOT, dado, ano, mes) %>%
    summarise(vac_completa=vac_completa, eventos=eventos, frac=eventos/sum(eventos, na.rm=T)) %>%
    ungroup() %>%
    mutate(fx_etaria='Total(12+)') %>%
    bind_rows(dadosBR.vac.hosp)
  dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
    filter(!fx_etaria %in% c('0-4', '5-11', '12-17', 'Total(5+)', 'Total(12+)', 'Total')) %>%
    group_by(SG_UF_NOT, dado, ano, mes, vac_completa) %>%
    summarise(eventos=sum(eventos, na.rm=T)) %>%
    group_by(SG_UF_NOT, dado, ano, mes) %>%
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
    arrange(SG_UF_NOT, dado, ano, mes, fx_etaria, vac_completa)
  
  dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
    filter(vac_completa %in% c('não', 'D1')) %>%
    select(-vac_completa) %>%
    group_by(SG_UF_NOT, dado, ano, mes, fx_etaria) %>%
    summarise(eventos = sum(eventos, na.rm=T), frac=sum(frac, na.rm=T)) %>%
    mutate(vac_completa = 'não/incompleta') %>%
    bind_rows(dadosBR.vac.hosp) %>%
    complete(SG_UF_NOT, dado, nesting(ano, mes), fx_etaria, vac_completa, fill=list(eventos=0, frac=0))
  
  gc()
  
  return(dadosBR.vac.hosp)
}