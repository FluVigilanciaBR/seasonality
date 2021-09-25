library(tidyverse)
library(lubridate)
require(geofacet)
source('../report/theme.publication.R')

tbl.ufs <- read.csv("../data/municip_macsaud_regmetr.csv", stringsAsFactors = F) %>%
  select(CO_UF, DS_UF_SIGLA, CO_REGIAO) %>%
  filter(CO_UF != 20) %>%
  distinct() %>%
  mutate(DS_UF_SIGLA = case_when(
    CO_UF == 0 ~ 'BR',
    TRUE ~ DS_UF_SIGLA
  ))

epiweek <- 33
dadosBR <- read_csv("../clean_data/clean_data_srag_hospdeath_epiweek.csv", col_types = cols(.default = "c")) %>%
  filter(as.integer(DT_SIN_PRI_epiyear) >= 2021) %>%
  select(SG_UF,
         SG_UF_NOT,
         DT_SIN_PRI,
         HOSPITAL,
         EVOLUCAO,
         CLASSI_FIN,
         PCR_SARS2,
         idade_em_anos,
         NU_NOTIFIC,
         DT_NOTIFIC,
         CO_MUN_NOT,
         VACINA_COV,
         DOSE_1_COV,
         DOSE_2_COV,
         LAB_PR_COV) %>%
  distinct(NU_NOTIFIC, CO_MUN_NOT, DT_NOTIFIC, .keep_all=T)

janssen.lbl <- 'JHAN|JAS|JANS|JOHN|JOHS|JHON|JSNS|JON|JAHN|JENSEN|JESEN'

dadosBR <- dadosBR %>%
  mutate(
    SG_UF_NOT = as.integer(SG_UF_NOT),
    idade_em_anos = as.integer(idade_em_anos),
    janssen = grepl(janssen.lbl, LAB_PR_COV),
    DT_SIN_PRI = as.Date(DT_SIN_PRI),
    DOSE_1_COV = as.Date(DOSE_1_COV, format='%d/%m/%Y'),
    DOSE_2_COV = as.Date(DOSE_2_COV, format='%d/%m/%Y'),
    vac_completa = case_when(
      (VACINA_COV == 1 & janssen & (DT_SIN_PRI > DOSE_1_COV + 14)) |
      ((VACINA_COV == 1) & (DT_SIN_PRI > DOSE_2_COV + 14)) ~ 1,
      (VACINA_COV == 2 |
         ((VACINA_COV == 1) & is.finite(DOSE_1_COV) & !is.finite(DOSE_2_COV)) |
         (VACINA_COV == 1 & is.finite(DOSE_1_COV) & !janssen & (DT_SIN_PRI < DOSE_2_COV + 14))
         ) ~ 0,
      TRUE ~ 9
    ),
    vac_completa = factor(vac_completa, levels=c(9, 0, 1), labels=c('ignorado', 'não', 'sim')),
    fx_etaria = cut(as.integer(idade_em_anos),
                    breaks=c(18, 50, 60, 70, 80, 140),
                    labels=c('18-49', '50-59', '60-69', '70-79', '80+'),
                    right=F)
  )

dadosBR.vac.hosp <- dadosBR %>%
  filter(HOSPITAL == 1,
         CLASSI_FIN == 5,
         !is.na(idade_em_anos),
         idade_em_anos >= 18,
         month(DT_SIN_PRI) == 7) %>%
  group_by(fx_etaria, vac_completa) %>%
  summarise(hosp=n()) %>%
  group_by(fx_etaria) %>%
  summarise(vac_completa=vac_completa, hosp=hosp, frac=hosp/sum(hosp)) %>%
  mutate(SG_UF_NOT = 0)

dadosBR.vac.hosp <- dadosBR %>%
  filter(HOSPITAL == 1,
         CLASSI_FIN == 5,
         !is.na(idade_em_anos),
         idade_em_anos >= 18,
         month(DT_SIN_PRI) == 7,
         !is.na(SG_UF_NOT)) %>%
  group_by(SG_UF_NOT, fx_etaria, vac_completa) %>%
  summarise(hosp=n()) %>%
  group_by(SG_UF_NOT, fx_etaria) %>%
  summarise(vac_completa=vac_completa, hosp=hosp, frac=hosp/sum(hosp)) %>%
  bind_rows(dadosBR.vac.hosp)

dadosBR.vac.hosp <- dadosBR.vac.hosp %>%
  group_by(SG_UF_NOT, vac_completa) %>%
  summarise(hosp=sum(hosp)) %>%
  group_by(SG_UF_NOT) %>%
  summarise(vac_completa=vac_completa, hosp=hosp, frac=hosp/sum(hosp)) %>%
  mutate(fx_etaria='Total') %>%
  bind_rows(dadosBR.vac.hosp) %>%
  arrange(SG_UF_NOT, fx_etaria, vac_completa) %>%
  left_join(tbl.ufs %>% select(CO_UF, DS_UF_SIGLA),
            by=c('SG_UF_NOT' = 'CO_UF')) %>%
  select(SG_UF_NOT, DS_UF_SIGLA, fx_etaria, vac_completa, hosp, frac)

gttl <- 'Hospitalizações por SRAG com classificação de COVID-19'
gsbttl <- paste0('Mês de julho, com base em dados digitados até a semana ', epiweek, ' de 2021.')
dadosBR.vac.hosp %>%
  filter(SG_UF_NOT != 0) %>%
  ggplot(aes(x=fx_etaria, y=frac, fill=vac_completa)) +
  geom_col() +
  scale_fill_colorblind(name='Cobertura completa') +
  labs(x='Faixa etária', y='Proporção') +
  theme_Publication(base_family = 'Roboto') +
  facet_geo(~DS_UF_SIGLA, grid='br_states_grid1') +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        axis.text.x = element_text(angle=35, hjust=1)) +
  ggtitle(gttl,
          subtitle=gsbttl)
ggsave(paste0('hosp.classifin5.vac.uf.julho.se', epiweek, '2021.png'), width=10.80, height = 10.80, units='in', dpi=100)

dadosBR.vac.hosp %>%
  filter(SG_UF_NOT == 0) %>%
  ggplot(aes(x=fx_etaria, y=frac, fill=vac_completa)) +
  geom_col() +
  scale_fill_colorblind(name='Cobertura completa') +
  labs(x='Faixa etária', y='Proporção') +
  theme_Publication(base_family = 'Roboto') +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        axis.text.x = element_text(angle=30, hjust=1)) +
  ggtitle(gttl,
          subtitle=gsbttl)
ggsave(paste0('hosp.classifin5.vac.br.julho.se', epiweek, '2021.png'), width=10.8, height = 10.8, dpi=100)

saveRDS(dadosBR, paste0('infogripe.vac.hosp.se', epiweek, '2021.rds'))
saveRDS(dadosBR.vac.hosp, paste0('infogripe.vac.hosp.adultos.mensal.julho.se', epiweek, '2021.rds'))

dadosBR.vac.hosp %>%
  complete(vac_completa, fx_etaria, fill=list(hosp=0, frac=0)) %>%
  filter(vac_completa == 'ignorado') %>%
write_csv2(file=paste0('infogripe.vac.hosp.adultos.mensal.julho.vacignorado.se', epiweek, '2021.csv'))
