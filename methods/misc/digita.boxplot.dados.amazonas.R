require(ggplot2)
source('theme.publication.R')

df <- read.csv('../../data/data/delay_table.csv')

p <- df %>%
  filter(epiyear == 2020,
         epiweek <= 16) %>%
  ggplot(aes(x=Regiao, y=Interna2Evoluca_DelayDays)) +
  geom_boxplot(na.rm=TRUE, outlier.shape = NA, fill='#fc8d62')

ylim1 <- df %>%
  filter(epiyear == 2020,
         epiweek <= 16) %>%
  select(Interna2Evoluca_DelayDays) %>%
  quantile(.95, na.rm=T) * 1.05

p <- p + coord_cartesian(ylim = c(0, ylim1)) + theme_Publication() +
  labs(x='Região', y='DT digitação - DT 1os sintomas (dias)') +
  scale_y_continuous(breaks=c(0, 7, 14, 21, 28))

ggsave('oportunidade_digita.pdf', width=8, height=6)


df.all <- read.csv('../clean_data/clean_data_srag_sragnofever_epiweek.csv', stringsAsFactors = F)
df <- df.all %>%
  filter(DT_SIN_PRI_epiyear == 2020, SG_UF_NOT == 13) %>%
  select(CO_MUN_RES, CO_MUN_NOT, SG_UF_NOT,
         HOSPITAL,
         DT_INTERNA,
         DT_INTERNA_epiweek,
         DT_SIN_PRI,
         DT_SIN_PRI_epiweek,
         EVOLUCAO,
         DT_EVOLUCA,
         DT_EVOLUCA_epiweek,
         DT_PCR,
         PCR_RESUL,
         PCR_SARS2,
         POS_PCRFLU,
         DT_PCR,
         TP_FLU_PCR,
         POS_PCROUT,
         PCR_VSR,
         PCR_PARA1,
         PCR_PARA2,
         PCR_PARA3,
         PCR_PARA4,
         PCR_ADENO,
         PCR_BOCA,
         PCR_RINO,
         PCR_OUTRO,
         DS_PCR_OUT,
         SARS2,
         RINO,
         BOCA,
         METAP,
         PARA1,
         PARA2,
         PARA3,
         PARA4,
         ADNO,
         VSR,
         OTHERS,
         FLU_LAB,
         FLU_CLINIC,
         TESTED,
         POSITIVE,
         NEGATIVE,
         DELAYED,
         INCONCLUSIVE,
         NOTTESTED,
         TESTING_IGNORED,
         CLASSI_FIN,
         CRITERIO)
df %>%
  saveRDS('~/ownCloud/Fiocruz/Influenza/Estados/Amazonas/dados/clean_data_sragnofever_amazonas_20201214.rds')
