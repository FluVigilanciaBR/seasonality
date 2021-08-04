uf.list <- c(0,
             33,
             35,
             31,
             32,
             43,
             41,
             42,
             50,
             51,
             52,
             53)

tmp.sragcovid.v2.obito <- calc.props.and.ic(df=dadosBR %>% filter(CLASSI_FIN==5, EVOLUCAO==2),
                                            epiweek_start=epiweek_start,
                                            epiweek_end=today.week-4,
                                            epiweek_ic_stop=53,
                                            breaks=c(0,60,70,80,150),
                                            labels=c("0-59", "60-69", '70-79', '80+'))
probs.obito <- tmp.sragcovid.v2.obito %>%
  filter(DT_SIN_PRI_epiweek == (53+10),
         SG_UF %in% uf.list) %>%
  select(SG_UF, age_cat, prop_median, prop_li, prop_ls) %>%
  rename(prop_median.se10 = prop_median,
         prop_li.se10 = prop_li,
         prop_ls.se10 = prop_ls)
proj.sragcovid.v2.obito <- tmp.sragcovid.v2.obito %>%
  right_join(probs.obito, by=c('SG_UF', 'age_cat'))
proj.sragcovid.v2.obito <- proj.sragcovid.v2.obito %>%
  filter(DT_SIN_PRI_epiweek > 53+10, age_cat=='0-59') %>%
  select(-starts_with('FX')) %>%
  transmute(SG_UF = SG_UF,
            DT_SIN_PRI_epiweek=DT_SIN_PRI_epiweek,
            n_median = round(cases/prop_median.se10),
            n_li = round(cases/prop_ls.se10),
            n_ls = round(cases/prop_li.se10)) %>%
  left_join(proj.sragcovid.v2.obito, by=c('SG_UF', 'DT_SIN_PRI_epiweek')) %>%
  mutate(obitos_median=round(n_median*prop_median.se10),
         obitos_li=round(n_li*prop_li.se10),
         obitos_ls=round(n_ls*prop_ls.se10)) %>%
  select(-starts_with('FX'))


tmp.sragcovid.v2 <- calc.props.and.ic(df=dadosBR %>% filter(CLASSI_FIN==5),
                                            epiweek_start=epiweek_start,
                                            epiweek_end=today.week-4,
                                            epiweek_ic_stop=53,
                                            breaks=c(0,60,70,80,150),
                                            labels=c("0-59", "60-69", '70-79', '80+'))
probs <- tmp.sragcovid.v2 %>%
  filter(DT_SIN_PRI_epiweek == (53+10),
         SG_UF %in% uf.list) %>%
  select(SG_UF, age_cat, prop_median, prop_li, prop_ls) %>%
  rename(prop_median.se10 = prop_median,
         prop_li.se10 = prop_li,
         prop_ls.se10 = prop_ls)


proj.sragcovid.v2 <- tmp.sragcovid.v2 %>%
  right_join(probs, by=c('SG_UF', 'age_cat'))

proj.sragcovid.v2 <- proj.sragcovid.v2 %>%
  filter(DT_SIN_PRI_epiweek > 53+10, age_cat=='0-59') %>%
  select(-starts_with('FX')) %>%
  transmute(SG_UF = SG_UF,
            DT_SIN_PRI_epiweek=DT_SIN_PRI_epiweek,
            n_median = round(cases/prop_median.se10),
            n_li = round(cases/prop_ls.se10),
            n_ls = round(cases/prop_li.se10)) %>%
  left_join(proj.sragcovid.v2, by=c('SG_UF', 'DT_SIN_PRI_epiweek')) %>%
  mutate(cases_median=round(n_median*prop_median.se10),
         cases_li=round(n_li*prop_li.se10),
         cases_ls=round(n_ls*prop_ls.se10)) %>%
  select(-starts_with('FX'))



# Observed data -------------
df <- calc.age.cat(dadosBR %>% filter(CLASSI_FIN==5), epiweek_end = today.week - 5,
                   breaks = c(0, 60, 70, 80, 200),
                   labels = c('0-59', '60-69', '70-79', '80+'))
df <- calc.age.cat(dadosBR %>% filter(CLASSI_FIN==5, EVOLUCAO==2), epiweek_end = today.week - 5,
                   breaks = c(0, 60, 70, 80, 200),
                   labels = c('0-59', '60-69', '70-79', '80+')) %>%
  rename(obitos=n) %>%
  right_join(df, by=c('SG_UF', 'DT_SIN_PRI_epiweek', 'age_cat'))
df <- proj.sragcovid.v2 %>%
  filter(age_cat != '0-59') %>%
  select(SG_UF,
         DT_SIN_PRI_epiweek,
         age_cat,
         cases_median,
         cases_li,
         cases_ls) %>%
  rename(casos_mediana=cases_median,
         casos_li=cases_li,
         casos_ls=cases_ls) %>%
  right_join(df, by=c('SG_UF', 'DT_SIN_PRI_epiweek', 'age_cat'))
df <- proj.sragcovid.v2.obito %>%
  filter(age_cat != '0-59') %>%
  select(SG_UF,
         DT_SIN_PRI_epiweek,
         age_cat,
         obitos_median,
         obitos_li,
         obitos_ls) %>%
  rename(obitos_mediana=obitos_median) %>%
  right_join(df, by=c('SG_UF', 'DT_SIN_PRI_epiweek', 'age_cat'))

df %>%
  filter(SG_UF %in% uf.list) %>%
  select(n, obitos, casos_mediana, casos_li, casos_ls, obitos_mediana, obitos_li, obitos_ls) %>%
  group_by(SG_UF) %>%
  summarize(dif_casos_li = sum(casos_li-n, na.rm=T),
            dif_casos_mediana = sum(casos_mediana-n, na.rm=T),
            dif_casos_ls = sum(casos_ls-n, na.rm=T),
            dif_obitos_li = sum(obitos_li-obitos, na.rm=T),
            dif_obitos_mediana = sum(obitos_mediana-obitos, na.rm=T),
            dif_obitos_ls = sum(obitos_ls-obitos, na.rm=T)) %>%
  write.csv2('Projecao_diferenca_hosp_obitos.csv', row.names=F)

df2 <- tbl.ufs %>%
  mutate(`0-59`=FX_0_a_4+
           FX_5_a_9+
           FX_10_a_14+
           FX_15_a_19+
           FX_20_a_29+
           FX_30_a_39+
           FX_40_a_49+
           FX_50_a_59,
         `60-69`=FX_60_a_69,
         `70-79`=FX_70_a_79,
         `80+`=FX_80_e_mais) %>%
  select(-starts_with('FX')) %>%
  pivot_longer(cols=all_of(c('0-59', '60-69', '70-79', '80+')), names_to='age_cat', values_to='pop') %>%
  mutate(CO_UF = as.integer(CO_UF)) %>%
  right_join(df %>% filter(SG_UF %in% uf.list), by=c('CO_UF'='SG_UF', 'age_cat')) %>%
  mutate(inc = 100000*n/pop,
         inc_mediana=100000*casos_mediana/pop,
         inc_li=100000*casos_li/pop,
         inc_ls=100000*casos_ls/pop,
         inc.obitos = 100000*obitos/pop,
         inc.obitos_mediana=100000*obitos_mediana/pop,
         inc.obitos_li=100000*obitos_li/pop,
         inc.obitos_ls=100000*obitos_ls/pop) %>%
  mutate(age_cat=factor(age_cat, levels=c('0-59', '60-69', '70-79', '80+'), labels=c('0-59', '60-69', '70-79', '80+')))

# Plot -------------
for (uf in uf.list){
  sigla <- as.character(tbl.ufs$DS_UF_SIGLA[tbl.ufs$CO_UF == uf])
  
  p.now.srag <- plot.nowcast(srag.uf %>% filter(CO_UF == uf), Fim=today.week-4 ) +
    ggtitle('SRAG em geral')
  plt <- plt.age.prop.pointrange(tmp.sragcovid.v2, uf, paste0(sigla, ': SRAG por COVID-19')) +
    theme(legend.position = 'none')
  p <- align_plots(plt, p.now.srag, align='v', axis='lr')
  png(paste0('./Figs/', sigla, '_prop_etaria_sragcovid.png'), height=6, width=9, units='in', res=100)
  print(plot_grid(p[[1]], p[[2]], ncol=1, rel_heights = c(1, .4)))
  dev.off()
  
  facet_cols=c('inc', 'inc.obitos')
  facet_labs=c('Casos de SRAG por COVID-19', 'Óbitos de SRAG por COVID-19')
  dado.labs <- facet_labs
  names(dado.labs) <- facet_cols
  ylabs='Incidência (por 100mil hab.)'
  
  p <- df2 %>%
    filter(CO_UF==uf) %>%
    select(-n, -obitos) %>%
    pivot_longer(cols=all_of(facet_cols),
                 names_to='dado', values_to = 'valor') %>%
    mutate(valor_mediana=case_when(
      dado=='inc'~inc_mediana,
      dado=='inc.obitos'~inc.obitos_mediana
    ),
    valor_li=case_when(
      dado=='inc'~inc_li,
      dado=='inc.obitos'~inc.obitos_li
    ),
    valor_ls=case_when(
      dado=='inc'~inc_ls,
      dado=='inc.obitos'~inc.obitos_ls
    )
    ) %>%
    ggplot(aes(x=DT_SIN_PRI_epiweek,
               y=valor,
               color=age_cat,
               fill=age_cat)) +
    geom_line(aes(linetype='Observado'))+
    geom_line(aes(y=valor_mediana, linetype='Projeção')) +
    geom_ribbon(aes(ymin=valor_li, ymax=valor_ls), alpha=.25, colour=NA)+
    scale_color_colorblind()+
    scale_fill_colorblind()+
    scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits) +
    scale_linetype_manual(values=c(1,2),name=NULL) +
    labs(title=sigla,
         y=ylabs,
         x='Semana epidemiológica de 1os sintomas',
         color='Faixa\netária',
         fill='Faixa\netária') +
    theme_Publication(base_family = 'Roboto') +
    theme(legend.key.width = unit(.5, 'cm'))+
    facet_wrap(~dado, ncol=1, scales='free_y',
               labeller=labeller(dado=dado.labs))
  png(paste0('./Figs/', sigla, '_projecao.png'), height=6, width=9, units='in', res=100)
  print(p)
  dev.off()
}
