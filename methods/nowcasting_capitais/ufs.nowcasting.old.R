# Old UF block from nowastingCapitaisMacrosaude.R
# When nowcasting was done without age structure

pred.ufs <- c()
pred.warning <- c()
pred.failed <- c()
for(uf in uf.list$CO_UF){
  dmax <- qthreshold %>%
    filter(SG_UF_NOT == uf) %>%
    select(dmax) %>%
    as.integer()
  wdw <- qthreshold %>%
    filter(SG_UF_NOT == uf) %>%
    select(wdw) %>%
    as.integer()
  
  gpj <- 0
  uf.name <- uf.list$DS_UF_SIGLA[uf.list$CO_UF == uf]
  pop <- uf.list$Populacao[uf.list$CO_UF == uf]
  dadosBR <- dados_full %>%
    filter(DT_SIN_PRI_epiyear >= 2020)
  if (uf != 0){
    dadosBR <- dadosBR %>%
      filter(SG_UF_NOT == uf)
  }
  
  Inicio <- min(dadosBR$DT_SIN_PRI)
  
  # Semana epidemiologica termina no Sabado, entao vou excluir os dados mais recentes caso a semana nao comece no sábado.
  Fim.sat <- today.week
  
  pred.srag.summy <- try.estimate('UF', uf, dadosBR, Inicio, today.week, Dmax=dmax, wdw=wdw, zero.inflated = TRUE)
  if (inherits(pred.srag.summy, 'error')){
    dmax <- dmax + 1
    wdw <- wdw + 2
    pred.warning <- c(pred.warning, uf)
    pred.srag.summy <- try.estimate('UF', uf, dadosBR, Inicio, today.week, Dmax=dmax, wdw=wdw, zero.inflated = TRUE)
    if (inherits(pred.srag.summy, 'error')){
      pred.failed <- c(pred.failed, uf)
      message(paste('Não foi possível executar. Pulando o local', uf))
      pred.srag.summy <- readRDS(paste0(preff,'/ufs_', lyear, '_', today.week-1, '.rds')) %>%
        filter(CO_UF == uf) %>%
        select(-epiweek, -epiyear) %>%
        right_join(epiweek.table %>% transmute(Date = DT_SIN_PRI_epiweek), by='Date') %>%
        fill(CO_UF, DS_UF_SIGLA, populacao, grupo_jur, .direction='down')
      pred.ufs <- pred.ufs %>%
        bind_rows(pred.srag.summy)
      
      next
    }
  }
  pred.srag.summy <- pred.srag.summy %>%
    mutate_at(vars(-("Date"), -starts_with("tendencia")), ~ .*100000/pop) %>%
    mutate(CO_UF = uf,
           DS_UF_SIGLA = uf.name,
           populacao = pop,
           grupo_jur = gpj)
  pred.ufs <- pred.ufs %>%
    bind_rows(pred.srag.summy)
  
  p.now.srag <- plot.nowcast(pred.srag.summy, Fim=today.week ) +
    ylab("Incidência de SRAG (por 100mil hab.)") +
    xlab("Semana de primeiros sintomas") +
    scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits) +
    theme_Publication(base_size = 16, base_family = 'Roboto') +
    ggtitle(uf.name) +
    theme(plot.margin=unit(c(1,0,5,5), units='pt'),
          axis.text = element_text(size = rel(1)),
          legend.margin = margin(0,0,0,0, unit='pt'),
          legend.justification=c(0,1), 
          legend.position=c(0.015, 1.05),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.key.size = unit(14, 'pt'),
          legend.text = element_text(family = 'Roboto', size = rel(1)))
  
  p.nivel <-  plot.ts.tendencia(df = pred.srag.summy,
                                xbreaks = xbreaks,
                                xlbls = xlbls,
                                xlimits = xlimits)
  
  png(filename = paste0(preff,"/Figs/UF/fig_", uf.name, ".png"),
      width=8, height=6, units='in', res=200)
  print(plot_grid(p.now.srag, p.nivel, align='v', axis='l', nrow=2, ncol=1, rel_heights=c(2.5, 1)))
  grid::grid.raster(logo, x = 0.999, y = 0.99, just = c('right', 'top'), width = unit(1, 'inches'))
  dev.off()
}
pred.warning <- as.data.frame(list('warning'=pred.warning))
pred.failed <- as.data.frame(list('failed'=pred.failed))
print('Error list:')
print(pred.failed)
pred.warning %>%
  write.csv('uf.warning.csv', row.names=F)
pred.failed %>%
  write.csv('uf.failed.csv', row.names=F)

pred.ufs <- pred.ufs %>%
  left_join(epiweek.table, by=c('Date' = 'DT_SIN_PRI_epiweek'))

saveRDS(pred.ufs, paste0(preff,'/ufs_', lyear, '_', today.week, '.rds'))
saveRDS(pred.ufs, paste0(preff,'/ufs_current.rds'))
saveRDS(pred.ufs, '../../data/data/ufs_current.rds')

plot.ufs.tendencia(pred.ufs %>% filter(Date == today.week, grupo_jur == 0, CO_UF != 0),
                   fpath=paste0(preff, "/Figs/UF/Mapa_ufs_tendencia.png"))

pred.ufs <- pred.ufs %>%
  mutate(escala = 'incidência')
pred.ufs <- pred.ufs %>%
  mutate(escala = 'casos',
         Median = round(populacao*Median/100000),
         Q1 = round(Q1*populacao/100000),
         Q3 = round(Q3*populacao/100000),
         IC80I = round(IC80I*populacao/100000),
         IC80S = round(IC80S*populacao/100000),
         IC90I = round(IC90I*populacao/100000),
         IC90S = round(IC90S*populacao/100000),
         LI = round(LI*populacao/100000),
         LS = round(LS*populacao/100000),
         Casos = round(Casos*populacao/100000),
         full_estimate = round(full_estimate*populacao/100000),
         Casos.cut = round(Casos.cut*populacao/100000),
         rolling_average = round(rolling_average*populacao/100000)
  ) %>%
  bind_rows(pred.ufs)

pred.ufs %>%
  mutate(Epiyear = case_when(
    Date <= epiweekmax ~ 2020,
    TRUE ~ 2021
  ),
  Date = case_when(
    Epiyear == 2020 ~ Date,
    TRUE ~ Date - epiweekmax
  )) %>%
  rename('Ano epidemiológico' = Epiyear,
         'Semana epidemiológica' = Date,
         'casos estimados' = full_estimate,
         IC95I = LI,
         IC95S = LS,
         'Casos semanais reportados até a última atualização' = Casos.cut,
         'média móvel' = rolling_average,
         'tendência de curto prazo' = tendencia.3s,
         'tendência de longo prazo' = tendencia.6s,
         'Grupo Jurídico' = grupo_jur,
         'População' = populacao) %>%
  select(-Median, -Casos, -epiweek, -epiyear) %>%
  write_csv2(paste0(preff,'/estados_e_pais_serie_estimativas_tendencia_sem_filtro_febre.csv'), na = '')
rm(pred.ufs)
gc(verbose=F)
