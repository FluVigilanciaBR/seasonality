#' Auxiliary function for nowcastingCapitaisMacrosaude.R

run.regsaud.am <- function(){
  # Reginais de saúde AM ---------
  # Regionais de saúde do AM
  tblamazonas <- read.csv('../misc/Amazonas/Tab_munregsaud_manaus.csv', stringsAsFactors = FALSE)  %>%
    transmute(codmun = co_municipio_ibge,
              co_regsaud_am = co_regiao,
              no_regiao_am = no_regiao)
  
  tblREGSAUDAM <- tblCADMUN %>%
    right_join(tblamazonas, by=c('CO_MUNICIP'='codmun')) %>%
    filter(!is.na(co_regsaud_am)) %>%
    select(co_regsaud_am, no_regiao_am, CO_UF, DS_UF_SIGLA, Populacao) %>%
    group_by(co_regsaud_am, no_regiao_am, CO_UF, DS_UF_SIGLA) %>%
    summarise(Populacao = sum(Populacao)) %>%
    mutate(DS_NOMEPAD_REGSAUDAM_clean = str_replace_all(no_regiao_am, ' ', '_')) %>%
    mutate(DS_NOMEPAD_REGSAUDAM_clean = str_replace_all(DS_NOMEPAD_REGSAUDAM_clean, '/', '-')) %>%
    ungroup()
  rownames(tblREGSAUDAM) <- NULL
  
  dados_am <- dados_full %>%
    filter(SG_UF == 13 | SG_UF_NOT == 13) %>%
    left_join(tblamazonas %>%
                select(codmun, co_regsaud_am),
              by=c('CO_MUN_NOT' = 'codmun'))
  dados_am$co_regsaud_am[dados_am$co_regsaud_am == 13000] <- 13001
  dados_am$co_regsaud_am[dados_am$CO_MUN_RES == 130260] <- 13000
  
  dados_am <- dados_am %>%
    filter(co_regsaud_am %in% tblREGSAUDAM$co_regsaud_am)
  
  qthreshold <- dados_am %>%
    select(co_regsaud_am, grupo_jur, DT_DIGITA_epiyear, DT_DIGITA_epiweek, SinPri2Digita_DelayWeeks) %>%
    filter(
      (DT_DIGITA_epiyear == lyear-1 & DT_DIGITA_epiweek > today.week.ori) |
        (DT_DIGITA_epiyear == lyear)
    ) %>%
    group_by(co_regsaud_am, grupo_jur) %>%
    summarize(dmax = quantile(SinPri2Digita_DelayWeeks, probs=.9, na.rm=TRUE)) %>%
    mutate(dmax = dmax + 2,
           dmax = case_when(
             dmax > args$dmax ~ as.numeric(args$dmax),
             dmax < 4 ~ 4,
             TRUE ~ as.numeric(dmax)),
           wdw = case_when(
             2*dmax > args$window ~ as.numeric(args$window),
             TRUE ~ as.numeric(2*dmax))
    )
  
  qthreshold <- dados_am %>%
    select(co_regsaud_am, DT_DIGITA_epiyear, DT_DIGITA_epiweek, SinPri2Digita_DelayWeeks) %>%
    filter(
      (DT_DIGITA_epiyear == lyear-1 & DT_DIGITA_epiweek > today.week.ori) |
        (DT_DIGITA_epiyear == lyear)
    ) %>%
    group_by(co_regsaud_am) %>%
    summarize(dmax = quantile(SinPri2Digita_DelayWeeks, probs=.9, na.rm=TRUE)) %>%
    mutate(dmax = dmax + 2,
           dmax = case_when(
             dmax > args$dmax ~ as.numeric(args$dmax),
             dmax < 4 ~ 4,
             TRUE ~ as.numeric(dmax)),
           wdw = case_when(
             2*dmax > args$window ~ as.numeric(args$window),
             TRUE ~ as.numeric(2*dmax)),
           grupo_jur = 0) %>%
    rbind(qthreshold)
  
  pred.regsaud.am <- c()
  pred.warning <- c()
  pred.failed <- c()
  for(k in 1:nrow(tblREGSAUDAM)){
    
    dadosBR0 <- dados_am %>%
      mutate(
        DT_SIN_PRI = ymd(DT_SIN_PRI),
        DT_DIGITA = ymd(DT_DIGITA),
      ) %>%
      filter(
        DT_SIN_PRI_epiyear >= 2020,
        co_regsaud_am == tblREGSAUDAM$co_regsaud_am[k]
      )
    title0 <- tblREGSAUDAM$no_regiao_am[k]
    
    for (gpj in c(0, 1, 2, 3)){
      if (gpj > 0){
        dadosBR <- dadosBR0 %>%
          filter(grupo_jur == gpj)
      } else {
        dadosBR <- dadosBR0
      }
      title <- paste0(title0, ttl.xtra[gpj + 1])
      
      if (nrow(dadosBR[dadosBR$DT_SIN_PRI_epiweek >= today.week - 20,]) > 10){
        dmax <- qthreshold %>%
          filter(co_regsaud_am == tblREGSAUDAM$co_regsaud_am[k],
                 grupo_jur == gpj) %>%
          select(dmax) %>%
          as.integer()
        wdw <- qthreshold %>%
          filter(co_regsaud_am == tblREGSAUDAM$co_regsaud_am[k],
                 grupo_jur == gpj) %>%
          select(wdw) %>%
          as.integer()
        
        
        Inicio <- min(dadosBR$DT_SIN_PRI)
        REGSAUDAM.id <- dadosBR$co_regsaud_am %>% unique()
        # Semana epidemiologica termina no Sabado, entao vou excluir os dados mais recentes caso a semana nao comece no sábado.
        Fim.sat <- today.week
        warn.lbl <- paste(REGSAUDAM.id, 'gpj=', gpj)
        pred.srag.summy <- try.estimate('REGSAUDE AM', warn.lbl,
                                        dadosBR, Inicio, today.week, Dmax=dmax, wdw=wdw, zero.inflated = TRUE)
        if (inherits(pred.srag.summy, 'error')){
          dmax <- dmax + 1
          wdw <- wdw + 2
          pred.warning <- c(pred.warning, warn.lbl)
          pred.srag.summy <- try.estimate('REGSAUDE AM', warn.lbl,
                                          dadosBR, Inicio, today.week, Dmax=dmax, wdw=wdw, zero.inflated = TRUE)
          if (inherits(pred.srag.summy, 'error')){
            pred.failed <- c(pred.failed, warn.lbl)
            message(paste('Não foi possível executar. Pulando o local', warn.lbl))
            pred.srag.summy <- readRDS(paste0(preff,'/estimativas_regsaud_am', lyear, '_', today.week-1, '.rds')) %>%
              filter(co_regsaud_am == as.integer(REGSAUDAM.id),
                     grupo_jur == gpj) %>%
              select(-epiweek, -epiyear, -co_regsaud_am, -no_regiao_am) %>%
              right_join(epiweek.table %>% transmute(Date = DT_SIN_PRI_epiweek), by='Date') %>%
              fill(co_reg_saud_am, DS_NOMEPAD_regsaud, CO_UF, DS_UF_SIGLA, populacao, grupo_jur, .direction='down')
            pred.regsaud.am <- pred.regsaud.am %>%
              bind_rows(pred.srag.summy)
            
            next
          }
        }
        pop <- tblREGSAUDAM$Populacao[k]
        REGSAUDAM.name <- tblREGSAUDAM$DS_NOMEPAD_REGSAUDAM_clean[k]
        uf <- tblREGSAUDAM$CO_UF[k]
        uf.name <- tblREGSAUDAM$DS_UF_SIGLA[k]
        pred.srag.summy <- pred.srag.summy %>%
          mutate_at(vars(-("Date"), -starts_with("tendencia")), ~ .*100000/pop) %>%
          mutate(co_regsaud_am = as.integer(REGSAUDAM.id),
                 DS_NOMEPAD_regsaud = title,
                 CO_UF = uf,
                 DS_UF_SIGLA = uf.name,
                 populacao = pop,
                 grupo_jur = gpj)
        pred.regsaud.am <- pred.regsaud.am %>%
          bind_rows(pred.srag.summy)
        p.now.srag <- plot.nowcast(pred.srag.summy, Fim=today.week ) +
          ylab("Incidência de SRAG (por 100mil hab.)") +
          xlab("Semana de primeiros sintomas") +
          scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits) +
          theme_Publication(base_size = 16, base_family = 'Roboto') +
          ggtitle(paste0(tblREGSAUDAM$DS_UF_SIGLA[k], ": ", title)) +
          theme(plot.margin=unit(c(1,0,5,5), units='pt'),
                axis.text = element_text(size = rel(1)),
                legend.margin = margin(0,0,0,0, unit='pt'),
                legend.justification=c(0,1),
                legend.position=c(0.015, 1.05),
                legend.background = element_blank(),
                legend.key = element_blank(),
                legend.key.size = unit(14, 'pt'),
                legend.text = element_text(family = 'Roboto', size = rel(1)))
        p.nivel <- plot.ts.tendencia(df = pred.srag.summy,
                                     xbreaks = xbreaks,
                                     xlbls = xlbls,
                                     xlimits = xlimits)
        png(filename = paste0("./Figs/REGSAUD/fig_", tblREGSAUDAM$DS_UF_SIGLA[k], '_', tblREGSAUDAM$co_regsaud_am[k],
                              suff.xtra[gpj + 1],
                              ".png"),
            width=8, height=6, units='in', res=200)
        print(plot_grid(p.now.srag, p.nivel, align='v', axis='l', nrow=2, ncol=1, rel_heights=c(2.5, 1)))
        grid::grid.raster(logo, x = 0.999, y = 0.95, just = c('right', 'top'), width = unit(1, 'inches'))
        dev.off()
      }
    }
  }
  pred.warning <- as.data.frame(list('warning'=pred.warning))
  pred.failed <- as.data.frame(list('failed'=pred.failed))
  print('Error list:')
  print(pred.failed)
  pred.warning %>%
    write.csv('regsaudam.warning.csv', row.names=F)
  pred.failed %>%
    write.csv('regsaudam.failed.csv', row.names=F)
  
  pred.regsaud.am <- pred.regsaud.am %>%
    left_join(tblREGSAUDAM %>% select(co_regsaud_am, no_regiao_am), by='co_regsaud_am')
  saveRDS(pred.regsaud.am, paste0(preff,'/estimativas_regsaud_am', lyear, '_', today.week, '.rds'))
  saveRDS(pred.regsaud.am, paste0(preff,'/estimativas_regsaud_am.rds'))
  
  pred.regsaud.am <- pred.regsaud.am %>%
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
    bind_rows(pred.regsaud.am)
  
  pred.regsaud.am %>%
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
           'População' = populacao) %>%
    select(-Median, -Casos) %>%
    write_csv2(paste0(preff,'/regsaud_am_serie_estimativas_tendencia_sem_filtro_febre.csv'), na = '')
  
  
}