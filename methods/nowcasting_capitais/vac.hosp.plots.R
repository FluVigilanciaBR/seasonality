source('../report/theme.publication.R')

plot.mes <- function(list.dado=c('pop', 'sragcovid'),
                     lbl.dado=c('População geral', 'Casos de SRAG\npor COVID-19'),
                     co.uf=0,
                     gttl='Cobertura vacinal: população vs. SRAG por COVID-19',
                     gsbttl=paste0('Mês de ', mes.lbl, ', com base em dados digitados até a semana ', epiweek, ' de ', lyear)
){
  ignored.fx <- c('Ign',
                  'Total',
                  'Total(5+)',
                  'Total(12+)',
                  'Total(18+)',
                  NA)
  xlbls <- dadosBR.vac.hosp.2 %>%
    filter(!(fx_etaria %in% ignored.fx)) %>%
    pull(fx_etaria) %>%
    unique()
  xbreaks <- .5+seq(2, 3*length(xlbls), 3)
  plt <- dadosBR.vac.hosp.2 %>%
    filter(dado %in% list.dado,
           ano==ano.num,
           mes==mes.num,
           SG_UF_NOT==co.uf,
           !(fx_etaria %in% ignored.fx),
           vac_completa %in% c('ignorado', 'não/incompleta', 'D2/DU+')) %>%
    droplevels() %>%
    complete(nesting(ano, mes), nesting(SG_UF_NOT, DS_UF_SIGLA, fx_etaria), dado=c('', list.dado), vac_completa, fill=list(frac=0)) %>%
    distinct() %>%
    mutate(vac_completa = factor(vac_completa,
                                 levels=c('ignorado', 'não/incompleta', 'D2/DU+'),
                                 labels=c('ignorado', 'não vacinado\nou apenas D1', 'ao menos D2\nou dose única'))) %>%
    mutate(dado = factor(dado, levels=c('', list.dado),
                         ordered = T)) %>%
    arrange(SG_UF_NOT, ano, mes, fx_etaria, vac_completa, dado) %>%
    mutate(x = as.numeric(factor(fx_etaria)),
           x = case_when(
             dado == '' ~ 3*(x-1)+1,
             dado == 'pop' ~ 3*(x-1)+2,
             TRUE ~ 3*(x-1)+3
           ),
           dado=factor(dado, levels=list.dado,
                       labels=lbl.dado,
                       ordered=T)) %>%
    ggplot(aes(x=x, y=frac, fill=vac_completa, alpha=dado)) +
    geom_col() +
    scale_x_continuous(breaks=xbreaks, labels=xlbls, limits = c(1.5,3*length(xlbls)+.5)) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values=colorblind_pal()(4)[2:4],
                      breaks=c('ignorado', 'não vacinado\nou apenas D1', 'ao menos D2\nou dose única'),
                      name='Status vacinal') +
    scale_alpha_manual(values=c(1,.55), breaks=lbl.dado, name='') +
    labs(x='Faixa etária', y='Proporção') +
    ggtitle(gttl,
            subtitle=gsbttl) +
    theme_Publication(base_size=16, base_family = 'Roboto') +
    theme(legend.position='bottom',
          legend.direction='horizontal',
          axis.text.x = element_text(angle=35, hjust=1))
  return(plt)
}

plot.mensal <- function(list.dado=c('pop', 'sragcovid'),
                        lbl.dado=c('População geral', 'Casos de SRAG\npor COVID-19'),
                        co.uf=0,
                        gttl='Cobertura vacinal: população vs. SRAG por COVID-19',
                        gsbttl=paste0('Distribuição mensal, com base em dados digitados até a semana ', epiweek, ' de ', lyear)
){
  ignored.fx <- c('Ign',
                  'Total',
                  'Total(5+)',
                  'Total(12+)',
                  'Total(18+)',
                  NA)
  xlbls <- dadosBR.vac.hosp.2 %>%
    filter(!(fx_etaria %in% ignored.fx)) %>%
    pull(fx_etaria) %>%
    unique()
  xbreaks <- .5+seq(2, 3*length(xlbls), 3)
  xlimits <- c(1.5,3*length(xlbls)+.5)
  xlbls <- xlbls[seq(1, length(xlbls), 1)]
  plt <- dadosBR.vac.hosp.2 %>%
    filter(dado %in% list.dado,
           between(ano*100+mes, 2021*100+3, ano.num*100+mes.num),
           SG_UF_NOT==co.uf,
           !(fx_etaria %in% ignored.fx),
           vac_completa %in% c('ignorado', 'não/incompleta', 'D2/DU+')) %>%
    droplevels() %>%
    complete(nesting(ano, mes), nesting(SG_UF_NOT, fx_etaria), dado=c('', list.dado), vac_completa, fill=list(frac=0)) %>%
    distinct() %>%
    mutate(vac_completa = factor(vac_completa,
                                 levels=c('ignorado', 'não/incompleta', 'D2/DU+'),
                                 labels=c('ignorado', 'não vacinado\nou apenas D1', 'ao menos D2\nou dose única')),
           dado = factor(dado, levels=c('', list.dado),
                         ordered = T)) %>%
    arrange(SG_UF_NOT, ano, mes, fx_etaria, vac_completa, dado) %>%
    mutate(x = as.numeric(factor(fx_etaria)),
           x = case_when(
             dado == '' ~ 3*(x-1)+1,
             dado == 'pop' ~ 3*(x-1)+2,
             TRUE ~ 3*(x-1)+3
           ),
           dado=factor(dado, levels=list.dado,
                       labels=lbl.dado,
                       ordered=T),
           mes=paste0(ano, '/', sprintf(fmt = '%02d', mes))
    ) %>%
    ggplot(aes(x=x, y=frac, fill=vac_completa, alpha=dado)) +
    geom_col(width=.95) +
    scale_x_continuous(breaks=xbreaks, labels=xlbls, limits = xlimits) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values=colorblind_pal()(4)[2:4],
                      breaks=c('ignorado', 'não vacinado\nou apenas D1', 'ao menos D2\nou dose única'),
                      name='Status vacinal') +
    scale_alpha_manual(values=c(1,.55), breaks=lbl.dado, name='') +
    labs(x='Faixa etária', y='Proporção') +
    ggtitle(gttl,
            subtitle=gsbttl) +
    theme_Publication(base_size=16, base_family = 'Roboto') +
    theme(legend.position='bottom',
          legend.direction='horizontal',
          axis.text.x = element_text(angle=45, hjust=1)) +
    facet_wrap(~mes, ncol = 3)
  return(plt)
}

plot.uf.mes <- function(list.dado=c('sragcovid'),
                        gttl='Cobertura vacinal nas hospitalizações de SRAG por COVID-19',
                        gsbttl=paste0('Mês de ', mes.lbl, ', com base em dados digitados até a semana ', epiweek, ' de 2021.')
){
  ignored.fx <- c('Ign',
                  'Total',
                  'Total(5+)',
                  'Total(12+)',
                  'Total(18+)',
                  NA)
  xbreaks <- dadosBR.vac.hosp.2 %>%
    filter(!(fx_etaria %in% ignored.fx)) %>%
    pull(fx_etaria) %>%
    unique()
  xbreaks <- xbreaks[seq(1, length(xbreaks), 2)]
  plt <- dadosBR.vac.hosp.2 %>%
    filter(dado %in% list.dado,
           ano==ano.num,
           SG_UF_NOT != 0, mes==mes.num,
           !(fx_etaria %in% ignored.fx),
           vac_completa %in% c('ignorado', 'não/incompleta', 'D2/DU+')) %>%
    mutate(vac_completa = factor(vac_completa,
                                 levels=c('ignorado', 'não/incompleta', 'D2/DU+'),
                                 labels=c('ignorado', 'não vacinado\nou apenas D1', 'ao menos D2\nou dose única'))
    ) %>%
    ggplot(aes(x=fx_etaria, y=frac, fill=vac_completa)) +
    geom_col() +
    scale_fill_manual(values=colorblind_pal()(4)[2:4],
                      breaks=c('ignorado', 'não vacinado\nou apenas D1', 'ao menos D2\nou dose única'),
                      name='Status vacinal') +
    scale_x_discrete(breaks=xbreaks) +
    scale_y_continuous(breaks=c(0, .5, 1), labels = scales::percent) +
    labs(x='Faixa etária', y='Proporção') +
    theme_Publication(base_size=14, base_family = 'Roboto') +
    theme(legend.position = 'bottom',
          legend.direction = 'horizontal',
          axis.text.x = element_text(angle=45, hjust=1)) +
    ggtitle(gttl,
            subtitle=gsbttl) +
    facet_geo(~DS_UF_SIGLA, grid='br_states_grid1')
  return(plt)
}

plot.incidencia <- function(title='Impacto da vacinação nos casos e óbitos de SRAG por COVID-19',
                            subtitle=paste0('Mês de ', mes.lbl, ', com base em dados digitados até a semana ', epiweek, ' de ', lyear, '.\n',
                                             'Restrito às notificações com informação vacinal no SIVEP-Gripe.'),
                            base_family='Roboto',
                            base_size=16){
  dadosBR.vac.hosp %>%
    filter(SG_UF_NOT==0,
           ano==ano.num,
           mes==mes.num,
           fx_etaria %in% c('5-11', '12-17'),
           !vac_completa %in% c('ignorado','não', 'D1')) %>%
    droplevels() %>%
    group_by(SG_UF_NOT,
             ano,
             mes,
             dado,
             vac_completa) %>%
    summarise(eventos = sum(eventos, na.rm=T),
              pop = sum(pop, na.rm=T)) %>%
    mutate(fx_etaria='5-17',
           inc=100000*eventos/pop,
           frac=NA) %>%
    bind_rows(dadosBR.vac.hosp %>%
                filter(SG_UF_NOT==0,
                       ano==ano.num,
                       mes==mes.num,
                       fx_etaria=='Total(18+)',
                       !vac_completa %in% c('ignorado','não', 'D1'))) %>%
    mutate(fx_etaria=factor(fx_etaria, levels=c('5-17', 'Total(18+)'),
                            labels=c('5 a 17 anos', '18 anos ou mais')),
           vac_completa=factor(vac_completa,
                               levels=c('não/incompleta', 'D2/DU+'),
                               labels=c('não vacinado ou\napenas D1', 'ao menos D2\nou dose única'),
                               )
           ) %>%
    ggplot(aes(x=dado, y=inc, fill=vac_completa)) +
    geom_col(position='dodge') +
    geom_text(aes(label=sprintf('%.2f', inc)), position=position_dodge(width=0.9), vjust=-0.25) +
    scale_fill_colorblind(name='Status vacinal') +
    scale_x_discrete(labels=c('Óbitos por COVID-19',
                              'Casos graves de COVID-19')) + 
    labs(x=element_blank(), y='Incidência por 100mil hab.',
         caption=paste0('Fontes: SIVEP-Gripe, SI-PNI, e FREIRE et al. (2019)\n',
                         'Projeção populacional municipal com estimadores bayesianos, Brasil 2010 - 2030.\n',
                         'Análise: InfoGripe e Observatório COVID-19 BR')) +
    facet_wrap(~fx_etaria, scale='free_y') +
    ggtitle(title,
            subtitle) +
    theme_Publication(base_size = base_size,
                      base_family = base_family) +
    theme(legend.position = 'bottom',
          legend.direction = 'horizontal')
}
