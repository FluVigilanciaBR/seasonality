source('../report/theme.publication.R')

plot.mes <- function(df,
                     list.dado=c('pop', 'sragcovid'),
                     lbl.dado=c('População geral', 'Casos de SRAG\npor COVID-19'),
                     co.uf=0,
                     epiyear.num,
                     epiweek.num,
                     vac_levels=c('ignorado', 'não', 'D1', 'D2/D+'),
                     vac_lbls=c('ignorado', 'não vacinado', 'apenas D1', 'ao menos D2\nou D'),
                     gttl='Cobertura vacinal: população vs. SRAG por COVID-19',
                     gsbttl=NULL
){
  num.colors = length(vac_lbls) + 1
  ignored.fx <- c('Ign',
                  'Total',
                  'Total(5+)',
                  'Total(12+)',
                  'Total(18+)',
                  NA)
  xlbls <- df %>%
    filter(!(fx_etaria %in% ignored.fx)) %>%
    pull(fx_etaria) %>%
    unique()
  xbreaks <- .5+seq(2, 3*length(xlbls), 3)
  plt <- df %>%
    filter(dado %in% list.dado,
           epiyear==epiyear.num,
           epiweek==epiweek.num,
           SG_UF_NOT==co.uf,
           !(fx_etaria %in% ignored.fx),
           vac_completa %in% vac_levels) %>%
    droplevels() %>%
    complete(nesting(epiyear, epiweek), nesting(SG_UF_NOT, DS_UF_SIGLA, fx_etaria), dado=c('', list.dado), vac_completa, fill=list(frac=0)) %>%
    distinct() %>%
    mutate(vac_completa = factor(vac_completa,
                                 levels=vac_levels,
                                 labels=vac_lbls)) %>%
    mutate(dado = factor(dado, levels=c('', list.dado),
                         ordered = T)) %>%
    arrange(SG_UF_NOT, epiyear, epiweek, fx_etaria, vac_completa, dado) %>%
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
    scale_fill_manual(values=colorblind_pal()(num.colors)[2:num.colors],
                      breaks=vac_lbls,
                      name='Status') +
    scale_alpha_manual(values=c(1,.55), breaks=lbl.dado, name='') +
    labs(x='Faixa etária', y='Proporção') +
    ggtitle(gttl,
            subtitle=gsbttl) +
    theme_Publication(base_size=16, base_family = 'Roboto') +
    theme(legend.position='bottom',
          legend.direction='horizontal',
          axis.text.x = element_text(angle=35, hjust=1),
          plot.background = element_rect(fill = "transparent", color = NA))
  return(plt)
}

plot.mensal <- function(df,
                        list.dado=c('pop', 'sragcovid'),
                        lbl.dado=c('População geral', 'Casos de SRAG\npor COVID-19'),
                        vac_levels=c('ignorado', 'não', 'D1', 'D2/D+'),
                        vac_lbls=c('ignorado', 'não vacinado', 'apenas D1', 'ao menos D2\nou D'),
                        co.uf=0,
                        epiyear.num,
                        epiweek.num,
                        gttl='Cobertura vacinal: população vs. SRAG por COVID-19',
                        gsbttl=NULL
){
  num.colors = length(vac_lbls) + 1
  ignored.fx <- c('Ign',
                  'Total',
                  'Total(5+)',
                  'Total(12+)',
                  'Total(18+)',
                  NA)
  xlbls <- df %>%
    filter(!(fx_etaria %in% ignored.fx)) %>%
    pull(fx_etaria) %>%
    unique()
  xbreaks <- .5+seq(2, 3*length(xlbls), 3)
  xlimits <- c(1.5,3*length(xlbls)+.5)
  xlbls <- xlbls[seq(1, length(xlbls), 1)]
  plt <- df %>%
    filter(dado %in% list.dado,
           between(epiyear*100+epiweek, 2021*100+3, epiyear.num*100+epiweek.num),
           SG_UF_NOT==co.uf,
           !(fx_etaria %in% ignored.fx),
           vac_completa %in% vac_levels) %>%
    droplevels() %>%
    complete(nesting(epiyear, epiweek), nesting(SG_UF_NOT, fx_etaria), dado=c('', list.dado), vac_completa, fill=list(frac=0)) %>%
    distinct() %>%
    mutate(vac_completa = factor(vac_completa,
                                 levels=vac_levels,
                                 labels=vac_lbls),
           dado = factor(dado, levels=c('', list.dado),
                         ordered = T)) %>%
    arrange(SG_UF_NOT, epiyear, epiweek, fx_etaria, vac_completa, dado) %>%
    mutate(x = as.numeric(factor(fx_etaria)),
           x = case_when(
             dado == '' ~ 3*(x-1)+1,
             dado == 'pop' ~ 3*(x-1)+2,
             TRUE ~ 3*(x-1)+3
           ),
           dado=factor(dado, levels=list.dado,
                       labels=lbl.dado,
                       ordered=T),
           epiweek=paste0(epiyear, '/', sprintf(fmt = '%02d', epiweek))
    ) %>%
    ggplot(aes(x=x, y=frac, fill=vac_completa, alpha=dado)) +
    geom_col(width=.95) +
    scale_x_continuous(breaks=xbreaks, labels=xlbls, limits = xlimits) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values=colorblind_pal()(num.colors)[2:num.colors],
                      breaks=vac_lbls,
                      name='Status') +
    scale_alpha_manual(values=c(1,.55), breaks=lbl.dado, name='') +
    labs(x='Faixa etária', y='Proporção') +
    ggtitle(gttl,
            subtitle=gsbttl) +
    theme_Publication(base_size=16, base_family = 'Roboto') +
    theme(legend.position='bottom',
          legend.direction='horizontal',
          axis.text.x = element_text(angle=45, hjust=1),
          plot.background = element_rect(fill = "transparent", color = NA)) +
    facet_wrap(~epiweek, ncol = 3)
  return(plt)
}

plot.uf.epiweek <- function(df,
                        list.dado=c('sragcovid'),
                        vac_levels=c('ignorado', 'não', 'D1', 'D2/D+'),
                        vac_lbls=c('ignorado', 'não vacinado', 'apenas D1', 'ao menos D2\nou D'),
                        epiyear.num,
                        epiweek.num,
                        gttl='Cobertura vacinal nas hospitalizações de SRAG por COVID-19',
                        gsbttl=NULL
){
  num.colors = length(vac_lbls) + 1
  ignored.fx <- c('Ign',
                  'Total',
                  'Total(5+)',
                  'Total(12+)',
                  'Total(18+)',
                  NA)
  xbreaks <- df %>%
    filter(!(fx_etaria %in% ignored.fx)) %>%
    pull(fx_etaria) %>%
    unique()
  xbreaks <- xbreaks[seq(1, length(xbreaks), 2)]
  plt <- df %>%
    filter(dado %in% list.dado,
           epiyear==epiyear.num,
           SG_UF_NOT != 0, epiweek==epiweek.num,
           !(fx_etaria %in% ignored.fx),
           vac_completa %in% vac_levels) %>%
    mutate(vac_completa = factor(vac_completa,
                                 levels=vac_levels,
                                 labels=vac_lbls)
    ) %>%
    ggplot(aes(x=fx_etaria, y=frac, fill=vac_completa)) +
    geom_col() +
    scale_fill_manual(values=colorblind_pal()(num.colors)[2:num.colors],
                      breaks=vac_lbls,
                      name='Status') +
    scale_x_discrete(breaks=xbreaks) +
    scale_y_continuous(breaks=c(0, .5, 1), labels = scales::percent) +
    labs(x='Faixa etária', y='Proporção') +
    theme_Publication(base_size=14, base_family = 'Roboto') +
    theme(legend.position = 'bottom',
          legend.direction = 'horizontal',
          axis.text.x = element_text(angle=45, hjust=1),
          plot.background = element_rect(fill = "transparent", color = NA)) +
    ggtitle(gttl,
            subtitle=gsbttl) +
    facet_geo(~DS_UF_SIGLA, grid='br_states_grid1')
  return(plt)
}

plot.incidencia <- function(df,
                            epiyear.num,
                            epiweek.num,
                            title='Impacto da vacinação nos casos e óbitos de SRAG por COVID-19',
                            vac_levels=c('não', 'D1', 'D2/D+'),
                            vac_lbls=c('não vacinado', 'apenas D1', 'ao menos D2\nou D'),
                            xlabels=c('Óbitos\npor COVID-19',
                                      'Casos graves\nde COVID-19'),
                            subtitle=paste0('Mês de ', epiweek.lbl, ', com base em dados digitados até a semana ', epiweek, ' de ', lyear, '.\n',
                                            'Restrito às notificações com informação vacinal no SIVEP-Gripe.'),
                            base_family='Roboto',
                            base_size=16){
  df %>%
    filter(epiweek==epiweek.num,
           epiyear==epiyear.num)%>%
    group_by(SG_UF_NOT, dado, epiyear, epiweek, fx_etaria) %>%
    summarise(vac_completa=vac_completa,
              pop=pop,
              eventos.vac.indep = sum(eventos),
              eventos=eventos,
              frac=frac,
              inc=case_when(
                eventos.vac.indep < 20 ~ NA_real_,
                TRUE ~ inc
              )) %>%
    mutate(inc.lbl = case_when(
      is.na(inc) & vac_completa=='D1+' ~ 'total de eventos\ninsuficiente',
      is.na(inc) ~ '',
      TRUE ~ sprintf('%.2f', inc)
      ),
      inc = ifelse(inc.lbl=='total de eventos\ninsuficiente', 0, inc)
    ) %>%
    mutate(vac_completa=factor(vac_completa,
                               levels=vac_levels,
                               labels=vac_lbls,
           )
    ) %>%
    ggplot(aes(x=dado, y=inc, fill=vac_completa)) +
    geom_col(position='dodge') +
    geom_text(aes(label=inc.lbl), position=position_dodge(width=0.9), vjust=-0.25) +
    scale_fill_tableau(name='Status', palette='Color Blind') +
    scale_y_continuous(expand=expansion(mult=c(0.05,0.1))) +
    scale_x_discrete(labels=xlabels) + 
    labs(x=element_blank(), y='Incidência por 100mil hab.',
         caption=paste0('Fontes: SIVEP-Gripe, RNDS, e FREIRE et al. (2019)\n',
                        'Projeção populacional municipal com estimadores bayesianos, Brasil 2010 - 2030.\n',
                        'Análise: InfoGripe e Observatório COVID-19 BR')) +
    facet_wrap(~fx_etaria, scale='free_y') +
    ggtitle(title,
            subtitle) +
    theme_Publication(base_size = base_size,
                      base_family = base_family) +
    theme(legend.position = 'bottom',
          legend.direction = 'horizontal',
          plot.background = element_rect(fill = "transparent", color = NA))
}

plot.inc.serie <- function(df,
                           vac.lvls=c('não',
                                      'D1',
                                      'D+',
                                      'D2+'),
                           vac.lbls=c('não vacinado',
                                      'apenas D1',
                                      'ao menos D',
                                      'ao menos D2'),
                           x.lbl='Semana de primeiros sintomas',
                           y.lbl='Incidência por 100mil hab.',
                           ttl='SRAG por COVID-19',
                           subttl=NULL,
                           base_family='Roboto',
                           base_size=16){
  df %>%
    filter(vac_completa %in% vac.lvls,
           !grepl(pattern='T', fx_etaria),
           !(epiyear==2021 & epiweek < 35)) %>%
    mutate(vac_completa=factor(vac_completa,
                               levels=vac.lvls,
                               labels=vac.lbls)) %>%
    ggplot(aes(x=data, y=inc, color=vac_completa)) +
    geom_line() +
    scale_color_colorblind(name='Status') +
    scale_x_date(date_labels='%Y/%b', date_breaks = '2 months') +
    theme_Publication(base_family=base_family,
                      base_size=base_size) +
    facet_wrap(~fx_etaria, scales = 'free_y') +
    theme(axis.text.x = element_text(angle=30, hjust=1),
          panel.grid.minor.x = element_line(colour="#f0f0f0", linetype=2),
          plot.background = element_rect(fill = "transparent", color = NA)) + 
    labs(x=x.lbl, y=y.lbl) + 
    ggtitle(ttl, subtitle=subttl)
}

plot.inc.serie.compara.janela <- function(df,
                                          vac.lvls=c('não',
                                                     'D1',
                                                     'D+',
                                                     'D2+'),
                                          vac.lbls=c('não vacinado',
                                                     'apenas D1',
                                                     'ao menos D',
                                                     'ao menos D2'),
                                          x.lbl='Semana de primeiros sintomas',
                                          y.lbl='Incidência por 100mil hab.',
                                          ttl='SRAG por COVID-19',
                                          janela.lvls=c(1, 2, 4),
                                          janela.lbls=c('1 semana', '2 semanas', '4 semanas'),
                                          janela.lt=c(1,3,2),
                                          subttl=NULL,
                                          base_family='Roboto',
                                          base_size=16){
  df %>%
    filter(vac_completa %in% vac.lvls,
           !grepl(pattern='T', fx_etaria),
           !(epiyear==2021 & epiweek < 35),
           janela %in% janela.lvls) %>%
    mutate(vac_completa=factor(vac_completa,
                               levels=vac.lvls,
                               labels=vac.lbls),
           data=epiweek2date(epiyear, epiweek)-(as.integer(janela)-1)*7/2,
           inc=inc/as.integer(janela)) %>%
    ggplot(aes(x=data, y=inc, color=vac_completa, linetype=factor(janela))) +
    geom_line() +
    scale_color_colorblind(name='Status') +
    scale_x_date(date_labels='%Y/%b', date_breaks = '2 months') +
    scale_linetype_manual(breaks=janela.lvls, values=janela.lt, labels=janela.lbls,
                          name='Janela móvel') +
    theme_Publication(base_family=base_family,
                      base_size=base_size) +
    facet_wrap(~fx_etaria, scales = 'free_y') +
    theme(axis.text.x = element_text(angle=30, hjust=1),
          panel.grid.minor.x = element_line(colour="#f0f0f0", linetype=2),
          plot.background = element_rect(fill = "transparent", color = NA)) + 
    labs(x=x.lbl, y=y.lbl) + 
    ggtitle(ttl, subtitle=subttl)
}
