suppressWarnings(suppressPackageStartupMessages(library(lubridate)))
suppressWarnings(suppressPackageStartupMessages(library(foreign)))
suppressWarnings(suppressPackageStartupMessages(library(INLA)))
suppressWarnings(suppressPackageStartupMessages(library("tidyverse")))
suppressWarnings(suppressPackageStartupMessages(library(magick)))
suppressWarnings(suppressPackageStartupMessages(library(grid)))
suppressWarnings(suppressPackageStartupMessages(library(cowplot)))
options(dplyr.summarise.inform=F) 
source("../data_filter/episem.R")
source("nowcasting_v2.R")
source("slope.estimate.quant.R")
source("mapas_macsaud.R")

int_breaks_rounded <- function(x, n = 5)  pretty(x, n)[round(pretty(x, n),1) %% 1 == 0]

logo <- image_read('../report/Figs/infogripe.png')

# Codes for DF's central health region
bsb_ras <- c(
  530005,
  530080,
  530050,
  539931,
  539925,
  530015,
  530090
)

## Read command line arguments
suppressPackageStartupMessages(library("argparse"))
# create parser object
parser <- ArgumentParser()
# specify our desired options
# by default ArgumentParser will add an help option
parser$add_argument("-t", "--type", type="character", default='srag',
                    help="Type of data input [srag, sragflu, obitoflu]. Default %(default)s")
parser$add_argument("-f", "--filtertype", type="character", default='sragnofever',
                    help="Type of filter [srag, sragnofever, hospdeath]. Default %(default)s")
parser$add_argument("-d", "--date", type="character", default=format(Sys.Date(), '%Y-%m-%d'),
                    help="Date to use as base, in format YYYY-MM-DD [default Sys.Date()]")
parser$add_argument("-g", "--graphs", action='store_true', default=FALSE,
                    help="If argument passed, generate graphs")
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults,
args <- parser$parse_args()

if (args$graphs){
  suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))
  source('../report/theme.publication.R')
}

if (!(args$filtertype %in% c('srag', 'sragnofever', 'hospdeath'))){
  stop(paste0('Invalid filter type', args$filtertype))
}

# Latest week with closed counts on DT_DIGITA is actualy the previous one
if (args$date == 'max'){
  today <- as.Date(as.character(max(d.orig$date)), format='%Y%m%d')
  print(paste0('Base date: ', today))
  today <- as.Date(today - 7,origin = '1970-01-01')
  today <- episem(today)
} else {
  today <- as.Date(args$date)
  print(paste0('Base date: ', today))
  today <- as.Date(today - 7,origin = '1970-01-01')
  today <- episem(today)
}
lyear <- as.integer(strsplit(today, 'W')[[1]][1])
today.week <- as.integer(strsplit(today, 'W')[[1]][2])

# Tabela cadmun no git
tblCADMUN <- read.csv("../data/municip_macsaud_regmetr.csv", stringsAsFactors = F)
tblmumpop <- read.csv("../data/tab_datasus_pop_faixateraria.csv", stringsAsFactors = F)
tblmumpop[,4:ncol(tblmumpop)] <- tblmumpop %>%
  select(-codmun, -municipio, -ano) %>%
  mutate_all(~ as.character(.)) %>%
  mutate_all(~ gsub('\\.', '', .)) %>%
  mutate_all(~ as.numeric(.))

tblCADMUN <- tblmumpop %>%
  filter(ano == 2019) %>%
  transmute(CO_MUNICIP = codmun, Populacao = Total) %>%
  right_join(tblCADMUN)

# Suffix filter based on filtertype
suff_list <- list('srag' = '', 'sragnofever' = '_sragnofever', 'hospdeath' = '_hospdeath')
suff <- suff_list[args$filtertype]
preff_list <- list(srag = 'srag', sragnofever = '.', hospdeath = 'hospdeath')
preff <- as.character(preff_list[args$filtertype])

# Read dataset
path_file <- paste0("../clean_data/clean_data_", args$type, suff, "_epiweek.csv")


# RegionalSaude <- st_read("~/Git/PROCC /covid-19/malha/regional_saude_2019.gpkg" )

dados_full <- read.csv( path_file, stringsAsFactors = F) %>%
  select(SG_UF,
         SG_UF_NOT,
         CO_MUN_NOT,
         CO_MUN_RES,
         CO_UNI_NOT,
         CO_UN_INTE,
         DT_SIN_PRI,
         DT_SIN_PRI_epiweek,
         DT_SIN_PRI_epiyear,
         DT_DIGITA,
         SinPri2Digita_DelayWeeks) %>%
  filter(DT_SIN_PRI_epiyear >= 2020) %>%
  mutate(DT_SIN_PRI = ymd(DT_SIN_PRI),
         DT_DIGITA = ymd(DT_DIGITA)) %>%
  left_join(tblCADMUN %>% select(CO_MUNICIP, CO_MACSAUD), by=c('CO_MUN_NOT' = 'CO_MUNICIP')) 
# dados_full <- read.csv2("~/Downloads/INFLUD_09-06-2020.csv", stringsAsFactors = F)
# dados_full2 <- read.csv2("~/Downloads/INFLUD-07-07-2020.csv", stringsAsFactors = F)

# Ajustar semana epidemiológica para manter os dados de 2020:
epiweekmax <- as.integer(lastepiweek(2020))
if (lyear > 2020){
  today.week <- today.week + epiweekmax
  dados_full <- dados_full %>%
    mutate(epiyear = DT_SIN_PRI_epiyear,
           epiweek = DT_SIN_PRI_epiweek,
           DT_SIN_PRI_epiweek = case_when(
      DT_SIN_PRI_epiyear > 2020 ~ DT_SIN_PRI_epiweek + epiweekmax,
      TRUE ~ DT_SIN_PRI_epiweek
    ))
}
epiweek.table <- dados_full %>%
  select(DT_SIN_PRI_epiweek, epiweek, epiyear) %>%
  unique() %>%
  arrange(DT_SIN_PRI_epiweek)

epilbls <- c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52)
xbreaks <- c(epilbls, epilbls + epiweekmax)
xlbls <- c(epilbls, epilbls)
xlimits <- c(1, dados_full %>% 
               filter(DT_SIN_PRI_epiyear == lyear) %>%
               select(DT_SIN_PRI_epiweek) %>% max())

# Função para gerar as estimativas:
generate.estimate <- function(dadosBR, Inicio=Inicio, today.week=today.week){
  dadosBR <- dadosBR %>% 
    filter(DT_SIN_PRI_epiweek >= 1, DT_SIN_PRI_epiweek <= today.week) %>% 
    mutate(
      DelayWeeks = SinPri2Digita_DelayWeeks,
      DelayWeeks = ifelse(DelayWeeks < 0, NA, DelayWeeks)
    ) 
  
  # Atraso em semanas 
  Dmax = 10
  
  dados.srag.ag <- dadosBR %>% 
    mutate(
      DelayWeeks = ifelse(DelayWeeks > Dmax, NA, DelayWeeks)
    ) %>% 
    drop_na(DelayWeeks) %>% 
    group_by(DT_SIN_PRI_epiweek, DelayWeeks) %>% 
    dplyr::summarise(
      Casos = n()
    ) %>%  # View()
    # Passando para o formato wide
    spread(key = DelayWeeks, value = Casos) %>% #  View()
    # Adicianoando todas as data, alguns dias nao houveram casos com primeiros sintomas
    # e dias apos "Hoje" serão incluídos para previsão 
    full_join( 
      y = tibble(DT_SIN_PRI_epiweek = seq(epiweek(Inicio), today.week)), 
      by = "DT_SIN_PRI_epiweek" ) %>% # View() 
    # Voltando para o formato longo
    gather(key = DelayWeeks, value = Casos, -DT_SIN_PRI_epiweek) %>% 
    mutate(
      DelayWeeks = as.numeric(DelayWeeks),
      # Preparing the run-off triangle
      Casos = ifelse( 
        test = (DT_SIN_PRI_epiweek + DelayWeeks) <= today.week, 
        yes = replace_na(Casos, 0), 
        no = NA)
    ) %>% dplyr::rename( Date = DT_SIN_PRI_epiweek) %>%  ungroup() %>% 
    # Sorting by date
    dplyr::arrange(Date) %>% 
    # Creating Time and Delay indexes
    mutate( 
      Time = as.numeric(Date - min(Date) + 1)
    ) %>% 
    dplyr::rename( Delay = DelayWeeks)
  
  # Model equation
  model.srag <- Casos ~ 1 + 
    f(Time, model = "rw1", constr = T
      #hyper = list("prec" = list(prior = half_normal_sd(.1) ))
    ) +
    f(Delay, model = "rw1", constr = T
      #hyper = list("prec" = list(prior = half_normal_sd(.1) ))
    ) + 
    # Efeito tempo-atraso
    f(TimeDelay, model = "iid", constr = T)
  
  
  output.srag <- nowcast.INLA(
    model.day = model.srag,
    dados.ag = dados.srag.ag %>%
      mutate(TimeDelay = paste(Time, Delay))
  )
  
  dados.srag.ag.day.plot <- dados.srag.ag %>% group_by(Date) %>%
    dplyr::summarise( Casos = sum(Casos, na.rm = T))
  
  pred.srag <- nowcasting(output.srag, dados.srag.ag, Fim=today.week, Dm=Dmax)
  
  # Tendência via modelo linear com janela móvel
  variation.lvl.3s <- pred.srag$Date %>%
    unique() %>%
    map(slope.estimate.quant, pred.srag=pred.srag) %>%
    unlist() %>%
    as_tibble_col(column_name = 'tendencia.3s')
  variation.lvl <- pred.srag$Date %>%
    unique() %>%
    map(slope.estimate.quant, pred.srag=pred.srag, w=6) %>%
    unlist() %>%
    as_tibble_col(column_name = 'tendencia.6s') %>%
    cbind(Date = unique(pred.srag$Date)) %>%
    cbind(variation.lvl.3s)
  
  pred.srag.summy <- pred.srag %>% group_by(Date) %>% 
    dplyr::summarise( #Mean = mean(Casos),
      Median = replace_na(round(median(Casos, na.rm=T)), 0), 
      Q1 = round(quantile(Casos, probs = 0.25, na.rm=T)),
      Q3 = round(quantile(Casos, probs = 0.75, na.rm=T)),
      IC80I = round(quantile(Casos, probs = 0.1, na.rm=T)),
      IC80S = round(quantile(Casos, probs = 0.9, na.rm=T)),
      IC90I = round(quantile(Casos, probs = 0.05, na.rm=T)),
      IC90S = round(quantile(Casos, probs = 0.95, na.rm=T)),
      LI = round(quantile(Casos, probs = 0.025, na.rm=T)),
      LS = round(quantile(Casos, probs = 0.975, na.rm=T))
    ) %>%
    mutate(LS = case_when(
      LS > 30 ~ pmin(3*Median, LS),
      TRUE ~ LS
    ),
    IC90S = case_when(
      IC90S > 30 ~ pmin(3*Median, IC90S),
      TRUE ~ LS
    )) %>%
    right_join(dados.srag.ag.day.plot, by='Date') %>%
    arrange(Date) %>%
    mutate(full_estimate = case_when(
      is.na(Median) ~ Casos,
      TRUE ~ Median),
      Casos.cut = case_when(
        Date <= today.week - 2 ~ Casos,
        TRUE ~ NA_real_),
      rolling_average = round(zoo::rollmean(full_estimate, k=3, fill=NA))) %>%
    left_join(variation.lvl, by='Date')
  
  return(pred.srag.summy)
}

# Function for trend plot
plot.ts.tendencia <- function(df, xbreaks=c(1, seq(4, 52, 4)), xlbls=c(1, seq(4, 52, 4)), xlimits=c(1, 53)){
  plt <- df %>%
    select(Date, tendencia.3s, tendencia.6s) %>%
    mutate(tendencia.3s = case_when(
      Date < today.week - 1 ~ NA_real_,
      TRUE ~ tendencia.3s
    )) %>%
    rename('curto prazo'=tendencia.3s, 'longo prazo'=tendencia.6s) %>%
    pivot_longer(-Date, names_to = 'janela', values_to = 'tendencia') %>%
    arrange(desc(janela)) %>%
    ggplot(aes(x=Date, y=tendencia, color=janela)) +
    geom_hline(yintercept = 0, color='grey', size=1.5, linetype=2) +
    geom_line() +
    geom_point() +
    scale_y_continuous(breaks = seq(-1,1,.5),
                       labels=c('Prob. queda\n> 95%', 'Prob. queda\n> 75%', 'Estabilidade./\noscilação', 'Prob. cresc.\n> 75%', 'Prob. cresc.\n> 95%'),
                       limits = c(-1,1),
                       name=NULL) +
    scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits, name=NULL) +
    scale_color_discrete(name=NULL) +
    theme_Publication() +
    theme(plot.margin=unit(c(1,0,5,5), units='pt'),
          axis.text.y = element_text(size = rel(.8), angle=30),
          legend.margin = margin(0,0,0,0, unit='pt'),
          legend.justification=c(0,1), 
          legend.position=c(0.015, 1.05),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.key.size = unit(14, 'pt'),
          legend.text = element_text(family = 'Roboto', size = rel(.8))
    )
  
  return(plt)
}

# Read CNES data
cnes <- read.csv2('../data/tbEstabelecimento202011.csv', stringsAsFactors = F) %>%
  select(CO_CNES, CO_NATUREZA_JUR)
dados_full <- dados_full %>%
  left_join(cnes, by=c('CO_UNI_NOT' = 'CO_CNES')) %>%
  mutate(grupo_jur = as.integer(CO_NATUREZA_JUR/1000))
ttl.xtra <- c('', ' - ADM PÚBLICA', ' - ENT. EMPRESARIAS', ' - ENT. SEM FINS LUCR.')
suff.xtra <- c('', '_admpub', '_entempr', '_entsflucr')

# UFs:
uf.list <- tblCADMUN %>% 
  filter(CO_STATUS_municip == 'ATIVO') %>%
  select(CO_UF, DS_UF_SIGLA, Populacao) %>%
  group_by(CO_UF, DS_UF_SIGLA) %>%
  summarise(Populacao = sum(Populacao)) %>%
  rbind(data.frame(list(CO_UF = 0, DS_UF_SIGLA = 'BR', Populacao = 0)))
uf.list$Populacao[uf.list$CO_UF == 0] <- sum(uf.list$Populacao)
pred.ufs <- c()
for(uf in uf.list$CO_UF){
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
  
  pred.srag.summy <- generate.estimate(dadosBR, Inicio, today.week)
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
  grid::grid.raster(logo, x = 0.999, y = 0.95, just = c('right', 'top'), width = unit(1, 'inches'))
  dev.off()
}

pred.ufs <- pred.ufs %>%
  left_join(epiweek.table, by=c('Date' = 'DT_SIN_PRI_epiweek'))

saveRDS(pred.ufs, paste0(preff,'/ufs_', lyear, '_', today.week, '.rds'))
saveRDS(pred.ufs, paste0(preff,'/ufs_current.rds'))

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
  select(-Median, -Casos) %>%
  write_csv2(paste0(preff,'/estados_e_pais_serie_estimativas_tendencia_sem_filtro_febre.csv'), na = '')

# Capitais:
tblCADMUN.capital <- tblCADMUN %>% filter(IN_CAPITAL %in% c('E', 'F'))
pred.capitais <- c()
for(k in 1:nrow(tblCADMUN.capital)){
  co_mun <- tblCADMUN.capital$CO_MUNICIP[k]
  co_uf <- tblCADMUN.capital$CO_UF[k]
  co_uf.name <- tblCADMUN.capital$DS_UF_SIGLA[k]
  if (co_mun != 530010){
    dadosBR0 <- dados_full %>% 
      mutate(
        DT_SIN_PRI = ymd(DT_SIN_PRI),
        DT_DIGITA = ymd(DT_DIGITA),
      ) %>% 
      filter(
        DT_SIN_PRI_epiyear >= 2020,
        CO_MUN_RES == as.character(tblCADMUN.capital$CO_MUNICIP[k])
      )
    title0 <- tblCADMUN.capital$DS_NOMEPAD_municip[k]
  } else {
    dadosBR0 <- dados_full %>% 
      mutate(
        DT_SIN_PRI = ymd(DT_SIN_PRI),
        DT_DIGITA = ymd(DT_DIGITA),
      ) %>% 
      filter(
        DT_SIN_PRI_epiyear >= 2020,
        CO_MUN_RES %in% as.character(bsb_ras)
      )
    title0 <- 'REGIAO DE SAUDE CENTRAL'
  }
  
  pop <- tblCADMUN.capital$Populacao[k]
  mun.id <- co_mun

  
  # Semana epidemiologica termina no Sabado, entao vou excluir os dados mais recentes caso a semana nao comece no sábado.
  Fim.sat <- today.week

  for (gpj in c(0, 1, 2, 3)){
    if (gpj > 0){
      dadosBR <- dadosBR0 %>%
        filter(grupo_jur == gpj)
    } else {
      dadosBR <- dadosBR0
    }
    title <- paste0(title0, ttl.xtra[gpj + 1])
    Inicio <- min(dadosBR$DT_SIN_PRI)
    pred.srag.summy <- generate.estimate(dadosBR, Inicio, today.week)
    pred.srag.summy <- pred.srag.summy %>%
      mutate_at(vars(-("Date"), -starts_with("tendencia")), ~ .*100000/pop) %>%
      mutate(CO_MUN_RES = as.integer(mun.id),
             CO_MUN_RES_nome = title,
             CO_UF = co_uf,
             DS_UF_SIGLA = co_uf.name,
             populacao = pop,
             grupo_jur = gpj)
    
    pred.capitais <- pred.capitais %>%
      bind_rows(pred.srag.summy)
        
    p.now.srag <- plot.nowcast(pred.srag.summy, Fim=today.week ) +
      ylab("Incidência de SRAG (por 100mil hab.)") +
      xlab("Semana de primeiros sintomas") +
      scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits) +
      theme_Publication(base_size = 16, base_family = 'Roboto') +
      ggtitle(paste0(tblCADMUN.capital$DS_UF_SIGLA[k], ": ", title)) +
      theme(plot.margin=unit(c(1,0,5,5), units='pt'),
            axis.text = element_text(size = rel(1)),
            legend.margin = margin(0,0,0,0, unit='pt'),
            legend.justification=c(0,1), 
            legend.position=c(0.015, 1.05),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.key.size = unit(14, 'pt'),
            legend.text = element_text(family = 'Roboto', size = rel(1))
      )
    p.nivel <-  plot.ts.tendencia(df = pred.srag.summy,
                                  xbreaks = xbreaks,
                                  xlbls = xlbls,
                                  xlimits = xlimits)
    
    
    png(filename = paste0(preff,
                          "/Figs/Capitais/fig_",
                          tblCADMUN.capital$DS_UF_SIGLA[k],
                          '_',
                          str_replace_all(tblCADMUN.capital$DS_NOMEPAD_municip[k], ' ', '_'),
                          suff.xtra[gpj + 1],
                          ".png"),
        width=8, height=6, units='in', res=200)
    print(plot_grid(p.now.srag, p.nivel, align='v', axis='l', nrow=2, ncol=1, rel_heights=c(2.5, 1)))
    grid::grid.raster(logo, x = 0.999, y = 0.95, just = c('right', 'top'), width = unit(1, 'inches'))
    dev.off()
    
  }
}

pred.capitais <- pred.capitais %>%
  left_join(epiweek.table, by=c('Date' = 'DT_SIN_PRI_epiweek'))
saveRDS(pred.capitais, paste0(preff,'/capitais_', lyear, '_', today.week, '.rds'))
saveRDS(pred.capitais, paste0(preff,'/capitais_current.rds'))

plot.ufs.tendencia(pred.capitais %>% filter(Date == today.week, grupo_jur == 0))

pred.capitais <- pred.capitais %>%
  mutate(escala = 'incidência')
pred.capitais <- pred.capitais %>%
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
  bind_rows(pred.capitais)

pred.capitais %>%
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
  select(-Median, -Casos) %>%
  write_csv2(paste0(preff,'/capitais_serie_estimativas_tendencia_sem_filtro_febre.csv'), na = '')

# Macrorregionais de saúde:
tblMACSAUD <- tblCADMUN %>%
  filter(!is.na(CO_MACSAUD)) %>%
  select(CO_MACSAUD, DS_NOMEPAD_macsaud, CO_UF, DS_UF_SIGLA, Populacao) %>%
  group_by(CO_MACSAUD, DS_NOMEPAD_macsaud, CO_UF, DS_UF_SIGLA) %>%
  summarise(Populacao = sum(Populacao)) %>%
  mutate(DS_NOMEPAD_macsaud_clean = str_replace_all(DS_NOMEPAD_macsaud, ' ', '_')) %>%
  mutate(DS_NOMEPAD_macsaud_clean = str_replace_all(DS_NOMEPAD_macsaud_clean, '/', '-')) %>%
  ungroup()
rownames(tblMACSAUD) <- NULL
pred.macros <- c()
for(k in 1:nrow(tblMACSAUD)){
  
  dadosBR0 <- dados_full %>% 
    mutate(
      DT_SIN_PRI = ymd(DT_SIN_PRI),
      DT_DIGITA = ymd(DT_DIGITA),
    ) %>% 
    filter(
      DT_SIN_PRI_epiyear >= 2020,
      CO_MACSAUD == tblMACSAUD$CO_MACSAUD[k]
    )
  
  
  for (gpj in c(0)){
    if (gpj > 0){
      dadosBR <- dadosBR0 %>%
        filter(grupo_jur == gpj)
    } else {
      dadosBR <- dadosBR0
    }
    if (nrow(dadosBR) > 10){
      Inicio <- min(dadosBR$DT_SIN_PRI)
        
  
      macsaud.id <- dadosBR$CO_MACSAUD %>% unique()
      # Semana epidemiologica termina no Sabado, entao vou excluir os dados mais recentes caso a semana nao comece no sábado.
      Fim.sat <- today.week
      
      pred.srag.summy <- generate.estimate(dadosBR, Inicio, today.week)
      pop <- tblMACSAUD$Populacao[k]
      macsaud.name <- tblMACSAUD$DS_NOMEPAD_macsaud[k]
      uf <- tblMACSAUD$CO_UF[k]
      uf.name <- tblMACSAUD$DS_UF_SIGLA[k]
      pred.srag.summy <- pred.srag.summy %>%
        mutate_at(vars(-("Date"), -starts_with("tendencia")), ~ .*100000/pop) %>%
        mutate(CO_MACSAUD = as.integer(macsaud.id),
               DS_NOMEPAD_macsaud = macsaud.name,
               CO_UF = uf,
               DS_UF_SIGLA = uf.name,
               populacao = pop,
               grupo_jur = gpj)
      pred.macros <- pred.macros %>%
        bind_rows(pred.srag.summy)
      
      p.now.srag <- plot.nowcast(pred.srag.summy, Fim=today.week ) +
        ylab("Incidência de SRAG (por 100mil hab.)") +
        xlab("Semana de primeiros sintomas") +
        scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits) +
        theme_Publication(base_size = 16, base_family = 'Roboto') +
        ggtitle(paste0(tblMACSAUD$DS_UF_SIGLA[k], ": ", tblMACSAUD$DS_NOMEPAD_macsaud[k], ttl.xtra[gpj + 1])) +
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
      
      png(filename = paste0(preff,"/Figs/MACSAUD/fig_", tblMACSAUD$DS_UF_SIGLA[k], '_', tblMACSAUD$CO_MACSAUD[k], suff.xtra[gpj + 1], ".png"),
          width=8, height=6, units='in', res=200)
      print(plot_grid(p.now.srag, p.nivel, align='v', axis='l', nrow=2, ncol=1, rel_heights=c(2.5, 1)))
      grid::grid.raster(logo, x = 0.999, y = 0.95, just = c('right', 'top'), width = unit(1, 'inches'))
      dev.off()
    }
  }
  
}

pred.macros <- pred.macros %>%
  filter(grupo_jur == 0) %>%
  select(-grupo_jur) %>%
  left_join(epiweek.table, by=c('Date' = 'DT_SIN_PRI_epiweek'))

saveRDS(pred.macros, paste0(preff,'/macros_', lyear, '_', today.week, '.rds'))
saveRDS(pred.macros, paste0(preff,'/macros_current.rds'))

if (args$filtertype == 'sragnofever'){
  pred.macros$CO_UF %>%
    unique() %>%
    map(plot.macsaude.tendencia, df=pred.macros %>% filter(Date == today.week))
  
  plot.macsaude.tendencia(uf='BR', df=pred.macros %>% filter(Date == today.week), orientation = 'portrait')
  plot.macsaude.tendencia(uf='BR', df=pred.macros %>% filter(Date == today.week), orientation = 'landscape')
}

pred.macros <- pred.macros %>%
  mutate(escala = 'incidência')
pred.macros <- pred.macros %>%
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
  bind_rows(pred.macros)

pred.macros %>%
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
  write_csv2(paste0(preff,'/macsaud_serie_estimativas_tendencia_sem_filtro_febre.csv'), na = '')
