suppressWarnings(suppressPackageStartupMessages(library(lubridate)))
suppressWarnings(suppressPackageStartupMessages(library(foreign)))
suppressWarnings(suppressPackageStartupMessages(library(INLA)))
suppressWarnings(suppressPackageStartupMessages(library(data.table)))
suppressWarnings(suppressPackageStartupMessages(library("tidyverse")))
suppressWarnings(suppressPackageStartupMessages(library(magick)))
suppressWarnings(suppressPackageStartupMessages(library(grid)))
suppressWarnings(suppressPackageStartupMessages(library(cowplot)))
suppressWarnings(suppressPackageStartupMessages(library(RcppRoll)))
options(dplyr.summarise.inform=F) 
source("../data_filter/episem.R")
source("generate.estimate.R")
source("mapas_macsaud.R")
source('plot.ts.tendencia.R')
source('plot.age.ts.R')
source('calc.transmission.thresholds.R')
source('run.regsaud.am.R')
source('run.regsaud.sc.R')

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
parser$add_argument("-q", "--qthreshold", type='double', default=0.95,
                    help="Quantile for calculating local Dmax")
parser$add_argument('--dmax', type='integer', default=15,
                    help="Maximum delay. Default %(default)s")
parser$add_argument('--window', type='integer', default=NULL,
                    help="Maximum delay. Default 2.25*dmax")
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults,
args <- parser$parse_args()

if (!(args$filtertype %in% c('srag', 'sragnofever', 'hospdeath'))){
  stop(paste0('Invalid filter type', args$filtertype))
}

if (is.null(args$window)){
  args$window = as.integer(round(2.25*args$dmax))
}
qthres.probs <- args$qthreshold

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
tblregsaud <- read.csv("../data/base_territorial/rl_municip_regsaud.csv", stringsAsFactors = F)
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
  
# fx.breaks=c(seq(0, 80, 10), 140)
# fx.labels=c('0-9', '10-19','20-29', '30-39', '40-49', '50-59',
#             '60-69', '70-79', '80+')

fx.breaks=c(0, 5, 12, 18, seq(30, 80, 10), 140)
fx.labels=c('0-4', '5-11', '12-17','18-29', '30-39', '40-49', '50-59',
            '60-69', '70-79',
            '80+')
fx.labels.num=as.character(seq(1,length(fx.breaks)-1))

# Suffix filter based on filtertype
suff_list <- list('srag' = '', 'sragnofever' = '_sragnofever', 'hospdeath' = '_hospdeath')
suff <- suff_list[args$filtertype]
preff_list <- list(srag = 'srag', sragnofever = '.', hospdeath = 'hospdeath')
preff <- as.character(preff_list[args$filtertype])

# Read dataset
path_file <- paste0("../clean_data/clean_data_", args$type, suff, "_epiweek.csv.gz")


# RegionalSaude <- st_read("~/Git/PROCC /covid-19/malha/regional_saude_2019.gpkg" )
dados_full <- fread( path_file, stringsAsFactors = F, data.table=T) %>%
  filter(DT_SIN_PRI_epiyear >= 2020) %>%
  select(NU_NOTIFIC,
         CO_MUN_RES,
         CO_MUN_NOT,
         CO_UNI_NOT,
         CO_UN_INTE,
         SG_UF,
         SG_UF_NOT,
         idade_em_anos,
         HOSPITAL,
         DT_INTERNA,
         DT_INTERNA_epiweek,
         DT_INTERNA_epiyear,
         DT_SIN_PRI,
         DT_SIN_PRI_epiweek,
         DT_SIN_PRI_epiyear,
         EVOLUCAO,
         DT_EVOLUCA,
         DT_EVOLUCA_epiweek,
         DT_EVOLUCA_epiyear,
         DT_DIGITA,
         DT_DIGITA_epiweek,
         DT_DIGITA_epiyear,
         DT_NOTIFIC,
         DT_NOTIFIC_epiweek,
         DT_NOTIFIC_epiyear,
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
         CRITERIO,
         SinPri2Digita_DelayWeeks)
if (args$filtertype == 'sragnofever'){
  dados_full %>%
    filter(SG_UF_NOT == 13) %>%
    saveRDS(paste0('~/ownCloud/Fiocruz/Influenza/Estados/Amazonas/dados/clean_data_sragnofever_amazonas_',
                   str_remove_all(args$date, '-'),
                   '.rds'))
}

dados_full <- dados_full %>%
  select(SG_UF,
         SG_UF_NOT,
         NU_NOTIFIC,
         CO_MUN_NOT,
         CO_MUN_RES,
         CO_UNI_NOT,
         CO_UN_INTE,
         DT_SIN_PRI,
         DT_SIN_PRI_epiweek,
         DT_SIN_PRI_epiyear,
         DT_NOTIFIC,
         DT_DIGITA,
         DT_DIGITA_epiweek,
         DT_DIGITA_epiyear,
         idade_em_anos,
         SinPri2Digita_DelayWeeks) %>%
  distinct(NU_NOTIFIC, CO_MUN_NOT, DT_NOTIFIC, .keep_all = T) %>%
  mutate(DT_SIN_PRI = ymd(DT_SIN_PRI),
         DT_DIGITA = ymd(DT_DIGITA),
         fx_etaria = as.character(cut(idade_em_anos,
                                      breaks=fx.breaks,
                                      right=F,
                                      labels=fx.labels.num
                                      )
                                  )
  ) %>%
  left_join(tblCADMUN %>% select(CO_MUNICIP, CO_MACSAUD), by=c('CO_MUN_NOT' = 'CO_MUNICIP')) 
gc(verbose=FALSE)
# dados_full <- read.csv2("~/Downloads/INFLUD_09-06-2020.csv", stringsAsFactors = F)
# dados_full2 <- read.csv2("~/Downloads/INFLUD-07-07-2020.csv", stringsAsFactors = F)

# Ajustar semana epidemiológica para manter os dados de 2020:

epiweekmax <- ifelse(lyear==2020, 0, as.integer(lastepiweek(lyear-1)))
l.addepiweek <- list('2020'=0, '2021'=53, '2022'=52)                        
epiweek.shift <- sum(unlist(l.addepiweek[as.character(seq(2020, lyear-1))]))
today.week.ori <- today.week
if (lyear > 2020){
  today.week.ori <- today.week
  today.week <- today.week + epiweekmax + epiweek.shift
}
dados_full <- dados_full %>%
  mutate(epiyear = DT_SIN_PRI_epiyear,
         epiweek = DT_SIN_PRI_epiweek,
         DT_SIN_PRI_epiweek = case_when(
           epiyear == 2020 ~ as.integer(epiweek),
           epiyear == 2021 ~ as.integer(epiweek) + as.integer(l.addepiweek['2021']),
           epiyear == 2022 ~ as.integer(epiweek) + as.integer(l.addepiweek['2021']) + as.integer(l.addepiweek['2022'])
         ),
         DT_DIGITA_epiweek = case_when(
           DT_DIGITA_epiyear == 2020 ~ as.integer(DT_DIGITA_epiweek),
           DT_DIGITA_epiyear == 2021 ~ as.integer(DT_DIGITA_epiweek) + as.integer(l.addepiweek['2021']),
           DT_DIGITA_epiyear == 2022 ~ as.integer(DT_DIGITA_epiweek) + as.integer(l.addepiweek['2021']) + as.integer(l.addepiweek['2022'])
         )
  ) %>%
  filter(DT_DIGITA_epiweek <= today.week,
         DT_SIN_PRI_epiweek <= today.week)

epiweek.table <- dados_full %>%
  select(DT_SIN_PRI_epiweek, epiweek, epiyear) %>%
  unique() %>%
  arrange(DT_SIN_PRI_epiweek)

epilbls <- c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52)
xbreaks <- c(epilbls + epiweek.shift, epilbls + epiweekmax + epiweek.shift)
xlbls <- c(epilbls, epilbls)
xlimits <- c(1 + epiweek.shift + epiweekmax, dados_full %>% 
               filter(DT_SIN_PRI_epiyear == lyear) %>%
               select(DT_SIN_PRI_epiweek) %>% max())


try.estimate <- function(nivel, uf, dadosBR, Inicio=Inicio, today.week=today.week, Dmax=10, wdw=40, zero.inflated=TRUE, ...){
  tryCatch(generate.estimate(dadosBR, Inicio=Inicio, today.week=today.week, Dmax=Dmax, wdw=wdw, zero.inflated=zero.inflated, ...),
           error=function(e) {
             message=paste('Erro no processamento. Nivel:', nivel, 'local:', uf)
             e}
           )  
}


# Read CNES data
cnes <- read_csv2('../data/tbEstabelecimento_atual_clean.csv') %>%
  select(CO_CNES, CO_NATUREZA_JUR)
dados_full <- dados_full %>%
  left_join(cnes, by=c('CO_UNI_NOT' = 'CO_CNES')) %>%
  mutate(grupo_jur = as.integer(CO_NATUREZA_JUR/1000))
rm(cnes)
gc(verbose=F)

ttl.xtra <- c('', ' - ADM PÚBLICA', ' - ENT. EMPRESARIAS', ' - ENT. SEM FINS LUCR.')
suff.xtra <- c('', '_admpub', '_entempr', '_entsflucr')

# UF ------
uf.list <- tblCADMUN %>% 
  filter(CO_STATUS_municip == 'ATIVO') %>%
  select(CO_UF, DS_UF_SIGLA, Populacao) %>%
  group_by(CO_UF, DS_UF_SIGLA) %>%
  summarise(Populacao = sum(Populacao)) %>%
  rbind(data.frame(list(CO_UF = 0, DS_UF_SIGLA = 'BR', Populacao = 0)))
uf.list$Populacao[uf.list$CO_UF == 0] <- sum(uf.list$Populacao)

qthreshold <- dados_full %>%
  select(SG_UF_NOT, DT_DIGITA_epiyear, DT_DIGITA_epiweek, SinPri2Digita_DelayWeeks) %>%
  filter(
    (DT_DIGITA_epiyear == lyear-1 & DT_DIGITA_epiweek >= today.week.ori) |
      (DT_DIGITA_epiyear == lyear)
  ) %>%
  group_by(SG_UF_NOT) %>%
  summarize(dmax = quantile(SinPri2Digita_DelayWeeks, probs=qthres.probs, na.rm=TRUE)) %>%
  mutate(dmax = dmax + 2,
         dmax = case_when(
    dmax > args$dmax ~ as.numeric(args$dmax),
    dmax < 4 ~ 4,
    TRUE ~ as.numeric(dmax)),
    wdw = case_when(
      ceiling(2.25*dmax) > args$window ~ as.numeric(args$window),
      TRUE ~ as.numeric(ceiling(2.25*dmax)))
    )
qthreshold <- dados_full %>%
  filter(
    (DT_DIGITA_epiyear == lyear-1 & DT_DIGITA_epiweek >= today.week.ori) |
      (DT_DIGITA_epiyear == lyear)
  ) %>%
  select(SinPri2Digita_DelayWeeks) %>%
  summarize(dmax = quantile(SinPri2Digita_DelayWeeks, probs=qthres.probs, na.rm=TRUE)) %>%
  mutate(SG_UF_NOT = 0,
         dmax = dmax + 2,
         dmax = case_when(
           dmax > args$dmax ~ as.numeric(args$dmax),
           dmax < 4 ~ 4,
           TRUE ~ as.numeric(dmax)),
         wdw = case_when(
           ceiling(2.25*dmax) > args$window ~ as.numeric(args$window),
           TRUE ~ as.numeric(ceiling(2.25*dmax)))
  ) %>%
  rbind(qthreshold)

# Faixa etária UF --------------
dados.obs <- dados_full %>%
  filter(SinPri2Digita_DelayWeeks <= args$dmax,
         !is.na(fx_etaria)) %>%
  group_by(DT_SIN_PRI_epiweek, fx_etaria, SinPri2Digita_DelayWeeks) %>%
  summarise(Y = n()) %>%
  ungroup() %>%
  complete(DT_SIN_PRI_epiweek=1:today.week, fx_etaria, SinPri2Digita_DelayWeeks=0:args$dmax, fill=list(Y=0)) %>%
  mutate(dt.mais.delay = DT_SIN_PRI_epiweek + SinPri2Digita_DelayWeeks,
         Y = ifelse(dt.mais.delay<=today.week, Y, NA),
         SG_UF_NOT = 0,
         dt_event=DT_SIN_PRI_epiweek,
         delay=SinPri2Digita_DelayWeeks,
         Time=dt_event)
dados.obs <- dados_full %>%
  filter(SinPri2Digita_DelayWeeks <= args$dmax,
         !is.na(SG_UF_NOT),
         !is.na(fx_etaria)) %>%
  group_by(SG_UF_NOT, DT_SIN_PRI_epiweek, fx_etaria, SinPri2Digita_DelayWeeks) %>%
  summarise(Y = n()) %>%
  ungroup() %>%
  complete(SG_UF_NOT, DT_SIN_PRI_epiweek=1:today.week, fx_etaria, SinPri2Digita_DelayWeeks=0:args$dmax, fill=list(Y=0)) %>%
  mutate(dt.mais.delay = DT_SIN_PRI_epiweek + SinPri2Digita_DelayWeeks,
         Y = ifelse(dt.mais.delay<=today.week, Y, NA),
         dt_event=DT_SIN_PRI_epiweek,
         delay=SinPri2Digita_DelayWeeks,
         Time=dt_event) %>%
  bind_rows(dados.obs)

df.uf.age <- c()
pred.ufs <- c()

for (uf in uf.list$CO_UF){
  uf.name <- uf.list$DS_UF_SIGLA[uf.list$CO_UF == uf]
  pop <- uf.list$Populacao[uf.list$CO_UF == uf]
  gpj <- 0
  
  trials <- 0
  consistent <- F
  i.dmax <- qthreshold$dmax[qthreshold$SG_UF_NOT==uf]
  i.wdw <- qthreshold$wdw[qthreshold$SG_UF_NOT==uf]-1
  while (trials < 3 & !consistent){
    age.BR.h.srag.pred <- nowcasting_age(dados.obs %>% filter(
      SG_UF_NOT == uf,
      delay <= i.dmax,
      Time >= today.week - i.wdw))
    age.BR.h.srag.pred <- nowcasting.summary(age.BR.h.srag.pred, age = T)
    consistent <- age.BR.h.srag.pred$age %>%
      filter(dt_event == today.week) %>%
      transmute(dispersion = LS/(Median+1) < 20) %>%
      all()
    trials <- trials + 1
    i.dmax <- max(i.dmax - 2, 4)
    i.wdw <- round(2.25*i.dmax) - 1
    if (!consistent){
      fx.explode <- age.BR.h.srag.pred$age %>%
        filter(dt_event == today.week,
               LS/(Median+1) > 20) %>%
        select(fx_etaria)
      age.BR.h.srag.pred$age <- age.BR.h.srag.pred$age %>%
        mutate(LS = case_when(
          fx_etaria %in% fx.explode$fx_etaria ~ as.numeric(IC80S),
          TRUE ~ as.numeric(LS)
        ))
      print(paste0('UF: ', uf.name, '. Tentativa: ', trials, '.\nExplosão nas fx etárias:'))
      print(fx.explode)
    }
  }
  
  if (uf == 0){
    age.BR.h.srag.pred$age <- dados_full %>%
      filter(!is.na(fx_etaria)) %>%
      group_by(DT_SIN_PRI_epiweek, fx_etaria) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      complete(DT_SIN_PRI_epiweek=1:today.week, fx_etaria, fill=list(n=0)) %>%
      rename(dt_event=DT_SIN_PRI_epiweek) %>%
      left_join(age.BR.h.srag.pred$age, by=c('dt_event', 'fx_etaria')) %>%
      mutate(fx_etaria=factor(fx_etaria,
                              levels=fx.labels.num,
                              labels=fx.labels),
             Y = ifelse(is.na(Median), n, Median),
             n = ifelse(dt_event >= today.week-4, NA, n))
  } else {
    age.BR.h.srag.pred$age <- dados_full %>%
      filter(!is.na(fx_etaria),
             SG_UF_NOT == uf) %>%
      group_by(DT_SIN_PRI_epiweek, fx_etaria) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      complete(DT_SIN_PRI_epiweek=1:today.week, fx_etaria, fill=list(n=0)) %>%
      rename(dt_event=DT_SIN_PRI_epiweek) %>%
      left_join(age.BR.h.srag.pred$age, by=c('dt_event', 'fx_etaria')) %>%
      mutate(fx_etaria=factor(fx_etaria,
                              levels=fx.labels.num,
                              labels=fx.labels),
             Y = ifelse(is.na(Median), n, Median),
             n = ifelse(dt_event >= today.week-4, NA, n))
    
  }
  
  plt <- age.BR.h.srag.pred$age %>%
    plot.age.ts(xlimits=xlimits, xbreaks=xbreaks, xlbls=xlbls) + 
    labs(
      x = "Semana de primeiros sintomas",
      y = "Casos de SRAG",
      color = "Faixa Etária", 
      title = uf.name,
      subtitle = paste0("Novos casos semanais por faixa etária. Dados até a semana ", today.week.ori, ' ', lyear)) +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  png(filename = paste0(preff,"/Figs/UF/fig_", uf.name, "_fx_etaria.png"),
      width=9, height=6, units='in', res=200)
  print(plt)
  grid::grid.raster(logo, x = 0.999, y = 0.99, just = c('right', 'top'), width = unit(1, 'inches'))
  dev.off()
  
  df.uf.age <- df.uf.age %>%
    bind_rows(age.BR.h.srag.pred$age %>%
                mutate(SG_UF_NOT = uf,
                       DS_UF_SIGLA = uf.name))

  if (uf == 0){
    dadosBR <- dados_full %>%
      select(DT_SIN_PRI_epiweek, SG_UF_NOT) %>%
      group_by(DT_SIN_PRI_epiweek) %>%
      dplyr::summarise( Casos = n()) %>%
      rename(Date = DT_SIN_PRI_epiweek) %>%
      ungroup() %>%
      right_join(epiweek.table %>% transmute(Date = DT_SIN_PRI_epiweek), by='Date') %>%
      replace_na(list(Casos = 0))
  } else {
    dadosBR <- dados_full %>%
      filter(SG_UF_NOT == uf) %>%
      select(DT_SIN_PRI_epiweek, SG_UF_NOT) %>%
      group_by(DT_SIN_PRI_epiweek) %>%
      dplyr::summarise( Casos = n()) %>%
      rename(Date = DT_SIN_PRI_epiweek) %>%
      ungroup() %>%
      right_join(epiweek.table %>% transmute(Date = DT_SIN_PRI_epiweek), by='Date') %>%
      replace_na(list(Casos = 0))
  }
  pred.srag.summy <- generate.slope(dadosBR,
                                    age.BR.h.srag.pred$total.samples %>%
                                      ungroup() %>%
                                      transmute(Date=dt_event, sample=sample, Casos=Y),
                                    today.week=today.week,
                                    Dmax=qthreshold$dmax[qthreshold$SG_UF_NOT==uf]) %>%
    mutate_at(vars(-("Date"), -starts_with("tendencia")), ~ .*100000/pop) %>%
    mutate(CO_UF = uf,
           DS_UF_SIGLA = uf.name,
           populacao = pop,
           grupo_jur = gpj)
  pred.ufs <- pred.ufs %>%
    bind_rows(pred.srag.summy)
  
  p.now.srag <- plot.nowcast(pred.srag.summy %>%
                               filter(between(Date, xlimits[1], xlimits[2])),
                             Fim=today.week) +
    ylab("Incidência de SRAG (por 100mil hab.)") +
    xlab("Semana de primeiros sintomas") +
    scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits) +
    theme_Publication(base_size = 16, base_family = 'Roboto') +
    ggtitle(uf.name) +
    theme(plot.margin=unit(c(1,0,5,5), units='pt'),
          axis.text = element_text(size = rel(1)),
          axis.text.x = element_text(angle=45, hjust=1),
          legend.margin = margin(0,0,0,0, unit='pt'),
          legend.justification=c(0,1), 
          legend.position=c(0.015, 1.05),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.key.size = unit(14, 'pt'),
          legend.text = element_text(family = 'Roboto', size = rel(1)))
  
  p.nivel <-  plot.ts.tendencia(df = pred.srag.summy  %>%
                                  filter(between(Date, xlimits[1], xlimits[2])),
                                xbreaks = xbreaks,
                                xlbls = xlbls,
                                xlimits = xlimits)
  
  png(filename = paste0(preff,"/Figs/UF/fig_", uf.name, ".png"),
      width=8, height=6, units='in', res=200)
  print(plot_grid(p.now.srag, p.nivel, align='v', axis='l', nrow=2, ncol=1, rel_heights=c(2.5, 1)))
  grid::grid.raster(logo, x = 0.999, y = 0.99, just = c('right', 'top'), width = unit(1, 'inches'))
  dev.off()
}
rm(dados.obs)
df.uf.age <- df.uf.age %>%
  rename(DT_SIN_PRI_epiweek=dt_event,
         casos_notificados=n,
         mediana_da_estimativa=Median) %>%
  left_join(epiweek.table, by=c('DT_SIN_PRI_epiweek')) %>%
  arrange(SG_UF_NOT, fx_etaria, DT_SIN_PRI_epiweek) %>%
  mutate(mediana_da_estimativa=case_when(
    !is.na(mediana_da_estimativa) ~ as.numeric(mediana_da_estimativa),
    TRUE ~ as.numeric(casos_notificados)
  )) %>%
  group_by(SG_UF_NOT, fx_etaria) %>%
  mutate(media_movel=round(roll_mean(mediana_da_estimativa, n=3, align='center', fill=NA)))
df.uf.age %>%
  saveRDS(file=paste0(preff,'/uf.estimativas.fx.etaria.rds'))
df.uf.age %>%
  mutate(fx_etaria=str_replace(fx_etaria, '-', ' a '),
         mediana_da_estimativa=round(mediana_da_estimativa),
         LI=round(LI),
         LS=round(LS),
         Q1=round(Q1),
         Q3=round(Q3)) %>%
  select(-DT_SIN_PRI_epiweek, -fx_etaria.num, -Time, -Y) %>%
  write_csv2(file=paste0(preff,'/estados_e_pais_serie_estimativas_fx_etaria_sem_filtro_febre.csv'), na='')

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
  rename('Ano epidemiológico' = epiyear,
         'Semana epidemiológica' = epiweek,
         'casos estimados' = full_estimate,
         IC95I = LI,
         IC95S = LS,
         'Casos semanais reportados até a última atualização' = Casos.cut,
         'média móvel' = rolling_average,
         'tendência de curto prazo' = tendencia.3s,
         'tendência de longo prazo' = tendencia.6s,
         'Grupo Jurídico' = grupo_jur,
         'População' = populacao) %>%
  select(-Median, -Casos, -Date) %>%
  write_csv2(paste0(preff,'/estados_e_pais_serie_estimativas_tendencia_sem_filtro_febre.csv'), na = '')
rm(pred.ufs)
gc(verbose=F)

# Capitais -----
tblCADMUN.capital <- tblCADMUN %>% filter(IN_CAPITAL %in% c('E', 'F'))

qthreshold <- dados_full %>%
  select(CO_MUN_RES, grupo_jur, DT_DIGITA_epiyear, DT_DIGITA_epiweek, SinPri2Digita_DelayWeeks) %>%
  filter(CO_MUN_RES != '530010') %>%
  filter((CO_MUN_RES %in% tblCADMUN.capital$CO_MUNICIP) &
           ((DT_DIGITA_epiyear == lyear-1 & DT_DIGITA_epiweek > today.week.ori) |
              (DT_DIGITA_epiyear == lyear))
  ) %>%
  group_by(CO_MUN_RES, grupo_jur) %>%
  summarize(dmax = quantile(SinPri2Digita_DelayWeeks, probs=qthres.probs, na.rm=TRUE)) %>%
  mutate(dmax = ceiling(dmax + 2),
         dmax = case_when(
           dmax > args$dmax ~ as.numeric(args$dmax),
           dmax < 4 ~ 4,
           TRUE ~ as.numeric(dmax)),
         wdw = case_when(
           ceiling(2.25*dmax) > args$window ~ as.numeric(args$window),
           TRUE ~ as.numeric(ceiling(2.25*dmax)))
  )
qthreshold <- dados_full %>%
  filter(CO_MUN_RES %in% bsb_ras &
           ((DT_DIGITA_epiyear == lyear-1 & DT_DIGITA_epiweek >= today.week.ori) |
              (DT_DIGITA_epiyear == lyear))
  ) %>%
  select(grupo_jur, SinPri2Digita_DelayWeeks) %>%
  group_by(grupo_jur) %>%
  summarize(dmax = quantile(SinPri2Digita_DelayWeeks, probs=qthres.probs, na.rm=TRUE)) %>%
  mutate(CO_MUN_RES = 530010,
         dmax = ceiling(dmax + 2),
         dmax = case_when(
           dmax > args$dmax ~ as.numeric(args$dmax),
           dmax < 4 ~ 4,
           TRUE ~ as.numeric(dmax)),
         wdw = case_when(
           ceiling(2.25*dmax) > args$window ~ as.numeric(args$window),
           TRUE ~ as.numeric(ceiling(2.25*dmax)))
  ) %>%
  rbind(qthreshold)
qthreshold <- dados_full %>%
  select(CO_MUN_RES, DT_DIGITA_epiyear, DT_DIGITA_epiweek, SinPri2Digita_DelayWeeks) %>%
  filter(CO_MUN_RES != 530010) %>%
  filter((CO_MUN_RES %in% tblCADMUN.capital$CO_MUNICIP) &
           ((DT_DIGITA_epiyear == lyear-1 & DT_DIGITA_epiweek > today.week.ori) |
              (DT_DIGITA_epiyear == lyear))
  ) %>%
  group_by(CO_MUN_RES) %>%
  summarize(dmax = quantile(SinPri2Digita_DelayWeeks, probs=qthres.probs, na.rm=TRUE)) %>%
  mutate(dmax = ceiling(dmax + 2),
         dmax = case_when(
           dmax > args$dmax ~ as.numeric(args$dmax),
           dmax < 4 ~ 4,
           TRUE ~ as.numeric(dmax)),
         wdw = case_when(
           ceiling(2.25*dmax) > args$window ~ as.numeric(args$window),
           TRUE ~ as.numeric(ceiling(2.25*dmax))),
         grupo_jur = 0
  ) %>%
  rbind(qthreshold)
qthreshold <- dados_full %>%
  filter(CO_MUN_RES %in% bsb_ras &
           ((DT_DIGITA_epiyear == lyear-1 & DT_DIGITA_epiweek >= today.week.ori) |
              (DT_DIGITA_epiyear == lyear))
  ) %>%
  select(SinPri2Digita_DelayWeeks) %>%
  summarize(dmax = quantile(SinPri2Digita_DelayWeeks, probs=qthres.probs, na.rm=TRUE)) %>%
  mutate(CO_MUN_RES = 530010,
         dmax = ceiling(dmax + 2),
         dmax = case_when(
           dmax > args$dmax ~ as.numeric(args$dmax),
           dmax < 4 ~ 4,
           TRUE ~ as.numeric(dmax)),
         wdw = case_when(
           ceiling(2.25*dmax) > args$window ~ as.numeric(args$window),
           TRUE ~ as.numeric(ceiling(2.25*dmax))),
         grupo_jur = 0
  ) %>%
  rbind(qthreshold)

codmun.list <- c(110020,172100)
qthreshold <- qthreshold %>%
  mutate(dmax = ifelse(CO_MUN_RES %in% codmun.list, dmax + 2, dmax),
         wdw = ifelse(CO_MUN_RES %in% codmun.list, round(2.5*dmax), wdw))

fx.breaks=c(seq(0, 80, 10), 140)
fx.labels=c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59',
            '60-69', '70-79', '80+')
fx.labels.num=as.character(seq(1,length(fx.breaks)-1))

dados_full <- dados_full %>%
  mutate(fx_etaria = as.character(cut(idade_em_anos,
                                      breaks=fx.breaks,
                                      right=F,
                                      labels=fx.labels.num)
                                  )
         )

pred.capitais <- c()
pred.warning <- c()
pred.failed <- c()
df.uf.age <- c()
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
        CO_MUN_RES == tblCADMUN.capital$CO_MUNICIP[k]
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
        CO_MUN_RES %in% bsb_ras
      )
    title0 <- 'REGIAO DE SAUDE CENTRAL'
  }
  
  pop <- tblCADMUN.capital$Populacao[k]
  mun.id <- co_mun

  
  # Semana epidemiologica termina no Sabado, entao vou excluir os dados mais recentes caso a semana nao comece no sábado.
  Fim.sat <- today.week

  for (gpj in c(0)){
    if (gpj > 0){
      dadosBR <- dadosBR0 %>%
        filter(grupo_jur == gpj)
    } else {
      dadosBR <- dadosBR0
    }
    
    if (gpj == 0 | nrow(dadosBR[dadosBR$DT_SIN_PRI_epiweek >= today.week - 10,]) > 10){
      dmax <- qthreshold %>%
        filter(CO_MUN_RES == tblCADMUN.capital$CO_MUNICIP[k],
               grupo_jur == gpj) %>%
        select(dmax) %>%
        as.integer()
      wdw <- qthreshold %>%
        filter(CO_MUN_RES == tblCADMUN.capital$CO_MUNICIP[k],
               grupo_jur == gpj) %>%
        select(wdw) %>%
        as.integer()
      title <- paste0(title0, ttl.xtra[gpj + 1])
      Inicio <- min(dadosBR$DT_SIN_PRI)

      warn.lbl <- paste(mun.id, 'gpj=', gpj)
      if (gpj > 0){
        pred.srag.summy <- try.estimate('Capital', warn.lbl,
                                        dadosBR, Inicio, today.week, Dmax=dmax, wdw=wdw, zero.inflated = TRUE)
        if (inherits(pred.srag.summy, 'error')){
          dmax <- dmax + 1
          wdw <- wdw + 2
          pred.warning <- c(pred.warning, warn.lbl)
          
          pred.srag.summy <- try.estimate('Capital', warn.lbl,
                                          dadosBR, Inicio, today.week, Dmax=dmax, wdw=wdw, zero.inflated = TRUE)
          if (inherits(pred.srag.summy, 'error')){
            message(paste('Não foi possível executar. Pulando o local', warn.lbl))
            pred.failed <- c(pred.failed, warn.lbl)
            pred.srag.summy <- readRDS(paste0(preff,'/capitais_', lyear, '_', today.week-1, '.rds')) %>%
              filter(CO_MUN_RES == as.integer(mun.id),
                     grupo_jur == gpj) %>%
              select(-epiweek, -epiyear) %>%
              right_join(epiweek.table %>% transmute(Date = DT_SIN_PRI_epiweek), by='Date') %>%
              fill(CO_MUN_RES, CO_MUN_RES_nome, CO_UF, DS_UF_SIGLA, populacao, grupo_jur, .direction='down')
            pred.capitais <- pred.capitais %>%
              bind_rows(pred.srag.summy)
            next
          }
        }
      } else {
        dadosBR <- dadosBR0 %>%
          filter(SinPri2Digita_DelayWeeks <= args$dmax,
                 !is.na(fx_etaria)) %>%
          group_by(DT_SIN_PRI_epiweek, fx_etaria, SinPri2Digita_DelayWeeks) %>%
          summarise(Y = n()) %>%
          ungroup() %>%
          complete(DT_SIN_PRI_epiweek=1:today.week, fx_etaria, SinPri2Digita_DelayWeeks=0:args$dmax, fill=list(Y=0)) %>%
          mutate(dt.mais.delay = DT_SIN_PRI_epiweek + SinPri2Digita_DelayWeeks,
                 Y = ifelse(dt.mais.delay<=today.week, Y, NA),
                 dt_event=DT_SIN_PRI_epiweek,
                 delay=SinPri2Digita_DelayWeeks,
                 Time=dt_event)
        
        trials <- 0
        consistent <- F
        while (trials < 3 & !consistent & dmax >= 4){
          age.BR.h.srag.pred <- nowcasting_age(dadosBR %>% filter(
            delay <= dmax,
            Time >= today.week - (wdw-1)))
          age.BR.h.srag.pred <- nowcasting.summary(age.BR.h.srag.pred, age = T)
          consistent <- age.BR.h.srag.pred$age %>%
            filter(dt_event > today.week-dmax) %>%
            transmute(dispersion = LS/(Median+1) < 20) %>%
            all()
          trials <- trials + 1
          dmax <- dmax - 2
          wdw <- round(2.25*dmax) - 1
          if (!consistent){
            fx.explode <- age.BR.h.srag.pred$age %>%
              filter(dt_event > today.week-dmax,
                     LS/(Median+1) > 20) %>%
              select(fx_etaria) %>%
              distinct()
            age.BR.h.srag.pred$age <- age.BR.h.srag.pred$age %>%
              mutate(LS = case_when(
                fx_etaria %in% fx.explode$fx_etaria ~ as.numeric(IC80S),
                TRUE ~ as.numeric(LS)
              ))
            print(paste0('Capital: ', title0, '. Tentativa: ', trials, '.\nExplosão nas fx etárias:'))
            print(fx.explode)
          }
        }
        
        age.BR.h.srag.pred$age <- dadosBR0 %>%
          filter(!is.na(fx_etaria)) %>%
          group_by(DT_SIN_PRI_epiweek, fx_etaria) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          complete(DT_SIN_PRI_epiweek=1:today.week, fx_etaria, fill=list(n=0)) %>%
          rename(dt_event=DT_SIN_PRI_epiweek) %>%
          left_join(age.BR.h.srag.pred$age, by=c('dt_event', 'fx_etaria')) %>%
          mutate(fx_etaria=factor(fx_etaria,
                                  levels=as.character(1:9),
                                  labels=c(paste0((0:7)*10, " - ",(0:7)*10+9), "80+")),
                 Y = ifelse(is.na(Median), n, Median),
                 n = ifelse(dt_event >= today.week-4, NA, n))
        
        plt <- age.BR.h.srag.pred$age  %>%
          plot.age.ts(xlimits=xlimits, xbreaks=xbreaks, xlbls=xlbls) + 
          labs(
            x = "Semana de primeiros sintomas",
            y = "Casos de SRAG",
            color = "Faixa Etária", 
            title = paste0(tblCADMUN.capital$DS_UF_SIGLA[k], ": ", title),
            subtitle = paste0("Novos casos semanais por faixa etária. Dados até a semana ", today.week.ori, ' ', lyear)) +
          theme(axis.text.x = element_text(angle=45, hjust=1))
        png(filename = paste0(preff,"/Figs/Capitais/fig_",
                              tblCADMUN.capital$DS_UF_SIGLA[k],
                              '_',
                              str_replace_all(tblCADMUN.capital$DS_NOMEPAD_municip[k], ' ', '_'),
                              "_fx_etaria.png"),
            width=9, height=6, units='in', res=200)
        print(plt)
        grid::grid.raster(logo, x = 0.999, y = 0.99, just = c('right', 'top'), width = unit(1, 'inches'))
        dev.off()
        
        df.uf.age <- df.uf.age %>%
          bind_rows(age.BR.h.srag.pred$age %>%
                      mutate(CO_MUN_RES = as.integer(mun.id),
                             CO_MUN_RES_nome = title,
                             CO_UF = co_uf,
                             DS_UF_SIGLA = co_uf.name))
        
        dadosBR <- dadosBR0 %>%
          group_by(DT_SIN_PRI_epiweek) %>%
          dplyr::summarise( Casos = n()) %>%
          rename(Date = DT_SIN_PRI_epiweek) %>%
          ungroup() %>%
          right_join(epiweek.table %>% transmute(Date = DT_SIN_PRI_epiweek), by='Date') %>%
          replace_na(list(Casos = 0))
        gc(verbose = F)

        pred.srag.summy <- generate.slope(dadosBR,
                                          age.BR.h.srag.pred$total.samples %>%
                                            ungroup() %>%
                                            transmute(Date=dt_event, sample=sample, Casos=Y),
                                          today.week=today.week,
                                          Dmax=dmax)
      }
      
      if (co_mun != 530010){
        pred.srag.summy <- pred.srag.summy %>%
          mutate_at(vars(-("Date"), -starts_with("tendencia")), ~ .*100000/pop)
        ylab.lbl <- "Incidência de SRAG (por 100mil hab.)"
      } else {
        ylab.lbl <- 'Casos de SRAG'
      }
      pred.srag.summy <- pred.srag.summy %>%
        mutate(CO_MUN_RES = as.integer(mun.id),
               CO_MUN_RES_nome = title,
               CO_UF = co_uf,
               DS_UF_SIGLA = co_uf.name,
               populacao = pop,
               grupo_jur = gpj)
      
      pred.capitais <- pred.capitais %>%
        bind_rows(pred.srag.summy)
      
      p.now.srag <- plot.nowcast(pred.srag.summy  %>%
                                   filter(between(Date, xlimits[1], xlimits[2])),
                                 Fim=today.week ) +
        ylab(ylab.lbl) +
        xlab("Semana de primeiros sintomas") +
        scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits) +
        theme_Publication(base_size = 16, base_family = 'Roboto') +
        ggtitle(paste0(tblCADMUN.capital$DS_UF_SIGLA[k], ": ", title)) +
        theme(plot.margin=unit(c(1,0,5,5), units='pt'),
              axis.text = element_text(size = rel(1)),
              axis.text.x = element_text(angle=45, hjust=1),
              legend.margin = margin(0,0,0,0, unit='pt'),
              legend.justification=c(0,1), 
              legend.position=c(0.015, 1.05),
              legend.background = element_blank(),
              legend.key = element_blank(),
              legend.key.size = unit(14, 'pt'),
              legend.text = element_text(family = 'Roboto', size = rel(1))
        )
      p.nivel <-  plot.ts.tendencia(df = pred.srag.summy  %>%
                                      filter(between(Date, xlimits[1], xlimits[2])),
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
      grid::grid.raster(logo, x = 0.999, y = 0.99, just = c('right', 'top'), width = unit(1, 'inches'))
      dev.off()
    }
  }
}
pred.warning <- as.data.frame(list('warning'=pred.warning))
pred.failed <- as.data.frame(list('failed'=pred.failed))
print('Error list:')
print(pred.failed)
pred.warning %>%
  write.csv('capitais.warning.csv', row.names=F)
pred.failed %>%
  write.csv('capitais.failed.csv', row.names=F)

pred.capitais <- pred.capitais %>%
  left_join(epiweek.table, by=c('Date' = 'DT_SIN_PRI_epiweek'))
saveRDS(pred.capitais, paste0(preff,'/capitais_', lyear, '_', today.week, '.rds'))
saveRDS(pred.capitais, paste0(preff,'/capitais_current.rds'))
saveRDS(pred.capitais, '../../data/data/capitais_current.rds')

plot.ufs.tendencia(pred.capitais %>% filter(Date == today.week, grupo_jur == 0))

pred.capitais <- pred.capitais %>%
  mutate(escala = case_when(
    CO_MUN_RES == 530010 ~ 'casos',
    TRUE ~ 'incidência')
    )
pred.capitais <- pred.capitais %>%
  filter(escala == 'incidência') %>%
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
  rename('Ano epidemiológico' = epiyear,
         'Semana epidemiológica' = epiweek,
         'casos estimados' = full_estimate,
         IC95I = LI,
         IC95S = LS,
         'Casos semanais reportados até a última atualização' = Casos.cut,
         'média móvel' = rolling_average,
         'tendência de curto prazo' = tendencia.3s,
         'tendência de longo prazo' = tendencia.6s,
         'Grupo Jurídico' = grupo_jur,
         'População' = populacao) %>%
  select(-Median, -Casos, -Date) %>%
  write_csv2(paste0(preff,'/capitais_serie_estimativas_tendencia_sem_filtro_febre.csv'), na = '')
rm(pred.capitais)
gc(verbose=F)

df.uf.age <- df.uf.age %>%
  rename(DT_SIN_PRI_epiweek=dt_event,
         casos_notificados=n,
         mediana_da_estimativa=Median) %>%
  left_join(epiweek.table, by=c('DT_SIN_PRI_epiweek')) %>%
  arrange(CO_MUN_RES, fx_etaria, DT_SIN_PRI_epiweek) %>%
  mutate(mediana_da_estimativa=case_when(
    !is.na(mediana_da_estimativa) ~ as.numeric(mediana_da_estimativa),
    TRUE ~ as.numeric(casos_notificados)
  )) %>%
  group_by(CO_MUN_RES, fx_etaria) %>%
  mutate(media_movel=round(roll_mean(mediana_da_estimativa, n=3, align='center', fill=NA)))
df.uf.age %>%
  saveRDS(file=paste0(preff,'/capitais.estimativas.fx.etaria.rds'))
df.uf.age %>%
  mutate(fx_etaria=str_replace(fx_etaria, '-', ' a '),
         mediana_da_estimativa=round(mediana_da_estimativa),
         LI=round(LI),
         LS=round(LS),
         Q1=round(Q1),
         Q3=round(Q3)) %>%
  select(-DT_SIN_PRI_epiweek, -fx_etaria.num, -Time, -Y) %>%
  write_csv2(file=paste0(preff,'/capitais_serie_estimativas_fx_etaria_sem_filtro_febre.csv'), na='')

if (args$filtertype != 'sragnofever'){
  quit()
}

# Macrorregionais de saúde: ----
tblMACSAUD <- tblCADMUN %>%
  filter(!is.na(CO_MACSAUD),
         CO_MACSAUD != 0) %>%
  select(CO_MACSAUD, DS_NOMEPAD_macsaud, CO_UF, DS_UF_SIGLA, Populacao) %>%
  group_by(CO_MACSAUD, DS_NOMEPAD_macsaud, CO_UF, DS_UF_SIGLA) %>%
  summarise(Populacao = sum(Populacao)) %>%
  mutate(DS_NOMEPAD_macsaud_clean = str_replace_all(DS_NOMEPAD_macsaud, ' ', '_')) %>%
  mutate(DS_NOMEPAD_macsaud_clean = str_replace_all(DS_NOMEPAD_macsaud_clean, '/', '-')) %>%
  ungroup()
rownames(tblMACSAUD) <- NULL

qthreshold <- dados_full %>%
  select(CO_MACSAUD, DT_DIGITA_epiyear, DT_DIGITA_epiweek, SinPri2Digita_DelayWeeks) %>%
  filter((CO_MACSAUD %in% tblMACSAUD$CO_MACSAUD) &
           ((DT_DIGITA_epiyear == lyear-1 & DT_DIGITA_epiweek > today.week.ori) |
              (DT_DIGITA_epiyear == lyear))
  ) %>%
  group_by(CO_MACSAUD) %>%
  summarize(dmax = quantile(SinPri2Digita_DelayWeeks, probs=qthres.probs, na.rm=TRUE)) %>%
  mutate(dmax = dmax + 2,
         dmax = case_when(
           dmax > args$dmax ~ as.numeric(args$dmax),
           dmax < 4 ~ 4,
           TRUE ~ as.numeric(dmax)),
         wdw = case_when(
           ceiling(2.25*dmax) > args$window ~ as.numeric(args$window),
           TRUE ~ as.numeric(ceiling(2.25*dmax)))
  )

macros.unicas <- tblMACSAUD %>%
  group_by(CO_UF) %>%
  summarise(n=n()) %>%
  filter(n==1, CO_UF !=0) %>%
  ungroup() %>%
  left_join(tblMACSAUD, by='CO_UF')
pred.ufs <- readRDS('ufs_current.rds') %>%
  filter(CO_UF %in% macros.unicas$CO_UF) %>%
  select(-epiweek, -epiyear)

pred.macros <- c()
pred.warning <- c()
pred.failed <- c()
for(k in 1:nrow(tblMACSAUD)){

  dmax <- qthreshold %>%
    filter(CO_MACSAUD == tblMACSAUD$CO_MACSAUD[k]) %>%
    select(dmax) %>%
    as.integer()
  wdw <- qthreshold %>%
    filter(CO_MACSAUD == tblMACSAUD$CO_MACSAUD[k]) %>%
    select(wdw) %>%
    as.integer()
  
  dadosBR0 <- dados_full %>% 
    mutate(
      DT_SIN_PRI = ymd(DT_SIN_PRI),
      DT_DIGITA = ymd(DT_DIGITA),
    ) %>% 
    filter(
      DT_SIN_PRI_epiyear >= 2020,
      CO_MACSAUD == tblMACSAUD$CO_MACSAUD[k]
    )
  
  macsaud.id <- tblMACSAUD$CO_MACSAUD[k]
  # Semana epidemiologica termina no Sabado, entao vou excluir os dados mais recentes caso a semana nao comece no sábado.
  Fim.sat <- today.week
  pop <- tblMACSAUD$Populacao[k]
  macsaud.name <- tblMACSAUD$DS_NOMEPAD_macsaud[k]
  uf <- tblMACSAUD$CO_UF[k]
  uf.name <- tblMACSAUD$DS_UF_SIGLA[k]
  
  for (gpj in c(0)){
    if (gpj > 0){
      dadosBR <- dadosBR0 %>%
        filter(grupo_jur == gpj)
    } else {
      dadosBR <- dadosBR0
    }
    if (nrow(dadosBR) > 10){
      Inicio <- min(dadosBR$DT_SIN_PRI)
      warn.lbl <- macsaud.id
      if ((gpj == 0) & (uf %in% macros.unicas$CO_UF)){
        pred.srag.summy <- pred.ufs %>%
          filter(CO_UF == uf) %>%
          mutate(nivel = calc.transmission.thresholds(rolling_average),
                 CO_MACSAUD = as.integer(macsaud.id),
                 DS_NOMEPAD_macsaud = macsaud.name) %>%
          fill(nivel, .direction = 'downup')
      } else {
        
        pred.srag.summy <- try.estimate('MACSAUDE', warn.lbl,
                                        dadosBR, Inicio, today.week, Dmax=dmax, wdw=wdw, zero.inflated = TRUE)
        if (inherits(pred.srag.summy, 'error')){
          dmax <- dmax - 2
          wdw <- ceiling(2.25*dmax)
          pred.warning <- c(pred.warning, warn.lbl)
          pred.srag.summy <- try.estimate('MACSAUDE', warn.lbl,
                                          dadosBR, Inicio, today.week, Dmax=dmax, wdw=wdw, zero.inflated = TRUE)
          if (inherits(pred.srag.summy, 'error')){
            pred.failed <- c(pred.failed, warn.lbl)
            message(paste('Não foi possível executar. Pulando o local', warn.lbl))
            pred.srag.summy <- readRDS(paste0(preff,'/macros_', lyear, '_', today.week-1, '.rds')) %>%
              filter(CO_MACSAUD == as.integer(macsaud.id)) %>%
              select(-epiweek, -epiyear) %>%
              mutate(grupo_jur=0) %>%
              right_join(epiweek.table %>% transmute(Date = DT_SIN_PRI_epiweek), by='Date') %>%
              fill(nivel, CO_MACSAUD, DS_NOMEPAD_macsaud, CO_UF, DS_UF_SIGLA, populacao, grupo_jur, .direction='down')
            pred.macros <- pred.macros %>%
              bind_rows(pred.srag.summy)
            next
          }
        }
        pred.srag.summy <- pred.srag.summy %>%
          mutate_at(vars(-("Date"), -starts_with("tendencia")), ~ .*100000/pop) %>%
          mutate(nivel = calc.transmission.thresholds(rolling_average),
                 CO_MACSAUD = as.integer(macsaud.id),
                 DS_NOMEPAD_macsaud = macsaud.name,
                 CO_UF = uf,
                 DS_UF_SIGLA = uf.name,
                 populacao = pop,
                 grupo_jur = gpj) %>%
          fill(nivel, .direction = 'downup')
      }
      
      pred.macros <- pred.macros %>%
        bind_rows(pred.srag.summy)
      
      p.now.srag <- plot.nowcast(pred.srag.summy  %>%
                                   filter(between(Date, xlimits[1], xlimits[2])),
                                 Fim=today.week ) +
        ylab("Incidência de SRAG (por 100mil hab.)") +
        xlab("Semana de primeiros sintomas") +
        scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits) +
        theme_Publication(base_size = 16, base_family = 'Roboto') +
        ggtitle(paste0(tblMACSAUD$DS_UF_SIGLA[k], ": ", tblMACSAUD$DS_NOMEPAD_macsaud[k], ttl.xtra[gpj + 1])) +
        theme(plot.margin=unit(c(1,0,5,5), units='pt'),
              axis.text = element_text(size = rel(1)),
              axis.text.x = element_text(angle=45, hjust=1),
              legend.margin = margin(0,0,0,0, unit='pt'),
              legend.justification=c(0,1), 
              legend.position=c(0.015, 1.05),
              legend.background = element_blank(),
              legend.key = element_blank(),
              legend.key.size = unit(14, 'pt'),
              legend.text = element_text(family = 'Roboto', size = rel(1)))
      
      p.nivel <-  plot.ts.tendencia(df = pred.srag.summy  %>%
                                      filter(between(Date, xlimits[1], xlimits[2])),
                                    xbreaks = xbreaks,
                                    xlbls = xlbls,
                                    xlimits = xlimits)
      
      png(filename = paste0(preff,"/Figs/MACSAUD/fig_", tblMACSAUD$DS_UF_SIGLA[k], '_', tblMACSAUD$CO_MACSAUD[k], suff.xtra[gpj + 1], ".png"),
          width=8, height=6, units='in', res=200)
      print(plot_grid(p.now.srag, p.nivel, align='v', axis='l', nrow=2, ncol=1, rel_heights=c(2.5, 1)))
      grid::grid.raster(logo, x = 0.999, y = 0.99, just = c('right', 'top'), width = unit(1, 'inches'))
      dev.off()
    }
  }
  
}

pred.warning <- as.data.frame(list('warning'=pred.warning))
pred.failed <- as.data.frame(list('failed'=pred.failed))
print('Error list:')
print(pred.failed)
pred.warning %>%
  write.csv('macsaud.warning.csv', row.names=F)
pred.failed %>%
  write.csv('macsaud.failed.csv', row.names=F)

pred.macros <- pred.macros %>%
  filter(grupo_jur == 0) %>%
  select(-grupo_jur) %>%
  left_join(epiweek.table, by=c('Date' = 'DT_SIN_PRI_epiweek'))

saveRDS(pred.macros, paste0(preff,'/macros_', lyear, '_', today.week, '.rds'))
saveRDS(pred.macros, paste0(preff,'/macros_current.rds'))
saveRDS(pred.macros, '../../data/data/macros_current.rds')

if (args$filtertype == 'sragnofever'){
  pred.macros$CO_UF %>%
    unique() %>%
    map(plot.macsaude.tendencia, df=pred.macros %>% filter(Date == today.week))
  
  plot.macsaude.tendencia(uf='BR', df=pred.macros %>% filter(Date == today.week), orientation = 'portrait')
  plot.macsaude.tendencia(uf='BR', df=pred.macros %>% filter(Date == today.week), orientation = 'landscape')
}

plot.macsaude.nivel(pred.macros %>% mutate(sabado = epiweek2date(epiyear, epiweek)))

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

fill.lbl <- c('Pré-epidêmica', 'Epidêmica', 'Alta', 'Muito Alta', 'Extremamente Alta')
pred.macros %>%
  mutate(nivel = factor(nivel,
                 levels=c(0, 1, 2, 3, 4),
                 labels=fill.lbl)) %>%
  rename('Ano epidemiológico' = epiyear,
         'Semana epidemiológica' = epiweek,
         'casos estimados' = full_estimate,
         IC95I = LI,
         IC95S = LS,
         'Casos semanais reportados até a última atualização' = Casos.cut,
         'média móvel' = rolling_average,
         'tendência de curto prazo' = tendencia.3s,
         'tendência de longo prazo' = tendencia.6s,
         'transmissão comunitária' = nivel,
         'População' = populacao) %>%
  select(-Median, -Casos, -Date) %>%
  write_csv2(paste0(preff,'/macsaud_serie_estimativas_tendencia_sem_filtro_febre.csv'), na = '')
rm(pred.macros)
gc(verbose=F)

# Regionais de saúde do AM ------------
run.regsaud.am(qthres.probs)
gc(verbose=F)

# Regionais de saúde do SC ------------
run.regsaud.sc(qthres.probs)
