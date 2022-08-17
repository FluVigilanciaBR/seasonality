require(WaveletComp)
require(data.table)
require(tidyverse)
require(RcppRoll)
source('../data_filter/episem.R')

plot.profile.and.series <- function(uf, start=2010, end=2019, title=NULL,
                                    base_size=14){
  p <- df %>%
    filter(UF== uf,
           between(epiyear, start, end)) %>%
    mutate(rolling_average = roll_mean(SRAG, n=3, align='center', fill=NA_real_),
           epiyear=factor(epiyear)) %>%
    ggplot() +
    geom_line(aes(x=epiweek, y=rolling_average , linetype=epiyear))
  dtmp <- df.typical %>%
    filter(UF==uf)
  p + geom_area(data=dtmp, aes(x=epiweek, y=`corredor baixo`), fill='green', alpha=.5) +
    geom_ribbon(data=dtmp, aes(x=epiweek, ymin=`corredor baixo`, ymax=`corredor mediano`), fill='yellow', alpha=.5) +
    geom_ribbon(data=dtmp, aes(x=epiweek, ymin=`corredor mediano`, ymax=`corredor alto`), fill='orange', alpha=.5) +
    geom_ribbon(data=dtmp, aes(x=epiweek, ymin=`corredor alto`, ymax=Inf), fill='red', alpha=.5) +
    geom_line(data=dtmp, aes(x=epiweek, y=`limiar pré-epidêmico`), linetype=3, color='darkgrey', size=2) +
    theme_Publication(base_size = base_size) +
    guides(linetype='none') +
    labs(x='Semana epidemiológica', y='Incidência', subtitle=title) + 
    scale_x_continuous(breaks = c(1, seq(4, 52, 4))) 
}

plot.series <- function(uf, start=2010, end=2019, title=NULL,
                                    base_size=14){
  p <- df %>%
    filter(UF== uf,
           between(epiyear, start, end)) %>%
    mutate(rolling_average = roll_mean(SRAG, n=3, align='center', fill=NA_real_),
           epiyear=factor(epiyear)) %>%
    ggplot() +
    geom_line(aes(x=epiweek, y=rolling_average , linetype=epiyear)) +
    theme_Publication(base_size = base_size) +
    guides(linetype='none') +
    labs(x='Semana epidemiológica', y='Incidência', subtitle=title) + 
    scale_x_continuous(breaks = c(1, seq(4, 52, 4))) 
  return(p)
}

plot.profile <- function(uf, base_size=14, title=NULL){
  df.typical %>%
    filter(UF==uf) %>%
    ggplot(aes(x=epiweek)) +
    geom_area(aes(x=epiweek, y=`corredor baixo`), fill='green', alpha=.5) +
    geom_ribbon(aes(x=epiweek, ymin=`corredor baixo`, ymax=`corredor mediano`), fill='yellow', alpha=.5) +
    geom_ribbon(aes(x=epiweek, ymin=`corredor mediano`, ymax=`corredor alto`), fill='orange', alpha=.5) +
    geom_ribbon(aes(x=epiweek, ymin=`corredor alto`, ymax=Inf), fill='red', alpha=.5) +
    theme_Publication(base_size = base_size) +
    guides(linetype='none') +
    labs(x='Semana epidemiológica', y='Incidência', subtitle=title) + 
    scale_x_continuous(breaks = c(1, seq(4, 52, 4))) 
}

plot.wavelet <- function(wave, title=NULL){
  wt.image(wave, show.date = T, siglvl=0.001, legend.params = list(label.digits=3), color.key='q',
           periodlab='Período (semanas)', timelab = 'Semana epidemiológica',
           spec.period.axis = list(at=seq(13, 113, 13)),
           main=title)
}

df <- fread('../clean_data/srag_hospdeath_current_estimated_incidence.csv.gz')
df <- df %>%
  filter(epiyear < 2020) %>%
  mutate(domingo=epiweek2date(epiyear, epiweek))
df.typical <- fread('../clean_data/srag_hospdeath_mem-typical.csv')
df.report <- fread('../clean_data/srag_hospdeath_mem-report.csv')
df.typical <- df.report %>%
  select(UF, `limiar pré-epidêmico`) %>%
  left_join(df.typical, by='UF')

# wavelet ----
wave <- df %>%
  filter(UF==13) %>%
  mutate(rolling_average=roll_mean(SRAG, align='center'),
         date=domingo) %>%
  analyze.wavelet('rolling_average', loess.span=0, dt=1, dj=1/50, lowerPeriod=13, upperPeriod=112)

png('wavelet_Amazonas.png')
plot.wavelet(wave, 'Amazonas')
dev.off()



