suppressWarnings(suppressPackageStartupMessages(library("tidyverse")))
suppressWarnings(suppressPackageStartupMessages(library(lme4)))

slope.estimate.quant <- function(end.week, pred.srag, w=3){
  base.week <- end.week - (w - 1)
  if (!base.week %in% pred.srag$Date){
    return(NA)
  }
  norm.casos <- pred.srag %>%
    filter(Date == base.week) %>%
    rename(valorbase = Casos) %>%
    select(sample, valorbase)
  norm.casos <- norm.casos %>%
    right_join(pred.srag %>%
                 filter(Date >= base.week & Date <= end.week),
               by='sample') %>%
    mutate(Casos = case_when(
      valorbase > 0 ~ Casos/valorbase,
      TRUE ~ Casos))
    #mutate(Casos = Casos/valorbase)
  tmp <- lmList(Casos ~ Date | sample, data = norm.casos)
  slope <- coefficients(tmp) %>%
    select(Date) %>%
    summarise(LI = quantile(Date, probs=.05, na.rm=T),
              Q1 = quantile(Date, probs=.25, na.rm=T),
              Q3 = quantile(Date, probs=.75, na.rm=T),
              LS = quantile(Date, probs=.95, na.rm=T)) %>%
    ungroup() %>%
    transmute(slope = case_when(
      LI > 0 ~ 1,
      LI < 0 & Q1 >= 0 ~ .5,
      Q1 < 0 & Q3 > 0 ~ 0,
      Q3 <= 0 & LS > 0 ~ -.5,
      LS < 0 ~ -1
    ) ) %>%
    as.numeric()
  return(slope)
}
