library(tidyverse)
library(httr)
library(jsonlite)
query <- "http://api.sidra.ibge.gov.br/values/T/6579/P/ANO/V/9324/N6/all"



get_pop <- function(x) {
    str_replace(query,'ANO',as.character(x)) %>% 
    httr::GET() %>%
    httr::content('text', encoding = 'utf-8') %>% 
    fromJSON(simplifyDataFrame = TRUE) %>% 
    filter(row_number() != 1) %>% 
    as_tibble() %>% 
    mutate(ano = D1C,
           codmuni =D3C,
           nomemun = D3N,
           pop = as.numeric(V)) %>% 
    select (ano,codmuni,nomemun,pop)
  
}


year <-  c(2008, 2009, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

popall <- year %>% map_df(get_pop)

query <- 'http://api.sidra.ibge.gov.br/values/t/1552/p/2010/n6/all//v/93/c1/0/c2/0/c286/0/c287/0'
tmp <- query %>%
       httr::GET() %>%
       httr::content('text', encoding = 'utf-8') %>% 
       fromJSON(simplifyDataFrame = TRUE) %>% 
       filter(row_number() != 1) %>% 
       as_tibble() %>% 
       mutate(ano = D1C,
                           codmuni =D2C,
                           nomemun = D2N,
                           pop = as.numeric(V)) %>% 
       select (ano,codmuni,nomemun,pop)