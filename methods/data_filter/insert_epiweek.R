#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
source('episem.R')

for(fin in args){
  print(fin)
  
  df <- read.csv(fin, encoding='utf-8')
  target_cols <- c('DT_NOTIFIC', 'DT_DIGITA')
  for(col in target_cols){
    epicol <- paste0(col, '_epiyearweek')
    df[,epicol] <- sapply(df[,col], episem)
    epiyear_col <- paste0(col, '_epiyear')
    df[,epiyear_col] <- sapply(strsplit(df[,epicol],'W'), '[', 1)
    epiweek_col <- paste0(col, '_epiweek')
    df[,epiweek_col] <- sapply(strsplit(df[,epicol],'W'), '[', 2)
  }
  
  fout <- gsub('.{4}$', '', tail(strsplit(fin, '/')[[1]], 1))
  fout <- paste0('../clean_data/', fout, '_epiweek.csv')
  write.csv(df, file=fout, row.names=FALSE, na='', encoding='utf-8')
}
