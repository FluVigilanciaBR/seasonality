#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
source('episem.R')

for(fin in args){
  print(fin)
  
  df <- read.csv(fin)
  target_cols <- c('DT_NOTIFIC', 'DT_SIN_PRI', 'DT_DIGITA', 'DT_INTERNA')
  for(col in target_cols){
    epicol <- paste0(col, '_epiyearweek')
    df[epicol] <- ''
    df[df[col]!='', epicol] <- sapply(df[df[col]!='', col], episem, 'W')
    epiyear_col <- paste0(col, '_epiyear')
    df[df[col]!='', epiyear_col] <- sapply(strsplit(df[df[col]!='', epicol],'W'), '[', 1)
    epiweek_col <- paste0(col, '_epiweek')
    df[df[col]!='', epiweek_col] <- sapply(strsplit(df[df[col]!='', epicol],'W'), '[', 2)
  }
  
  fout <- gsub('.{4}$', '', tail(strsplit(fin, '/')[[1]], 1))
  fout <- paste0(fout, '_epiweek.csv')
  write.csv(df, file=fout, row.names=FALSE, na='')
}
