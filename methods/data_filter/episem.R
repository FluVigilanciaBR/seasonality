# PROJETO ALERTA DENGUE -------------------------------------
# Funcoes auxiliadoras para formatacao dados de clima do Alerta dengue
# Claudia Codeco 2015
# -----------------------------------------------------------

# data2SE ---------------------------------------------------------------------
#'@description Find to which epidemiological week belongs a given day. Uses episem function 
#'(formula generated data).
#'@title Define Epidemiological Week
#'@param date string vector with dates to be converted
#'@param format date format
#'@return data.frame with the epidemiological weeks. 
#'@examples
#'data2SE("01-02-2020",format="%d-%m-%Y")
#'data2SE("12-02-2008",format="%d-%m-%Y")
#'data2SE(c("03-04-2013","07-01-2019"),format="%d-%m-%Y")

data2SE <- function(days, format = "%d/%m/%Y"){
  sem <- rep(NA,length(days))      
  days<-as.Date(as.character(days),format=format)
  for (i in 1:length(days)) {
    sem[i]<-episem(days[i])      
  }
  sem
}

# episem ---------------------------------------------------------------------
#' @description Find to which epidemiological week belongs a given day 
#' @author Oswaldo Cruz
#' @title Define Epidemiological Week
#' @param date date to be converted (class Date)
#' @param separa symbol between year and week
#' @param retorna What should be return, if epidemiological year and week ('YW'), epi. year only ('Y') or epi. week only ('W').
#'   Default: 'YW'.
#' @return epidemiological week or year. If separa = '', the output is numeric; otherwise is a character.
#' @examples
#' episem(x= as.Date("2018-12-31", format="%Y-%m-%d"))
#' episem(x= as.Date("2015-01-01", format="%Y-%m-%d"), separa='-')
#' episem(x= as.Date("2015-01-01", format="%Y-%m-%d"), retorna='Y')

episem <- function(x, format="%Y-%m-%d", separa='W', retorna='YW') {
  if (is.na(x) == T) {
    message("episem: Date not valid, returning NA")
    return(NA) 
  }
  # semana epi 1 de 2000 02/01/2000
  if (class(x)!= "Date") {
    x <- as.Date(split(x, ' ')[1], format = format)
    #warning("Precisa ser do tipo Date - Convertendo de texto")
  }
  ##  funcoes auxiliares - poderia usar a lubridate mas achei assim mais simples
  
  year  <- function(dt) {as.numeric(format(dt,"%Y"))}  ## retorna ano
  wday <- function(dt) {as.numeric(format(dt,"%w"))}   ## retorna dia sendo  0 = domingo a 6= sabado
  passado <- function(dt,diff=1) {as.Date(paste(as.numeric(format(dt,"%Y"))-diff,format(dt,"%m-%d"),sep="-"))} ## ano - x
  
  ## Inicio 
  
  ano <- year(x) # extrai ano
  dia1 <- as.Date(paste(ano,'01','01',sep='-'),format = "%Y-%m-%d") # primeiro do ano 
  
  diasem <- wday(dia1)  #descobre o dia da semana do dia1 
  fwd <- ifelse (diasem <=3, dia1 - diasem , dia1 + (7 - diasem) ) #se for menor ou igua a 3 (quarta) 
  fwd <- as.Date(fwd,origin = '1970-01-01') # reformata em data pois ela perde a formatacao 
  
  ## caso a data seja menor que a da 1o semana do ano (fwd)
  if (x < fwd) {
    dia1 <- passado(dia1)  # ano -1 
    diasem <- wday(dia1)  #dia da semana 
    fwd <- ifelse (diasem <=3, dia1 - diasem , dia1 + (7 - diasem) )
    fwd <- as.Date(fwd,origin = '1970-01-01')
  }
  
  diafim <- as.Date(paste(ano,'12','31',sep='-')) #Ultimo dia do ano
  diasem <- wday(diafim)                          #dia semana do ultimo dia
  
  ewd <- ifelse (diasem < 3, diafim - diasem - 1, diafim + 6 - diasem) 
  ewd <- as.Date(ewd,origin = '1970-01-01') # ultima semana epi do ano
  
  if (x > ewd) fwd <- ewd + 1 #caso a data (x) seja maior ou igual a ultiam semaan do ano
  epiweek <- floor(as.numeric(x - fwd) / 7 ) + 1 #numero de semanas e a diff da data e da primeira semana div por 7
  
  if(epiweek==0) epiweek <- 1 ## gatilho se for 0 vira semana 1
  epiyear <- year(fwd + 180) ## ano epidemiologico
  
  if (retorna=='YW'){
    out <- sprintf("%4d%s%02d",epiyear,separa,epiweek)  ## formata string com separador
  } else if (retorna=='Y') {
    out <- epiyear
  } else {
    out <- epiweek
  }
  
  if (separa =="") {
    return(as.numeric(out))
  } else {
    return(out)
  }
}



#' lastepiweek -----------------------------------
#' @description Calculate number of year's last epidemiological week using Brazilian standard.
#' @name lastepiweek
#' @author Marcelo F Gomes
#' @param ano Year
#' @keywords internal
#' @examples 
#' lastepiweek(2018)

lastepiweek <- function(ano){
  
  # Calcula o valor da última semana do ano
  
  diafim <- as.Date(paste(ano,'12','31',sep='-')) #Ultimo dia do ano
  diasem <- as.numeric(format(diafim,"%w"))       #dia semana do ultimo dia
  
  ewd <- ifelse (diasem < 3, diafim - diasem - 1, diafim + 6 - diasem) # Obtém a data do último sábado
  ewd <- as.Date(ewd,origin = '1970-01-01') # ultima semana epi do ano
  
  return(episem(ewd,retorna='W'))
}

# SE2date ---------------------------------------------------------------------
#'@description Return the first day of the Epidemiological Week
#'@title Return the first day of the Epidemiological Week
#'@param SE string vector with dates to be converted, format 201420
#'@return data.frame with SE and first day.
#'@examples
#'SE2date(se=201812)
#'SE2date(se = c(201401:201409))

SE2date <- function(se){
  if(!class(se[1]) %in% c("numeric","integer")) stop("se should be numeric or integer")
  
  #load("R/sysdata.rda")
  #SE$sem <- SE$Ano*100 + SE$SE
  res <- data.frame(SE = se, ini = as.Date("1970-01-01"))
  for (i in 1:length(res$SE)) res$ini[i] <- SE$Inicio[SE$SE == res$SE[i]]
  res
}


# seqSE ---------------------------------------------------------------------
#'@description Creates a sequence of epidemiological weeks and respective initial and final days
#'@title Sequence of epidemiological weeks
#'@param from first week in format 201401
#'@param to first week in format 201401
#'@return data.frame with the epidemiological weeks and corresponding extreme days. WARNING: only works from 2010 to 2019.
#'@examples
#'seqSE(201802, 201910)


seqSE <- function(from, to){
  #load("R/sysdata.rda")
  #SE$SE <- SE$Ano*100 + SE$SE
  N <- dim(SE)[1]
  
  if (from < SE$SE[1]){
    from <- SE$SE[1]
    #warning(paste("first SE set to", from))
  }
  
  if (to > SE$SE[N]){
    to <- SE$SE[N]
    warning(paste("This function only works from 2010 to 2019. Last SE set to", to))
  }
  
  SE[which(SE$SE==from):which(SE$SE==to),]
}
