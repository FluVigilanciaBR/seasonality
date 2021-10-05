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
#' episem(x= as.Date("2015-01-01", format="%Y-%m-%d"))
#' episem(x= as.Date("2015-01-01", format="%Y-%m-%d"), separa='-')
#' episem(x= as.Date("2015-01-01", format="%Y-%m-%d"), retorna='Y')

episem <- function(x, format="%Y-%m-%d", separa='', retorna='YW') {
  # semana epi 1 de 2000 02/01/2000
  if (class(x)!= "Date") {
    x <- as.Date(x, format = format)
    #warning("Precisa ser do tipo Date - Convertendo de texto")
  }
  if (is.na(x) == T) {
    message("episem: Date not valid, returning NA")
    return(NA) 
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
