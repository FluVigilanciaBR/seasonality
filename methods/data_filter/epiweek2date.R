#' Obtain Saturday of corresponding epiweek
#' 
#' @param y Epidemiological year
#' @param w Epidemiological week
#' 
#' Returns
#' date
#'
#' @keyword external
epiweek2date <- function(y, w){
  day1 <- as.Date(paste(y,'01','01',sep='-')) # 
  
  weekday1 <- as.numeric(format(day1,"%w")) # Weekday of day1
  
  ## Sunday of epiweek 01 of year y ##
  fwd <- ifelse (weekday1 <=3, day1 - weekday1 , day1 + (7 - weekday1) )
  fwd <- as.Date(fwd,origin = '1970-01-01') # reformata em data pois ela perde a formatacao 
  
  sat <- fwd + w*7 - 1
  
  return(as.Date(sat, origin='1970-01-01'))
}