require(tidyverse)

calc.transmission.thresholds <- function(x){
  lvl <- case_when(x < 0.5 ~ 0,
                   x < 1 ~ 1,
                   x < 5 ~ 2,
                   x < 10 ~ 3,
                   x >= 10 ~ 4
  )
  return(lvl)
}
