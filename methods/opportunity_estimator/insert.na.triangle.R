insert.na.triangle <- function(tbl){
  # Table dimensions:
  last.row <- nrow(tbl)
  last.col <- ncol(tbl)
  
  tbl.obs <- tbl
  
  # Creating the run-off triangle data frame
  tbl.obs[outer(1:last.row, 0:(last.col-1), FUN = "+") > last.row] <- NA
  return(tbl.obs)
}