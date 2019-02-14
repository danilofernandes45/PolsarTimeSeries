library(raster)

getTimeSerie <- function(file, dim, n, tal){
  sar_data <- raster(paste(file, ".grd", sep = ""))
  block <- getValuesBlock(sar_data, row = dim[1], nrows = dim[2], col = dim[3], ncols = dim[4], format = "matrix")
  
  nrows <- dim[2]
  ncols <- dim[4]
  
  #Empirical numbers
  x <- 1 + (nrows - n) %/% (n - tal)
  y <- 1 + (ncols - n) %/% (n - tal)
  
  serie <- array(0, dim = c(x*y, n, n))
  
  init_col <- 1
  init_row <- 1
  max_row <- n
  max_col <- n
  
  count <- 1
  
  while(max_row <= nrows){
    
    while(max_col <= ncols){
      
      serie[count, , ] <- block[init_row:max_row, init_col:max_col]
      init_col <- max_col - (tal - 1)
      max_col <- init_col + (n - 1)
      count = count + 1
      
    }
    
    init_col <- 1
    max_col <- n
    
    init_row <- max_row - (tal - 1)
    max_row <- init_row + (n - 1)
    
  }
  
  return(serie)
  
}