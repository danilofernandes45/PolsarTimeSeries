library(raster)

getTimeSerie <- function(file, dim, n, tal){
  sar_data <- raster(paste(file, ".grd", sep = ""))
  block <- getValuesBlock(sar_data, row = dim[1], nrows = dim[2], col = dim[3], ncols = dim[4], format = "matrix")
  
  nrows <- dim[2]
  ncols <- dim[4]
  x <- 0
  y <- 0
  #Empirical numbers
  if(n > tal){
    x <- 1 + (nrows - n) %/% (n - tal)
    y <- 1 + (ncols - n) %/% (n - tal)
  } else {
    x <- nrows %/% tal
    if(nrows %% tal >= n){
      x <- x+1
    }
    y <- ncols %/% tal
    if(ncols %% tal >= n){
      y <- y + 1
    }
  }
  
  serie <- array(0, dim = c(x*y, n, n))
  
  init_col <- 1
  init_row <- 1
  max_row <- n
  max_col <- n
  
  count <- 1
  
  while(max_row <= nrows){
    
    while(max_col <= ncols){
      
      serie[count, , ] <- block[init_row:max_row, init_col:max_col]
      
      if(n > tal){
        init_col <- max_col - (tal - 1)
      } else {
        init_col <- max_col + (tal - n + 1)
      }
      max_col <- init_col + (n - 1)
      count = count + 1
      
    }
    
    init_col <- 1
    max_col <- n
    
    if(n > tal){
      init_row <- max_row - (tal - 1)
    } else {
      init_row <- max_row + (tal - n + 1)
    }
    
    max_row <- init_row + (n - 1)
    
  }
  
  return(serie)
  
}
