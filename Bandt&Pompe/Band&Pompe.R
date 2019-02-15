Bandt.Pompe <- function(series, dimension, delay){
  
  dyn.load("BandtPompe.so")
  
  probability <- .Call("BandtPompe", series, dimension, delay)
  
  return (probability)
}
