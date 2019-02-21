library(ggplot2)
require(ggthemes)

Bandt.Pompe <- function(elements, dimension, elementsize){
  dyn.load("BandtPompe.so")
  probability <- .Call("BandtPompe", elements, dimension, elementsize)
  return (probability)
}

shannonEntropy <- function(p){
  h <- p * log(p)
  h[is.nan(h)] <- 0
  return(-sum(h))
}

shannonNormalized <- function(p){
  return(shannonEntropy(p)/log(length(p)))
}

jensenDivergence<-function(p){
  cc = rep(1/length(p),length(p))
  s_p = shannonEntropy(p)
  s_q = shannonEntropy(cc)
  s_pq = shannonEntropy((p+cc)/2)
  divergence = sum(s_pq - (s_p/2) - (s_q/2))
  return(divergence)
}

constant <- function(p){
  k = (0.5)/length(p)
  a1 = (0.5 + k) * log(0.5 + k)
  a2 = (length(p) - 1) * k * log(k)
  a3 = (1 - 0.5) * log(length(p))
  b = -1/(a1 + a2 + a3)
  return(b)
}

Ccomplexity<-function(p){
  cc <- jensenDivergence(p) * constant(p) * shannonNormalized(p)
  return(cc)
}

readingMPR<-function(dimension,option=0){
  if(dimension == 3){ 
    continua = "continuaN6.txt"
    trozo = "trozosN6.txt"
  }
  if(dimension == 4){ 
    continua = "continuaN24.txt"
    trozo = "trozosN24.txt"
  }
  if(dimension == 5){ 
    continua = "continuaN120.txt"
    trozo = "trozosN120.txt"
  }
  if(dimension == 6){ 
    continua = "continuaN720.txt"
    trozo = "trozosN720.txt"
  }
  curva1x = Readtxt2(continua,1) 
  if(option==1) return(curva1x)
  curva1y = Readtxt2(continua,2)
  if(option==2) return(curva1y)
  curva2x = Readtxt2(trozo,1)
  if(option==3) return(curva2x)
  curva2y = Readtxt2(trozo,2)
  if(option==4) return(curva2y)
}

HCPlane <- function(probabilities, ns, dimension){
  
  Complexity <- Entropy <- rep(0,ns)
  fact <- factorial(dimension)
  shape.select <- c(17,18,19)
  
  # Paleta montada a partir de https://coolors.co/
  rainbow.colors <- palette(c("#494947", #DarkGreen
                              "#7494EA", #MutedDarkBlue
                              "#B14AED", #Violet
                              "#44CCFF", #BrightLightBlue
                              "#35FF69", #BrightGreen
                              "#ED8438", #Orange
                              "#E7AD99", #Pink
                              "#C18C5D", #LightBrown
                              "#BF6F00", #DarkYellow
                              "#FB4D3D", #BrightRed
                              "#495867")) #DarkGray
  
  for(i in c(1:ns)){
    Entropy[i] = shannonNormalized(probabilities[i,1:fact])
    Complexity[i] = Ccomplexity(probabilities[i,1:fact])
  }
  
  Entropy.Complexity <- data.frame(Entropy, Complexity, shape.colors = probabilities[,fact+1])
  
  c1x = readingMPR(dimension,1)
  c1y = readingMPR(dimension,2)
  c2x = readingMPR(dimension,3)
  c2y = readingMPR(dimension,4)
  
  p = qplot(x=c2x,y=c2y,geom="line",xlab="Shannon Entropy",ylab="MPR Statistical Complexity") +
    ggtitle("Entropy-Complexity Plane") + theme(plot.title = element_text(hjust=0.5)) +
    geom_line(aes(x=c1x,c1y)) + 
    geom_point(aes(x = Entropy.Complexity$Entropy,y = Entropy.Complexity$Complexity),
               shape = shape.select[Entropy.Complexity$shape.colors], size = 3, color = rainbow.colors[Entropy.Complexity$shape.colors]) 
  print(p)
}

Readtxt2<-function(name,column){
  data = read.table(name, stringsAsFactors=FALSE, fileEncoding="latin1")
  data = data[,column]
  if(mode(data)=="character"){
    data = type.convert(data)
  }
  data = na.omit(data)
  return(data)
}
