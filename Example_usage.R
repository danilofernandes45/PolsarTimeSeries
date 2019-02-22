source("SARTimeSerie.R")
source("Band&Pompe.R")
require(grid)
require(gridExtra)

ns <- 8
dim <- matrix(nrow = ns, ncol = 4)

#Forest regions in Guatemala
dim[1,] <- c(5600, 200, 2700, 200) #region 1
dim[2,] <- c(5200, 200, 2800, 200) #region 2
dim[3,] <- c(4100, 200, 2930, 200) #region 3
dim[4,] <- c(1075, 200, 1930, 200) #region 4

#Crop regions in Guatemala
dim[5,] <- c(400, 200, 1500, 200) #region 5

#Ground regions in Guatemala
#---------------------------------------------------
#Possibilly you will have problems with this regions
#It isn't uniform
dim[6,] <- c(250, 200, 1100, 200) #region 6
dim[7,] <- c(250, 200, 1850, 200) #region 7
dim[8,] <- c(1675, 200, 1930, 200) #region 8

#Get Time Serie from Guatemala SAR data

#Dimension 2
#--------------------------------------
n <- 2
tal <- 1
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p21 <- HCPlane(probability, ns, n)
p21 <- p21 + labs(x = "n2t1", y = "")

#--------------------------------------
n <- 2
tal <- 2
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p22 <- HCPlane(probability, ns, n)
p22 <- p22 + labs(x = "n2t2", y = "")

#--------------------------------------
n <- 2
tal <- 3
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p23 <- HCPlane(probability, ns, n)
p23 <- p23 + labs(x = "n2t3", y = "")

#--------------------------------------
n <- 2
tal <- 4
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p24 <- HCPlane(probability, ns, n)
p24 <- p24 + labs(x = "n2t4", y = "")

#--------------------------------------
n <- 2
tal <- 5
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p25 <- HCPlane(probability, ns, n)
p25 <- p25 + labs(x = "n2t5", y = "")

#Dimension 3
#--------------------------------------
n <- 3
tal <- 1
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p31 <- HCPlane(probability, ns, n)
p31 <- p31 + labs(x = "n3t1", y = "")

#--------------------------------------
n <- 3
tal <- 2
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p32 <- HCPlane(probability, ns, n)
p32 <- p32 + labs(x = "n3t2", y = "")

#--------------------------------------
n <- 3
tal <- 3
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p33 <- HCPlane(probability, ns, n)
p33 <- p33 + labs(x = "n3t3", y = "")

#--------------------------------------
n <- 3
tal <- 4
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p34 <- HCPlane(probability, ns, n)
p34 <- p34 + labs(x = "n3t4", y = "")

#--------------------------------------
n <- 3
tal <- 5
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p35 <- HCPlane(probability, ns, n)
p35 <- p35 + labs(x = "n3t5", y = "")


#Dimension 4
#--------------------------------------
n <- 4
tal <- 1
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p41 <- HCPlane(probability, ns, n)
p41 <- p41 + labs(x = "n4t1", y = "")

#--------------------------------------
n <- 4
tal <- 2
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p42 <- HCPlane(probability, ns, n)
p42 <- p42 + labs(x = "n4t2", y = "")

#--------------------------------------
n <- 4
tal <- 3
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p43 <- HCPlane(probability, ns, n)
p43 <- p43 + labs(x = "n4t3", y = "")

#--------------------------------------
n <- 4
tal <- 4
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p44 <- HCPlane(probability, ns, n)
p44 <- p44 + labs(x = "n4t4", y = "")

#--------------------------------------
n <- 4
tal <- 5
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p45 <- HCPlane(probability, ns, n)
p45 <- p45 + labs(x = "n4t5", y = "")

#Dimension 5
#--------------------------------------
n <- 5
tal <- 1
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p51 <- HCPlane(probability, ns, n)
p51 <- p51 + labs(x = "n5t1", y = "")

#--------------------------------------
n <- 5
tal <- 2
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p52 <- HCPlane(probability, ns, n)
p52 <- p52 + labs(x = "n5t2", y = "")

#--------------------------------------
n <- 5
tal <- 3
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p53 <- HCPlane(probability, ns, n)
p53 <- p53 + labs(x = "n5t3", y = "")

#--------------------------------------
n <- 5
tal <- 4
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p54 <- HCPlane(probability, ns, n)
p54 <- p54 + labs(x = "n5t4", y = "")

#--------------------------------------
n <- 5
tal <- 5
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p55 <- HCPlane(probability, ns, n)
p55 <- p55 + labs(x = "n5t5", y = "")

#Dimension 6
#--------------------------------------
n <- 6
tal <- 1
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p61 <- HCPlane(probability, ns, n)
p61 <- p61 + labs(x = "n6t1", y = "")

#--------------------------------------
n <- 6
tal <- 2
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p62 <- HCPlane(probability, ns, n)
p62 <- p62 + labs(x = "n6t2", y = "")

#--------------------------------------
n <- 6
tal <- 3
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p63 <- HCPlane(probability, ns, n)
p63 <- p63 + labs(x = "n6t3", y = "")

#--------------------------------------
n <- 6
tal <- 4
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p64 <- HCPlane(probability, ns, n)
p64<- p64 + labs(x = "n6t4", y = "")

#--------------------------------------
n <- 6
tal <- 5
xy <- get.number.patterns(dim[1,2], dim[1,4], n , tal)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)
my.elements <- matrix(nrow = xy, ncol = n*n) 

for(i in c(1:ns)){
  get.serie <- array(0, dim = c(xy, n, n))
  get.serie <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  for(j in c(1:(xy))){
    my.elements[j,] <- as.vector(t(get.serie[j,,]))
  }
  probability[i, 1:factorial(n)] <- Bandt.Pompe(as.vector(t(my.elements)), n, xy)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

p65 <- HCPlane(probability, ns, n)
p65 <- p65 + labs(x = "n6t5", y = "")

#----------------------------------------------------------------

grid.newpage()
pushViewport(viewport(layout = grid.layout(5, 5)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

print(p21, vp = vplayout(1, 1))
print(p22, vp = vplayout(1, 2)) 
print(p23, vp = vplayout(1, 3)) 
print(p24, vp = vplayout(1, 4))
print(p25, vp = vplayout(1, 5))
print(p31, vp = vplayout(2, 1)) 
print(p32, vp = vplayout(2, 2)) 
print(p33, vp = vplayout(2, 3)) 
print(p34, vp = vplayout(2, 4)) 
print(p35, vp = vplayout(2, 5)) 
print(p41, vp = vplayout(3, 1))
print(p42, vp = vplayout(3, 2))
print(p43, vp = vplayout(3, 3))
print(p44, vp = vplayout(3, 4))
print(p45, vp = vplayout(3, 5))
print(p51, vp = vplayout(4, 1))
print(p52, vp = vplayout(4, 2))
print(p53, vp = vplayout(4, 3))
print(p54, vp = vplayout(4, 4))
print(p55, vp = vplayout(4, 5))
print(p61, vp = vplayout(5, 1))
print(p62, vp = vplayout(5, 2))
print(p63, vp = vplayout(5, 3))
print(p64, vp = vplayout(5, 4))
print(p65, vp = vplayout(5, 5))

