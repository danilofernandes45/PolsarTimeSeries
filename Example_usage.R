source("SARTimeSerie.R")
source("Band&Pompe.R")

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
#--------------------------------------
#Bandit and Pompe parameters - Test 1
n <- 3
tal <- 1
timeSerie <- matrix(nrow = ns, ncol = 352836)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)

for(i in c(1:ns)){
  timeSerie[i,] <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  probability[i,1:factorial(n)] <- Bandt.Pompe(timeSerie[i,], n, tal)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

png("Dimension3Tal1.png")
HCPlane(probability, ns, n)
dev.off()

#--------------------------------------
#Bandit and Pompe parameters - Test 2
n <- 6
tal <- 7
timeSerie <- matrix(nrow = ns, ncol = 352836)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)

for(i in c(1:ns)){
  timeSerie[i,] <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  probability[i,1:factorial(n)] <- Bandt.Pompe(timeSerie[i,], n, tal)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

png("Dimension3Tal2.png")
HCPlane(probability, ns, n)
dev.off()
