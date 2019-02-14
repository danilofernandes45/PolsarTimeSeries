
#Forest regions in Guatemala
dim <- c(5600, 200, 2700, 200) #region 1
dim <- c(5200, 200, 2800, 200) #region 2
dim <- c(4100, 200, 2930, 200) #region 3
dim <- c(1075, 200, 1930, 200) #region 4

#Crop regions in Guatemala
dim <- c(400, 200, 1500, 200) #region 5

#Ground regions in Guatemala
#---------------------------------------------------
#Possibilly you will have problems with this regions
#It isn't uniform
dim <- c(250, 200, 1100, 200) #region 6
dim <- c(250, 200, 1850, 200) #region 7
dim <- c(1675, 200, 1930, 200) #region 8

#Get Time Serie from Guatemala SAR data
#--------------------------------------
#Bandit and Pompe parameters
n <- 3
tal <- 2
timeSerie <- getTimeSerie("guatemala", dim, n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
