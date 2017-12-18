anaerob <- read.table("../data/anaerob.txt" , header=TRUE)
attach(anaerob)
plot(Oxygen,Ventilation)

anaerobMODEL2 <- lm( formula=Ventilation~1+Oxygen+I(Oxygen^2) , x=TRUE)
osX <- seq(min(Oxygen) , max(Oxygen) , by=100)
lines(osX , anaerobMODEL2$coeff[1] + anaerobMODEL2$coeff[2]*osX + anaerobMODEL2$coeff[3]*osX^2)

summary(anaerobMODEL2)

# test, ci Beta0 = 24:
X <- anaerobMODEL2$x
betaHAT <- anaerobMODEL2$coeff
n <- length(Ventilation)

SSe <- sum( (Ventilation - X%*%betaHAT)^2 )
S <- sqrt( SSe/(n-3) )
# rychlejsie:
S <- summary(anaerobMODEL2)$sigma

a <- c(1,0,0)

# testovacia statistika:
T <- (t(a)%*%betaHAT - 24 ) / ( S*sqrt( t(a)%*%solve(t(X)%*%X)%*%a ) )
# p-hodnota:
2*( 1-pt(T,df=n-3) )



# PORCELANOVE DOSTICKY

porcelan <- read.table("../data/porcelan1.txt" , header=TRUE)
attach(porcelan)

porcelanMODEL <- lm(napatie~1+teplota1+teplota2 , x=TRUE)

#koeficient determinacie:
SSe <- sum( (napatie - porcelanMODEL$x%*%porcelanMODEL$coeff)^2 )
ubohyMODEL <- lm(napatie~1 , x=TRUE)
SSt <- sum( (napatie - ubohyMODEL$x %*% ubohyMODEL$coeff)^2 )

1-SSe/SSt
summary(porcelanMODEL)
 # frajerskejsi vypocet:
SSe <- ( (summary(porcelanMODEL)$sigma )^2 )*(12-3)
SSt <- ( (summary(ubohyMODEL)$sigma )^2 )*(12-1)
1-SSe/SSt
 #este frajerskejsi:
summary(porcelanMODEL)$r.squared

#__________________________________________
#ktory sok ma vacsi vplyv na napatie?
X <- porcelanMODEL$x
betaHAT <- porcelanMODEL$coeff
n <- 12
SSe <- sum( (napatie - porcelanMODEL$x%*%porcelanMODEL$coeff)^2 )
S <- sqrt( SSe/(n-3) )
a <- c(0,1,-1)

# testovacia statistika:
T <- (t(a)%*%betaHAT - 0) / ( S*sqrt( t(a)%*%solve(t(X)%*%X)%*%a ) )
# p-hodnota:
1-pt(T,df=n-3)

#___________________________
#test vyznamnosti regresie:
Fstatistika <- ( (SSt-SSe)/2 )  /  (SSe/(12-3))
#p-hodnota:
1-pf(Fstatistika,df1=2,df2=12-3)
summary(porcelanMODEL)


#model zavislosti od casov:
porcelan2 <- read.table("../data/porcelan2.txt" , header=TRUE)
attach(porcelan2)

porcelanMODEL2 <- lm(formula=NOVEnapatie~1+cas1+cas2)
#test vyznamnosti regresie:
summary( porcelanMODEL2 )

