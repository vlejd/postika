#TESTOVANIE HYPOTEZY O SUBMODELI:
newyorsky.vzduch <- read.table(file="../data/newyorsky_vzduch.txt" , header=TRUE )
attach(newyorsky.vzduch)

#model:
vzduchMODEL <- lm(formula=ozone~1+radiation+temperature+wind , x=TRUE)
vzduchMODEL

#submodel:
vzduchSUBMODEL <- lm(formula=ozone ~ 0+temperature+wind , x=TRUE)
vzduchSUBMODEL
SSeMODEL <- sum( (ozone - vzduchMODEL$x%*%vzduchMODEL$coeff)^2 )
SSeSUBMODEL <- sum( (ozone - vzduchSUBMODEL$x%*%vzduchSUBMODEL$coeff)^2 )
Fstatistika <- ( (SSeSUBMODEL-SSeMODEL)/2 ) / ( SSeMODEL/(111-4) )
#p-hodnota:
1-pf(Fstatistika , df1=2 , df2=111-4)

#REGRESNA DIAGNOSTIKA:
plot(vzduchMODEL, which=1)

#____________________
#skarede rezidua:

anaerob <- read.table("e:\\anaerob.txt" , header=TRUE)
attach(anaerob)
plot(Oxygen,Ventilation)
anaerobMODEL <- lm(formula=Ventilation~1+Oxygen , x=TRUE)
abline(anaerobMODEL$coeff)

plot(anaerobMODEL, which=1)
#____________________


plot(vzduchMODEL, which=3)
plot(vzduchMODEL, which=4)
plot(vzduchMODEL, which=2)
rezidua  <-  ozone - vzduchMODEL$x %*% vzduchMODEL$coeff
ks.test(rezidua,
        "pnorm",
        mean=mean(rezidua),
        sd=sqrt(var(rezidua)) )


#test ROVNOBEZNOSTI regresnych priamok:

mean(wind)
ozone1 <- ozone[wind<mean(wind)]
ozone2 <- ozone[wind>=mean(wind)]
n1 <- length(ozone1)
n2 <- length(ozone2)
temperature1 <- temperature[wind<mean(wind)]
temperature2 <- temperature[wind>mean(wind)]
vzduchMODEL1 <- lm(formula=ozone1 ~ 1+temperature1, x=TRUE)
vzduchMODEL2 <- lm(formula=ozone2 ~ 1+temperature2, x=TRUE)

plot(temperature1,ozone1,pch=1)
points(temperature2,ozone2,pch="*")
abline(vzduchMODEL1$coeff,col="red")
abline(vzduchMODEL2$coeff,col="blue")

stlpec1 <- c( rep(1,times=n1) , rep(0,times=n2) )
stlpec2 <- c( rep(0,times=n1) , rep(1,times=n2) )
stlpec3 <- c( temperature1 , rep(0,times=n2) )
stlpec4 <- c( rep(0,times=n1) , temperature2 )
Y <- c( ozone1 , ozone2 )
X <- cbind(stlpec1,stlpec2,stlpec3,stlpec4)

ZLOZENYmodel <- lm(formula=Y~0+X, x=TRUE)
ZLOZENYmodel

gamaHAT <- ZLOZENYmodel$coeff
SSe <- sum( (Y - X%*%gamaHAT)^2 )
S <- sqrt( SSe/(111-4) )
a <- c(0,0,1,-1)
# testovacia statistika:
T <- (t(a)%*%gamaHAT - 0) / (S*sqrt( t(a)%*%solve(t(X)%*%X)%*%a ) )
# p-hodnota:
1-pt(T,df=111-4)


#test TOTOZNOSTI regresnych priamok:

#submodel:
SUBMODEL <- lm(formula=ozone~1+temperature,x=TRUE)
SSeZLOZENYMODEL <- sum( (Y - X%*%ZLOZENYmodel$coeff)^2 )
SSeSUBMODEL <- sum( (ozone - SUBMODEL$x%*%SUBMODEL$coeff)^2 )
Fstatistika <- ( (SSeSUBMODEL-SSeZLOZENYMODEL)/2 ) / ( SSeZLOZENYMODEL/(111-4) )
#p-hodnota:
1-pf(Fstatistika , df1=2 , df2=111-4)

