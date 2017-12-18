# AKO ZISTIT, ZE DATA NIE SU Z NORMALNEHO ROZDELENIA?

# POZITIVNY SKLON:

sikme.data1 <- scan("e:\\sikme_data1.txt")

hist(sikme.data1,nclass=15)
#vyberovy koeficient SIKMOSTI	:
skewness(sikme.data1)
# najprv treba balik "moments":
library(moments)
skewness(sikme.data1)

#pozitivny sklon pomocou kvartilov:
quantile(sikme.data1,probs=.75) - median(sikme.data1)
median(sikme.data1) - quantile(sikme.data1,probs=.25)

# pozitivny sklon pomocou kvantiloveho diagramu:
 qqnorm(sikme.data1)
# je to to iste ako:
n<-length(sikme.data1)
plot( qnorm( (1:n)/(n+1) )   ,   sort(sikme.data1) )

#z akeho rozdelenia boli sikme.data1?
osX <- seq(0,12,by=.1)
plot(osX,dchisq(osX,df=5),
     type="l",
     main="Pozitivny sklon: chikvadrat_5")

# ako vznikli sikme.data1?
set.seed(8)
sikme.data1 <- rchisq(1000,df=5)



# NEGATIVNY SKLON:

sikme.data2 <- scan("e:\\sikme_data2.txt")

hist(sikme.data2,nclass=15)
# vyberovy koeficient SIKMOSTI:
skewness(sikme.data2)

# negativny sklon pomocou kvartilov:
quantile(sikme.data2,probs=.75) - median(sikme.data2)
median(sikme.data2) - quantile(sikme.data2,probs=.25)

# negativny sklon pomocou kvantiloveho diagramu:
qqnorm(sikme.data2)

# ako vznikli sikme.data2?
set.seed(6)
sikme.data2 <- 12-rchisq(1000,df=5)


# INE PRIZNAKY NENORMALNOSTI DAT:

symetricke1 <- scan(file="e:\\symetricke_data1.txt")
symetricke2 <- scan(file="e:\\symetricke_data2.txt")
symetricke3 <- scan(file="e:\\symetricke_data3.txt")

hist(symetricke1,nclass=20)
hist(symetricke2,nclass=20)
hist(symetricke3,nclass=20)

qqnorm(symetricke1)

qqnorm(symetricke2)
qqline(symetricke2)

qqnorm(symetricke3)

# porovnanie 2 rozdeleni:
qqplot(symetricke3,symetricke2)


# vyberovy koeficient SPICATOSTI:
kurtosis(symetricke1)
kurtosis(symetricke2)
kurtosis(symetricke3)


osX <- seq(-6,6,by=.1)
plot(osX,dnorm(osX),
     type="l",
     ylim=c(0,.8),
     col="red",
     xlab="x",
     ylab="3 rozne hustoty")
lines(osX,dunif(osX,min=-2,max=2),
      col="green")
lines(osX,dlogis(osX,scale=1/3),
      col="blue")
title(main="Rozne spicatosti")
legend(x=-6,y=.7,
       legend=c("Normalne rozdelenie","Logisticke rozdelenie","Rovnomerne rozdelenie"),
       lty=c(1,1,1),
       col=c("red","blue","green") )



# skuska na realnych datach:

loteria1 <- scan(file="e:\\loteria1.txt" , comment.char = "#")

hist(loteria1,nclass=15)

# 3.kvartil od medianu:
quantile(loteria1,probs=.75) - median(loteria1)
# 1.kvartil od medianu:
median(loteria1) - quantile(loteria1,probs=.25)

#nakreslenie kvartilov a medianu:
points( x=quantile( loteria1 , probs=c(.25,.5,.75) ),
        y=c(0,0,0),
        col="red",
        pch=7 )

qqnorm(loteria1)
skewness(loteria1)

# hustota do histogramu:
hist(loteria1,
     nclass=15,
     col="springgreen",
     probability=TRUE )
osX <- seq( min(loteria1) , max(loteria1) , by=1)
lines(osX , dnorm(osX , mean=mean(loteria1) , sd=sqrt(var(loteria1))  ) )
density(loteria1)
density(loteria1)$x
density(loteria1)$y
lines( density(loteria1) ,col="red")

# idealny histogram s hustotou:
hist(symetricke1,
     nclass=40,
     col="springgreen",
     probability=TRUE )
osX <- seq( min(symetricke1) , max(symetricke1) , by=.01)
lines(osX , dnorm(osX , mean=mean(symetricke1) , sd=sqrt(var(symetricke1))  ) )




###########################################################################################################
# PRIKLAD 3D-GRAFU:

require(graphics)
x <- seq(-10, 10, length = 50)
y <- x
rotsinc <- function(x, y) {
  sinc <- function(x) {
            y <- sin(x)/x
            y[is.na(y)] <- 1
            y
           }
  10 * sinc(sqrt(x^2 + y^2))
}
sinc.exp <- expression(z == Sinc(sqrt(x^2 + y^2)))
z <- outer(x, y, rotsinc)
par(bg = "white")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, 
    col = "lightblue")
title(sub = ".")
title(main = sinc.exp)


# SVETLO SVIETI Z INEJ STRANY:

persp(x, y, z, theta = 30, phi = 30, expand = 0.5, 
    col = "lightblue", ltheta = 120, shade = 0.75, ticktype = "detailed", 
    xlab = "X", ylab = "Y", zlab = "Z")
title(sub = ".")
title(main = sinc.exp)


# SIETOVY MODEL SOPKY:

library(datasets)
z <- 2 * volcano
x <- 10 * (1:nrow(z))
y <- 10 * (1:ncol(z))
persp(x, y, z, theta = 120, phi = 15, scale = FALSE, 
    axes = FALSE)
title(main = "Maunga Whau\nOne of 50 Volcanoes in the Auckland Region.", 
    font.main = 4)


# ZELENA SOPKA:

z0 <- min(z) - 20
z <- rbind(z0, cbind(z0, z, z0), z0)
x <- c(min(x) - 1e-10, x, max(x) + 1e-10)
y <- c(min(y) - 1e-10, y, max(y) + 1e-10)
fill <- matrix("green3", nr = nrow(z) - 1, nc = ncol(z) - 1)
fill[, i2 <- c(1, ncol(fill))] <- "gray"
fill[i1 <- c(1, nrow(fill)), ] <- "gray"
par(bg = "lightblue")
persp(x, y, z, theta = 120, phi = 15, col = fill, 
    scale = FALSE, axes = FALSE)
title(main = "Maunga Whau\nOne of 50 Volcanoes in the Auckland Region.", 
    font.main = 4)


#POHLAD NA SOPKU Z VYSKY:

persp(x, y, z, theta = 135, phi = 30, col = fill, 
    scale = FALSE, ltheta = -120, lphi = 15, shade = 0.65, axes = FALSE)


#SOPKA BEZ SIETE:

persp(x, y, z, theta = 135, phi = 30, col = "green3", 
    scale = FALSE, ltheta = -120, shade = 0.75, border = NA, 
    box = FALSE)


#PLASTICKA MAPA:
 
nrz <- nrow(z)
ncz <- ncol(z)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("green", "brown") ) 
# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)

persp(x, y, z, theta = 135, phi = 30, col = color[facetcol], 
    scale = FALSE, ltheta = -120, shade = 0.3, border = NA, box = FALSE)