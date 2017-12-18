# CHYBAJUCE HODNOTY:

kraby <- read.table("../data/kraby.txt" , header=TRUE , dec=",")
attach(kraby)

is.na(RW)
sum( is.na(RW) )
ocisteneRW <- RW[ !is.na(RW) ]
#iny postup:
na.omit(kraby)
# kraby <- na.omit(kraby)
#iny postup:
mean(RW)
RW[ is.na(RW) ] <- round( mean(RW, na.rm=TRUE) , digits=1)
kraby$RW <- RW

# vymazanie pamate:
# 1)
remove( list=objects() )
# 2) kopie stlpcov tabuliek vzniknute z "attach":
detach()

#pouzitie funkcie na tabulku:
apply( kraby[,4:8] , FUN=mean , MARGIN=2)

# zdruzovanie dat do skupin:
table( kraby[,1:2] )
aggregate(  kraby[,4:8]  ,  by = list(sp,sex) , FUN=mean)

#kolko krabov ma pancier sirsi nez 30?
pocitadlo <- 0
for (i in 1:200 )
{
if (CW[i]>30) pocitadlo <- pocitadlo + 1
}
pocitadlo
#takto sa to urobi rychlo:
length( CW[CW>30] )


#STATISTIKY POLOHY:

min(CW)
max(CW)
range(CW)
median(CW)
mean(CW)

quantile(CW, probs=1/4 )
quantile(CW, probs=2/4 )
quantile(CW, probs=3/4 )
quantile(CW, probs=c(0,1/4,2/4,3/4,1) )

#STATISTIKY VARIABILITY:

#vyberova disperzia:
var(CW)
#rucny vypocet:
sum(    (CW-mean(CW))^2    ) / (length(CW)-1)

#medzikvartilove rozpatie:
IQR <- quantile(CW, probs=3/4 ) - quantile(CW, probs=1/4 )

#(vnutorne) hradby:
dolna <- quantile(CW, probs=1/4 ) - 1.5*IQR
horna <- quantile(CW, probs=3/4 ) + 1.5*IQR
Outliers <- CW[CW<dolna | CW>horna]
Not.Outliers <- CW[CW>=dolna & CW<=horna]


# O B R A Z K Y :

loteria1 <- scan(file="../data/loteria1.txt" , comment.char = "#")
loteria2 <- scan(file="../data/loteria2.txt" , comment.char = "#")
loteria3 <- scan(file="../data/loteria3.txt" , comment.char = "#")

# STLPCOVY DIAGRAM:
barplot(loteria1)

# BOXPLOT:

boxplot(loteria1)
boxplot(loteria1 , notch=TRUE)
boxplot(loteria1 , plot=FALSE)
hranice.intervalu <- boxplot(loteria1 , plot=FALSE)$conf

boxplot(loteria1,loteria2[100:150],loteria3 ,
        notch=TRUE ,
        varwidth=TRUE ,
        col=c("blue","green","red") ,
        names=c("1. loteria","cast 2. loterie","3. loteria") )

colors()

#dokreslenie aritmetickych priemerov:
points(x=1:3 ,
       y=c( mean(loteria1),mean(loteria2[100:150]),mean(loteria3) ) ,
       pch=19
      )

# povedz o mediane

