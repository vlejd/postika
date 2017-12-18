# ***** PREMENNE, VEKTORY, MATICE *****

# pomocou HASHMARKu sa oddeluje komentar

# priradenie:
pReMeNnA <- 7
pReMeNnA
pReMeNnA <- "a"
pReMeNnA <- "slovo"
"a" -> pReMeNnA

# vektor:
WEKTOR <- c( 2 , 3.1 , 5.4 )
WEKTOR[2]
# oddelenie 2 prikazov:
novyWEKTOR <- c(1,4,5); novyWEKTOR <- 0
WEKTOR+5
sqrt(WEKTOR)
length(WEKTOR)

# matica:
A <- matrix( c(1,2,3,4,4,5,5,6,1), ncol=3, nrow=3, byrow=TRUE )
t(A)
A %*% A
# pozor na tzv. Hadamardov sucin:
A * A
solve(A)
A %*% solve(A)

z <- c(7,4,9)
A %*% z
t(z) %*% A
z %*% A
diag(6)
diag(z)
diag(A)

solve(A , c(2,3,4))

#stopa matice:
sum( diag(A) )

#determinant:
det(A)
#alebo:
prod(   eigen(A)$values   )

# ***** ZAKLADNA PRACA S NACITANYMI DATAMI*****

#nacitanie dat:
maliari <- read.table(file="e:\\klasickiMALIARI.txt" , header=TRUE)

# zobrazenie vybranych dat:
maliari$Composition
maliari[2,5]
maliari[7,]
maliari[ c(3,4,5,6,7,8) , ]
maliari[ 3:8 , ]
maliari[ -2 , ]
maliari[ -(21:50) , ]

Composition
attach(maliari)

# pouzitie logickych vyrazov:
maliari[ Composition>=16 , ]
maliari[ Composition>=15 & Colour>=15 , ]
max(Colour)
max(maliari)
max( maliari[ , -c(1,6)] )
maliari[ Composition==18 | Drawing==18 | Colour==18 | Expression==18 , ]
maliari[ !(School=="A" | School=="B") , ]
# to iste inac:
maliari[ School!="A" & School!="B" , ]

# usporiadanie:
order(Colour)
maliari[ order(Colour) , ]
maliari[ rev(order(Colour)) , ]

# usporiadanie podla abecedy:
maliari[ order(Name) , ]


# ranky:
z <- c(16,18,23,3)
rank(z)

# na kolkom mieste kto skoncil:
data.frame( Name , rank(Colour) )
data.frame( Name , rank(Colour,ties.method="min") )
# aj nazvy stlpcov:
data.frame( meno=Name , umiestnenie=rank(Colour,ties.method="min") )