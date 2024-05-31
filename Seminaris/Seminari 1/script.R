#----------- Pràctica 1 -----------
rm(list = ls())

# Exercici 1
#--------- Teoria ------------------
llista1<-c(2, 3, 4, 7, 1, -1)
llista2<-c(4, 25, 7, 8, -1, 6)
1/llista1
sin(llista1)
llista1/llista2
mean(llista1)
llista1_sorted<-sort(llista1)
sqrt(-4+0i)
x<-c("Gos", "Gat", "Tortuga")
z<-c(x, "Blau", "Vermell")
mixt<-c(z, 3, 4, 5)
z
mixt
typeof(z)
typeof(mixt)
mixt<-c(3, 4, 5, z)
typeof(mixt)
mixt
x<-1:20
y<-x>10
y
gender<-c(rep("dona",100), rep("home",200))
gender<-factor(gender)
levels(gender)
gender
satisfaction<-c(1,3,4,2,2)
fsatisfaction<-factor(satisfaction, levels = 1:5)
fsatisfaction
levels(fsatisfaction)
levels(fsatisfaction)<-c("molt dolent", "dolent", "normal", "bo", "molt bo")
levels(fsatisfaction)
summary(satisfaction)
summary(fsatisfaction)
gender<-factor(gender)
levels(gender)
gender
satisfaction<-c(1,3,4,2,2)
fsatisfaction<-factor(satisfaction, levels = 1:5)
fsatisfaction
levels(fsatisfaction)
levels(fsatisfaction)<-c("molt dolent", "dolent", "normal", "bo", "molt bo")
levels(fsatisfaction)
summary(satisfaction)
summary(fsatisfaction)
#-----------------------------------------

# Apartat A
vector<-c(2,4,1,5,8,7,1,5,3,5,0,1,9,0,5,8,9)
vector[vector == 5]
sum(vector == 5)
vector[vector == 5]<-0
vector
# Apartat B
vector2<-c(2,4,1,5,8,7,1,5,3,5,0,1,9,0,5,8,9)
vector2<-vector2[vector2 > 3]
vector2
# Apartat C
?seq
seqüència<-seq(2,20,by=0.4)
seqüència
# Apartat D
concatenació<-c(vector, vector2)
concatenació
# Apartat E
rm(list = ls())
# Apartat F
dolor<-c(0,3,1,1,0,2)
dolor<-factor(dolor) # Si cal especificar el nombre de nivells cal afegir l'argument levels = 1:nombre de nivells
levels(dolor)<-c("cap dolor", "baix dolor", "dolor mitjà", "dolor agut")
levels(dolor)
summary(dolor)

# Exercici 2
# -------------- Teoria -----------------
rm(list = ls())
x<-rnorm(20)
x
dim(x)<-c(4,5)
x
x<-rnorm(20)
x
x<-matrix(x, nrow = 4, ncol = 5, byrow = T) # byrow = True crea la matriu omplint fila per fila
x
nrow(x)
ncol(x)
t(x)
rownames(x)
colnames(x)
x<-matrix(1:10, nrow = 2, ncol = 5)
y<-matrix(11:18, nrow = 2, ncol = 4)
x
y
cbind(x,y)
rbind(x,y)
x<-matrix(rnorm(20), ncol = 4, nrow = 4)
x
x[3,2]
x[3,1:2]
x[,c(1,3)]
x[-1,-3]
#-------------------------------------------
# Apartat A
A<-matrix(rnorm(20), nrow = 4, ncol = 3, byrow = T)
B<-matrix(rnorm(20), nrow = 1, ncol = 3, byrow = T)
A
B
# Apartat B
C<-rbind(A,B)
C
# Apartat C
dim(C)
# Apartart D
mat1<-matrix(c(2,0,-1,3,4,-2), nrow = 2, ncol = 3)
mat1
mat2<-matrix(c(1,3,1,2,5,7), nrow = 3, ncol = 2)
mat2
producte<-mat1 %*% mat2
producte
producte[,2]

# Exercici 3
# --------------- Teoria ----------------
L<-list(nom="alex", edat=30, n.fills=2, nom.fills=c("ivan", "ruben"))
L
L[["nom"]]
L[[1]]
L$nom
names(L)
# ---------------------------------------
# Apartat A
vector1<-c(1980, 1988, 1996, 1998, 2000, 2002)
vector2<-c(71.5, 72.1, 73.7, 74.3, 75.2, 74.7)
vector3<-c("M", "M", "F", "F", "M", "M")
vector4<-c(179.3, 179.9, 180.5, 180.1, 180.3, 180.4)
# Apartat B
llista<-list(Any=vector1, Pes=vector2, Sexe=vector3, Alçada=vector4)
llista
# Apartat C
llista[[4]]
llista[["Alçada"]]
llista$Alçada

# Exercici 4
# ------------------ Teoria -----------------
d<-data.frame(mpes=c(71.5,72.1,73.7,74.3,75.2,74.7), sexe=c("M","M","F","F","M","M"))
d
d1<-as.data.frame(llista)
d2<-data.frame(matrix(vector(), 0, 4))
data(faithful)
names(faithful)
summary(faithful)
faithful[1,2]
faithful[2,]
faithful[faithful$eruptions<4,]
# -------------------------------------------
# Apartat A
df<-data.frame(Nom=c("Alice", "Paul", "Jerry", "Thomas", "Marguerite", "Linda"), Cognom=c("Ryan", "Collins", "Burke", "Dolan", "Black", "McGrath"), Edat=c(37,34,26,72,18,24), Sexe=c("F", "M", "M", "M", "F", "F"), Punts=c(278,242,312,740,177,195))
df
# Apartat B
punts<-df$Punts
mean(punts)
# Apartat C
menorsde35<-df$Punts[df$Edat<35]
menorsde35
# Apartat D
max(df$Edat)
# Apartat E
df
condicions<-df[(df$Punts>200) & (df$Edat<35), ]
condicions

# Exercici 5
# --------------- Teoria -----------------
adr<-"https://raw.githubusercontent.com/Miquelsc/est/main/nadons.txt"
nadons<-read.table(file = adr, header = T, sep = ",")
nadons
is.data.frame(nadons)
write.table(nadons, file = "./Estadística/Pràctica 1/nadons.txt", row.names = F, sep = " ")
# ----------------------------------------
# Apartat A
malaria<-read.table(file = "https://raw.githubusercontent.com/Miquelsc/est/main/malaria.txt", header = T, sep = ",")
# Apartat B
colnames(malaria)
# Apartat C
malaria
home_data<-malaria[malaria$Sexe == "H", ]
home_data
dona_data<-malaria[malaria$Sexe == "D", ]
dona_data
# Apartat D
mean(home_data$Edat)
mean(dona_data$Edat)
sd(home_data$Edat)
sd(dona_data$Edat)
max(home_data$Edat)
max(dona_data$Edat)
min(home_data$Edat)
min(dona_data$Edat)

# Exercici 6
# ---------------- Teoria ----------------
sample(1:20, 5)
sample(1:20, 5)
set.seed(9)
sample(1:20, 5)
set.seed(9)
sample(1:20, 5)
# ----------------------------------------
# Apartat A
sample(runif(201, 0, 2), size = 100, replace = F)
# Apartat B
dt(runif(21, -1, 1), df = 13)
# Apartat C
qt(0.01, df = 9)
# Apartat D
pnorm(142, 100, 15)

# Exercici 7
# ------------------ Teoria -----------------
#demo(graphics)
rm(list = ls())
par(mfrow=c(1,3))
x<-seq(0,1,length=20)
plot(sin(2*pi*x))
plot(sin(2*pi*x), type = "l")
plot(sin(2*pi**x), type = "b")
par(mfrow=c(1,1))
library(MASS)
?Cars93
plot(Cars93$Weight, Cars93$EngineSize, ylab = "EngineSize", xlab = "Weight", main = "Myplot")
?lines
lines(x=c(min(Cars93$Weight), max(Cars93$Weight)), y=c(min(Cars93$EngineSize), max(Cars93$EngineSize)), lwd=4, lty=3, col="green")
abline(h=3, lty=2)
abline(v=1999, lty=4)
fm1<-lm(EngineSize~Weight, Cars93, subset = Origin=="USA")
?lm
abline(coef(fm1), lty = 4, col = "red")
par(mfrow = c(1,2))
hist(Cars93$Weight, breaks = 10, xlim = c(1500,4500), col = "grey")
hist(Cars93$Weight, breaks = 10, xlim = c(1500,4500))
par(mfrow = c(1,1))
boxplot(Cars93$Weight)
# -------------------------------------
# Apartat A
x<-1:20
# Apartat B
w<-1+sqrt(x)/2
# Apartat C
df<-data.frame(x=x, y=x+rnorm(x)*w)
df
# Apartat D
par(mfrow = c(1,2))
hist(df$y)
boxplot(df$y)
# Apartat E
par(mfrow = c(1,1))
plot(x, df$y)
# Apartat F
fm2<-lm(x~df$y)
abline(coef(fm2), lty = 4, col = "red")

# Exercici 8
# ------------------- Teoria --------------------
pow2<-function(x) {
  x^2
}
pow2(4)
pow2(1:4)
opr<-function(x, y, z) {
  x+2*y+3*z
}
opr(1,2,3)
opr(z=3, y=2, x=1)
mypow<-function(x, pow=2) {
  x^pow
}
mypow(2,3)
mypow(2)
exemple<-function(x) {
  the.mean<-mean(x)
  the.sd<-sd(x)
  the.min<-min(x)
  the.max<-max(x)
  return(list(average=the.mean, stand.dev=the.sd, minimum=the.min, maximum=the.max))
}
res<-exemple(rnorm(20))
res$average
# -----------------------------------------
# Apartat A
pot<-function(x) {
  return(list(quadrat=x^2, cub=x^3, arrel=sqrt(abs(x))))
}
pot(2)
pot(-3)
# Apartat B
info<-function(x) {
  print(max(x))
  print(min(x))
  print(mean(x))
  hist(x)
}
x<-rnorm(50)
info(x)
# Apartat C
longitud<-function(x) {
  sqrt(sum(x^2))
}
vector<-c(3,4)
longitud(vector)
sumadelongituds<-function(x, y, z) {
  longitud(x)+longitud(y)+longitud(z)
}
vector1<-rnorm(5)
vector2<-rnorm(5)
vector3<-rnorm(5)
sumadelongituds(vector1,vector2,vector3)

# Exercici 9
# -------------------- Teoria ---------------------
rmatrix<-matrix(1:20,nrow = 2)
dim(rmatrix)
apply(rmatrix, 1, sum)
apply(rmatrix, 2, sum)
lapply(c(1,2,3), function(x){return (x*2)})
sapply(1:20, function(x){x-1})
# --------------------------------------------------
# Apartat A
x<-1:50000

funcio1<-function(x) {
  y<-c()
  time0<-Sys.time()
  for(num in x) {
    y<-append(y,2^num)
  } 
  print(Sys.time()-time0)
}
funcio1(x)

y<-rep(2, times=50000)
apply(y, MARGIN=1, FUN=pow2)
