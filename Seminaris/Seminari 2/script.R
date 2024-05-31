############# Seminari 2 ##############
rm(list = ls())
par(mfrow = c(1,1))
############# PART 1 ##################
##### Tècniques gràfiques d'ajust #####

## Exemple 1: Distribució Exponencial #
mostra<-rexp(n = 100, rate = 1)
mostra_ordenada<-sort(mostra)
y<-c()
for(i in 1:length(mostra)) {
  y<-append(y, log(1-i/(length(mostra)+1)))
}
plot(x = mostra_ordenada, y = y)
abline(lm(y~mostra_ordenada))
# Evidentment, aquesta mostra té forma
# d'exponencial perquè de fet ve generada
# per un model exponencial lambda=1 i per
# tant, el pendent és de -1. Un estimador
# per aquest és el quocient y/x que n'és 
# el pendent

# Fem ara el mateix, obtenint una mostra
# a partir d'una llei gaussiana.
mostra<-rnorm(n = 100, mean = 0, sd = 1)
mostra_ordenada<-sort(mostra)
y<-c()
for(i in 1:length(mostra)) {
  y<-append(y, log(1-i/(length(mostra)+1)))
}
plot(x = mostra_ordenada, y = y)
abline(lm(y~mostra_ordenada))
# Clarament no obtenim una recta i per
# tant, podriem sospitar que la nostra
# mostra no prové d'una llei exponencial.

#### Exemple 2: Distribució de Pareto ####
#install.packages('EnvStats')
library(EnvStats)
mostra<-rpareto(n = 100, location = 2, shape = 3)
mostra_ordenada<-sort(mostra)
y<-c()
for(i in 1:length(mostra)) {
  y<-append(y, log(1-i/(length(mostra)+1)))
}
plot(x = log(mostra_ordenada), y = y)
abline(lm(y~log(mostra_ordenada)))
# Observem la recta que volíem veure, ja que 
# la nostra mostra prové d'una distribucio 
# de Pareto amb location 2 i shape 3. El
# quocient entre: log(mostra_ordenada_k)/log(1-k/(n+1))
# és un estimador de log(location)-1/shape


#############################################
###########        Problemes        #########
#############################################


# Exercici 1
mostra1<-rexp(15, 5)
mostra2<-rexp(20, 5)
mostra3<-rexp(100, 5)

mostra1_ord<-sort(mostra1)
mostra2_ord<-sort(mostra2)
mostra3_ord<-sort(mostra3)

y<-c()
for(i in 1:length(mostra1)) {
  y<-append(y, log(1-i/(length(mostra1)+1)))
}

z<-c()
for(i in 1:length(mostra2)) {
  z<-append(z, log(1-i/(length(mostra2)+1)))
}

t<-c()
for(i in 1:length(mostra3)) {
  t<-append(t, log(1-i/(length(mostra3)+1)))
}

par(mfrow = c(1,3))
plot(x = mostra1_ord, y = y)
abline(lm(y~mostra1_ord))
plot(x = mostra2_ord, y = z)
abline(lm(z~mostra2_ord))
plot(x = mostra3_ord, y = t)
abline(lm(t~mostra3_ord))

# Exercici 2
#install.packages('evir')
rm(list = ls())
library(evir)
data("danish")
mostra<-danish
mostra_ord<-sort(mostra)
x<-log(mostra_ord)
y<-c()
for(i in 1:length(mostra)) {
  y<-append(y, log(1-i/(length(mostra)+1)))
}
plot(x,y)
abline(lm(y~x))
# Veiem que al dibuixar els
# punts, aquests queden
# alineats i per tant podriem
# pensar que la llei d'aquesta
# variable seria una pareto
# amb dos paràmetres que
# hauriem de determinar, però
# que venen donats implícitament
# pel pendent de la recta.

# Exemple 3

# Exemple per a una distribució 
# normal
par(mfrow = c(1,2))
x<-rexp(100, 0.4)
z<-c()
for(i in 1:length(x)) {
  z<-append(z, qnorm(i/(length(x)+1)))
}
x<-sort(x)
plot(x,z)
# Evidentment no és una recta i doncs la mostra prové d'una
# distribució exponencial i estem fent un test de normalitat.
qqnorm(x)
qqline(x)
# Les instruccions anteriors fan el test de normalitat automàticament
# Aquest té un aspecte diferent al que defineix la funció però en el fons
# cal veure si les dades formen una recta o no.
x<-rnorm(100)
qqnorm(x)
qqline(x)
# En aquest darrer cas la línia si que és recta ja que les dades provenen
# d'una distribució normal.

# Exemple per a una distribució
# normal
z<-c()
for(i in 1:100) {
  z<-append(z, qnorm(i/101))
}
x<-rnorm(100, 0.4)
x<-sort(x)
plot(x,z)
qqnorm(z)
qqline(z)
###############################
# Exercici 3
par(mfrow = c(1,2))
library(MASS)
x<-michelson$Speed
x_sorted<-sort(x)
z<-c()
for(i in 1:length(x)) {
  z<-append(z, qnorm(i/(length(x)+1)))
}
plot(x_sorted,z)
abline(lm(z~x_sorted))
qqnorm(x)
qqline(x)
# Podriem pensar per tant que
# la nostra distribució prové
# d'una llei normal estàndard

############# PART 2 ##################
######## Mètode de Montecarlo #########

rm(list = ls())
nn = 200
mostra.1 = rnorm(nn)
mostra.2 = rnorm(nn)
cor(mostra.1, mostra.2)
plot(mostra.1, mostra.2, pch=16)
abline(h=0, lty=2)
abline(v=0, lty=2)
title(paste("r =", round(cor(mostra.1, mostra.2), 3)))

# Exercici 4
correlacio=function(n) {
  x<-rnorm(n)
  y<-rnorm(n)
  cor(x,y)
}
correla=replicate(1000, correlacio(20))
quantile(correla,c(0.05, 0.95))
hist(correla)
abline(v=quantile(correla,c(0.05)))
abline(v=quantile(correla,c(0.95)))
points(mean(correla), 0)
mean(correla)

###### Exemple Taxis ######
# Apartat 1
obs<-function(N, n) {
  obs<-runif(n, min = 1, max = N)
  T_1<-max(obs)
  T_2<-2*mean(obs)
  return(c(T_1, T_2))
}

# Apartat 2
N<-100 # l'empresa de Taxis té 100 taxis en circulació
n<-50 # Realitzem 50 observacions al dia
total_obs<-c()
for(i in 1:1000) { # Fem l'experiment durant 1000 dies.
  total_obs<-append(total_obs, obs(N, n))
}

mat<-matrix(total_obs, ncol = 2, byrow = TRUE)
df<-data.frame(T_1=mat[,1], T_2=mat[,2])

# Apartat 3
par(mfrow = c(1,2))
hist(df$T_1)
hist(df$T_2)

# Apartat 4
b.T_1<-mean(df$T_1)-N
b.T_1 # Biaix de l'estimador T_1
b.T_2<-mean(df$T_2)-N
b.T_2 # Biaix de l'estimador T_2

# Apartat 5
S.T_1<-sum((df$T_1-mean(df$T_1))^2)/(length(df$T_1)-1)
S.T_1
S.T_1<-sd(df$T_1)^2
S.T_1
S.T_2<-sd(df$T_2)
S.T_2
MSE.T_1<-S.T_1-b.T_1^2
MSE.T_2<-S.T_2-b.T_2^2
MSE.T_1
MSE.T_2
# Com MSE.T_1 < MSE.T_2 tenim que
# MSE.T_1 produeix una millor 
# predicció de mitjana