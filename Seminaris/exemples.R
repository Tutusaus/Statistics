# Exemple
getwd()
setwd("./Estadística/")

# Classe 28/02/2024
rm(list = ls())
x<-c(rep(0, 40), rep(1, 26), rep(2, 14), rep(3, 6), rep(4, 3), rep(5, 0), rep(6, 1))
y<-c(40, 26, 14, 6, 3, 0, 1)
par(mfrow = c(1,2))
barplot(y, names.arg = c("0", "1", "2", "3", "4", "5", "6"))
abline(v = mean(x), col = "Red", lty = 3)
boxplot(x)
##################
mean(x)
sum(x)/length(x)
##################
median(x)
##################
sd(x)
sqrt(sum((x-mean(x))^2)/length(x))
sqrt(sum((x-mean(x))^2)/(length(x)-1))
sd2<-function(x) {return(sqrt(sum((x-mean(x))^2)/length(x)))}
sd2(x)
##################
#install.packages("moments")
library(moments)
skewness(x)
sum((x-mean(x))^3)/(length(x)*sd2(x)^3)
#################
kurtosis(x)
sum((x-mean(x))^4)/(length(x)*sd2(x)^4)
#################
moment(x, 1, central = 0) # Mitjana
sqrt(moment(x, 2, central = mean(x))) # Desviació estàndard
#################
sort(x)
min(x)
max(x)
Q1<-quantile(x, probs = 0.25)
Q2<-quantile(x, probs = 0.5) # Mediana
Q3<-quantile(x, probs = 0.75)
Q4<-quantile(x, probs = 1)
RI<-Q3-Q1
LI<-Q1-1.5*RI
LS<-Q3+1.5*RI
v_atíp<-c()
for(num in x) {
  if(num > LS) {
    v_atíp<-num
  }
}
v_atíp
#################
library(moments)
x<-rbinom(1000, 50, 0.5)
a<-4
b<--3
y<-a+b*x
mean(y)
a+b*mean(x)
sd(y)
abs(b)*sd(x)
skewness(y)
-skewness(x)
kurtosis(y)
kurtosis(x)
par(mfrow = c(2,2))
hist(x)
hist(y)
hist(x^2)
hist(1/x)
hist(log(x))
log(median(x))
median(log(x))
##################

# Exercici 2
x<-c(28,22,35,42,44,53,58,41,40,32,31,38,37,61,25,35)
mean(x)
median(x)
sd(x)
y<-sort(x)
y
fr<-c(3/length(x), 7/length(x), 3/length(x), 2/length(x), 1/length(x))
fr
x_j<-c(25, 35, 45, 55, 65)
mean_ordenats<-sum(x_j*fr)
mean_ordenats
par(mfrow = c(1,1))
hist(x)



########## TEOREMA CENTRAL DEL LÍMIT ########
y<-c()
for(i in 1:1000) {
  x<-rbinom(100, 1, 0.5)
  y<-append(y, sum(x))
}
y
hist(y)

?rexp
rm(list = ls())
y<-c()
for(i in 1:10000) {
  x<-rexp(100)
  y<-append(y, sqrt(100)*(mean(x)-1))
}
hist(y)

########## DISTRIBCIÓ XI QUADRAT ###########
x<-rnorm(n = 100, mean = 0, sd = 1)
x<-x^2
y<-sum(x) # Y segueix una distribució xi quadrat de 100 graus de llibertat

# Dibuixem 6 histogrames amb llei xi quadrat de 100 graus de llibertat on el seguent histograma té cada vegada més dades
par(mfrow = c(2, 3))
for(i in 1:6) {
  y<-c()
  for(j in 1:200) {
    x<-rnorm(100, 0, 1)
    x<-x^2
    x<-sum(x)
    y<-append(y, values = x)
  }
  hist(y)
}
?distribution

####### CRITERI XI QUADRAT DE PEARSON ########
# En aquest cas, tot  i que la llei es pot fer amb un vector aleatori, ho faré
# en el cas 1-dimensional.
?sample
x<-sample(1:6, 300)
x
a<-table(x)
a
t<-c()
for(i in 1:6) {
  t<-sum((a[names(a)==i]-50)^2)
}
a[names(a)==6]

E<-50

#################################################
###### CÀLCUL DE ERRORS DE DADES ################
#################################################
x<-rep(1:100, each = 1)
x
y<-rnorm(100, 0, 1)
y
v<-data.frame(x,y)
########### TOTAL SQUARE SUM ##############
plot(v)
abline(h=mean(y))
mean(y)
f_1<-mean(x)
TSS<-sum((y-f_1)^2)
TSS
########### REGRESSION SQUARE SUM #########
f_2<-function(x) {
  x_1<-mean(x)
  p_1<-0
  for(i in x) {
    if(i<x_1) {
      sum((y-)^2)
    }
  }
}
RSS<-sum((y-)^2)
## ........................ INACABAT
########## ARBRES DE REGRESSIÓ ########### SST=SSR+SSE
## ... NO FET (molt interessant) + Bootstrap --> random forest
########## XARXES NEURONALS ##############
######### MODEL LINEAL SIMPLE #############
v
plot(v)
abline(lm(y~x))
lm(y~x)
a<-cov(x,y)/sd(x)^2 # Observem que aquest és el pendent de la recta de regressió
b<-mean(y)-a*mean(x)   # Observem que aquest és l'ordenada a l'origen de la recta
Rquadrat<-cov(x,y)/(sd(x)*sd(y))
Rquadrat

############# TEOREMA CENTRAL DEL LÍMIT ##################
x<-rnorm(n = 10000, mean = 0, sd = 1)
hist(x)

rm(list = ls())

y<-c()
for(i in 1:10000) {
  x<-rexp(100)
  y<-append(y,(mean(x)-1)*sqrt(100))
}
hist(y)

#################
x<-pnorm(2.41)
2-2*x
#################
v<-c()
for(i in 1:100) {
  x<-rgeom(100, 0.5)
  y<-sum(x)
  v<-append(v,y)
}
v
expected_value<-sum(v)/length(v)
expected_value
100*(1-0.5)/0.5
#################
x<-rnorm(100, -1, 1)
mean(x)
sd(x)
#################
i<-0
run<-TRUE
while(run) {
  x<-mean(rnorm(100,5,25))
  if(x<0) {
    print(x)
    print(i)
    run<-FALSE
  }
  i<-i+1
}
qt(0.975, df =2)
#################
library("MASS")
plot_pareto_density <- function(x, alpha, xm) {
  y <- dpareto(x, location = xm, shape = alpha)
  plot(x, y, type = "l", col = "blue", lwd = 2, main = "Pareto Probability Density Function", xlab = "x", ylab = "Probability Density")
  legend("topright", legend = paste("alpha =", alpha, ", xm =", xm), col = "blue", lwd = 2)
}
alpha <- 3  # Shape parameter
xm <- 1     # Scale parameter
x <- seq(xm, 10, length.out = 1000)  # Adjust the range as needed

# Plot Pareto density function
plot_pareto_density(x, alpha, xm)

####################
rm(list = ls())
f<-function(x) {
  sum((x-1:length(x))^2)
}
nlm(f, c(10,10))
####################
library(evir)
library(MASS)
data(nidd.thresh)
fitdistr(nidd.thresh,"gamma")
