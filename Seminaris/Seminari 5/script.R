######################################
############ SEMINARI 5 ##############
######################################
# Suposem que ja sabem com trobar l'estimador
# de màxima versemblança d'una llei, introduïm
# ara una funció en R que calcula directament
# aquest estimador.
#
# La llibreria MASS ofereix aquesta
# possibilitat, més concretament la funció
# fitdistr.
# 
#Comprovem-ho:
library(MASS)
?fitdistr

##############
# EXPONENCIALS
x<-rexp(100, 2)
fitdistr(x, "exponential")
# Ara, fent-ho a ma, podem veure que l'estimador
# de màxima versemblança de lambda és lambda_MLE
# = 1/mitjana, per tant, fent aquesta darrera
# operació, el resultat obtingut hauria de ser
# el mateix.
1/mean(x)
# Podem veure com és cert.
##############

##############
# NORMALS ####
n<-100
y<-rnorm(n, 3, 6)
fitdistr(y, "normal")
# El MLE de mu és
mean(y)
# El MLE de sigma^2 és
sqrt(var(y))
# Ens fixem que la variància que mostra la
# funció no és la mateixa que la esperada.
# Això es deu a que la funció fa la correcció
# de Bessel.
sqrt((n-1)/n*var(y))
# Ara ja ho tindriem
##############

# La llibreria fitdistrplus inclou més models.
# El càlcul del MLE també es pot fer utilitzant
# funcions com nlm o optim per minimitzar funcions.
#
# Anem-ho a veure.
#
############# EXEMPLE 1 #############
rm(list = ls())
set.seed(1)
x<-rexp(100, rate = 2)
mean(x)
1/mean(x)
# Ara definim un funció que rebi un vector x, la mostra,
# i un paràmetre lambda que és el que volem trobar.
# La funció retorna la -log(L(X;lambda)).
mlogL<-function(lam,x) {
  return(-(length(x)*log(lam)-lam*sum(x)))
}
# Podem dibuixar gràficament com es comporta la funció
# -log(L(X;lambda)) en funció del valor lambda.
npoint<-100
lambda<-seq(0.1, 10, length = npoint)
plot(lambda, mlogL(lambda,x), type = "l")
# Finalment, utilitzem les funcions mencionades per trobar
# el valor de lambda_MLE que minimitza la funció definida:
nlm(mlogL, 1, x)
optim(1,mlogL,x=x)
1/mean(x)
######################################

############## EXERCICI 1 ############
rm(list = ls())
mlogL<-function(mu,sigmasq,x) {
  a<-c()
  for (i in 1:length(mu)) {
    a<-c(a, sum((x-mu[i])^2))
  }
  return(-(-length(x)/2*log(2*pi)-length(x)/2*log(sigmasq)-1/(2*sigmasq)*a))
}
set.seed(1)
x<-rnorm(n = 100, mean = 3, sd = 2)
npoint<-100
mu<-seq(0.1, 10, length = npoint)
plot(mu, mlogL(mu, 4, x), type = "l")
# Ara faltaria trobar el mínim amb la funció nlm que
# no se com va :(
# Sinó, podem utilitzar la següent funció
fitdistr(x, "normal")
# I comprovar que és el que haviem calculat a ma
muhat<-mean(x)
muhat
sqrt(sum((x-muhat)^2)/length(x))
#######################################

############## EXERCICI 2 #############
library(MASS)
# Mètode 1: Funció fitdistr
fitdistr(michelson$Speed, "normal")
# Mètode 2: Càlcul a ma
muhat<-mean(michelson$Speed)
muhat
sqrt(sum((michelson$Speed-muhat)^2)/length(michelson$Speed))
# Mètode 3: Utilitzant la funció nlm, optim, optimize o Brent. Escrivim si
# Seguint el mètode de l'exercici 1 
########################################

############## EXERCICI 3 ##############
# Disposem de la funció de versemblança i el seu estimador de màxima
# versemblança en el següent enllaç:
# https://math.stackexchange.com/questions/3234144/how-to-compute-the-mle-of-the-zero-truncated-poisson
rm(list = ls())
x<-c(rep(1,18), rep(2,18),rep(3,12),rep(4,7),rep(5,5))
f<-function(theta) {
  return(mean(x)/theta-exp(theta)/(exp(theta)-1))
}
uniroot(f, c(11, 13))
# Per tant, l'estimador de màxima versemblança per theta
# és theta_hat = 11.99993

###### CODI DEL PROFE MIQUEL ############
mostra<-c(rep(1,18), rep(2,18),rep(3,12),rep(4,7),rep(5,5))
mlogL<-function(z,m){ length(m)*(z+log(1-exp(-z)))+sum(log(factorial(m)))-sum(m)*log(z)}

x<-seq(0.1,5,0.001)
plot(x,mlogL(x,mostra),type='l')
nlm(mlogL,2,mostra)
########################################

############## EXERCICI 4 ##############
#install.packages("fitdistrplus")
#library(fitdistrplus)
rm(list = ls())
library(evir)
data(danish)
# Calculant l'estimador de màxima versemblança 
# per alpha>0 (shape) i beta>0 (scale) a ma obtenim el següent 
beta_hat<-min(danish)
alpha_hat<-length(danish)/sum(log(danish/beta_hat))
beta_hat
alpha_hat
# Per poder utilitzar la funció dpareto dins de la funció fitdistr cal
# instal·lar el paquet VGAM. A constinuació comprovem que s'ha instal·lat
# correctament.
#install.packages("VGAM")
library(VGAM)
x<-rpareto(100, 1, 2)
min(x)
length(x)/sum(log(x/min(x)))

fitdistr(danish, dpareto, start = list(scale = beta_hat, shape = alpha_hat))
# Aquesta última instrucció no funciona
########################################

############## EXERCICI 5 ##############
rm(list = ls())
x<-rgamma(100, 10, 3)
fitdistr(x, "gamma")
########################################

############## EXAMEN 2022 #############
rm(list = ls())
library(moments)
# Després d'igualar l'esperança i la variancia als dos primers moments trobem 
# els estimadors pel mètode dels moments.
pi_hat<-function(x) {
  return((moment(x,order = 2)/moment(x, order = 1)-2)/(mean(x)+(moment(x,order = 2)/moment(x, order = 1)-2)))
}
lambda_hat<-function(x) {
  return(mean(x)/(1-pi_hat(x)))
}
# Utilitzant el mètode dels moments estimem els paràmetres per la següent mostra
mostra<-c(0,0,1,0,0,1,0,0,1,0,0,2,2,0,0,1,1,3,4,0,0,2,0,0,0,0)
pi_hat(mostra)
lambda_hat(mostra)
# Per tant aquests estimadors son pi_hat=0.325 i lambda_hat=1.025641. Comprovant-ho manualment, tenim
A<-sum(mostra^2)/sum(mostra)-2
A/(mean(mostra)+A)
mean(mostra)/(1-(A/(mean(mostra)+A)))
# Estimem ara els paràmetres utilitzant l'estimador de màxima versemblança
# Comencem escrivint la func