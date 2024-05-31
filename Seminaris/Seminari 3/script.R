#############################
######## SEMINARI 3 #########
#############################
rm(list = ls())
# Atenció: Tot i que al full
# d'enunciats posi Seminari 4,
# aquest és el seminari 3


######## EXEMPLE 1 ########
#                         #
#  Interval de confiança  #
#   per la mitjana amb    #
#     sigma coneguda      #
#                         #
###########################
N<-1000
n<-10
mu<-4  
sigma<-2
x<-rnorm(N*n,mu,sigma)
# Creem una matriu amb les 1000
# mostres de normals N(4,4)
dim(x)<-c(N,n)
dim(x)
# Calculem la mitjana per
# cadascuna de les 1000 mostres
# de 10 mostres amb distribució
# N(4,4). 1 vol dir que si la
# matriu x té dimensions 1000x10
# apliqui la funció mean per
# files i no columnes
M<-apply(x,1,mean)
err<-qnorm(0.975)*sigma/sqrt(n)
A<-M-err
B<-M+err
# Verifiquem, per a cada interval,
# si el valor teòric mu = 4 hi
# pertany i comptem quantes vegades
# passa aquest esdeveniment
u<-(A<4) & (B>4)
sum(u)/length(u)
# Aquesta proporció s'aproxima a 0.95
###################################

########### EXERCICI 1 ############
rm(list = ls())
N<-1000
n<-20
m<-3
x<-rnorm(N*n,m,1)
dim(x)<-c(N,n)
dim(x)
M<-apply(x,1,mean)
err<-qnorm(0.95)*1/sqrt(n)
A<-M-err
B<-M+err
u<-(A<m) & (m<B)
sum(u)/length(u)
# Aquesta proporció s'aproxima a 0.9
###################################

########### EXEMPLE 2 #############
#                                 #
#  Interval de confiança per la   #
#  mitjana amb sigma desconeguda  #
#                                 #
###################################
rm(list = ls())
t.test(x, conf.level = 0.9)$conf.int
t.test(x) # Assumeix un conf.level=0.95
t.test(x)$conf.int
t.test(x, conf.level = 0.9)$conf.int
###################################

########### EXERCICI 2 ############
# Es fan 12 determinacions de la
# quantitat d'argent d'un mineral
# (en mg) i s'obté:
rm(list = ls())
x<-c(5.2, 4.8, 5.3, 5.7, 5.0, 4.7, 4.3, 5.5, 5.4, 5.1, 4.9, 5.8)
# Suposant normalitat, determinem
# l'interval de confiança amb nivell
# de confiança 0.95 per la mitjana
# suposant variància desconeguda.
# Si augmentem el nivell de confiança
# a 0.99, obtenim un interval més o
# menys ple?
t.test(x,conf.level = 0.95)$conf.int
# Podem veure que l'interval de conf.
# 95% per la mitjana és [4.86, 5.41]
t.test(x, conf.level = 0.99)$conf.int
# En aquest darrer test aquest interval
# és [4.75, 5.53]
# Veiem doncs que amb un nivell de
# confiança del 95% la longitud de
# l'interval és de 0.551135 mentre que
# amb una confiança del 99%, aquesta
# longitud és de 0.777705, o sigui que
# si més no, la longitud de l'interval
# tendeix a créixer a mesura que
# augmenta el nivell de confiança
###################################

####### EXERCICI 3 ########
#                         #
#  Interval de confiança  #
#   per a una proporció   #
#                         #
###########################
rm(list = ls())
# Es vol estimar la proporció
# p d’animals de granja d’una
# certa espècie amb una malaltia
# congènita a una determinada
# zona. Per això s’agafa una
# mostra de 200 animals i
# s’observa que el percentatge
# de malalts és del 8%. Calculeu
# l’interval de confiança per a
# p amb nivell de confiança 0.92.
N<-200
x<-0.08*N
p_0<-x/N
prop.test(x, N, conf.level = 0.92)
# Veiem que l'interval de confiança
# del 92% percent obtingut és
# [0.05041444, 0.12318411] i per tant
# podriem sospitar com a mínim que
# 0.05*N = 10 animals tenen la malaltia
# i com a màxim 0.123*N = 24
###########################

######## EXEMPLE 4 ########
#                         #
#  Interval de confiança  #
#       per a sigma       #
#                         #
###########################
rm(list = ls())
n<-10
gamma<-0.95
alpha<-1-gamma
a<-qchisq(alpha/2,n-1)
b<-qchisq(1-alpha/2,n-1)

z<-seq(0, 30, by=0.1)
y<-dchisq(z, n-1)
plot(z, y, type='l', ylim=c(0.0041,0.11))
lines(c(a,a), c(0,dchisq(a,n-1)))
lines(c(b,b), c(0,dchisq(b,n-1)))
###########################

######## EXERCICI 4 #######
rm(list = ls())
x<-c(7.9, 8.3, 4.8, 8.4, 7.9, 5.2, 5.6, 3.2, 9.1, 7.7, 6.5, 4.4)
# Calculem un interval de 
# confiança del 93% per a la
# variància. Suposem normalitat
n<-length(x)
var_mostral<-var(x)
gamma<-0.93
alpha<-1-gamma
a<-(n-1)*var_mostral/qchisq(1-alpha/2, df = n-1)
b<-(n-1)*var_mostral/qchisq(alpha/2, df = n-1)
a
b
############################

######## EXEMPLE 5 ########
#                         #
#  Interval de confiança  #
#    per a dues mostres   #
#                         #
###########################
# Suposem que tenim dues mostres
# que provenen de la distribució
# normal. X_i~N(mu_x,sigma_x^2)
# Y_i~N(mu_y,sigma_y^2).

########### CAS 1 ##########
# (X_i,Y_j) dades aparellades
# amb X_i independent a Y_j per
# a tot i diferent a j=i perquè
# i,j=1,...,n mateixa quantitat.
######## EXERCICI 5 ###########
#                             #
#  Interval de confiança per  #
#    a la reducció mitjana    #
#                             #
###############################
x<-c(93, 106, 87, 92, 102, 95, 88, 110)
y<-c(92, 102, 89, 92, 101, 96, 88, 105)
t.test(x, y, paired = TRUE, conf.level = 0.97)
# Com que el p-valor del test és
# 0.2753>0.05 acceptem H_0
# hipòtesis nul·la i per tant
# tenim evidència significativa que
# la dieta no ajuda a reduïr la
# pressió arterial
###############################

########### CAS 2 ##########
# X_i i Y_j son independents 
# entre si per a tot i i j.
######## EXERCICI 6 #############
#                               #
#  Interval de confiança per    #
#  a la reducció mitjana amb    #
#  sigma_x i sigma_y CONEGUDES  #
#                               #
#################################
rm(list = ls())
L<-c(6.7, 1.9, 6.4, 4.8, 2.6, 4.9, 6.7, 3.6, 1.5, 1.2, 2.4, 2.4, 4.6, 4.9, 4.8)
S<-c(6.2, 3.7, 4.5, 6.2, 6.0, 5.3, 3.5, 3.6, 3.1, 0.3, 5.3, 4.5, 4.5, 3.6, 4.5)
gamma<-0.95
alpha<-1-gamma
SE<-qnorm(1-alpha/2)*sqrt(3.5/length(L)+2.2/length(S))
a<-(mean(L)-mean(S))-SE
b<-(mean(L)-mean(S))+SE
# Observem que el valor real de les
# mitjanes de les dues poblacions
# es troba entre -1.56 i 0.84 amb una
# certesa del 95%. Com que el valor 0
# es troba dins aquest interval no
# podem rebutjar H_0 i per tant deduïm
# que els insectes tenen la mateixa
# longitud d'ales
#################################
######## EXERCICI 6 #############
#                               #
#  Interval de confiança per    #
#  a la reducció mitjana amb    #
#  sigma_x i sigma_y DESCONEGUDES  #
#                               #
#################################
rm(list = ls())
L<-c(6.7, 1.9, 6.4, 4.8, 2.6, 4.9, 6.7, 3.6, 1.5, 1.2, 2.4, 2.4, 4.6, 4.9, 4.8)
S<-c(6.2, 3.7, 4.5, 6.2, 6.0, 5.3, 3.5, 3.6, 3.1, 0.3, 5.3, 4.5, 4.5, 3.6, 4.5)
t.test(L, S, var.equal = TRUE, conf.level = 0.95)$conf.int
t.test(L, S, var.equal = FALSE, conf.level = 0.95)$conf.int
###############################

################################
#                              #
#           PROBLEMES          #
#                              #
################################


######## EXERCICI 1 ##########
getwd()
setwd("C:/Users/guill/Desktop/Mates/Cursos/Mates 3r/Estadística/Seminaris/Seminari 3")
getwd()
load(file = "malaria.RData")
malaria
######### APARTAT A ###########
# Trobem un interval de confiança
# del 95% per la mitjana d'edats.
# Suposant normalitat.
t.test(malaria$Edat, conf.level = 0.95)$conf.int
######### APARTAT B ###########
# Trobem un interval de confiança
# del 92% per la proporció de
# dones i un per la proporció
# d'homes
######## PER LES DONES ########
x<-sum(malaria$Sexe=="D")
n<-length(malaria$Sexe)
prop.test(x, n, conf.level = 0.92)$conf.int
######## PELS HOMES ###########
y<-sum(malaria$Sexe=="H")
prop.test(y, n, conf.level = 0.92)$conf.int
########## APARTAT C ##########
x<-malaria$Edat[malaria$Sexe=="D"]
y<-malaria$Edat[malaria$Sexe=="H"]
t.test(x, y, var.equal = TRUE, conf.level = 0.93)$conf.int
########## APARTAT D ##########
x<-malaria$Edat
n<-length(x)
gamma<-0.9
alpha<-1-gamma
a<-(n-1)*var(x)/qchisq(1-alpha/2, n-1)
b<-(n-1)*var(x)/qchisq(alpha/2, n-1)
a
b
##############################
######## EXERCICI 2 ##########
getwd()
setwd("C:/Users/guill/Desktop/Mates/Cursos/Mates 3r/Estadística/Seminaris/Seminari 3")
getwd()
load(file = "Nadons.RData")
Nadons
######### APARTAT A ##########
t.test(Nadons$pH, conf.level = 0.9)$conf.int
##############################
######### APARTAT B ##########
t.test(Nadons$pH[Nadons$Madre=="F"], conf.level = 0.9)$conf.int
t.test(Nadons$pH[Nadons$Madre=="NF"], conf.level = 0.9)$conf.int
##############################
######### APARTAT C ##########
x<-Nadons$pH[Nadons$Madre=="F"]
y<-Nadons$pH[Nadons$Madre=="NF"]
t.test(x, y, var.equal = FALSE, conf.level = 0.95)$conf.int
# Aparentment no podem dir
# quina mitjana és més elevada
# ja que el 0 es troba dins 
# l'interval
##############################
######### EXERCICI 3 #########
parcial<-c(7.9, 5.4, 8.3, 6.2, 8.2, 8.3, 7.8, 4.9, 6.2, 8.9, 7.8, 9.7, 7.2)
final<-c(8.2, 5.7, 6.0, 4.2, 7.5, 4.6, 6.2, 5.2, 5.3, 9.2, 6.5, 8.1, 4.5)
######### APARTAT A ##########
t.test(parcial, conf.level = 0.9)$conf.int
t.test(final, conf.level = 0.9)$conf.int
# No podem afirmar amb certesa
# que la nota mitjana del parcial
# és més alta que la del final
t.test(parcial, conf.level = 0.8)$conf.int
t.test(final, conf.level = 0.8)$conf.int
# En aquest cas si que tenim evidència
# significativa que les notes del
# final han anat pitjor que les
# notes del parcial
##############################
######### APARTAT B ##########
t.test(parcial, final, paired = TRUE, conf.level = 0.92)$conf.int
# Com que el 0 no es troba dins aquest
# interval de confiança. Podriem afirmar
# amb un 92% de confiança que la nota del
# parcial ha anat millor que la del final
##############################
########## APARTAT C #########
t.test(parcial, conf.level = 0.95)$conf.int*0.4+t.test(final, conf.level = 0.95)$conf.int*0.6
# Podem afirmar amb una confiança
# del 95% que tothom aprovarà el curs
##############################
########## APARTAT D #########
n<-length(parcial)
gamma<-0.93
alpha<-1-gamma
A_parcial<-(n-1)*var(parcial)/qchisq(1-alpha/2, n-1)
B_parcial<-(n-1)*var(parcial)/qchisq(alpha/2, n-1)
A_parcial
B_parcial
A_final<-(n-1)*var(final)/qchisq(1-alpha/2, n-1)
B_final<-(n-1)*var(final)/qchisq(alpha/2, n-1)
A_final
B_final
# Només observant els dos intervals
# no podriem dir quines de les dues
# coleccions de dades té més variabilitat
###############################