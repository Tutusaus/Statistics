################ EXÀMEN 2023 ################
# NOM: GUILLEM TUTUSAUS ALCARAZ
# NIU: 1533701

rm(list = ls())
getwd()
setwd("C:/Users/guill/Desktop/Mates/Cursos/Mates 3r/Estadística/Seminaris")
# Com que volem carregar el fitxer .txt utilitzarem la instrucció read.table
# que retornarà un data.frame. Si fos un fitxer tipus .R o .r aleshores hariem
# d'utilitzar l'instrucció source. Si el fitxer fos .RData o .rda, load.
# Existeixen més instruccions en funció del fitxer que es vulgui carregar.
# El podem consultar aquí: https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/data
plastic<-read.table(file = "plastic.txt")

################ EXERCICI 1 #################
##### APARTAT A #####
mean_t1<-mean(plastic$t1)
mean_t2<-mean(plastic$t2)
var_t1<-var(plastic$t1)
var_t2<-var(plastic$t2)
max_t1<-max(plastic$t1)
max_t2<-max(plastic$t2)
min_t1<-min(plastic$t1)
min_t2<-min(plastic$t2)
#####################
##### APARTAT B #####
par(mfrow = c(1,2))
# Fem els respectius boxplots (histogrames vistos des de dalt).
boxplot(plastic$t1)
boxplot(plastic$t2)
# Fem els respectius histogrames indicant amb una línia vertical
# la mitjana.
hist(plastic$t1, main = "Histograma de t1", xlab = "Gruix")
abline(v=mean_t1, col="red")
hist(plastic$t2, main = "Histograma de t2", xlab = "Gruix")
abline(v=mean_t2, col="red")
# Fem els respectius plots de normalitat vistos al Seminari 2.
t1_ordenada<-sort(plastic$t1)
y<-c()
for(i in 1:length(plastic$t1)) {
  y<-append(y, qnorm(i/(length(plastic$t1)+1)))
}
plot(x = t1_ordenada, y = y)
abline(lm(y~t1_ordenada))
# L'instrucció que t'ho fa automàticament és qqnorm
qqnorm(plastic$t1)
qqline(plastic$t1)
# Com hem obtingut una cosa que no sembla ser massa una
# recta podem sospitar que les dades no provenen d'una
# distribució normal.
t2_ordenada<-sort(plastic$t2)
y<-c()
for(i in 1:length(plastic$t2)) {
  y<-append(y, qnorm(i/(length(plastic$t2)+1)))
}
plot(x = t2_ordenada, y = y)
abline(lm(y~t2_ordenada))
# Aquesta encara té menys forma de recta que l'anterior.
qqnorm(plastic$t2)
qqline(plastic$t2)
# De l'anàlisi podem deduïr que el conjunt de dades no
# prové d'una distribució normal i doncs el test de 
# normalitat no és a favor.
#####################
##### APARTAT C #####
# Tornem a suposar normalitat.
t.test(plastic$t2, conf.level = 0.9)$conf.int
# Suposant normalitat del conjunt de dades (que ja hem
# vist a l'apartat b que no té molt sentit) trobem una
# cota inferior de 92.39 amb una confiança del 90%.
##### APARTAT D #####
# Cal fer un contrast sobre comparació de mitjanes
# suposant normalitat (Seminari 4 Apartat 4.2)
t.test(x=plastic$t1,y=plastic$t2,alternative="greater",var.equal=TRUE)
# Com que estem suposant una confiança del 95% (alpha=0.05)
# i el p-valor=0.1068>alpha suposem certa la hipòtesis nul·la
# i per tant, la mitjana de l'espessor és el mateix.
#####################
##### APARTAT E #####
# Cal fer un contrast sobre comparació de variàncies
# Suposant normalitat (Seminari 4 Apartat 4.1)
var.test(x = plastic$t1, y = plastic$t2)
# Com el p-valor és 0.97>alpha=0.05 podem suposar que
# les variàncies de ambdues mostres és la mateixa. Per
# tant té sentit fer el test de l'Apartat D suposant
# que les dues variàncies son iguals.
#####################
##### APARTAT F #####
# Cal construïr un interval de confiança per a mu_t1-mu_t2
# amb sigma_t1 i sigma_t2 desconegudes (Seminari 3 Apartat 4.2.2).
alpha<-0.1
n_t1<-length(plastic$t1)
n_t2<-length(plastic$t2)
nu<-n_t1+n_t2-2
Sc<-sqrt(((n_t1-1)*var_t1+(n_t2-1)*var_t2)/nu)
A<-(mean_t1-mean_t2)-qt(1-alpha/2,nu)*Sc*sqrt((1/n_t1)+(1/n_t2))
A
B<-(mean_t1-mean_t2)+qt(1-alpha/2,nu)*Sc*sqrt((1/n_t1)+(1/n_t2))
B
#####################
################ EXERCICI 2 #################
mostra<-c(1.0661,1.0733,1.0705,1.0724,1.0719,1.0748,1.0768,1.0811,1.0802,1.0769,1.0839,1.0861,1.0872,1.0848,1.0914,1.0980,1.0960,1.1004,1.1017,1.1011,1.1059,1.0999,1.0975)
n<-length(mostra)
##### APARTAT A #####
# Mitjançant uns càlculs podem veure que l'estimador de màxima versemblança
# de mu és mu_hat=mean(y) i alpha_hat=n*mu_hat^2/sum((y_i-mu_hat)^2/y_i).
# Considerem ara les dades i fem els càlculs corresponents.
mu_hat<-mean(mostra)
mu_hat
# Calculem alpha
mostra_tilde<-n/sum(1/mostra)
alpha_hat<-1/(1/mostra_tilde-1/mean(mostra))
alpha_hat
# Comprovem
#install.packages("statmod")
library(statmod)
mostra_2<-rinvgauss(n, mean = mu_hat, shape = alpha_hat)
mostra_2
mean(mostra_2)
# Veiem que està molt bé.
##### APARTAT B #####
density(mostra)
hist(mostra)
par(new=TRUE)
plot(density(mostra))
#####################
# Fem un altre exemple que no està a l'examen per mostrar el que fem.
set.seed(1)
mostra<-rnorm(1000)
hist(mostra)
par(new=TRUE)
plot(density(mostra))
