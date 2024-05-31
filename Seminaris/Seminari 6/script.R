######### SEMINARI 6: CONTRAST D'HIPÒTESIS #########
# Assumint que ja sabem fer tests de LRT i de Wald
############# EXERCICI 1 ############
rm(list = ls())
mostra<-c(rep(1,71),rep(2,28),rep(3,5),rep(4,2),rep(5,2),rep(6,1))
mostra
# Ens diuem que les dades provenen d'una distribució geomètrica.
# I cal fer un test d'hipòtesis per comprovar si la mitjana d'arbres malalts
# és de 3/5 amb una significació del 95%. És a dir, H0: p0=3/5 H1: p0\neq3/5.
# Calculant la funció log-versemblança
l<-function(p,x) {
  return(length(x)*(log(p/(1-p)))+sum(x)*log(1-p))
}
# Sabem a més que l'estadístic de màxima versemblança de p és
p_hat<-1/mean(mostra)
##################### APARTAT A #####################
p0<-3/5
# Calculem l'estadístic
2*(l(p_hat,mostra)-l(p0,mostra))
# Els quantils de la chi-sq
qchisq(0.025, 1)
qchisq(0.975, 1)
# Com el meu estadístic està dins l'interval de confiança del 95%.
# Podem afirmar amb una confiança del 95% que la proporció d'arbre 
# malalts és de 3/5.
#####################################################
##################### APARTAT B #####################
# Fent servir el test de Wald i sabem que el nombre de graus de
# llibertat és 1, tenim.
I<-function(p,n) {
  return(n/(p^2-p^3))
}
W<-(p_hat-p0)^2*I(p_hat,length(mostra))
W
# Amb Wald, observem el nou valor de l'estadístic
qchisq(0.025,1)
qchisq(0.975,1)
#####################################################
##################### APARTAT C #####################
# Fem un interval de confiança del 95% per p utilitzant Wald.
J<-function(p,x) {
  return(length(x)/p^2+(sum(x)-length(x))/(1-p)^2)
}
J_hat<-J(p_hat,mostra)
J_hat
I_hat<-1/J_hat
A<-p_hat-2*sqrt(I_hat)
B<-p_hat+2*sqrt(I_hat)
A
B
3/5
#####################################################
############# EXERCICI 2 ############
goles<-read.table("goles.txt")
goles



# Carreguem les dades
lligues=read.csv2("http://mat.uab.cat/~mbarcelona/est/goles.csv", header=T)
gols<-seq(0,10)

#log versemblança per Poisson
logLP<-function(y,l){x=rep(gols,y);sum(x)*log(l)-length(x)*l - sum(log(factorial(x)))}

#el MLE de lambda per Poisson és la mitjana, calculem la mitjana de cada lliga
hatl<-apply(lligues[2:6],2,function(x){sum(gols*x)/sum(x)})

#si forcem que totes les lambdes siguin iguals, la lambda òptima és la mitjana conjunta de gols de totes les lligues
#com que no totes les lligues juguen el mateix nombre de partits, una manera de calcular és

hatl0<-sum(apply(lligues,2,sum)[2:6]*hatl)/sum(apply(lligues,2,sum)[2:6])

#també podem fer el càlcul "a mà"
partitsperlliga<-apply(lligues,2,sum)[2:6]
hatl0ma<-(2.57*380+2.17*380+2.91*306+2.53*380+2.58*380)/(380*4+306)



#calculem l'estadístic
lambda<-(2)*
  (logLP(lligues[,2],hatl[1]) -logLP(lligues[,2],hatl0) +
     logLP(lligues[,3],hatl[2]) -logLP(lligues[,3],hatl0) +
     logLP(lligues[,4],hatl[3]) -logLP(lligues[,4],hatl0) +
     logLP(lligues[,5],hatl[4]) -logLP(lligues[,5],hatl0) +
     logLP(lligues[,6],hatl[5]) -logLP(lligues[,6],hatl0))

#la dimensió de H0 és 1 i la total és 5, per tant p=4. calculem el p valor
1-pchisq(lambda,4)
# molt petit