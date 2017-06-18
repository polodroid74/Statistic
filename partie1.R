#TP partie 1

#question7
t<-sample(1:1000,20,replace=T)
tank<-sort(t)
n<-length(tank) # on ordonne les valeurs, pour tracer le graphe de probabilités
#on prend 5 classe (règle de Sturges)
a0<-tank[1]-0.025*(tank[n]-tank[1])
a5<-tank[n]+0.025*(tank[n]-tank[1])
h<-(a5-a0)/5
bornes<-seq(a0,a5,h)
par(mfcol=c(1,2))
hist(tank, prob=T, breaks=bornes)
plot(tank,seq(1:n)/n)
reg<-lm((seq(1:n)/n)~tank) # on fait une régression linéaire pour tracer une droite la plus proche des valeurs

a<-reg$coefficient[1]
b<-reg$coefficient[2]
abline(a,b)

#calcul des estimateurs

#θ graphique (estimé par le graphe de probabilités)
θg<-1/b
print( θg)

#θ à partir de la moyenne empirique
θ_tilde <-2*mean(tank)-1
print(θ_tilde)

#θ à partir de la médiane empirique
θ_tilde_prime <-2*median(tank)-1
print(θ_tilde_prime)

#θ maximum des observations
θ_chapeau<-tank[n]
print(θ_chapeau)

#θ à partir de la variance minimale
θ_varmin<-(tank[n]^(n+1)-(tank[n]-1)^(n+1))/(tank[n]^n-(tank[n]-1)^n)
print(θ_varmin)

# moyenne des différents estimateurs
θ_moyen<-(θg+θ_tilde+θ_tilde_prime+θ_chapeau+θ_varmin)/5
print(θ_moyen)

#Question 8
m<-1000 # nombre d'échantillon
j<-100 # taille de l'échantillon
# on crée des tableaux qui vont contenir les valeurs des estimateurs pour les différents échantillons
tabg<-seq(1,m,1)
tabtilde<-seq(1,m,1)
tabtildeprime<-seq(1,m,1)
tabchapeau<-seq(1,m,1)
tabvarmin<-seq(1,m,1)

# on remplit les tableaux avec les estimateurs calculés pour chaque échantillon
for (i in 1:m) {
  t<-sample(1:1000,j,replace=T)
  tank<-sort(t)
  
  reg<-lm((seq(1:j)/j)~tank)
  tabg[i]<-1/(reg$coefficient[2])
  
  tabtilde[i]<-2*mean(tank)-1
  
  tabtildeprime[i]<-2*median(tank)-1
  
  tabchapeau[i]<-tank[j]
  
  tabvarmin[i]<-((tank[j]^(j+1))-(tank[j]-1)^(j+1))/((tank[j]^j)-(tank[j]-1)^j)
}

#calcul des estimateurs

#θ graphique (graphe de probabilités)
θg<-mean(tabg)
biaisg<-abs(1000-θg)
tabg<-(tabg-1000)^2
EQMθg <-mean(tabg)
print(θg)
print(biaisg)
print(EQMθg)

#θ à partir de la moyenne empirique
θ_tilde<-mean(tabtilde)
biais_tilde<-abs(1000-θ_tilde)
tabtilde<-(tabtilde-1000)^2
EQMθ_tilde<-mean(tabtilde)
print(θ_tilde)
print(biais_tilde)
print(EQMθ_tilde)

#θ à partir de la médiane empirique
θ_tilde_prime<-mean(tabtildeprime)
biais_tilde_prime<-abs(1000-θ_tilde_prime)
tabtildeprime<-(tabtildeprime-1000)^2
EQMθ_tilde_prime<-mean(tabtildeprime)
print(θ_tilde_prime)
print(biais_tilde_prime)
print(EQMθ_tilde_prime)

#θ maximum des observations
θ_chapeau<-mean(tabchapeau)
biais_chapeau<-abs(1000-θ_chapeau)
tabchapeau<-(tabchapeau-1000)^2
EQMθ_chapeau<-mean(tabchapeau)
print(θ_chapeau)
print(biais_chapeau)
print(EQMθ_chapeau)

#θ à partir de la variance minimale
θ_varmin<-mean(tabvarmin)
biais_varmin<-abs(1000-θ_varmin)
tabvarmin<-(tabvarmin-1000)^2
EQMθ_varmin<-mean(tabvarmin)
print(θ_varmin)
print(biais_varmin)
print(EQMθ_varmin)


# Tracer de courbes: biais des différents estimateurs en fonction de la taille de l'échantillon
# Et EQM des différents estimateurs en fonction de la taille de l'échantillon
m<-100 # nombre d'échantillon

tabg<-seq(1,m,1)
tabtilde<-seq(1,m,1)
tabtildeprime<-seq(1,m,1)
tabchapeau<-seq(1,m,1)
tabvarmin<-seq(1,m,1)
# création des tableaux pour contenir les biais pour chaque échantillon
tab_biais_g<-seq(1,100,1)
tab_biais_tilde<-seq(1,100,1)
tab_biais_tilde_prime<-seq(1,100,1)
tab_biais_chapeau<-seq(1,100,1)
tab_biais_varmin<-seq(1,100,1)
# création des tableaux pour contenir les EQM pour chaque échantillon
tab_EQM_g<-seq(1,100,1)
tab_EQM_tilde<-seq(1,100,1)
tab_EQM_tilde_prime<-seq(1,100,1)
tab_EQM_chapeau<-seq(1,100,1)
tab_EQM_varmin<-seq(1,100,1)

# on fixe les premières valeurs pour éviter quelles soient trop importantes et "écrasent" les courbes sur le graphique
for (i in 1:20){ 
tab_biais_g[i]<-40
tab_biais_tilde[i]<-40
tab_biais_tilde_prime[i]<-40
tab_biais_chapeau[i]<-40
tab_biais_varmin[i]<-40
}
# calcul du biais et de l'EQM pour chaque estimateur: on calcul sur des moyennes de 100 échantillons
# pour des échantillons de taille 21 à 100
for (i in 21:100){
  
  for (k in 1:m){
    t<-sample(1:1000,i,replace=T)
    tank<-sort(t)
    
    reg<-lm((seq(1:i)/i)~tank)
    tabg[k]<-1/(reg$coefficient[2])
    
    tabtilde[k]<-2*mean(tank)-1
    
    tabtildeprime[k]<-2*median(tank)-1
    
    tabchapeau[k]<-tank[i]
    
    tabvarmin[k]<-((tank[i]^(i+1))-(tank[i]-1)^(i+1))/((tank[i]^i)-(tank[i]-1)^i)
  }
  
  tab_biais_g[i]<-abs(1000-mean(tabg))
  tabg<-(tabg-1000)^2
  tab_EQM_g[i]<-mean(tabg)
  
  tab_biais_tilde[i]<-abs(1000-mean(tabtilde))
  tabtilde<-(tabtilde-1000)^2
  tab_EQM_tilde[i]<-mean(tabtilde)
  
  tab_biais_tilde_prime[i]<-abs(1000-mean(tabtildeprime))
  tabtildeprime<-(tabtildeprime-1000)^2
  tab_EQM_tilde_prime[i]<-mean(tabtildeprime)
  
  tab_biais_chapeau[i]<-abs(1000-mean(tabchapeau))
  tabchapeau<-(tabchapeau-1000)^2
  tab_EQM_chapeau[i]<-mean(tabchapeau)
  
  tab_biais_varmin[i]<-abs(1000-mean(tabvarmin))
  tabvarmin<-(tabvarmin-1000)^2
  tab_EQM_varmin[i]<-mean(tabvarmin)
  
  x<-seq(1:100)
  # tracer des 2 courbes côte à côte
  par(mfcol=c(2,1))
  plot(x,tab_biais_g,col="red",type="l")
  lines(x,tab_biais_tilde,col="blue")
  lines(x,tab_biais_tilde_prime,col="green")
  lines(x,tab_biais_chapeau,col="yellow")
  lines(x,tab_biais_varmin,col="purple")
  
  plot(x,tab_EQM_g,col="red",type="l",ylim = c(0,20000))
  lines(x,tab_EQM_tilde,col="blue")
  lines(x,tab_EQM_tilde_prime,col="green")
  lines(x,tab_EQM_chapeau,col="yellow")
  lines(x,tab_EQM_varmin,col="purple")
}


#question 10

tetha=1000
n=200 # taille des échantillons
m=100 # nombre d'échantillons
verif=0 # compteur
alpha=0.05 # seuil

  for (i in 1:m){
  
  	t<-sample(1:tetha,n,replace=T)
  
  	moyenne=mean(t)
  	tetha_n_tilde=2*moyenne-1
  	epsilon=sqrt(((qnorm(1-alpha/2)^2)/(3*n))*(tetha_n_tilde^2-1))
  	tetha_n_tilde=2*moyenne-1
  	print(epsilon)
  	if( (tetha >=tetha_n_tilde-epsilon) && (tetha <=tetha_n_tilde+epsilon)){
  		verif=verif + 1
  	}
  }
  print(verif/m * 100) # pourcentage de fois où le paramètre theta est dans l'intervalle de confiance



