m<-1000 # nombre d'échantillons
n<-100 # taille de l'échantillon
# tableaux stockant les valeurs des estimateurs pour les différents échantillons
tab_teta_chapeau<-seq(1,m,1)
tab_teta_chapeau1<-seq(1,m,1)
tab_teta_chapeau2<-seq(1,m,1)
tab_teta_varmin<-seq(1,m,1)

for (i in 1:m) {
  t<-sample(1:1000,n,replace=FALSE)
  tank<-sort(t)
  
  tab_teta_chapeau[i]<-tank[n]
  
  tab_teta_chapeau1[i]<-((n+1)/n)*tank[n]-1
  
  tab_teta_chapeau2[i]<-tank[n]+tank[1]-1
  
  tab_teta_varmin[i]<-((tank[n]^(n+1))-(tank[n]-1)^(n+1))/((tank[n]^n)-(tank[n]-1)^n)
}

#maximum des observations
teta_chapeau<-mean(tab_teta_chapeau)
biais_chapeau<-abs(1000-teta_chapeau)
tab_teta_chapeau<-(tab_teta_chapeau-1000)^2
EQMteta_chapeau<-mean(tab_teta_chapeau)
print(teta_chapeau)
print(biais_chapeau)
print(EQMteta_chapeau)

#teta_chapeau1
teta_chapeau1<-mean(tab_teta_chapeau1)
biais_chapeau1<-abs(1000-teta_chapeau1)
tab_teta_chapeau1<-(tab_teta_chapeau1-1000)^2
EQMteta_chapeau1<-mean(tab_teta_chapeau1)
print(teta_chapeau1)
print(biais_chapeau1)
print(EQMteta_chapeau1)

#teta_chapeau2
teta_chapeau2<-mean(tab_teta_chapeau2)
biais_chapeau2<-abs(1000-teta_chapeau2)
tab_teta_chapeau2<-(tab_teta_chapeau2-1000)^2
EQMteta_chapeau2<-mean(tab_teta_chapeau2)
print(teta_chapeau2)
print(biais_chapeau2)
print(EQMteta_chapeau2)

#teta à partir de la variance minimale
teta_varmin<-mean(tab_teta_varmin)
biais_varmin<-abs(1000-teta_varmin)
tab_teta_varmin<-(tab_teta_varmin-1000)^2
EQMteta_varmin<-mean(tab_teta_varmin)
print(teta_varmin)
print(biais_varmin)
print(EQMteta_varmin)


# Tracer de courbes: biais des différents estimateurs en fonction de la taille de l'échantillon
# Et EQM des différents estimateurs en fonction de la taille de l'échantillon
m<-100 # nombre d'échantillon

tabchapeau<-seq(1,m,1)
tabchapeau1<-seq(1,m,1)
tabchapeau2<-seq(1,m,1)
tabvarmin<-seq(1,m,1)
# tableaux contenant les valeurs de biais pour différents échantillons
tab_biais_chapeau<-seq(1,100,1)
tab_biais_chapeau1<-seq(1,100,1)
tab_biais_chapeau2<-seq(1,100,1)
tab_biais_varmin<-seq(1,100,1)
# tableaux contenant les valeurs de EQM pour différents échantillons
tab_EQM_chapeau<-seq(1,100,1)
tab_EQM_chapeau1<-seq(1,100,1)
tab_EQM_chapeau2<-seq(1,100,1)
tab_EQM_varmin<-seq(1,100,1)
for (i in 1:10){ 
  tab_biais_chapeau[i]<-40
  tab_biais_chapeau1[i]<-40
  tab_biais_chapeau2[i]<-40
  tab_biais_varmin[i]<-40
}
# calcul du biais et de l'EQM pour chaque estimateur: on calcul sur des moyennes de 100 échantillons
# pour des échantillons de taille 11 à 100
for (i in 11:100) {
  
  for (k in 1:m) {
    t<-sample(1:1000,i,replace=FALSE)
    tank<-sort(t)
    
    tabchapeau[k]<-tank[i]
    
    tabchapeau1[k]<-((i+1)/i)*tank[i]-1
    
    tabchapeau2[k]<-tank[i]+tank[1]-1
    
    tabvarmin[k]<-((tank[i]^(i+1))-(tank[i]-1)^(i+1))/((tank[i]^i)-(tank[i]-1)^i)
  }
  
  tab_biais_chapeau[i]<-abs(1000-mean(tabchapeau))
  tabchapeau<-(tabchapeau-1000)^2
  tab_EQM_chapeau[i]<-mean(tabchapeau)
  
  tab_biais_chapeau1[i]<-abs(1000-mean(tabchapeau1))
  tabchapeau1<-(tabchapeau1-1000)^2
  tab_EQM_chapeau1[i]<-mean(tabchapeau1)
  
  tab_biais_chapeau2[i]<-abs(1000-mean(tabchapeau2))
  tabchapeau2<-(tabchapeau2-1000)^2
  tab_EQM_chapeau2[i]<-mean(tabchapeau2)
  
  tab_biais_varmin[i]<-abs(1000-mean(tabvarmin))
  tabvarmin<-(tabvarmin-1000)^2
  tab_EQM_varmin[i]<-mean(tabvarmin)
  
  # tracé des courbes
  x<-seq(1:100)
  par(mfcol=c(2,1))
  plot(x,tab_biais_chapeau,col="red",type="l")
  lines(x,tab_biais_chapeau1,col="blue")
  lines(x,tab_biais_chapeau2,col="yellow")
  lines(x,tab_biais_varmin,col="green")
  
  plot(x,tab_EQM_chapeau,col="red",type="l")
  lines(x,tab_EQM_chapeau1,col="blue")
  lines(x,tab_EQM_chapeau2,col="yellow")
  lines(x,tab_EQM_varmin,col="green")
  
}
