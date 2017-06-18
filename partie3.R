iPhones<-read.table("iPhones.csv", sep=";", header=T)
names(iPhones)
attach(iPhones)

# Question 1
correspondanceTAC<-data.frame(numeroTAC=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),row.names=c("161200","161300","161400","171200","171300","171400","174200","174300","174400","177100","177300","177400","177500","177600","180900"))
NS<-seq(1,139,1)

# calcul des numéros de série que l'on stocke dans NS
for (i in 1:139) {
  IMEI<-as.character(iPhones[i,1])
  substring(IMEI,14,14)<-"0"
  SNR<-substring(IMEI, 9, 14)
  codeTAC<-substring(IMEI, 3, 8)
  TAC<-correspondanceTAC[codeTAC,1]
  NS[i]<-(TAC - 1)*10^6 + as.numeric(SNR)
}
max = NS[139]


# Question 2
# Calcul par un estimateur: on choisit teta_chapeau_1 car la partie 2 nous montre que c'est un bon estimateur pour un tirage sans remise
teta_chapeau_1 = (140/139)*max - 1
print(teta_chapeau_1)

# Question 3
# le tableau premier_par_periode contient les numéros de série du premier IPhone de chaque période
premier_par_periode<-seq(1,8,1)

# le tableau dernier_par_periode contient les numéros de série du dernier IPhone de chaque période
dernier_par_periode<-seq(1,8,1)

i<-1 # i sert à parcourir les numéros de série
premier_par_periode[1]<-1
PC<-as.character(iPhones[i,2])
PC<-as.numeric(substring(PC, 4, 5))

while (PC<29) {
  i<-i+1
  PC<-as.character(iPhones[i,2])
  PC<-as.numeric(substring(PC, 4, 5))
}
# lorsqu'on s'arrête, NS[i] contient la valeur du premier IPhone de la période suivante
# et donc NS[i-1] contient le dernier IPhone de la période terminée
dernier_par_periode[1]<-NS[i-1]
premier_par_periode[2]<-NS[i]

while (PC<33) {
  i<-i+1
  PC<-as.character(iPhones[i,2])
  PC<-as.numeric(substring(PC, 4, 5))
}
dernier_par_periode[2]<-NS[i-1]
premier_par_periode[3]<-NS[i]

while (PC<37) {
  i<-i+1
  PC<-as.character(iPhones[i,2])
  PC<-as.numeric(substring(PC, 4, 5))
}
dernier_par_periode[3]<-NS[i-1]
premier_par_periode[4]<-NS[i]

while (PC<41) {
  i<-i+1
  PC<-as.character(iPhones[i,2])
  PC<-as.numeric(substring(PC, 4, 5))
}
dernier_par_periode[4]<-NS[i-1]
premier_par_periode[5]<-NS[i]

while (PC<45) {
  i<-i+1
  PC<-as.character(iPhones[i,2])
  PC<-as.numeric(substring(PC, 4, 5))
}
dernier_par_periode[5]<-NS[i-1]
premier_par_periode[6]<-NS[i]

while (PC<49) {
  i<-i+1
  PC<-as.character(iPhones[i,2])
  PC<-as.numeric(substring(PC, 4, 5))
}
dernier_par_periode[6]<-NS[i-1]
premier_par_periode[7]<-NS[i]

# la condition est PC > 2 car il n'y a pas d'IPhones en semaine 1 dans les données
while (PC > 02){
  i<-i+1
  PC<-as.character(iPhones[i,2])
  PC<-as.numeric(substring(PC, 4, 5))
}

dernier_par_periode[7]<-NS[i-1]
premier_par_periode[8]<-NS[i]
dernier_par_periode[8]<-NS[139]

produits_par_periode<-seq(1,8,1)
for (i in 1:8){
  produits_par_periode[i]<-(140/139)*dernier_par_periode[i] - premier_par_periode[i]
}

# vérification des tableaux
print(premier_par_periode)
print(dernier_par_periode)
print(produits_par_periode)

# tracé du nombre d'IPhone par période
periode<-seq(1:8)
plot(periode,produits_par_periode,type="o")