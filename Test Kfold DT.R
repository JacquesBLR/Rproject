
#install.packages("ade4")
library(ade4)
#install.packages("caret")
library(caret)
#install.packages("data.table")
library(data.table)

library(tree)
library(DMwR)
library(rpart.plot)

set.seed(3333)
d_portuguese=read.table("student-por.csv",sep=";",header=TRUE)
d_portuguese_binary = d_portuguese

d_portuguese_binary$G3 <- ifelse(d_portuguese$G3 > 9,1,0)
d_portuguese_binary[["G3"]] = factor(d_portuguese_binary[["G3"]])

# On retire les variables suivantes des données
d_portuguese_binary[c("G1", "G2", "y", "y1")] = NULL

intrain <- createDataPartition(y = d_portuguese_binary[["G3"]], p= 0.7, list = FALSE)
training <- d_portuguese_binary[intrain,]
testing <- d_portuguese_binary[-intrain,]




# Nombre de valeurs sur lequel on veut tester le paramètre
Val =100

resultats = matrix(0,10,Val)
test = matrix(0,10,Val)

for (n in 1:10){
  # 5-fold cross-validation to select k
  # from the set {1,...,10}

  #Affectation des folds aux observations des données training
  fold = data.frame (sample(rep(1:5,each=91))) # creation des groupes B_v
  
  cvpred = matrix(0,nrow=455,ncol=Val) # initialisation de la matrice des prédicteurs
  
  for (k in 1:Val)
    for (v in 1:5)
    {

      
      sample1 = training[which(fold!=v),1:30] #jeu d'entrainement
      sample2 = training[which(fold==v),1:30] #jeu de test
      
      class1 = data.frame(training[which(fold!=v),31]) #jeu d'entrainement contenant la variable à prédire
      
      # Modèle Youssouf
      #failures+absences+schoolsup+goout+higher+traveltime+famrel+Mjob+Fjob+Medu+Fedu+school+G1+G2
      
      # Modèle d'entrainement
      rt.G3 = rpart(class1[,1]~ ., data = sample1, control=rpart.control(minsplit=k, cp=0))
      
      # prédiction
      rt.predictions.G3 = predict(rt.G3, sample2, type="class")
      
      # Affectation de la prédiction
      cvpred[which(fold==v),k] = rt.predictions.G3 #as.numeric(ifelse(rt.predictions.G3[,2] > 0.5, 1, 0))
      
     
    }

  # Récupération de la variable à prédire du jeu d'entrainement
  class = as.numeric(training[,31])
  
  # Comptage des mauvaises classificationss
  resultats[n,] = apply(cvpred,2,function(x) sum(class!=x)) # calcule l'erreur de classif.
  
  # Rang du résultat k pour l'itération n
  #test[n,]=rank(resultats[n,])
  
  
}   
# Moyenne des résultats k sur les n itérations
meanresult = apply(resultats,2,mean)

meanresult

##############################################################################
# étapes de sélection du meilleur arbre

#rt.G3Simple <- prune(rt.G3,cp=0.035)

#rt.G3Optimal <- prune(rt.G3,cp=rt.G3$cptable[which.min(rt.G3$cptable[,4]),1])

#prp(rt.G3Optimal,extra=1)

#rt.predictions.G3$y = factor(ifelse(rt.predictions.G3$pass > 0.5, 1,0))

#cvpred[which(fold==v),k] = as.numeric(ifelse(rt.predictions.G3[,2] > 0.5, 1, 0))
