library(DMwR)
data(sales)
#Ajout d'un commentaire
summary(sales)
Nouveaux changements
c(nlevels(sales$ID), nlevels(sales$Prod))

length(which(is.na(sales$Quant) & is.na(sales$Val)))

table(sales$Insp)/nrow(sales) * 100

sales$Uprice <- sales$Val/sales$Quant


attach(sales)
num.prod = as.numeric(table(Prod))
sum(num.prod < 20) # nb de produits ayant <20 transactions
av.uprice = tapply(Uprice, Prod, mean, na.rm=T)

BP.uprice = tapply(Uprice, Prod, function(x) boxplot.stats(x)$stats)

#write.csv(sales,file="/Users/jacquesaguilera/GDrive/Ensae/Scolarité Ensae/Apprentisage Statistique/TP3/datasales.csv")

# C'est la somme des valeurs outliers
n = sum(boxplot.stats(Uprice)$out).
n

#1.
out.uprice = tapply(Uprice, Prod, function(x) boxplot.stats(x)$out)

out.uprice=nlevels(boxplot.stats(Uprice)$out
out.uprice

