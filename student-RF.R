
install.packages("FactoMineR")
library(FactoMineR)

library(randomForest)

install.packages("ade4")
library(ade4)
install.packages("caret")
library(caret)
install.packages("data.table")
library(data.table)


install.packages("DMwR")

library(rminer)

library(randomForest)
library(DMwR)
library(TTR)
library(quantmod)
library(caret)
library(e1071)
library(class)


#SVM for Portuguese test score
d_portuguese=read.table("student-por.csv",sep=";",header=TRUE)
print(nrow(d_portuguese)) # 382 students
#summary(d_portuguese)

#One Hot encoding of multiclass variables
nominal_variables = c('Mjob', 'Fjob', 'reason', 'guardian')
for (f in nominal_variables){
  df_all_dummy = acm.disjonctif(d_portuguese[f])
  d_portuguese[f] = NULL
  d_portuguese = cbind(d_portuguese, df_all_dummy)
}
#summary(d_portuguese)

#BINARY CLASSIFICATION
d_portuguese_binary = d_portuguese
#We binarize G3 (G3 = 1 if pupil score > 9, 0 else)
d_portuguese_binary$G3 <- ifelse(d_portuguese$G3 > 9,1,0)
#We must factorize G3
d_portuguese_binary[["G3"]] = factor(d_portuguese_binary[["G3"]])
#summary(d_portuguese_binary)

set.seed(3333)
intrain <- createDataPartition(y = d_portuguese_binary[["G3"]], p= 0.7, list = FALSE)
training <- d_portuguese_binary[intrain,]
testing <- d_portuguese_binary[-intrain,]

set.seed(1234)


test = d_portuguese_binary[colnames(d_portuguese_binary)!="y"]
test = test[colnames(test)!="y1"]

rf = randomForest(G3 ~ ., ntree = 500, data = test)

output = rf$importance

print(rf)







