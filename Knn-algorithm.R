wine <- read.csv("BlogFeedback\\wine1.csv", header = FALSE)

colnames(wine) <- c(
"ClassId",
"Alcohol",
"Malic acid",
"Ash",
"Alcalinity of ash",  
"Magnesium",
"Total phenols",
"Flavanoids",
"Nonflavanoid phenols",
"Proanthocyanins",
"Color intensity",
"Hue",
"OD280/OD315",
"Proline"
)


summary(wine)

sapply(wine, sd)

install.packages("moments")
library(moments)
skew <- apply(wine[1:14], 2, skewness)
print(skew)

correlations <- cor(wine[1:14])
print(correlations)

#Wykresy


sample<-wine[1:14]
par(mfrow=c(3,5))
for(i in 1:14) {
  hist(sample[,i], main=names(sample)[i])
}

par(mfrow=c(3,5))
for(i in 1:14) {
  plot(density(sample[,i]), main=names(sample)[i])
}


par(mfrow=c(3,5))
for(i in 1:14) {
  boxplot(sample[,i], main=names(sample)[i])
}

install.packages("corrplot")
library(corrplot)
par(mfrow=c(1,1))
corrplot(correlations, method="circle")

str(wine)

#wine <- wine[-1]

table(wine$ClassId)



#wine$ClassId <- ifelse(wine$ClassId <= 1, 'low', ifelse(wine$ClassId <= 2, 'medium', 'high'))

wine$ClassId <- factor(wine$ClassId, levels = c(1, 2, 3), labels = c("1", "2", "3"))
round(prop.table(table(wine$ClassId)) * 100, digits = 1)

summary(wine[2:4])

normalize <- function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}
  


normalize(c(1,2,3,4,5))

normalize(c(10,20,30,40,50))

wine_n <- as.data.frame(lapply(wine[2:14], normalize))

summary(wine_n$Alcohol)

wine_train <- wine_n[1:118, ]
wine_test <- wine_n[119:178, ]

wine_train_labels <- wine[1:118, 1]
wine_test_labels <- wine[119:178, 1]

library(class)
library(gmodels)
wine_test_pred <- knn(train=wine_train, test=wine_test, cl=wine_train_labels, k=5)
CrossTable(x=wine_test_labels, y=wine_test_pred, prop.chisq=FALSE)

wine_z <- as.data.frame(scale(wine[-1]))
summary(wine_z$Alcohol)

wine_train <- wine_z[1:118, ]
wine_test <- wine_z[119:178, ]

#print("z tymi danymi atrybuty działają poprawnie")
wine_train <- wine_z[61:178, ]
wine_test <- wine_z[1:60, ]

wine_train_labels <- wine[61:178, 1]
wine_test_labels <- wine[1:60, 1]

print("========================================================================")
print("================================ KNN = 1 ===============================")
print("========================================================================")
wine_test_pred <- knn(train=wine_train, test=wine_test, cl=wine_train_labels, k=1)
CrossTable(x=wine_test_labels, y=wine_test_pred, prop.chisq=FALSE)
cm <- confusionMatrix(wine_test_labels, wine_test_pred)
cm
tab <- cm$table
TP <- sum(tab[1,1])
TP
FN <- sum(tab[2:3,1])
FN
FP <- sum(tab[1,2:3])
FP
TN <- sum(tab[2:3,2:3])
TN
accuracy <- (TP+TN) / (TP+FN+FP+TN)
accuracy
TPR <- TP / (TP+FN)
TPR
SPEC = TN / (TN+FP)
SPEC
FPR <- FP/ (FP+TN)
FPR
FDR <- FP / (FP+TP)
FDR
POS <- TP/(TP+FP)
POS
NEG <- TN/(TN+FN)
NEG
F1 <-TP / (TP+0.5*(FP+FN))
F1
FBETA<-(1+(0.5^2)) * ((TPR*POS)/(((0.5^2)*TPR)+POS))
FBETA
MCC <- (TP*TN - FP*FN) / (sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))
MCC

print("========================================================================")
print("================================ KNN = 2 ===============================")
print("========================================================================")

wine_test_pred <- knn(train=wine_train, test=wine_test, cl=wine_train_labels, k=2)
CrossTable(x=wine_test_labels, y=wine_test_pred, prop.chisq=FALSE)
cm <- confusionMatrix(wine_test_labels, wine_test_pred)
cm
tab <- cm$table
TP <- sum(tab[1,1]) 
FN <- sum(tab[2:3,1])
FP <- sum(tab[1,2:3])
TN <- sum(tab[2:3,2:3])
accuracy <- (TP+TN) / (TP+FN+FP+TN)
accuracy
TPR <- TP / (TP+FN)
TPR
SPEC = TN / (TN+FP)
SPEC
FPR <- FP/ (FP+TN)
FPR
FDR <- FP / (FP+TP)
FDR
POS <- TP/(TP+FP)
POS
NEG <- TN/(TN+FN)
NEG
F1 <-TP / (TP+0.5*(FP+FN))
F1
FBETA<-(1+(0.5^2)) * ((TPR*POS)/(((0.5^2)*TPR)+POS))
FBETA
MCC <- (TP*TN - FP*FN) / (sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))
MCC
print("========================================================================")
print("================================ KNN = 3 ==============================")
print("========================================================================")
wine_test_pred <- knn(train=wine_train, test=wine_test, cl=wine_train_labels, k=3)
CrossTable(x=wine_test_labels, y=wine_test_pred, prop.chisq=FALSE)

cm <- confusionMatrix(wine_test_labels, wine_test_pred)
cm
tab <- cm$table
TP <- sum(tab[1,1]) 
FN <- sum(tab[2:3,1])
FP <- sum(tab[1,2:3])
TN <- sum(tab[2:3,2:3])
accuracy <- (TP+TN) / (TP+FN+FP+TN)
accuracy
TPR <- TP / (TP+FN)
TPR
SPEC = TN / (TN+FP)
SPEC
FPR <- FP/ (FP+TN)
FPR
FDR <- FP / (FP+TP)
FDR
POS <- TP/(TP+FP)
POS
NEG <- TN/(TN+FN)
NEG
F1 <-TP / (TP+0.5*(FP+FN))
F1
FBETA<-(1+(0.5^2)) * ((TPR*POS)/(((0.5^2)*TPR)+POS))
FBETA
MCC <- (TP*TN - FP*FN) / (sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))
MCC
print("========================================================================")
print("================================ KNN = 5 ===============================")
print("========================================================================")
wine_test_pred <- knn(train=wine_train, test=wine_test, cl=wine_train_labels, k=5)
CrossTable(x=wine_test_labels, y=wine_test_pred, prop.chisq=FALSE)

cm <- confusionMatrix(wine_test_labels, wine_test_pred)
cm
tab <- cm$table
TP <- sum(tab[1,1]) 
FN <- sum(tab[2:3,1])
FP <- sum(tab[1,2:3])
TN <- sum(tab[2:3,2:3])
accuracy <- (TP+TN) / (TP+FN+FP+TN)
accuracy
TPR <- TP / (TP+FN)
TPR
SPEC = TN / (TN+FP)
SPEC
FPR <- FP/ (FP+TN)
FPR
FDR <- FP / (FP+TP)
FDR
POS <- TP/(TP+FP)
POS
NEG <- TN/(TN+FN)
NEG
F1 <-TP / (TP+0.5*(FP+FN))
F1
FBETA<-(1+(0.5^2)) * ((TPR*POS)/(((0.5^2)*TPR)+POS))
FBETA
MCC <- (TP*TN - FP*FN) / (sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))
MCC
print("========================================================================")
print("================================ KNN = 7 ===============================")
print("========================================================================")
wine_test_pred <- knn(train=wine_train, test=wine_test, cl=wine_train_labels, k=7)
CrossTable(x=wine_test_labels, y=wine_test_pred, prop.chisq=FALSE)

cm <- confusionMatrix(wine_test_labels, wine_test_pred)
cm
tab <- cm$table
TP <- sum(tab[1,1]) 
FN <- sum(tab[2:3,1])
FP <- sum(tab[1,2:3])
TN <- sum(tab[2:3,2:3])
accuracy <- (TP+TN) / (TP+FN+FP+TN)
accuracy
TPR <- TP / (TP+FN)
TPR
SPEC = TN / (TN+FP)
SPEC
FPR <- FP/ (FP+TN)
FPR
FDR <- FP / (FP+TP)
FDR
POS <- TP/(TP+FP)
POS
NEG <- TN/(TN+FN)
NEG
F1 <-TP / (TP+0.5*(FP+FN))
F1
FBETA<-(1+(0.5^2)) * ((TPR*POS)/(((0.5^2)*TPR)+POS))
FBETA
MCC <- (TP*TN - FP*FN) / (sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))
MCC
print("========================================================================")
print("================================ KNN = 10 ==============================")
print("========================================================================")
wine_test_pred <- knn(train=wine_train, test=wine_test, cl=wine_train_labels, k=10)
CrossTable(x=wine_test_labels, y=wine_test_pred, prop.chisq=FALSE)

cm <- confusionMatrix(wine_test_labels, wine_test_pred)
cm
tab <- cm$table
TP <- sum(tab[1,1]) 
FN <- sum(tab[2:3,1])
FP <- sum(tab[1,2:3])
TN <- sum(tab[2:3,2:3])
accuracy <- (TP+TN) / (TP+FN+FP+TN)
accuracy
TPR <- TP / (TP+FN)
TPR
SPEC = TN / (TN+FP)
SPEC
FPR <- FP/ (FP+TN)
FPR
FDR <- FP / (FP+TP)
FDR
POS <- TP/(TP+FP)
POS
NEG <- TN/(TN+FN)
NEG
F1 <-TP / (TP+0.5*(FP+FN))
F1
FBETA<-(1+(0.5^2)) * ((TPR*POS)/(((0.5^2)*TPR)+POS))
FBETA
MCC <- (TP*TN - FP*FN) / (sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))
MCC


