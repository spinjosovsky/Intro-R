



#dossier de travail
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#base d'apprentissage
image_train <- read.table("../input/segmentation.train.txt",sep=",", header=TRUE) 
image_train$Base <- as.factor("train")

#base de test
image_test <- read.table("../input/segmentation.test.txt",sep=",", header=TRUE) 
image_test$Base <- as.factor("test")

#mettre la base de train et test ensemble
image_data <- rbind(image_train, image_test)

#Summary de tout la base
print(summary(image_data))


#Repartition de la cible sur test et train
print(summary(image_train$REGION.TYPE)) 
print(summary(image_test$REGION.TYPE))



#fonction d'évaluation
error_rate <- function(yobs,ypred)
{
  mc <- table(yobs,ypred) #taux d'erreur
  err <- 1.0 - sum(diag(mc))/sum(mc) 
  return(err)
}



#installation du package arbre de décision 
install.packages("rpart")
library(rpart)

#arbre de decision
image_train$Base <- NULL
image_test$Base <- NULL
arbre_1 <- rpart(REGION.TYPE ~ ., data = image_train) 
print(arbre_1)


#prédiction sur échantillon test
pred_1 <- predict(arbre_1,newdata=image_test,type="class")

#taux d'erreur 
print(error_rate(image_test$REGION.TYPE,pred_1))


#---------------------------------------------------------------------------


#---------------------
#   décision stump
#---------------------
param_stump = rpart.control(cp=0,maxdepth=1,minsplit=2,minbucket=1) 
arbre_2 <- rpart(REGION.TYPE ~ ., data = image_train,control=param_stump) 
print(arbre_2)

#prédiction et taux d'erreur
pred_2 <- predict(arbre_2,newdata=image_test,type="class") 
print(error_rate(image_test$REGION.TYPE,pred_2))



#---------------------
#   arbre profond
#---------------------
param_deep = rpart.control(cp=0,maxdepth=30,minsplit=2,minbucket=1) 
arbre_3 <- rpart(REGION.TYPE ~ ., data = image_train,control=param_deep)

#prédiction et taux d'erreur
pred_3 <- predict(arbre_3,newdata=image_test,type="class") 
print(error_rate(image_test$REGION.TYPE,pred_3))





#librairie adabag 
install.packages("adabag") #lancer juste une fois
library(adabag)

#---------------------
#       Bagging
#---------------------
bag_1 <- bagging(REGION.TYPE ~ ., data = image_train, mfinal=20)

#prédiction
predbag_1 <- predict(bag_1,newdata = image_test)

#taux d'erreur 
print(error_rate(image_test$REGION.TYPE,predbag_1$class))



#premier arbre 
print(bag_1$trees[[1]])


#importance 
print(sort(bag_1$importance,decreasing=TRUE))


#sortie graphique 
importanceplot(bag_1,cex.names=0.5,horiz=TRUE)


#-----------------------------------
#       Bagging de decision stump
#------------------------------------
#prédiction
bag_stump <- bagging(REGION.TYPE~.,data=image_train,mfinal=20,control=param_stump) 
predbag_stump <- predict(bag_stump,newdata = image_test) 

#taux d'erreur 
print(error_rate(image_test$REGION.TYPE,predbag_stump$class))


#-----------------------------------
#      Bagging arbre plus profond
#------------------------------------
#prédiction
bag_deep <- bagging(REGION.TYPE~.,data=image_train,mfinal=20, control=param_deep)
predbag_deep <- predict(bag_deep,newdata = image_test) 

#taux d'erreur
print(error_rate(image_test$REGION.TYPE,predbag_deep$class))




#-----------------------------------
#     Jouer sur le noùbre d'arbres
#------------------------------------
#nombre m d'arbres à tester 
m_a_tester <- c(1,5,10,20,50,100,200)

#apprentissage-test 
train_test_bag <- function(m)
{
    bag <- bagging(REGION.TYPE ~ .,data=image_train,mfinal=m,control=param_deep) 
    predbag <- predict(bag,newdata = image_test) 
    return(error_rate(image_test$REGION.TYPE,predbag$class))
}

#évaluation 20 fois de chaque valeur de m
result <- replicate(20,sapply(m_a_tester,train_test_bag))

#graphique, abscisse : m, ordonnée : moyenne des erreurs pour chaque m 
plot(m_a_tester,apply(result,1,mean),xlab="m",ylab="Err. rate",type="b")





#-----------------------------------
#          Random Forest
#-----------------------------------
install.packages("randomForest")
library(randomForest)

#prédiction
rf_1 <- randomForest(REGION.TYPE ~ ., data = image_train, ntree = 20)
predrf_1 <- predict(rf_1,newdata=image_test,type="class")

#erreur en test 
print(error_rate(image_test$REGION.TYPE,predrf_1))


#accès au 1er arbre 
print(getTree(rf_1,1))


#apparition des variables 
print(data.frame(cbind(colnames(image_train)[2:20],varUsed(rf_1))))

#importance des variables 
varImpPlot(rf_1)



#nombre m d'arbres à tester 
m_a_tester <- c(1,5,10,20,50,100,200)

#apprentissage-test 
train_test_rf <- function(m)
{
    rf <- randomForest(REGION.TYPE ~ .,data=image_train,ntree=m) 
    predrf <- predict(rf,newdata = image_test) 
    return(error_rate(image_test$REGION.TYPE,predrf))
}

#évaluation 20 fois de chaque valeur de m
result <- replicate(20,sapply(m_a_tester,train_test_rf))

#graphique
plot(m_a_tester,apply(result,1,mean),xlab="m",ylab="Err. rate",type="b")



#-----------------------------------
#          Boosting
#-----------------------------------
#prédiction
bo_1 <- boosting(REGION.TYPE ~ ., data = image_train,mfinal=20, boos=FALSE)
predbo_1 <- predict(bo_1,newdata = image_test)

#taux d'erreur 
print(error_rate(image_test$REGION.TYPE,predbo_1$class))



#nombre m d'arbres à tester 
m_a_tester <- c(1,5,10,20,50,100,200)

#apprentissage-test 
train_test_boosting <- function(m)
{
    bo <- boosting(REGION.TYPE ~ .,data=image_train,mfinal=m,coeflearn='Zhu') 
    predbo <- predict(bo,newdata = image_test) 
    return(error_rate(image_test$REGION.TYPE,predbo$class))
}

#évaluation 20 fois de chaque valeur de m
result <- replicate(20,sapply(m_a_tester,train_test_boosting))

#graphique
plot(m_a_tester,apply(result,1,mean),xlab="m",ylab="Err. rate",type="b")




