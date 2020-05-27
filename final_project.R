# install.packages("keVrnlab")
library(MASS)
library(ggplot2)
library(reshape2)
library(corrplot)
library(e1071)
library(caret)
library(scatterplot3d)
library(glmnet)
library(nnet)
library(NeuralNetTools)
library(wordspace)
library(kernlab)


WD = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(WD)
## Loading the training and test data

train <- read.csv(file = "HAR/HAR_train.csv")
test <- read.csv(file = "HAR/HAR_test.csv")

XTrain <- as.matrix(train[, -c(1,563)])
XTest <- as.matrix(test[, -c(1,563)])

ytrain <- as.factor(train[,563])
ytest <- as.factor(test[,563])

#==================================================
# Vizualizing some of the data
#body acceleration on X Y Z axis

x_acc <- ggplot()+
  geom_histogram(aes(x = train$tBodyAccmeanX[which(ytrain == "LAYING")]),
                 stat = "bin", binwidth = .03)+
  labs(title = "X axis Acceleration : 'Laying'")

y_acc <- ggplot()+
  geom_histogram(aes(x = train$tBodyAccmeanY[which(ytrain == "LAYING")]),
                 stat = "bin", binwidth = .03)+
  labs(title = "Y axis Acceleration : 'Laying'")

z_acc <- ggplot()+
  geom_histogram(aes(x = train$tBodyAccmeanZ[which(ytrain == "LAYING")]),
                 stat = "bin", binwidth = .03)+
  labs(title = "Z axis Acceleration : 'Laying'")

#body gyro on X Y Z axis

x_gyro <- ggplot()+
  geom_histogram(aes(x = train$tBodyGyromeanX[which(ytrain == "LAYING")]),
                 stat = "bin", binwidth = .02)+
  labs(title = "X axis gyro : 'Laying'")
  

y_gyro <- ggplot()+
  geom_histogram(aes(x = train$tBodyGyromeanY[which(ytrain == "LAYING")]),
                 stat = "bin", binwidth = .02)+
  labs(title = "Y axis gyro : 'Laying'")

z_gyro <- ggplot()+
  geom_histogram(aes(x = train$tBodyGyromeanZ[which(ytrain == "LAYING")]),
                 stat = "bin", binwidth = .02)+
  labs(title = "Z axis gyro : 'Laying'")

#now the same thing for walking

x_acc1 <- ggplot()+
  geom_histogram(aes(x = train$tBodyAccmeanX[which(ytrain == "WALKING_UPSTAIRS")]),
                 stat = "bin", binwidth = .02)+
  labs(title = "X axis Acceleration : 'WALKING_UPSTAIRS'")

y_acc1 <- ggplot()+
  geom_histogram(aes(x = train$tBodyAccmeanY[which(ytrain == "WALKING_UPSTAIRS")]),
                 stat = "bin", binwidth = .02)+
  labs(title = "Y axis Acceleration : 'WALKING_UPSTAIRS'")

z_acc1 <- ggplot()+
  geom_histogram(aes(x = train$tBodyAccmeanZ[which(ytrain == "WALKING_UPSTAIRS")]),
                 stat = "bin", binwidth = .02)+
  labs(title = "Z axis Acceleration : 'WALKING_UPSTAIRS'")

#body gyro on X Y Z axis

x_gyro1 <- ggplot()+
  geom_histogram(aes(x = train$tBodyGyromeanX[which(ytrain == "WALKING_UPSTAIRS")]),
                 stat = "bin", binwidth = .02)+
  labs(title = "X axis gyro : 'WALKING_UPSTAIRS'")

y_gyro1 <- ggplot()+
  geom_histogram(aes(x = train$tBodyGyromeanY[which(ytrain == "WALKING_UPSTAIRS")]),
                 stat = "bin", binwidth = .02)+
  labs(title = "Y axis gyro : 'WALKING_UPSTAIRS'")

z_gyro1 <- ggplot()+
  geom_histogram(aes(x = train$tBodyGyromeanZ[which(ytrain == "WALKING_UPSTAIRS")]),
                 stat = "bin", binwidth = .02)+
  labs(title = "Z axis gyro : 'WALKING_UPSTAIRS'")
#=================================================
#Correlation Matrix

corr <- round(cor(XTrain),2)
col1 <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                           "white", "yellow", "#FF7F00", "red", "#7F0000"))
# corr_plot <- corrplot(corr, method = "color", tl.pos = "n", col = col1(100))


#================================================
#Training an SVM classifier using the training data

SVM_model <- svm(XTrain, ytrain, kernel = "radial", cost = 15)
predict_svm <- predict(SVM_model, XTest)
svm_confuse <- confusionMatrix(predict_svm, ytest)

c_vec <- seq(1,50, 1)
errorvec <- c()
for(i in 1:50) {
  svm_train <- svm(XTrain, ytrain, kernel = 'radial', cost = c_vec[i])
  errorvec[i] <- sum(predict(svm_train, XTest) != ytest)
  print(i)
}

plot(x = c_vec, y = errorvec,
     main = "Regularization Parameter vs Error",
     xlab = "Cost",
     ylab = "Error")

#================================================
# vizualizing the 3D data - Acceleration
colors <- c("red", "darkblue", "green", "yellow", "lightblue", "grey")
colors <- colors[ytrain]
s3d <- scatterplot3d(x = train$tBodyAccmeanX, y = train$tBodyAccmeanY,
                     z = train$tBodyAccmeanZ, color = colors,
                     main = "Acceleration: X Y Z", xlab = "X accel",
                     ylab = "Y accel", zlab = "Z accel")
legend(s3d$xyz.convert(-2, 1, 1.1), legend = levels(ytrain),
       col =  c("red", "darkblue", "green", "yellow", "lightblue", "grey"), pch = 16,
       ncol = 2, cex = .5)

# vizualizing the 3D data - Gyro
colors <- c("red", "darkblue", "green", "yellow", "lightblue", "grey")
colors <- colors[ytrain]
s3d1 <- scatterplot3d(x = train$tBodyGyromeanX, y = train$tBodyGyromeanY,
                      z = train$tBodyGyromeanZ, color = colors,
                     main = "Angular Velocity : X Y Z different angle", xlab = "X gyro",
                     ylab = "Y gyro", zlab = "Z gyro", angle = 45)
legend(s3d1$xyz.convert(-2, 1, 1.15), legend = levels(ytrain),
       col =  c("red", "darkblue", "green", "yellow", "lightblue", "grey"), pch = 16,
       ncol = 2, cex = .5)



##=======================================================================
#                            Part 2 of the Final Project
#========================================================================

#computing the principal component analysis, walking upstairs
XTrain.walkup <- XTrain[which(ytrain == "WALKING_UPSTAIRS"),]

pc <- prcomp(XTrain.walkup, center = T, scale = T)


#plot the eigenvectors and their relationship to residual variance
pc.var <- pc$sdev^2
pc.pvar <- pc.var/sum(pc.var)
resvar <- 1 - cumsum(pc.pvar)

plot(resvar[1:100], xlim = c(1,100), ylim = c(0,1), 
     xlab = "Number of Principal Componenets",
     ylab = "Precentage of Residual Variance Not Captured", 
     type = "b", 
     col = "orange", 
     main = "PCA for Walking Upstairs")

#plot the points of the new matrix in the basis of the eigenvectors

up3d <- scatterplot3d(x = pc$x[,1], y = pc$x[,2],
                     z = pc$x[,3], color = "blue",
                     main = "Walking Upstairs Projected in the Eigenvector Basis", 
                     xlab = "PC-1",
                     ylab = "PC-2", 
                     zlab = "PC-3")

#do the same as abovce for the data with label "LAYING"
XTrain.laying <- XTrain[which(ytrain == "LAYING"),]

pc <- prcomp(XTrain.laying, center = T, scale = T)


#plot the eigenvectors and their relationship to residual variance
pc.var <- pc$sdev^2
pc.pvar <- pc.var/sum(pc.var)
resvar <- 1 - cumsum(pc.pvar)

plot(resvar[1:100], xlim = c(1,100), ylim = c(0,1), 
     xlab = "Number of Principal Componenets",
     ylab = "Precentate of Residual Variance Not Captured", 
     type = "b", 
     col = "orange", 
     main = "PCA for Laying")

#plot the points of the new matrix in the basis of the eigenvectors

up3d <- scatterplot3d(x = pc$x[,1],
                      y = pc$x[,2],
                      z = pc$x[,3], 
                      color = "green",
                      main = "Laying Projected in the Eigenvector Basis", 
                      xlab = "PC-1",
                      ylab = "PC-2", zlab = "PC-3")

##===================================================================
#              Performing the PCA on the entire data set

pca <- prcomp(XTrain, center =  T, scale = T)

#Plotting the residual variance in relation to the number of prinicpal components 

pca.var <- pca$sdev^2
pca.pvar <- pca.var/sum(pca.var)
resvar <- 1 - cumsum(pca.pvar)

plot(resvar[1:100], 
     xlim = c(1,100), 
     ylim = c(0,1), 
     xlab = "Number of Principal Componenets",
     ylab = "Precentate of Residual Variance Captured", 
     type = "b", 
     col = "red", 
     main = "PCA for the full HAR dataset")
abline(h = .1)
abline(v = match(max(resvar[which(resvar < .1)]), resvar))

#plot the training data projected onto the first three eigentvector bases
colors <- c("red", "darkblue", "green", "yellow", "lightblue", "grey")
colors <- colors[ytrain]
full3d <- scatterplot3d(x = pca$x[,1], 
                        y = pca$x[,2],
                     z = pca$x[,3], 
                     color = colors,
                     main = "HAR data projected onto the first three Principal Components", 
                     xlab ="PC-1",
                     ylab = "PC-2", 
                     zlab = "PC-3", 
                     pch = 1)

legend(full3d$xyz.convert(-2, 1, -30), 
       legend = levels(ytrain),
       col =  c("red", "darkblue", "green", "yellow", "lightblue", "grey"), 
       pch = 16,
       ncol = 1, cex = .5)
##using the PCA dimesion reduction as input for different classification methods
pc <- prcomp(XTrain, center = T, scale = T)
lr_accruacy <- c()
qda_accuracy <- c()
nnet_accuracy <-  c()
#choose number of components
k = 60
xtrainproj <- data.frame(pc$x[,1:k])

xtestproj <- predict(pc, newdata = XTest)
xtestproj <- as.data.frame(xtestproj[,1:k])

##      Logistic Regression 


myglm <- cv.glmnet(as.matrix(xtrainproj), ytrain, 
                   alpha =1, 
                   family = "multinomial")
ypredstr <- predict(myglm, newx = as.matrix(xtestproj), 
                    s = "lambda.min", 
                    type = "class")
confusionMatrix(as.factor(ypredstr), ytest)
lr_accruacy <- append(lr_accruacy, sum(as.factor(ypredstr) == ytest)/length(ytest))


plot(x = c(30,40,50,60), y = lr_accruacy, 
     type = "l", 
     main = "Logistic regression: Number of Principal Components vs. Accuracy",
     xlab = "Number of Components",
     ylab = "Accuracy")



##Quadratic Discriminant Analysis
#choose number of components
k = 60
xtrainproj <- data.frame(pc$x[,1:k])

xtestproj <- predict(pc, newdata = XTest)
xtestproj <- as.data.frame(xtestproj[,1:k])


qda_pca <- qda(xtrainproj, ytrain)
qda_pred <-predict(qda_pca, xtestproj)
confusionMatrix(as.factor(qda_pred$class), ytest)
qda_accuracy <- append(qda_accuracy, sum(as.factor(qda_pred$class) == ytest)/length(ytest))


plot(x = c(30,40,50,60), y = qda_accuracy, 
     type = "l",
     main = "Quadratic Discriminant Analysis: Principal Components vs. Accuracy",
     xlab = "Number of Components", 
     ylab = "Accuracy")




##Feed forward neural networks

#choose number of components
pc <- prcomp(XTrain, center = T, scale = T)
accuracy <- c()


k = 110
xtrainproj <- data.frame(pc$x[,1:k])

xtestproj <- predict(pc, newdata = XTest)
xtestproj <- as.data.frame(xtestproj[,1:k])


lambda <- .001
hidden_size <- c(6:20)
cross_net <- function(s) {
  model <- nnet(x = as.matrix(xtrainproj), y = class.ind(ytrain),
                size = s,
                maxit = 5000,
                decay = lambda,
                skip = T,
                trace =F,
                MaxNWts = 8000,
                softmax = T)
  nnet_pred <- predict(model, xtestproj)
  nnet_pred <- factor(apply(X = nnet_pred, MARGIN = 1, FUN = which.max))
  levels(nnet_pred) <- levels(ytrain)
  accuracy <- round(sum(nnet_pred == ytest) / length(ytest), digits = 4)
  return(paste(accuracy *100, '%', sep = ""))
}

accuracy_vec <- sapply(hidden_size, cross_net)

nnet_pca <- nnet(x = as.matrix(xtrainproj), y = class.ind(ytrain), 
                 size = 7, 
                 maxit = 5000, 
                 decay = lambda, 
                 skip = T, 
                 trace =F, 
                 MaxNWts = 8000, 
                 softmax = T)

nnet_pred <- predict(nnet_pca, xtestproj)
nnet_pred <- factor(apply(X = nnet_pred, MARGIN = 1, FUN = which.max))
levels(nnet_pred) <- levels(ytrain)
confusionMatrix(as.factor(nnet_pred), ytest)
accuracy <- append(accuracy,round(sum(nnet_pred == ytest) / length(ytest), digits = 4))

plot(x = c(30,40,50,60,70,90, 110), y = accuracy, 
     type = "l",
     main = "FF Neural Network: Principal Components vs. Accuracy",
     xlab = "Number of Components", 
     ylab = "Accuracy")


#============ Spending a bit of time with kernel pca =============

poids <- rowNorms(as.matrix(XTrain), method = "euclidean", p =2)
poids <- poids / sum(poids)


subset <- sample(nrow(XTrain), size = 2048, prob = poids)

X0 <- XTrain[subset,]

ndim <- 30
pc <- kpca(X0, kernel = "rbfdot", features = ndim, kpar = list(sigma = 0.0113))

##=============== plot the decay of the eigenvalues ==============

pc_var <- eig(pc)
pc_var <- pc_var/sum(pc_var)
resvarr <- 1- cumsum(pc_var)


plot(resvarr[1:ndim], xlim = c(1,ndim), ylim = c(0,1), 
     xlab = "Number of Principal Componenets",
     ylab = "Precentage of Residual Variance Not Captured", 
     type = "b", 
     col = "orange", 
     main = "KPCA Number of Pricipal Components vs Varaince not Captured")
ktest <- predict(pc, XTest)
ktrain <- predict(pc, XTrain)
colors <- c("red", "darkblue", "green", "yellow", "lightblue", "grey")
colors <- colors[as.factor(ytest)]
full3d <- scatterplot3d(x = ktest[,1], 
                        y = ktest[,2],
                        z = ktest[,3], 
                        color = colors,
                        main = "Data projected onto Principal Components from Kernel PCA", 
                        xlab ="PC-1",
                        ylab = "PC-2", 
                        zlab = "PC-3", 
                        pch = 1,
                        angle = 220
                        )
legend(full3d$xyz.convert(25, -5, -0), 
       legend = levels(ytrain),
       col =  c("red", "darkblue", "green", "yellow", "lightblue", "grey"), 
       pch = 16,
       ncol = 1, cex = .5)


#====================== SVM kernel pca =================
model <- svm (ktrain, ytrain, kernel = "radial", cost = 1)
lsvm_pred <- predict(model, ktest)
accuracy <- mean(lsvm_pred == ytest)
confusionMatrix(as.factor(lsvm_pred), ytest)


#====================== QDA kernel pca =================
model <- qda(ktrain, ytrain)
kqda_pred <- predict(model, ktest)
accuracy <- mean(as.factor(kqda_pred$class) == ytest)
confusionMatrix(as.factor(kqda_pred$class), ytest)


#====================== NNET kernel pca =================
nnet_pca <- nnet(x = as.matrix(ktrain), y = class.ind(ytrain), 
                 size = 7, 
                 maxit = 5000, 
                 decay = lambda, 
                 skip = T, 
                 trace =F, 
                 MaxNWts = 8000, 
                 softmax = T)
nnet_pred <- predict(nnet_pca, ytest)
nnet_pred <- factor(apply(X = nnet_pred, MARGIN = 1, FUN = which.max))
levels(nnet_pred) <- levels(ytrain)
confusionMatrix(as.factor(nnet_pred), ytest)
accuracy <- append(accuracy,round(sum(nnet_pred == ytest) / length(ytest), digits = 4))
