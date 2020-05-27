##======= Homework #4   =====


# install.packages("e1071")
#install.packages("mvtnorm")
library("MASS")
library("mvtnorm")
library("e1071")
library('caret')
library('class')
library('gmodels')

load('zip.train.RData')
load('zip.test.RData')
WD = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(WD)
#set up matrix data for means and cov maticies
mu1 <- c(1,2)
mu2 <- c(2,-.5)

sig1 <- cbind(c(4,0) , c(0,4))
sig2 <- cbind(c(9,0) , c(0,4))

prop <- c(rep(1,2), rep(2,3))
mixture <- sample(prop, size = 200, replace = T)
pi1 <- sum(mixture == 1)
pi2 <- sum(mixture == 2)
#Create the sample data
p1_xi <- rnorm(pi1, 1, 2)
p1_yi <- rnorm(pi1, 2, 2)

p2_xi <- rnorm(pi2, 2, 3)
p2_yi <- rnorm(pi2, -.5, 2)

x_i <- append(p1_xi, p2_xi)
y_i <- append(p1_yi, p2_yi)

class <- factor(c(rep(1, pi1), rep(2, pi2)))
X <- cbind(x_i, y_i)
plot <- plot(x_i, y_i, col = c("red", "blue")[class],
             main = "C = C* = 2.32")

# Create the grid
i <- c(0:49)
G_x <- cbind(min(x_i) + i * (max(x_i) - min(x_i))/50)
G_y <- cbind(min(y_i) + i * (max(y_i) - min(y_i))/50)
X_g <- expand.grid(G_x, G_y)
GridX <- X_g
names(GridX) <- c("x_i", "y_i")

g_1 <- function(x, y) {
  dmvnorm(c(x, y), mu1, sig1) - dmvnorm(c(x, y), mu2, sig2)
}

g_2 <- function(x, y) {
  dmvnorm(c(x, y), mu2, sig2) - dmvnorm(c(x, y), mu1, sig1)
}
#Create contour plots for the Bayes Classifier
class1 <- matrix(data = mapply(g_1, X_g$Var1, X_g$Var2), byrow = F, nrow = 50)
class2 <- matrix(data = mapply(g_2, X_g$Var1, X_g$Var2), byrow = F, nrow = 50)

contour(G_x, G_y, class1, levels = 0, drawlabels = F, add = T)
contour(G_x, G_y, class2, levels = 0, drawlabels = F, add = T)


# create the SVM model
model <- svm(X, class, kernel = 'radial', cost = 2.32)

predict_grid  <- predict(model, X_g)

contour(G_x, G_y, matrix(predict_grid, 50, 50), levels = 1.5, 
        drawlabels = F, add = T, col = "green")

# Paramenter Tuning using cross validation

c_vec <- seq(.5,10, (10-.5)/50)
error1 <- c()
c_star1 <- c()
for(f in 1:1) {
  folds <- sample(1:200, 200, F)
  for(i in 1:length(c_vec)) {
    error_vec <- c()
    for(j in 1:10) {
      p <- c(1:20) + 20*(j -1)
      index <- folds[p]
      #print(index)
      model_train <<- svm(X[index,], class[index], kernel = 'radial', cost = c_vec[i])
      #print(predict(model_train, X[-index,]))
      error_vec[j] <- sum(predict(model_train, X[-index,]) != class[-index]) 
    }
    error1[i] <- mean(error_vec)
  }
  c_star1 <- append(c_star1, c_vec[match(min(error1), error1)])
}




plot(x = c_vec, y = error1/length(predict_grid), type ='o',
     main = "Error of the Classifier in Relation to Cost",
     xlab = "Cost",
     ylab = "Error, Proportion of Misclassified Points")


plot <- plot(x_i, y_i, col = c("red", "blue")[class],
             main = "C = 2.32")
contour(G_x, G_y, class1, levels = 0, drawlabels = F, add = T)
contour(G_x, G_y, class2, levels = 0, drawlabels = F, add = T)
ideal_model <- svm(X, class, kernel = 'radial', cost = mean(c_star1))
predict_grid1  <- predict(ideal_model, X_g)

contour(G_x, G_y, matrix(predict_grid1, 50, 50), levels = 1.5, drawlabels = F, add = T, col = "green")

## Working with the Post Office Data - LDA first
X <- zip.train[,-1]
num.class <- as.factor(zip.train[,1])
lda.model <- lda(X, num.class)

test.data <- zip.test[,-1]
lda.pred <- predict(lda.model, test.data)

predict.nums <- lda.pred$class
actual <- as.factor(zip.test[,1])

lda.compare <- data.frame(predict.nums, actual)

lda.confuse <- confusionMatrix(predict.nums, actual)


df <- data.frame(c(1:10))
for(i in 0:9) {
  for(j in 0:9) {
    df[i +1,j +1] <- sum(predict.nums[which(actual == j)] == i)
  }
}

df <- rbind(0:9, df)
df <- cbind(c(NA,0:9), df)
names(df) <- c(1:10)

# Now onto the SVM classifier for the data

svm.model <- svm(X, num.class, kernel = 'radial', cost = .1)

svm.pred <- predict(svm.model, test.data)

svm.compare <- data.frame(predict.nums, actual)

svm.confuse <- confusionMatrix(predict.nums, actual)

c_vec <- seq(.5,10, (10-.5)/50)
error <- c()
c_star <- c()

# 
# for(i in 1:length(c_vec)){
#   svm.model <- svm(X, num.class, kernel = 'radial', cost =c_vec[i])
#   error[i] <- sum(predict(svm.model, test.data) != actual) 
# }


costfunc <- function(x) {
  svm.model <- svm(X, num.class, kernel = 'radial', cost = x)
  return(sum(predict(svm.model, test.data) != actual))
}

error <- sapply(c_vec, costfunc, USE.NAMES = F)

plot(x = c_vec, y = error / length(actual), type ='o',
     main = "Error of the Classifier in Relation to Cost",
     xlab = "Cost",
     ylab = "Error, Number of Misclassified Points")
#Check out the results
svm.model <- svm(X, num.class, kernel = 'radial', cost = 3)

svm.pred <- predict(svm.model, test.data)

svm.compare <- data.frame(predict.nums, actual)

svm.confuse <- confusionMatrix(svm.pred, actual)

svmDf <- data.frame(c(1:10))
for(i in 0:9) {
  for(j in 0:9) {
    svmDf[i +1,j +1] <- sum(svm.pred[which(actual == j)] == i)
  }
}

svmDf <- rbind(0:9, svmDf)
svmDf <- cbind(c(NA,0:9), svmDf)
names(svmDf) <- c(1:10)




