##======= Homework #3   =====


#install.packages("MASS")
#install.packages("matlib")
#install.packages("mvtnorm")
library("MASS")
library("mvtnorm")
#set up matrix data for means and cov maticies
mu1 <- c(1,2)
mu2 <- c(6,6)
mu3 <- c(6,-2)

sig1 <- cbind(c(1,0) , c(0,4))
sig2 <- cbind(c(9,0) , c(0,1))
sig3 <- cbind(c(2.25,0) , c(0,4))

#create mixtire of sample data points
pi_vec <- c(1,1,2,2,2,3,3,3,3,3)

mixture <- sample(pi_vec, size = 100, replace = T)
pi_1 <- sum(mixture == 1)
pi_2 <- sum(mixture == 2)
pi_3 <- sum(mixture == 3)
class_vec <- c(rep("Pi_1", pi_1), rep("Pi_2", pi_2), rep("Pi_3", pi_3))
class_vec <- factor(class_vec)

p1_xi <- rnorm(pi_1, 1, 1)
p1_yi <- rnorm(pi_1, 2, 2)

p2_xi <- rnorm(pi_2, 6, 3)
p2_yi <- rnorm(pi_2, 6, 1)

p3_xi <- rnorm(pi_3, 6, 1.5)
p3_yi <- rnorm(pi_3, -2, 2)


x_i <- append(p1_xi, append(p2_xi, p3_xi))
y_i <- append(p1_yi, append(p2_yi, p3_yi))
class <- append(rep(1, pi_1), append(rep(2, pi_2), rep(3, pi_3)))
big_X <- cbind(x_i, y_i, class_vec)
#plot the datapoints
dataframe4plot <- data.frame(x_i, y_i, class_vec)
plot(x_i, y_i, col = c("red", "blue", "green")[class_vec])



i <- c(0:49)

#create the grid matrix
G_x <- cbind(min(x_i) + i * (max(x_i) - min(x_i))/50)
G_y <- cbind(min(y_i) + i * (max(y_i) - min(y_i))/50)
X_g <- expand.grid(G_x, G_y)
GridX <- X_g
names(GridX) <- c("x_i", "y_i")
#create the vectors of Bayes Classifier data
g_1 <- function(x, y) {
  dmvnorm(c(x, y), mu1, sig1) - max(dmvnorm(c(x, y), mu2, sig2), dmvnorm(c(x, y), mu3, sig3))
}

g_2 <- function(x, y) {
  dmvnorm(c(x, y), mu2, sig2) - max(dmvnorm(c(x, y), mu3, sig3), dmvnorm(c(x, y), mu1, sig1))
}

g_3 <- function(x, y) {
  dmvnorm(c(x, y), mu3, sig3) - max(dmvnorm(c(x, y), mu2, sig2), dmvnorm(c(x, y), mu1, sig1))
}

class1 <- matrix(data = mapply(g_1, X_g$Var1, X_g$Var2), byrow = F, nrow = 50)
class2 <- matrix(data = mapply(g_2, X_g$Var1, X_g$Var2), byrow = F, nrow = 50)
class3 <- matrix(data = mapply(g_3, X_g$Var1, X_g$Var2), byrow = F, nrow = 50)
## add contours to plot, display the Bayes Classifier boundaries
contour(G_x, G_y, class1, levels = 0, drawlabels = F, add = T)
contour(G_x, G_y, class2, levels = 0, drawlabels = F, add = T)
contour(G_x, G_y, class3, levels = 0, drawlabels = F, add = T)

## Linear Discriminant Analysis

x_LDA <- lda(big_X[,c(1,2)], class_vec)

model <- predict(x_LDA, GridX)
predict_class <- model$class
prob_class <- model$posterior


lda_1 <- c()
lda_2 <- c()
lda_3 <- c()
#fill vectors of the LDA boundary data
for( i in 1:length(prob_class[,1])) {
  lda_1[i] <- model$posterior[i,1] - max(prob_class[i,2] ,prob_class[i,3])
}

for( i in 1:length(prob_class[,2])) {
  lda_2[i] <- model$posterior[i,2] - max(prob_class[i,1] ,prob_class[i,3])
}

for( i in 1:length(prob_class[,3])) {
  lda_3[i] <- model$posterior[i,3] - max(prob_class[i,2] ,prob_class[i,1])
}


ldaclass1 <- matrix(data = lda_1, byrow = F, nrow = 50)
ldaclass2 <- matrix(data = lda_2, byrow = F, nrow = 50)
ldaclass3 <- matrix(data = lda_3, byrow = F, nrow = 50)
#plot the boundaries on the bayes classifier boundaries and the sample data
contour(G_x, G_y, ldaclass1, levels = 0, drawlabels = F, add = T, col = "pink")
contour(G_x, G_y, ldaclass2, levels = 0, drawlabels = F, add = T, col = "pink")
contour(G_x, G_y, ldaclass3, levels = 0, drawlabels = F, add = T, col = "pink")
