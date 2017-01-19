#set the work directory
setwd('/Users/fiona/RWorkspace/iris')
#read iris data
iris <- read.csv('iris.csv',header = F)
#select the two kind of iris and two variable to train the model
iris.train <- iris[1:100,c(1,2,5)]
#data process，replace the name of the iris with 0,1,Iris-setosa=1， Iris-versicolor=0
iris.train$V5 <- as.numeric(iris.train$V5)-1
#compute p1 and p0
count0 <- nrow(iris.train[iris.train$V5 == 0,])
count1 <- nrow(iris.train[iris.train$V5 == 1,])
countall <- nrow(iris.train)
p1 <- count1/countall
p0 <- count0/countall
#define x and y
xb <- 1
x <- cbind(iris.train[,1:2], xb)
y <- iris.train[,3]
#compute the first-order derivative
m <- countall
derivative1 <- c(0,0,0)
for(i in 1:m){
  xi <- matrix(x[i,],nrow = 3, ncol = 1)
  xi <- apply(xi, 2, as.numeric)
  yi <- as.numeric(y[i])
  y_p <-  yi - p1
  derivative1 <- derivative1 - (xi* y_p)
}
#compute the second-order derivative
derivative2 <- matrix(data = c(0,0,0,0,0,0,0,0,0), nrow = 3, ncol = 3);
for(i in 1:m){
  xi <- matrix(x[i,],nrow = 3, ncol = 1)
  xi <- apply(xi, 2, as.numeric)
  txi <- matrix(x[i,],nrow = 1, ncol = 3)
  txi <- apply(txi, 2, as.numeric)
  xtx <- xi%*%txi
  derivative2 <- derivative2 + xtx*p1*p0
}
#define initial value of beita 
beita <- c(1,1,1)
#compute beita iteration 1000 times
for(i in 1:1000){
  beita <- beita - (solve(derivative2)%*%derivative1)
}
#draw a picture
x1 <- iris.train[iris.train$V5 == 0, 1]
y1 <- iris.train[iris.train$V5 == 0, 2]
x2 <- iris.train[iris.train$V5 == 1, 1]
y2 <- iris.train[iris.train$V5 == 1, 2]

plot(x1, y1, pch = 1, col = "red", axes =TRUE, xlab="sepal length", ylab="sepal width", xlim=c(4,7), ylim=c(1,5))
points(x2,y2)
#draw the line 1934*X1-2372*X2-3234=0.5
abline(-1.363617,0.8153457)
