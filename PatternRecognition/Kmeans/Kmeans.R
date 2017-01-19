#============================================================#
#Author : Fiona, GaoYi高屹  1641472
#There are two parts of the project
#code part and function part
#code part is using iris data and abalone data to test kmeans
#please run the function part first before code part
#============================================================#

#============================================================#
#Code Part
#============================================================#

setwd("/Users/fiona/RWorkspace/Kmeans")
#======================================================================
#Use iris dataset test my kmeans
#======================================================================
irisData <- read.csv("iris.csv")
colnames(irisData) <- c("sepal_length","sepal_width", "petal_length", "petal_width", "class")
trainData <- irisData[, 1:4]

irisKmeans <- KmeansFun(3, trainData,50000)
PlotKmeans(irisKmeans,1,2,3,1)
PlotKmeans(irisKmeans,1,3,3,1)
PlotKmeans(irisKmeans,1,4,3,1)
PlotKmeans(irisKmeans,2,3,3,1)
PlotKmeans(irisKmeans,2,4,3,1)
PlotKmeans(irisKmeans,3,4,3,1)

#======================================================================
#Use abalone dataset test my kmeans
#======================================================================
abalone <- read.csv("abalone.csv")
abalone.use <- abalone[1:500,c(2,3,4,5)]
names(abalone.use) <- c("Length","Diameter","Height","Whole_weight")
abaloneKmeans <- KmeansFun(3,abalone.use,10000)
PlotKmeans(abaloneKmeans,1,2,3,0.1)
PlotKmeans(abaloneKmeans,1,3,3,0.1)
PlotKmeans(abaloneKmeans,1,4,3,0.1)
PlotKmeans(abaloneKmeans,2,3,3,0.1)
PlotKmeans(abaloneKmeans,2,4,3,0.1)
PlotKmeans(abaloneKmeans,3,4,3,0.1)


#============================================================#
#Function Part
#============================================================#
#=====================================================================
#calculate the distances between two vector
#=====================================================================
Disted <- function(xi, xj){
  dist <- 0
  for(w in 1:length(xi)){
    dist <- dist + (xi[w] - xj[w])^2
  }
  return(sqrt(dist))
}
#=====================================================================
#use Kmeans to train the data
#k is a number, means how many classes you want to  get
#Data is the train data
#ktime is a number, you can specify the loop time in case you need too much time to get the result
#=====================================================================
KmeansFun <- function(k, Data, ktime = 10000){
  Class <- -1
  # random get the Mean Vector
  oriMeanVec <- Data[sample(nrow(Data),k),]
  # initialize MeanVec
  MeanVec <- as.data.frame(matrix(0,ncol=length(oriMeanVec),nrow = nrow(oriMeanVec)))
  Data <- cbind(Data, Class)
  # if the MeanVector change
 # xi <- Data[,1:(length(Data)-1)]
  count <- 0
  judg <- !identical(MeanVec,oriMeanVec)
  while(judg & count < ktime){
#    browser()
    count <- count + 1
    for (i in 1:nrow(Data)) {
#      browser()
      distVec <- c()
      for(j in 1:k){
#        browser()
        distVec[j] <- Disted(Data[i, 1:(length(Data)-1)], oriMeanVec[j,])
      }
      Data[i, "Class"]  <- which.min(distVec)
    }
    
    for(i in 1:k){
      classData <- Data[Data$Class == i, 1:(length(Data)-1)]
      if(nrow(classData) == 0){
        MeanVec[i,] <- oriMeanVec[i,]
      }
      MeanVec.one <- apply(classData, 2, mean)
      MeanVec[i,] <- MeanVec.one
    }
    judg <- !identical(MeanVec,oriMeanVec)
    oriMeanVec <- MeanVec
 }
    
 return(list(Data,oriMeanVec)) 
  
}
#=====================================================================
#Draw the result in a picture
#result is a data frame, is the result from KmeansFun
#r1 and r2 are two number,are the attribute that you want to draw because the data maybe have more than 2 attribute and
#the chart can only show 2 attribute
#k is a number, means how many classes you have aggregate in the result
#alpha is a number from 0 to 1, it define a transparency for the data point, to make the chart more clearly 
#and alpha is not necessary
#=====================================================================
PlotKmeans <- function(result, r1, r2, k, alpha = 1){
  #result <- balanceKmeans
  #r1 <- 1;r2 <- 2
  #k <- 3
  titleNames <- names(result[[1]])
  mx <- result[[2]][,as.numeric(r1)]
  my <- result[[2]][,as.numeric(r2)]
  x <- result[[1]][,as.numeric(r1)]
  y <- result[[1]][,as.numeric(r2)]
  MinX <- min(x) - 1
  MaxX <- max(x) + 1
  MinY <- min(y) - 1
  MaxY <- max(y) + 1
  plot(mx,my,pch = 4, col = "black", axes =TRUE,xlab=titleNames[as.numeric(r1)], ylab=titleNames[as.numeric(r2)], xlim=c(MinX,MaxX), ylim=c(MinY,MaxY))
  myColor <- rainbow(k, alpha=alpha)
  for(i in 1:k){
    xi <- x[result[[1]]$Class == i]
    yi <- y[result[[1]]$Class == i]
    points(xi,yi,col = myColor[i])
  }
}

