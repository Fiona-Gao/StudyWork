#============================================================#
#Author : Fiona, GaoYi高屹  1641472
#There are two parts of the project
#code part and function part
#code part is a example of how to use the function
#please run the function part first before code part
#============================================================#
install.packages("sampling")
library(sampling)

#============================================================#
#code part
#============================================================#
setwd('/Users/fiona/RWorkspace/DecisionTree')
#read Car Data
carData <- read.csv("car.data")
#defind column names
names(carData) <- c("buying", "maint", "doors", "persons", "lug_boot", "safety","target")
#get train data and test data
#look at the level of the target and extract 80% data as train data and other 20% as test data.
summary(carData$target)
trainNum <- strata(carData, stratanames = "target", size = c(967,307,52,55),method = "srswor")
carData.train <- carData[as.numeric(row.names(trainNum)),]
carData.test <- carData[-as.numeric(row.names(trainNum)),]
#call TreeGenerate to generate a tree
tree <- TreeGenerate(carData.train,"target","root","begin")
#using the tree to test the test data
testValue <- testValue(carData.test,tree = tree)
#return the accuracy of the prediction of the test data
Accuracy(testValue = testValue, testData.target = carData.test$target)



#============================================================#
#Function part
#============================================================#


#============================================================#
#Function Name : Gain
#data : train Data
#target: String, the name of target column
#Calculate the information entropy
#============================================================#

Gain <- function(data,target){
  #get target levels
  levelNames <- levels(as.factor(as.character(data[,names(data) == target])))
  Ent <- 0
  #calculate the information entropy
  data.target <- data[,names(data) == target]
  for (i in 1:length(levelNames)){
    pi <- nrow(data[data.target == levelNames[i],])/nrow(data)
    Ent <- Ent-pi*log2(pi)
  }
  return(Ent)
}

#============================================================#
#Function Name : getSonNodeData
#data : father dataset
#variable : string, split variable
#value : string, split variable value
#return sonNode dataset
#============================================================#

getSonNodeData <- function(data, variable, value){
  #return the dataset that the value of the variable = value and remove the variable column
  data.variable <- data[,names(data) == variable]
  sonData <- data[data.variable == value, names(data) != variable]
  return(sonData)
}

#============================================================#
#Function Name : chooseVariable
#data : train data
#traget: target
#choose the variable to split the tree using C4.5 mothed
#============================================================#

chooseVariable <- function(data,target){
  nD <- nrow(data)
  if(length(names(data)) < 2){
    return("test")
  }
  EntD <- Gain(data = data, target = target)
  variableData <- data[, names(data) != target]
  variableName <- names(variableData)

  if(is.null(variableName)){
    dataNames <- names(data)
    variableName <- dataNames[names(data) != "target"]
  }
  gainList <- data.frame()
  for (i in 1:length(variableName)){
    GainDa <- EntD
    IVa <- 0
    attLevels <- levels(as.factor(as.character(data[,names(data) == variableName[i]])))
    for (j in 1:length(attLevels)) {
      DvData.variable <- data[, names(data) == variableName[i]]
      DvData <- data[DvData.variable == attLevels[j],]
      EntDv <- Gain(data = DvData, target = target)
      GainDa <- GainDa - nrow(DvData)/nD * EntDv
      IVa <- IVa - nrow(DvData)/nD * log2(nrow(DvData)/nD)
    }
    gainRatio <- GainDa/IVa
    gainList <- rbind(gainList, c(as.character(variableName[i]), GainDa, gainRatio),stringsAsFactors = F)
  }
  names(gainList) <- c("variableName","GainDa", "gainRatio")
  gainList.num <- gainList$GainDa
  gain.mean <- mean(as.numeric(gainList.num))
  gainList.part <- gainList[as.numeric(gainList.num) >= gain.mean,]
  chooesAtt <- gainList.part[gainList.part$gainRatio == max(as.numeric(gainList.part$gainRatio)),1]
  return(chooesAtt)
}

#============================================================#
#Function Name : TreeGenerate
#data : train data
#traget : string, target
#rootNode : string, the name of father node
#rootLine : string, the name of line name,the value of the choose variable
#generate a tree
#============================================================#

TreeGenerate <- function(data,target,rootNode,rootLine){
  tree <- data.frame()
  if(is.factor(data)){
    data.target <- as.factor(as.character(data))
  }else{
    data.target <- as.factor(as.character(data[, names(data) == target]))
  }
  classNum <- summary(data.target)
  classNumN <- names(classNum[classNum == max(classNum)])
  if(is.factor(data)){
    data.attribute <- NULL
  }else{
    
    data.attribute <- as.data.frame(data[, names(data) != target])
  }
  target.levels <- levels(as.factor(as.character(data.target)))
  if(length(classNumN) > 1){
    classNumN <- classNumN[1]
  }
  if(length(target.levels) == 1){
    tree <- rbind(tree, c(rootNode,rootLine,as.character(target.levels),""))
    names(tree) <- c("preName","preValue","curName","curValue")
    return(tree)
  }
  if(is.null(data.attribute)){
    tree <- rbind(tree, c(rootNode,rootLine,as.character(classNumN),""),stringsAsFactors = F)
    names(tree) <- c("preName","preValue","curName","curValue")
    return(tree)
  }else if(nrow(unique(data.attribute)) == 1){
    tree <- rbind(tree, c(rootNode,rootLine,as.character(classNumN),""),stringsAsFactors = F)
    names(tree) <- c("preName","preValue","curName","curValue")
    return(tree)
  }
  attr <- chooseVariable(data,target)
  attrData <- data[,names(data) == attr]
  attrLevels <- levels(as.factor(as.character(attrData)))
  for (i in 1:length(attrLevels)) {
    Dv <- getSonNodeData(data = data, variable = attr, value = attrLevels[i])
    tree <- rbind(tree,c(rootNode,rootLine,attr,attrLevels[i]),stringsAsFactors=FALSE)
    names(tree) <- c("preName","preValue","curName","curValue")
    nextTree <- TreeGenerate(data = Dv,target = target, rootNode = attr, rootLine = attrLevels[i])
    tree <- rbind(tree, nextTree,stringsAsFactors = F)
  }
  names(tree) <-  c('preName','preValue','curName','curValue')
  return(tree)
}

#============================================================#
#Function Name : test
#data : test
#tree : your tree structure
#preName : string, father node name
#Value : string, the line between father node and current node
#generate a tree
#============================================================#
test <- function(data, tree, preName, Value){
  treeStructure <- tree[tree$preName == preName & tree$preValue == Value,]
  curName <- treeStructure[1,3]
  curValue <- as.character(data[,names(data) == curName])
  if(length(as.character(curValue)) == 0){
    return(curName)
  }else{
    test(data = data,tree = tree,preName = curName, Value = curValue)
  }
}

#============================================================#
#Function Name : testValue
#data : test
#tree : your tree structure
#============================================================#
testValue <- function(data,tree){
  testValue <- data.frame()
  for (i in 1:nrow(data)) {
    row <- data[i,]
    testValue <- rbind(testValue, test(row, tree, "root", "begin"), stringsAsFactors = F)
  }
  return(testValue)
}

#============================================================#
#Function Name : Accuracy
#testValue : the result of using your tree to test the test data 
#============================================================#
Accuracy <- function(testValue, testData.target){
  comtest <- cbind(testValue, testData.target)
  return(nrow(comtest[comtest[,1] == comtest[,2],])/nrow(carData.test))
}


write.csv(testValue[,1:8],"tree.csv", row.names = F)


