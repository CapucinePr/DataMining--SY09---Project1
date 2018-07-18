setwd("C:/Users/Yann-Maillet/Documents/UTC/GI04/SY09/Projet1/donnees")


Spam <- read.csv("spam.csv", header=T, row.names=1)

Spam$z <- as.factor(Spam$z)

sample <- Spam[sample(nrow(Spam), 1000),]

devtools::install_github('topepo/caret/pkg/caret')
library("caret")

install.packages("kernlab")
require(kernlab)                    #J'ai du le faire dans la console R  (et non Rstudio)


trainIndex <- createDataPartition(sample$z, p = .8, list = FALSE, times = 1)
dataTrain <- sample[ trainIndex,]

dataTest <- sample[-trainIndex,]


### finding optimal value of a tuning parameter
sigDist <- sigest(z ~ ., data = dataTrain, frac = 1)
### creating a grid of two tuning parameters, .sigma comes from the earlier line. we are trying to find best value of .C
svmTuneGrid <- data.frame(.sigma = sigDist[[1]], .C = 2^(-2:7))


x <- train(z ~ .,
           data = dataTrain,
           method = "svmRadial",
           preProc = c("center", "scale"),
           tuneGrid = svmTuneGrid,
           trControl = trainControl(method = "repeatedcv", repeats = 5, 
                                    classProbs =  FALSE))