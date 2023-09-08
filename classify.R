library(datasets)
library(caret)

# load data set
data("iris")

# To achieve reproducible model
set.seed(100)

# Split data set 
TrainingIndex <- createDataPartition(iris$Species, p=0.8, list = FALSE)
TrainingSet <- iris[TrainingIndex,]
TestingSet <- iris[-TrainingIndex,]

########## SVM ##########
# Build Training Model
svmModel <- train(Species ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
               )

# Build Cross Validation model
svmModel.cv <- train(Species ~ ., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv", number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1)
                  )

# Prediction
svmModel.training <- predict(svmModel, TrainingSet)
svmModel.testing <- predict(svmModel, TestingSet)
svmModel.cv <- predict(svmModel.cv, TrainingSet)

# Model performance
svmModel.training.confusion <- confusionMatrix(svmModel.training,
                                               TrainingSet$Species
                                               )
svmModel.testing.confusion <- confusionMatrix(svmModel.testing,
                                              TestingSet$Species
                                              )
svmModel.cv.confusion <- confusionMatrix(svmModel.cv,
                                         TrainingSet$Species
                                         )

# Print Confusion Matrix
print(svmModel.training.confusion)
print(svmModel.testing.confusion)
print(svmModel.cv.confusion)

# Feature importance
Importance <- varImp(svmModel)
plot(Importance, col = "red")

########## Random Forest ##########
ctrl <- trainControl(method="repeatedcv", repeats = 3)
randomFModel <- train(Species ~ ., data = TrainingSet,
                  method = "rf",
                  trControl = ctrl,
                  preProcess = c("scale", "center"),
                  tuneLength = 20
                  )

# Prediction
randomFModel.training <- predict(randomFModel, TrainingSet)
randomFModel.testing <- predict(randomFModel, TestingSet)

# Model Performance
randomFModel.training.confusion <- confusionMatrix(randomFModel.training,
                                               TrainingSet$Species
                                               )
randomFModel.testing.confusion <- confusionMatrix(randomFModel.testing,
                                              TestingSet$Species
                                              )

# Print Confusion Matrix
print(randomFModel.training.confusion)
print(randomFModel.testing.confusion)

########## Random Forest ##########
ctrl <- trainControl(method="repeatedcv", repeats = 3)
randomFModel <- train(Species ~ ., data = TrainingSet,
                  method = "rf",
                  trControl = ctrl,
                  preProcess = c("scale", "center"),
                  tuneLength = 20
                  )

# Prediction
randomFModel.training <- predict(randomFModel, TrainingSet)
randomFModel.testing <- predict(randomFModel, TestingSet)

# Model Performance
randomFModel.training.confusion <- confusionMatrix(randomFModel.training,
                                               TrainingSet$Species
                                               )
randomFModel.testing.confusion <- confusionMatrix(randomFModel.testing,
                                              TestingSet$Species
                                              )

# Print Confusion Matrix
print(randomFModel.training.confusion)
print(randomFModel.testing.confusion)

########## KNN ##########
# Building the Model
ctrl <- trainControl(method="repeatedcv", repeats = 3)
knnModel <- train(Species ~ ., data = TrainingSet,
                method = "knn",
                trControl = ctrl,
                preProcess = c("scale", "center"),
                tuneLength = 20
                )

plot(knnModel)

# Prediction
knnModel.training <- predict(knnModel, TrainingSet)
knnModel.testing <- predict(knnModel, TestingSet)

# Model Performance
knnModel.training.confusion <- confusionMatrix(knnModel.training,
                                               TrainingSet$Species
                                               )
knnModel.testing.confusion <- confusionMatrix(knnModel.testing,
                                              TestingSet$Species
                                              )

# Print Confusion Matrix
print(knnModel.training.confusion)
print(knnModel.testing.confusion)