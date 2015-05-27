
svmmodel <- svm(trainset$count~., data = extractFeatures(trainset))
submissionsvm <- data.frame(datetime=testset$datetime, count=NA)
submissionsvm[, "count"] <- predict(svmmodel, extractFeatures(testset))
rmsle(actual = testset$count,predicted = submissionsvm$count)

tune.randomForest(trainset$count~., data = extractFeatures(trainset), nodesize = 3:7)


rftest <- function(tree) {
   for (i in 1:length(tree)) {
     model <- randomForest(trainset$count~., data = extractFeatures(trainset), ntree = tree[i])
     submission <- data.frame(datetime=testset$datetime, count=NA)
     submission[, "count"] <- predict(model, extractFeatures(testset))
     print(rmsle(actual = testset$count,predicted = submission$count))
     
   }
 }

obj <- tune.svm(trainset$count~., data = extractFeatures(trainset), 
            gamma = 2^(-1:1), cost = 2^(2:4))
)
