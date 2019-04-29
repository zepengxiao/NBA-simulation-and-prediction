Y = as.matrix(as.numeric(factor(data_fit$Result)))
X = as.matrix(data_fit[,-c(9, 1, 2, 3)])
rr = glmnet(X, Y, family = "binomial", alpha = 0)
cv.out = cv.glmnet(X, Y, alpha = 0)
bestlam = cv.out$lambda.min
ridgecoefs = predict(rr, type = "coefficients", s = bestlam)

predict = numeric(nrow(X))
for (i in 1:nrow(X)) {
  predicta = predict.glmnet(rr, t(as.matrix(X[i,])))[which(cv.out$lambda == cv.out$lambda.min)]
  predict[i] = exp(predicta) / (1 + exp(predicta))
}

set.seed(665267179)
predictMatch_ridge = numeric(length(predict))
for (i in 1:length(predict)) {
  if (runif(1) < predict[i]) {
    predictMatch_ridge[i] = "Win"
  } else {
    predictMatch_ridge[i] = "Loss"
  }
}

fittedMatch = fitted(glm1)
predictMatch = numeric(length(fittedMatch))
for (j in 1:length(fittedMatch)) {
  if (runif(1) < fittedMatch[j]) {
    predictMatch[j] = "Win"
  } else {
    predictMatch[j] = "Loss"
  }
}

table(result = data_fit$Result, resultHat = factor(predict > 0.5))
table(result = data_fit$Result, resultHat = factor(predictMatch_ridge))
table(result = data_fit$Result, resultHat = factor(predictMatch))