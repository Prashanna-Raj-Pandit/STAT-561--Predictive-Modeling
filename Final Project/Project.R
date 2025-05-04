library(caret)
library(MASS)   
library(caret)  
library(pROC)
library(car)
library(glmnet)
library (ggplot2)
library (lattice)
library (DMwR2)

# Load and prepare data
data <- read.csv("Customer Churn.csv")
names(data) <- gsub(" +", "_", trimws(names(data)))

# Convert appropriate columns to factors
data$Age.Group <- as.factor(data$Age.Group)
data$Tariff.Plan <- as.factor(data$Tariff.Plan)
data$Status <- as.factor(data$Status)
data$Complains <- as.factor(data$Complains)
data$Churn <- as.factor(data$Churn)

# Check correlation between numeric variables
numeric_data <- data[sapply(data, is.numeric)]
correlation_matrix <- cor(numeric_data)
print(correlation_matrix)

# Split data: 10% for future holdout, then 90% of remaining into train/test
set.seed(123)
future_idx <- sample(1:nrow(data), size = 0.1 * nrow(data))
future_data <- data[future_idx, ]
remaining_data <- data[-future_idx, ]

train_idx <- sample(1:nrow(remaining_data), size = 0.9 * nrow(remaining_data))
train <- remaining_data[train_idx, ]
test <- remaining_data[-train_idx, ]

### ---------------- POISSON REGRESSION ----------------
model_poisson <- glm(Frequency.of.use ~ Subscription..Length + Charge..Amount + Seconds.of.Use +
                       Frequency.of.SMS + Distinct.Called.Numbers + Age.Group +
                       Tariff.Plan + Status + Customer.Value, 
                     data = train, family = poisson)

summary(model_poisson)

# Evaluate Poisson
poisson_preds <- predict(model_poisson, newdata = test, type = "response")
poisson_rmse <- sqrt(mean((poisson_preds - test$Frequency.of.use)^2))
cat("Poisson Test RMSE on test data:", poisson_rmse, "\n")


poisson_preds_future <- predict(model_poisson, newdata = future_data, type = "response")
poisson_rmse_future <- sqrt(mean((poisson_preds_future - future_data$Frequency.of.use)^2))
cat("Poisson Test RMSE on future data:", poisson_rmse_future, "\n")


# Check overdispersion
residual_deviance <- model_poisson$deviance
df_residual <- model_poisson$df.residual
cat("Dispersion ratio:", residual_deviance / df_residual, "\n")  # > 1 suggests overdispersion

# Diagnostic plots
plot(fitted(model_poisson), residuals(model_poisson), pch=19, col="red",
     xlab = "Fitted values", ylab="Residuals", main="Residuals vs Fitted")
abline(h=0, col="black", lwd=2)

### ---------------- NEGATIVE BINOMIAL ----------------
set.seed(123)
folds <- createFolds(train$Frequency.of.use, k = 5, list = TRUE, returnTrain = TRUE)
results <- vector("list", length = 5)

for (i in seq_along(folds)) {
  train_fold <- train[folds[[i]], ]
  test_fold <- train[-folds[[i]], ]
  
  model_nb <- glm.nb(Frequency.of.use ~ Subscription..Length + Charge..Amount + Seconds.of.Use +
                       Frequency.of.SMS + Distinct.Called.Numbers + Age.Group +
                       Tariff.Plan + Status + Customer.Value,
                     data = train_fold)
  preds <- predict(model_nb, newdata = test_fold, type = "response")
  results[[i]] <- sqrt(mean((preds - test_fold$Frequency.of.use)^2))
}

nb_rmse <- mean(unlist(results))
cat("Average Negative Binomial RMSE (5-fold CV) on test data:", round(nb_rmse, 3), "\n")

future_preds<-predict(model_nb,newdata = future_data,type = "response")
future_result<-sqrt(mean(future_preds-future_data$Frequency.of.use)^2)
cat("Negative Binomial RMSE future data:", round(future_result, 3), "\n")

# Multicollinearity check
vif_values <- vif(model_nb)
print(vif_values)

# Model diagnostics
model_nb_full <- glm.nb(Frequency.of.use ~ Subscription..Length + Charge..Amount + Seconds.of.Use +
                          Frequency.of.SMS + Distinct.Called.Numbers + Age.Group +
                          Tariff.Plan + Status + Customer.Value, data = train)

plot(fitted(model_nb_full), residuals(model_nb_full), col="red",
     xlab="Fitted Values", ylab="Deviance Residuals", main="NB: Residuals vs Fitted")
abline(h=0, col="black", lwd=2)


### ---------------- REGULARIZATION ----------------
data_clean <- na.omit(data)
x <- model.matrix(Frequency.of.use ~ Subscription..Length + Charge..Amount + Seconds.of.Use +
                    Frequency.of.SMS + Distinct.Called.Numbers + Age.Group +
                    Tariff.Plan + Status + Customer.Value, data = data_clean)[, -1]
y <- data_clean$Frequency.of.use

x_train <- x[train_idx, ]
y_train <- y[train_idx]
x_test <- x[-train_idx, ]
y_test <- y[-train_idx]

## RIDGE
set.seed(123)
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0, family = "poisson")
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = cv_ridge$lambda.min)
ridge_preds <- predict(ridge_model, newx = x_test)
ridge_rmse <- sqrt(mean((ridge_preds - y_test)^2))
cat("Ridge Test RMSE:", ridge_rmse, "\n")
plot(cv_ridge)

## LASSO
set.seed(123)
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, family = "poisson")
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = cv_lasso$lambda.min)
lasso_preds <- predict(lasso_model, newx = x_test)
lasso_rmse <- sqrt(mean((lasso_preds - y_test)^2))
cat("Lasso Test RMSE:", lasso_rmse, "\n")
print(coef(lasso_model))
plot(cv_lasso)

# Future prediction using Lasso
future_clean <- na.omit(future_data)
x_future <- model.matrix(Frequency.of.use ~ Subscription..Length + Charge..Amount + Seconds.of.Use +
                           Frequency.of.SMS + Distinct.Called.Numbers + Age.Group +
                           Tariff.Plan + Status + Customer.Value, data = future_clean)[, -1]

future_preds_ridge <- predict(ridge_model, newx = x_future)
future_rmse_ridge <- sqrt(mean((future_preds_ridge - future_clean$Frequency.of.use)^2))
cat("Lasso RMSE on Future Data:", future_rmse_ridge, "\n")


future_preds_lasso <- predict(lasso_model, newx = x_future)
future_rmse_lasso <- sqrt(mean((future_preds_lasso - future_clean$Frequency.of.use)^2))
cat("Lasso RMSE on Future Data:", future_rmse_lasso, "\n")

# Compare models
model_comparison <- data.frame(
  Model = c("Poisson", "Negative Binomial", "Ridge", "Lasso"),
  Test_RMSE = c(poisson_rmse, nb_rmse, ridge_rmse, lasso_rmse)
)
print(model_comparison)
best_model <- model_comparison$Model[which.min(model_comparison$Test_RMSE)]
cat("Best performing model:", best_model, "\n")



#Logistic Model

#---------------------------
# Load Required Libraries
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC")
}
if (!requireNamespace("themis", quietly = TRUE)) {
  install.packages("themis")
}
if (!requireNamespace("recipes", quietly = TRUE)) {
  install.packages("recipes")
}

library(caret)
library(pROC)
library(themis)
library(recipes)

# Check the distribution of Complains in the dataset
cat("Distribution of Complains in the dataset:\n")
print(table(data$Complains))

# Stratified splitting to ensure both classes are represented in training and test sets
set.seed(123)  # For reproducibility
split_idx <- createDataPartition(data$Complains, p = 0.9, list = FALSE)  # 80% train, 20% test
train <- data[split_idx, ]
test <- data[-split_idx, ]

# Recode the target variable for caret compatibility
train$Complains <- factor(ifelse(train$Complains == 1, "yes", "no"), levels = c("no", "yes"))
test$Complains <- factor(ifelse(test$Complains == 1, "yes", "no"), levels = c("no", "yes"))

# Check the distribution of Complains in the training dataset
cat("\nDistribution of Complains in the training dataset before SMOTE:\n")
print(table(train$Complains))

# Apply SMOTE to balance the dataset
recipe <- recipe(Complains ~ ., data = train) %>%
  step_dummy(all_nominal_predictors()) %>%  # Convert all factor variables to numeric dummy variables
  step_smote(Complains, over_ratio = 1)    # Over-sample until both classes are balanced

# Prepare the recipe and bake the balanced dataset
train_smote <- prep(recipe, training = train) %>% bake(new_data = NULL)

# Check the distribution of Complains in the training dataset after SMOTE
cat("\nDistribution of Complains in the training dataset after SMOTE:\n")
print(table(train_smote$Complains))

# Train Logistic Regression model
log_model_smote <- glm(
  Complains ~ .,  # Use all predictors
  data = train_smote,
  family = binomial(link = "logit")
)

# Display model summary
cat("\nSummary of Logistic Model (After SMOTE):\n")
summary(log_model_smote)

# Preprocess the test dataset using the same recipe
test_processed <- prep(recipe, training = train) %>% bake(new_data = test)

# Predict probabilities on the test set
pred_prob_smote <- predict(log_model_smote, newdata = test_processed, type = "response")

# Apply a threshold of 0.5 for classification
pred_class_smote <- ifelse(pred_prob_smote > 0.5, "yes", "no")
pred_class_smote <- factor(pred_class_smote, levels = levels(test$Complains))

# Check the distribution of predicted classes
cat("\nDistribution of Predicted Classes (After SMOTE):\n")
print(table(pred_class_smote))

# Check the distribution of actual classes in the test set
cat("\nDistribution of Actual Classes:\n")
print(table(test$Complains))

# Confusion matrix with 0.5 threshold
cat("\nConfusion Matrix with 0.5 Threshold (After SMOTE):\n")
conf_matrix_smote <- confusionMatrix(pred_class_smote, test$Complains, positive = "yes")
print(conf_matrix_smote)

# ROC and AUC
roc_obj_smote <- roc(response = test$Complains, predictor = pred_prob_smote)
auc_value_smote <- auc(roc_obj_smote)
cat("\nAUC Value (After SMOTE):", auc_value_smote, "\n")

# Plot the ROC curve
plot(roc_obj_smote, main = "ROC Curve - Logistic Regression (After SMOTE)")
abline(a = 0, b = 1, lty = 2, col = "orange")

