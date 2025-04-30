library(dplyr)
library(readxl)
library(ggplot2)
library(corrplot)
library(nnet)
absent= read_excel("Absenteeism_at_work.xls")
names(absent) #column names
View(absent)

########### Part 1
# Convert relevant variables to factors BEFORE modeling
absent$Month_of_absence <- as.factor(absent$Month_of_absence)
absent$Day_of_the_week <- factor(absent$Day_of_the_week,
                                   levels = 2:6,
                                   labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
absent$Seasons <- factor(absent$Seasons,
                         levels = 1:4,
                         labels = c("Summer", "Autumn", "Winter", "Spring"))
absent$Education <- factor(absent$Education,
                           levels = 1:4,
                           labels = c("High School", "Graduate", "Postgraduate", "Master/Doctor"))
str(absent)

### 1. Split data into training (70%) and test set (20%)
set.seed(123)
train_size <- floor(0.7 * nrow(absent))
train_indices <- sample(seq_len(nrow(absent)), size = train_size)

# Split outcome and predictors
x <- model.matrix(Absenteeism_time_in_hours ~ . - ID, data = absent)[, -1]  # Remove intercept
y <- absent$Absenteeism_time_in_hours

x_train <- x[train_indices, ]
x_test <- x[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]

table(absent$Month_of_absence)
### 2. Lasso Regression
set.seed(123)
cv.out.lasso <- cv.glmnet(x_train, y_train, alpha = 1, nlambda = 1000)
bestlam_lasso <- cv.out.lasso$lambda.min
lasso.mod <- glmnet(x_train, y_train, alpha = 1, lambda = bestlam_lasso)
lasso.pred <- predict(lasso.mod, s = bestlam_lasso, newx = x_test)
lasso.error <- mean((lasso.pred - y_test)^2)
print(paste("Lasso Test MSE:", lasso.error))

### 3. Ridge Regression
# Alpha = 0 for Ridge
set.seed(123)
cv.out.ridge <- cv.glmnet(x_train, y_train, alpha = 0,nfolds = 10, nlambda = 1000)
bestlam_ridge <- cv.out.ridge$lambda.min
ridge.mod <- glmnet(x_train, y_train, alpha = 0, lambda = bestlam_ridge)
ridge.pred <- predict(ridge.mod, s = bestlam_ridge, newx = x_test)
ridge.error <- mean((ridge.pred - y_test)^2)
print(paste("Ridge Test MSE:", ridge.error))


### 4. Elastic Net Regression (alpha = 0.5)
set.seed(123)
cv.out.elastic <- cv.glmnet(x_train, y_train, alpha = 0.5, nfolds = 10, nlambda = 1000)
bestlam_elastic <- cv.out.elastic$lambda.min
elastic.mod <- glmnet(x_train, y_train, alpha = 0.5, lambda = bestlam_elastic)
elastic.pred <- predict(elastic.mod, s = bestlam_elastic, newx = x_test)
elastic.error <- mean((elastic.pred - y_test)^2)
print(paste("Elastic Net Test MSE:", elastic.error))

### 5.
library(glmnet)

# Define grid for alpha and lambda
alpha_grid <- seq(0, 1, by = 0.05)  # You can make it fine like 0.01 if you want
lambda_grid <- 10^seq(3, -3, length = 100)  # Adjusted: not too big to make computation faster

cv_errors <- matrix(NA, length(alpha_grid), 2)

# Cross-validation over alpha
set.seed(123)
for (i in 1:length(alpha_grid)) {
  alpha_val <- alpha_grid[i]
  
  cv_model <- cv.glmnet(x_train, y_train, alpha = alpha_val, lambda = lambda_grid, nfolds = 10)
  
  cv_errors[i, ] <- c(cv_model$lambda.min, min(cv_model$cvm))
}

# Find best alpha and lambda
best_index <- which.min(cv_errors[, 2])
best_alpha <- alpha_grid[best_index]
best_lambda <- cv_errors[best_index, 1]

cat("Best alpha:", best_alpha, "\n")
cat("Best lambda:", best_lambda, "\n")

# Fit Elastic Net model with best alpha and lambda
elastic_best_model <- glmnet(x_train, y_train, alpha = best_alpha, lambda = best_lambda)

# Predict and calculate Test MSE
elastic_best_pred <- predict(elastic_best_model, s = best_lambda, newx = x_test)
elastic_best_mse <- mean((elastic_best_pred - y_test)^2)

cat("Elastic Net Test MSE:", elastic_best_mse, "\n")

# Collect all test errors
error_table <- data.frame(
  Model = c("Lasso", "Ridge", "Elastic Net (tuned)"),
  Alpha = c(1, 0, best_alpha),
  Lambda = c(bestlam_lasso, bestlam_ridge, best_lambda),
  Test_MSE = c(lasso.error, ridge.error, elastic_best_mse)
)
print(error_table)

########### Part 2

#recode absenteeism
absent <- absent %>%
  mutate(absenteeism = case_when(
    Absenteeism_time_in_hours >= 0 & Absenteeism_time_in_hours <= 20 ~ "Low",
    Absenteeism_time_in_hours > 20 & Absenteeism_time_in_hours <= 40 ~ "Moderate",
    Absenteeism_time_in_hours > 40 ~ "High"
  ))
absent$absenteeism <- factor(absent$absenteeism, levels = c("Low", "Moderate", "High"))
View(absent)

str(absent)
set.seed(123)
train_size <- floor(0.7 * nrow(absent))
train_indices <- sample(seq_len(nrow(absent)), size = train_size)

# Predictors
x <- model.matrix(absenteeism ~ . - ID - Absenteeism_time_in_hours, data = absent)[, -1]
y <- absent$absenteeism

x_train <- x[train_indices, ]
x_test <- x[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]

############## LASSO
  
cv.out.lasso.cat <- cv.glmnet(x_train, y_train, alpha = 1, family = "multinomial", type.measure = "class")
bestlam_lasso_cat <- cv.out.lasso.cat$lambda.min
lasso.mod.cat <- glmnet(x_train, y_train, alpha = 1, lambda = bestlam_lasso_cat, family = "multinomial")

# Predict on test set
lasso.pred.cat <- predict(lasso.mod.cat, newx = x_test, s = bestlam_lasso_cat, type = "class")

# Classification Error
lasso.error.cat <- mean(lasso.pred.cat != y_test)
print(paste("Lasso Classification Error:", lasso.error.cat))


