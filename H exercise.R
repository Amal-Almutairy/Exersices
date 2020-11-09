# Ridge Regression and the Lasso
library(ISLR)
library(glmnet)
library(dplyr)
library(tidyr)


Hitters = na.omit(Hitters)


x = model.matrix(Salary~., Hitters)[,-1] # trim off the first column
# leaving only the predictors
y = Hitters %>%
  select(Salary) %>%
  unlist() %>%
  as.numeric()

# Ridge Regression
grid = 10^seq(10, -2, length = 100)
ridge_mod = glmnet(x, y, alpha = 0, lambda = grid)

dim(coef(ridge_mod))
plot(ridge_mod)    # Draw plot of coefficients


ridge_mod$lambda[50] #Display 50th lambda value


coef(ridge_mod)[,50] # Display coefficients associated with 50th lambda value
sqrt(sum(coef(ridge_mod)[-1,50]^2)) # Calculate l2 norm



ridge_mod$lambda[60] #Display 60th lambda value
coef(ridge_mod)[,60] # Display coefficients associated with 60th lambda value
sqrt(sum(coef(ridge_mod)[-1,60]^2)) # Calculate l2 norm


predict(ridge_mod, s = 50, type = "coefficients")[1:20,]


lasso_mod
set.seed(1)

train = Hitters %>%
  sample_frac(0.5)

test = Hitters %>%
  setdiff(train)

x_train = model.matrix(Salary~., train)[,-1]
x_test = model.matrix(Salary~., test)[,-1]

y_train = train %>%
  select(Salary) %>%
  unlist() %>%
  as.numeric()

y_test = test %>%
  select(Salary) %>%
  unlist() %>%
  as.numeric()



ridge_mod = glmnet(x_train, y_train, alpha=0, lambda = grid, thresh = 1e-12)
ridge_pred = predict(ridge_mod, s = 4, newx = x_test)
mean((ridge_pred - y_test)^2)

mean((mean(y_train) - y_test)^2)

ridge_pred = predict(ridge_mod, s = 1e10, newx = x_test)
mean((ridge_pred - y_test)^2)

ridge_pred = predict(ridge_mod, s = 0, newx = x_test, exact = T)
mean((ridge_pred - y_test)^2)

lm(Salary~., data = train)
predict(ridge_mod, s = 0, exact = T, type="coefficients")[1:20,]



set.seed(1)
cv.out = cv.glmnet(x_train, y_train, alpha = 0) # Fit ridge regression model on training data
bestlam = cv.out$lambda.min  # Select lamda that minimizes training MSE
bestlam
plot(cv.out)
out = glmnet(x, y, alpha = 0) # Fit ridge regression model on full dataset
predict(out, type = "coefficients", s = bestlam)[1:20,] # Display coefficients using lambda chosen by CV


The Lasso
lasso_mod = glmnet(x_train, 
                   y_train, 
                   alpha = 1, 
                   lambda = grid) # Fit lasso model on training data

plot(lasso_mod)    # Draw plot of coefficients

set.seed(1)
cv.out = cv.glmnet(x_train, y_train, alpha = 1) # Fit lasso model on training data
plot(cv.out) # Draw plot of training MSE as a function of lambda
bestlam = cv.out$lambda.min # Select lamda that minimizes training MSE
lasso_pred = predict(lasso_mod, s = bestlam, newx = x_test) # Use best lambda to predict test data
mean((lasso_pred - y_test)^2) # Calculate test MSE


out = glmnet(x, y, alpha = 1, lambda = grid) # Fit lasso model on full dataset
lasso_coef = predict(out, type = "coefficients", s = bestlam)[1:20,] # Display coefficients using lambda chosen by CV
lasso_coef

lasso_coef[lasso_coef != 0] # Display only non-zero coefficients



grid = 1^seq(0, 1)
ridge_mod = glmnet(x, y, alpha = 0, lambda = grid)


#___________________________________

### 2
# Apply a lasso model with glmnet.
# What is the minimum MSE?
# What is the minimum MSE within 1 standard error?
# What are the lambda values for these MSEs?
  

# The Lasso
set.seed(1)
cv.out = cv.glmnet(x_train, y_train, alpha = 0) # Fit ridge regression model on training data
bestlam = cv.out$lambda.min  # Select lambda that minimizes training MSE
bestlam
plot(cv.out)
out = glmnet(x, y, alpha = 0) # Fit ridge regression model on full dataset
predict(out, type = "coefficients", s = bestlam)[1:20,] # Display coefficients using lambda chosen by CV


lasso_mod = glmnet(x_train, 
                   y_train, 
                   alpha = 1, 
                   lambda = grid) 

lasso_mod

# Fit lasso model on training data
plot(lasso_mod)    # Draw plot of coefficients

plot(cv.out) # Draw plot of training MSE as a function of lambda
bestlam = cv.out$lambda.min 
lasso_pred = predict(lasso_mod, s = bestlam, newx = x_test) # Use best lambda to predict test data
mean((lasso_pred - y_test)^2) # Calculate test MSE


out = glmnet(x, y, alpha = 1, lambda = grid) # Fit lasso model on full dataset
lasso_coef = predict(out, type = "coefficients", s = bestlam)[1:20,] # Display coefficients using lambda chosen by CV
lasso_coef

lasso_coef[lasso_coef != 0] # Display only non-zero coefficients


# What is the minimum MSE?
min(lasso_mod$cvm)
# What is the minimum MSE within 1 standard error?
lasso_mod$lambda.1se
lasso_mod$cvm[lasso_mod$lambda == lasso_mod$lambda.1se]
# What are the lambda values for these MSEs?
lasso_mod$lambda.min


set.seed(1)
cv.out = cv.glmnet(x_train, y_train, alpha = 1) 
# Fit lasso model on training data
plot(cv.out) # Draw plot of training MSE as a function of lambda
bestlam = cv.out$lambda.min # Select lambda that minimizes training MSE
lasso_pred = predict(lasso_mod, s = bestlam, newx = x_train) # Use best lambda to predict test data
mean((lasso_pred - y_test)^2) # Calculate test MSE


------------------------

# Ridge Regression
grid = 10^seq(10, -2, length = 100)
ridge_mod = glmnet(x, y, alpha = 0, lambda = grid)

x_train = model.matrix(Salary~., h_train)[,-1] 
x_test = model.matrix(Salary~., h_test)[,-1]

# leaving only the predictors
y_train = h_train %>%
  select(Salary) %>%
  unlist() %>%
  as.numeric()

# leaving only the predictors
y_test = h_test %>%
  select(Salary) %>%
  unlist() %>%
  as.numeric()

ridge_mod = glmnet(x_train, y_train, alpha=0, lambda = grid, thresh = 1e-12)
ridge_pred = predict(ridge_mod, s = 4, newx = x_test)

ridge_pred = predict(ridge_mod, s = 0, newx = x_test, exact = T)
mean((ridge_pred - y_test)^2)

dim(coef(ridge_mod))

set.seed(1)
cv.out = cv.glmnet(x_train, y_train, alpha = 0) # Fit ridge regression model on training data
bestlam = cv.out$lambda.min  # Select lamda that minimizes training MSE
bestlam

plot(ridge_mod)    # Draw plot of coefficients

ridge_mod$lambda[50] #Display 50th lambda value

coef(ridge_mod)[,50] # Display coefficients associated with 50th lambda value
sqrt(sum(coef(ridge_mod)[-1,50]^2)) # Calculate l2 norm


ridge_mod$lambda[60] #Display 60th lambda value
coef(ridge_mod)[,60] # Display coefficients associated with 60th lambda value
sqrt(sum(coef(ridge_mod)[-1,60]^2)) # Calculate l2 norm


predict(ridge_mod, s = 50, type = "coefficients")[1:20,]

# Fit ridge regression model on training data
bestlam = cv.out$lambda.min  # Select lamda that minimizes training MSE
bestlam
plot(cv.out)
out = glmnet(x, y, alpha = 0) # Fit ridge regression model on full dataset
predict(out, type = "coefficients", s = bestlam)[1:20,] 















