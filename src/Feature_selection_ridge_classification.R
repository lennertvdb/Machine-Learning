## Logistic regression 

# load library
library(pROC)
library(ROSE)
library(glmnet)


# normalised data
train <- read.csv("./../data/3_gold/normalised_features_train.csv")
val <- read.csv("./../data/3_gold/normalised_features_val.csv")

# PERFORM SUBSETTING
# OPTION 1
train_subset <- subset(train, select = -c(id))
val_subset <- subset(val, select = -c(id))

# OPTION 2
subset_vector_train <- c("debt_to_income",
                         "grade",
                         "income_verif_status",
                         "num_bankrupts",
                         "num_mortgages",
                         "num_open_credit",
                         "num_records",
                         "revol_balance",
                         "revol_util",
                         "term",
                         "emp_title_flag",
                         "num_mortgages_flag",
                         "num_total_credit_flag",
                         "address_postal_11650",
                         "address_postal_22690",
                         "address_postal_30723",
                         "address_postal_48052",
                         "address_postal_70466",
                         "address_postal_86630",
                         "address_postal_93700",
                         "application_type_INDIVIDUAL",
                         "date_funded_year",
                         "earliest_cr_line_year",
                         "home_status_RENT",
                         "sqrt_amount",
                         "log_annual_income",
                         "default")

subset_vector_test <- c("debt_to_income",
                        "grade",
                        "income_verif_status",
                        "num_bankrupts",
                        "num_mortgages",
                        "num_open_credit",
                        "num_records",
                        "revol_balance",
                        "revol_util",
                        "term",
                        "emp_title_flag",
                        "num_mortgages_flag",
                        "num_total_credit_flag",
                        "address_postal_11650",
                        "address_postal_22690",
                        "address_postal_30723",
                        "address_postal_48052",
                        "address_postal_70466",
                        "address_postal_86630",
                        "address_postal_93700",
                        "application_type_INDIVIDUAL",
                        "date_funded_year",
                        "earliest_cr_line_year",
                        "home_status_RENT",
                        "sqrt_amount",
                        "log_annual_income")

train_subset <- subset(train, select = subset_vector_train)
val_subset <- subset(val, select = subset_vector_test)

# OPTIONAL - correct class imbalance
#train_balanced_over <- ovun.sample(default ~ ., data = train_subset, method = "over", N = 86858)$data
#train_balanced_under <- ovun.sample(default ~ ., data = train_subset, method = "under", N = 26668)$data
#train_balanced_rose <- ROSE(default ~ ., data = train_subset, seed = 1)$data

data_to_use <- train_subset  # train_subset - train_balanced_over - train_balanced_under - train_balanced_rose

## LOGISTIC REGRESSION MODEL
model_binomial <-  glm(default ~ ., data = data_to_use, family = "binomial")
model_proportions <- predict(model_binomial, newdata = val_subset, type = "response")
auc(val$default, model_proportions)

## REGULARISATION (RIDGE AND LASSO)
train.x <- model.matrix(default ~ ., data = data_to_use)
train.y <- data_to_use$default
val.x <- model.matrix(default ~ ., data = data_to_use)
val.y <- data_to_use$default
grid <- 10 ^ seq(10, -10, length = 100)

# RIDGE
model_ridge <- cv.glmnet(train.x, train.y, family = "binomial", alpha = 0, lambda = grid, nfolds = 5)
model_proportions <- predict(model_ridge, newx = val.x, s = model_ridge$lambda.min, type = "response")
hist(model_proportions)               # histogram
print(model_ridge$lambda.min)         # best lambda
print(auc(val.y, model_proportions))  # AUC

## LASSO
model_lasso <- cv.glmnet(train.x, train.y, family = "binomial", alpha = 1, lambda = grid, nfolds = 5)
model_proportions <- predict(model_lasso, newx = val.x, s = model_lasso$lambda.min, type = "response")
hist(model_proportions)               # histogram
print(model_ridge$lambda.min)         # best lambda
print(auc(val.y, model_proportions))  # AUC

