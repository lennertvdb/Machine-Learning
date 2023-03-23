## Logistic regression 

# load library
library(pROC)
library(ROSE)
library(MASS)
library(magrittr)
# load data set (4 options available)

# option 1: external testing normalised
# train <- read.csv("./../data/3_gold/normalised_features_train_external.csv")
# val <- read.csv("./../data/3_gold/normalised_features_test.csv")

# option 2: external testing standarised
# train <- read.csv("./../data/3_gold/standardised_features_train_external.csv")
# val <- read.csv("./../data/3_gold/standardised_features_test.csv")

# option 3: internal testing normalised
train <- read.csv("./../data/3_gold/normalised_features_train.csv")
val <- read.csv("./../data/3_gold/normalised_features_val.csv")

# option 4: internal testing standardised
#train <- read.csv("./../data/3_gold/standardised_features_train.csv")
#val <- read.csv("./../data/3_gold/standardised_features_val.csv")

# PERFORM SUBSETTING
# OPTION 1 - select what you want
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

## OPTION 2 - remove what you don't want
# train_subset <- subset(train, select = -c(id))
# val_subset <- subset(val, select = -c(id))


# OPTIONAL - correct class imbalance
#train_balanced_over <- ovun.sample(default ~ ., data = train_subset, method = "over", N = 86858)$data
#train_balanced_under <- ovun.sample(default ~ ., data = train_subset, method = "under", N = 26668)$data
#train_balanced_rose <- ROSE(default ~ ., data = train_subset, seed = 1)$data

data_to_use <- train_subset  # train_subset - train_balanced_over - train_balanced_under - train_balanced_rose

## BUILD logistic regression model
model_binomial <-  glm(default ~ ., data = data_to_use, family = "binomial")

# Forward Stepwise Selection
# model_binomial <- model_binomial %>% stepAIC(trace = FALSE)
# coef(model_binomial)

model_proportions <- predict(model_binomial, newdata = val_subset, type = "response")

val$id <- as.integer(val$id)
typeof(val$id)

## MAKING csv in comment when internal testing
prop_test <- data.frame(val$id, model_proportions)
colnames(prop_test) <- c("id","default")

write.csv(prop_test, file = "./../data/results/logisticregression4forwardstepwise.csv", row.names = FALSE)

## AUC VALUE in comment when externally testing
auc(val$default, model_proportions)

## EXTRA interpreting the model in comment when externally testing
hist(model_proportions)
model_predictions = rep(0, length(model_proportions))
model_predictions[model_proportions > 0.5] = 1
table(model_predictions,val$default)
hist(model_predictions)

## optional: write proportions and labels to file
results <- data.frame(val$default, model_proportions)
write.csv(results, file = "./../data/results/lr_prop_validation.csv", row.names = FALSE)
