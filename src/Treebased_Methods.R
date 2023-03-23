### Treebased methods ###

######### Read data  ##########
library(parallelMap)
library(parallel)
parallelStartSocket(cpus = detectCores())
library(pROC)
library(ROSE)
library(MASS)
library(magrittr)

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

######## set seed ##########

set.seed(123)

######### Import  libraries ##########

library(randomForest)
library(pROC)

######### Method #########
bag <- randomForest(y = train_subset[, 27], x = train_subset[, 1:26], data = train_subset, importance = TRUE, mtry = 12, nodesize = 37)

bag <- bag %>% stepAIC(trace = FALSE)
model_proportions <- predict(bag, newdata = val_subset, type = "response")

# ## MAKING csv in comment when internal testing
# prop_test <- data.frame(val$id, model_proportions)
# colnames(prop_test) <- c("id","default")
# 
# write.csv(prop_test, file = "./../data/results/randomforest1.csv", row.names = FALSE)

## AUC VALUE in comment when external
auc(val$default, model_proportions)

## EXTRA interpreting the model in comment when external
hist(model_proportions)
model_predictions = rep(0, length(model_proportions))
model_predictions[model_proportions > 0.5] = 1
table(model_predictions,val$default)
hist(model_predictions)
