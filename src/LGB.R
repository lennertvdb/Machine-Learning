### LightGBM ###

# setting seed
set.seed(123)

# load library
library(pROC)
library(ROSE)
library(MASS)
library(magrittr)
library(pROC)
library(lightgbm)

######### Read data  ##########

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

train_subset<-subset(train,select=subset_vector_train)
train_y<-train$default

val_subset<-subset(val,select=subset_vector_test)

val_matrix<-as.matrix(val_subset)


# OPTIONAL - correct class imbalance
#train_balanced_over <- ovun.sample(default ~ ., data = train_subset, method = "over", N = 86858)$data
#train_balanced_under <- ovun.sample(default ~ ., data = train_subset, method = "under", N = 26668)$data
train_balanced_rose <- ROSE(default ~ ., data = train_subset, seed = 1)$data

train_balanced_over_Y <- train_balanced_over$default
train_balanced_over <- subset(train_balanced_over, select = -c(default))

# train_subset <- subset(train_subset, select = -c(default))

dtrain <- lgb.Dataset(data = as.matrix(train_balanced_over), label = train_balanced_over_Y)
dval <- lgb.Dataset(data = as.matrix(val_subset), label = val$default)


#### TUNING PARAMETERS ####

# speed up computation with parallel processing
# library(doParallel)
# all_cores <- parallel::detectCores(logical = FALSE)
# registerDoParallel(cores = all_cores)

lgb.grid<-list(objective="binary",
                num_leaves=10,
                max_depth=12,
                metric="auc",
                boosting="gbdt",
                learning_rate=0.005,
                num_iterations=2950,
                min_data_in_leaf=40)

#when using imbalance delete the default
# train_balanced_over <- subset(train_balanced_over, select = -c(train_subset$default))

lgb.model <- lgb.train(data = dtrain,params = lgb.grid)
preds<-predict(lgb.model,val_matrix)

# in comment when externally testing
auc(val$default, preds)

# val$id <- as.integer(val$id)
# typeof(val$id)
# 
# ## MAKING csv in comment when internal testing\
# prop_test <- data.frame(val$id, preds)
# colnames(prop_test) <- c("id","default")
# 
# write.csv(prop_test, file = "./../data/results/lgb19-12.csv", row.names = FALSE)
# 
# 
# ## optional: write proportions and labels to file
# results <- data.frame(val$default, preds)
# write.csv(results, file = "./../data/results/lgb2_prop_validation.csv", row.names = FALSE)
