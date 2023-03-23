library(e1071)
library(ROCR)
library(mlr)
library(h2o)
library(parallelMap)
library(parallel)
parallelStartSocket(cpus = detectCores())
set.seed(123)

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
                        "log_annual_income",
                        "default")
train_subset <- subset(train, select = subset_vector_train)
val_subset <- subset(val, select = subset_vector_test)


# # Splitting data
# train_Data <- read.csv("normalised_features_train.csv")
# train_Data <- subset(train_Data, select = -c(log_num_open_credit))
# 
# val_Data <- read.csv("normalised_features_val.csv")
# val_Data <- subset(val_Data, select = -c(log_num_open_credit))
# 
# # Selecting features
# train_X <- subset(train_Data, select = -c(default))
# train_y <- train_Data$default
# 
# 
# val_X <- subset(val_Data, select = -c(default))
# val_y <- val_Data$default
# 
# set.seed(1)


#create a task
traintask <- makeClassifTask(data = train_subset,target = "default") 
testtask <- makeClassifTask(data = val_subset,target = "default")

#create learner
# bag <- makeLearner("classif.rpart",predict.type = "prob")
# bag.lrn <- makeBaggingWrapper(learner = bag,bw.iters = 100,bw.replace = TRUE)
# 
# rdesc <- makeResampleDesc("CV",iters=5L)
# 
# r <- resample(learner = bag.lrn , task = traintask, resampling = rdesc, measures = list(tpr,fpr,fnr,fpr,acc,auc) ,show.info = T)

rf.lrn <- makeLearner("classif.randomForest",predict.type = "prob")
rf.lrn$par.vals <- list(ntree = 100L, importance=TRUE)
r <- resample(learner = rf.lrn, task = traintask, resampling = rdesc, measures = list(auc), show.info = T)

getParamSet(rf.lrn)

#set parameter space
params <- makeParamSet(makeIntegerParam("mtry",lower = 7,upper = 15),makeIntegerParam("nodesize",lower = 25,upper =45 ))

#set validation strategy
rdesc <- makeResampleDesc("CV",iters=5L)

#set optimization technique
ctrl <- makeTuneControlRandom(maxit = 5L)

#start tuning
tune <- tuneParams(learner = rf.lrn, task = traintask, resampling = rdesc, measures = list(auc), par.set = params, control = ctrl, show.info = T)
