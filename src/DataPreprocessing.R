### Data Preprocessing ###
#
# Machine Learning - Group 08
#

######### Read data  ##########

# if you are internally testing use
train <- read.csv("./../data/2_silver/features_train.csv")
val <- read.csv("./../data/2_silver/features_val.csv")


# if you are externally testing use
# train <- read.csv("./../data/1_bronze/train.csv")
# val <- read.csv("./../data/1_bronze/test.csv")

train_X <- subset(train, select = -c(default))
train_y <- train$default

# # if you are internally testing use
val_X <- subset(val, select = -c(default))
val_y <- val$default

# if you are externally testing use
# val_X = val

######## set seed ##########

set.seed(123)

######### Import  libraries ##########

library(dummy)
library(corrplot)
library(ggplot2)

######### Define functions ##########

# Impute function
impute <- function(x, method = mean, val = NULL) {
  if (is.null(val)) {
    val <- method(x, na.rm = TRUE)
  }
  x[is.na(x)] <- val
  return(x)
}

# Handle outliers function
handle_outlier_z <- function(col){
  col_z <- scale(col)
  ifelse(abs(col_z)>3,
         sign(col_z)*3*attr(col_z,"scaled:scale") + attr(col_z,"scaled:center"), col)
}

# Make modus function
modus <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Create category function
create_category<-function(x,bins,strings=FALSE){
  x<-cut(x,breaks = bins,label= strings)
  x<-x-1
}

# Create category NA flag function
naFlag <- function(df, df_val = NULL) {
  if (is.null(df_val)) {
    df_val <- df
  }
  mask <- sapply(df_val, anyNA)
  out <- lapply(df[mask], function(x)as.numeric(is.na(x)))
  if (length(out) > 0) names(out) <- paste0(names(out), "_flag")
  return(as.data.frame(out))
}

######### checking missing values ##########

colMeans(is.na(train_X))
colMeans(is.na(val_X))

######### apply NA flag function ###########

train_X <- cbind(train_X,
                 naFlag(df = train_X))

val_X <- cbind(val_X,
               naFlag(df = val_X))    # should also be in validation set right?


######### handle outliers ##########

num.cols <- sapply(train_X, is.numeric)
num.cols["id"] <- FALSE
train_X[, num.cols] <-  sapply(train_X[, num.cols], FUN = handle_outlier_z)


######### prepossess features ##########

### feature 1.a - address (state)

# extract state from address field
# extract_states <- function(adres_col){
#   idx_regexec <- regexec("\\s(\\w{2})\\s\\d{5}$", adres_col)
#   match <- regmatches(adres_col, idx_regexec)
#   state_abr <- sapply(match, `[`, 2)
#   return(state_abr)
# }

# # get states
# train_X$address_state <- extract_states(train_X$address)
# val_X$address_state <- extract_states(val_X$address)

# show distribution between states
# table(train_X$address_state)
# barplot(table(train_X$address_state))
# 
# # Categorical data encoding
# cats <- categories(x = train_X[, c("address_state","id")])  # get categories and dummies
# 
# # apply on train an val set
# dummies_train <- dummy(train_X[, c("address_state","id")])
# dummies_val <- dummy(val_X[, c("address_state","id")])
# train_X <- cbind(train_X, dummies_train)
# val_X <- cbind(val_X, dummies_val)

### feature 1.b - address (postal code)

# extract postal code from address field
extract_postal <- function(adres_col){
  idx_regexec <- regexec("(\\d{5})$", adres_col)
  match <- regmatches(adres_col, idx_regexec)
  postal <- sapply(match, `[`, 2)
  return(postal)
}

# get postal
train_X$address_postal <- extract_postal(train_X$address)
val_X$address_postal <- extract_postal(val_X$address)

# get dummies of postal codes
dummies_train <- dummy(train_X[, c("address_postal","id")])
dummies_val <- dummy(val_X[, c("address_postal","id")])
train_X <- cbind(train_X, dummies_train)
val_X <- cbind(val_X, dummies_val)

# drop irrelevant columns 
train_X <- subset(train_X, select = -c(address, address_postal))
val_X <- subset(val_X, select = -c(address, address_postal))

# 1.c - additional information (home type)
train$port<-ifelse(grepl("Port",train$address),1,0)
df <- aggregate(train$id, by = list(train$port,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) +  geom_bar(position="fill", stat="identity")

train$Suite<-ifelse(grepl("Suite",train$address),1,0)
df <- aggregate(train$id, by = list(train$Suite,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) +  geom_bar(position="fill", stat="identity")

train$Apt<-ifelse(grepl("Apt.",train$address),1,0)
df <- aggregate(train$id, by = list(train$Apt,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) +  geom_bar(position="fill", stat="identity")


### feature 2 - amount
hist(sqrt(train_X$amount))
boxplot(log(train_X$amount), horizontal = TRUE)

# amount - default
df <- aggregate(train$id, by = list(train$amount,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) + geom_bar(position="fill", stat="identity")

ggplot(data=train, aes(x=default, y=amount, group = default))+geom_boxplot()

### feature 3 - annual_income
hist(train_X$annual_income, breaks = 200)
boxplot(train_X$annual_income, horizontal = TRUE)
hist(log(train_X$annual_income))
boxplot(log(train_X$annual_income), horizontal = TRUE)


# get median of annual income
mean(train_X$annual_income, na.rm = TRUE)
median(train_X$annual_income, na.rm = TRUE)

# Impute the values
train_X$annual_income <- impute(train_X$annual_income, method = median)
val_X$annual_income <- impute(val_X$annual_income, val = median(train_X$annual_income))

# annual_income - default
df <- aggregate(train$id, by = list(log(train$annual_income),train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) + geom_bar(position="fill", stat="identity")

ggplot(data=train, aes(x=default, y=log(annual_income), group = default))+geom_boxplot()

### feature 4 - debt_to_income

# Data exploration of the debt to income
hist(train$debt_to_income,breaks =10)

#After plotting the histogram we notice 1 large values and most of them in one large bin at the left hand of the 
#graph. So we check with a boxplot for outliers.
boxplot(train_X$debt_to_income)
#The boxplot shows us that we have 1 extreme outlier. This outlier may skew our data, as a result this should 
#be fixed

# Handle outliers
train_debt_to_income_z <- scale(train_X$debt_to_income)
hist(train_debt_to_income_z)

train_X$debt_to_income<-handle_outlier_z(train_X$debt_to_income)

boxplot(train_X$debt_to_income)
hist(scale(train_X$debt_to_income))

# debt_to_income - default
df <- aggregate(train$id, by = list(train$debt_to_income,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) + geom_bar(position="fill", stat="identity")

ggplot(data=train, aes(x=default, y=log(debt_to_income), group = default))+geom_boxplot()

### feature 5 - emp_length

# almost even distribution except for the 10+year class ( this is also our modus)
unique(train_X$emp_length)
length(unique(train_X$emp_length))

# we have a categorical variable so we need to determine the modus to impute the NA's

modus_emp_length<-modus(train_X$emp_length)
train_X$emp_length<-impute(train_X$emp_length,val = modus_emp_length)
sum(is.na(train_X$emp_length))

val_X$emp_length<-impute(val_X$emp_length, val = modus_emp_length)

# encode variable
unique(train_X$emp_length)
length(unique(train_X$emp_length))

#convert to numerical through regex. Note that <1y and 1 are already grouped together. Thus this is already binned.

match <- regmatches(train_X$emp_length, regexec("\\d+", train_X$emp_length))
train_X$emp_length <- sapply(match, `[`, 1)
train_X$emp_length<-as.numeric(train_X$emp_length)

#create bins [1,2](0)-[3,5](1)-[6-9](2)-[10+](3)
train_X$emp_length<-cut(train_X$emp_length,breaks=c(0,2,5,9,10),label=FALSE)
train_X$emp_length<-train_X$emp_length-1

match <- regmatches(val_X$emp_length, regexec("\\d+", val_X$emp_length))
val_X$emp_length <- sapply(match, `[`, 1)
val_X$emp_length<-as.numeric(val_X$emp_length)

val_X$emp_length<-cut(val_X$emp_length,breaks=c(0,2,5,9,10),label=FALSE)  #### valx -> val_x
val_X$emp_length<-val_X$emp_length-1

boxplot(train_X$emp_length)
hist(train_X$emp_length)

# emp_length - default
df <- aggregate(train$id, by = list(train$emp_length,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) + geom_bar(position="fill", stat="identity")

### feature 6 - grade
barplot(table(train_X$grade))

# encode variable
unique(train_X$grade)
grade_levels <- c("G", "F", "E", "D", "C","B","A")
train_X$grade<- as.numeric(factor(train_X$grade, levels = grade_levels))
val_X$grade <- as.numeric(factor(val_X$grade, levels= grade_levels))

barplot(table(train_X$grade))

# grade - default
df <- aggregate(train$id, by = list(train$grade,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) + geom_bar(position="fill", stat="identity")

### feature 7 - interest rate
hist(train_X$interest_rate)
boxplot(train_X$interest_rate)

# potential corrplot
#numeric_features<-c("amount","annual_income","debt_to_income","emp_length","grade","interest_rate","monthly_payment","revol_util","num_total_credit")
#huh<-train_X[,numeric_features]
#res<-cor(train_X[,numeric_features])
#corrplot(res, type = "upper", order = "hclust", 
#         tl.col = "black", tl.srt = 45)

# interest_rate - default
df <- aggregate(train$id, by = list(train$interest_rate,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) + geom_bar(position="fill", stat="identity")

ggplot(data=train, aes(x=default, y=interest_rate, group = default))+geom_boxplot()

### feature 8 - purpose
# check possible values
barplot(table(train_X$purpose))
table(train_X$purpose)

# Check different values
unique(train_X$purpose)

# Check importance of different purposes
prop.table(table(train_X$purpose))

# We make purpose binary: 1 if debt_consolidation 
#                         0 if not debt_consolidation

train_X$purpose <- ifelse(train_X$purpose =="debt_consolidation", 1,0)

val_X$purpose <- ifelse(val_X$purpose =="debt_consolidation", 1,0)

barplot(table(train_X$purpose))

# purpose - default
df <- aggregate(train$id, by = list(train$purpose,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) + geom_bar(position="fill", stat="identity")




### feature 9 - revol_util
hist(train_X$revol_util,breaks = 30)
summary(train_X$revol_util)

# Impute the missing values
train_X$revol_util <- impute(train_X$revol_util, method = mean)
val_X$revol_util <- impute(val_X$revol_util, val = mean(train_X$revol_util))

# EDA
mean(train_X$revol_util, na.rm = TRUE)
sd(train_X$revol_util, na.rm = TRUE)
median(train_X$revol_util, na.rm = TRUE)
quantile(train_X$revol_util, na.rm = TRUE)
summary(train_X)

# revol_util - default
df <- aggregate(train$id, by = list(train$revol_util,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) + geom_bar(position="fill", stat="identity")

ggplot(data=train, aes(x=default, y=revol_util, group = default))+geom_boxplot()

### feature 10 - term 

# Check different values
unique(train_X$term)

# We make purpose binary: 1 if term = 36 
#                         0 if term = 60

train_X$term <- ifelse(train_X$term == " 36 months", 1,0)
val_X$term <- ifelse(val_X$term == " 36 months", 1,0)
sum(train_X$term)

barplot(table(train_X$term))

# term - default
df <- aggregate(train$id, by = list(train$term,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) + geom_bar(position="fill", stat="identity")


### feature 11 - monthly_payment

#total monthly payment
boxplot(train_X$monthly_payment, names = "monthly_payment", show.names=TRUE)
summary(train_X$monthly_payment)
hist(train_X$monthly_payment)
hist(sqrt(train_X$monthly_payment))

#Missing values monthly payment

train_X$monthly_payment <- impute(train_X$monthly_payment, method = mean)  # maybe median?
summary(train_X$monthly_payment)

val_X$monthly_payment <- impute(val_X$monthly_payment, val = mean(train_X$monthly_payment))
summary(val_X$monthly_payment)

# monthly_payment - default
df <- aggregate(train$id, by = list(train$monthly_payment,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) + geom_bar(position="fill", stat="identity")

ggplot(data=train, aes(x=default, y=monthly_payment, group = default))+geom_boxplot()

### feature 12 - num_open_credit

#number of open credit lines: credit cards
boxplot(train_X$num_open_credit, names = 'num_open_credit', show.names=TRUE )
summary(train_X$num_open_credit)
hist(train_X$num_open_credit)
hist(log(train_X$num_open_credit))

# num_open_credit - default
df <- aggregate(train$id, by = list(train$num_open_credit,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) + geom_bar(position="stack", stat="identity")

### feature 13 - num_total_credit

#number of total credit lines
summary(train_X$num_total_credit)
boxplot(train_X$num_total_credit, names = 'num_total_credit', show.names=TRUE)
hist(train_X$num_total_credit)
hist(sqrt(train_X$num_total_credit))

#Missing values num_total_credit 
train_X$num_total_credit <- impute(train_X$num_total_credit, method = mean)  # maybe median?
summary(train_X$num_total_credit)

val_X$num_total_credit <- impute(val_X$num_total_credit, val = mean(train_X$num_total_credit))
summary(val_X$num_total_credit)

# num_total_credit - default
df <- aggregate(train$id, by = list(train$num_total_credit,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) + geom_bar(position="fill", stat="identity")

### feature 14 - num_records
#number of derogatory public records
summary(train_X$num_records)
barplot(table(train_X$num_records))

#Missing values num_records
train_X$num_records[is.na(train_X$num_records)] <- 0
val_X$num_records[is.na(val_X$num_records)] <- 0

### ADDITIONAL FEATURES ######

# 1 - application Type
dummies_train <- dummy(train_X[, c("application_type","id")])
dummies_val <- dummy(val_X[, c("application_type","id")])
train_X <- cbind(train_X, dummies_train)
val_X <- cbind(val_X, dummies_val)


# 2 - date_funded_year
extract_year <- function(date_col){
  idx_regexec = regexec("-(\\d{4})", date_col)
  match <- regmatches(date_col, idx_regexec)
  year <- sapply(match, `[`, 2)
  return(year)
}

train_X$date_funded_year <- extract_year(train_X$date_funded)
val_X$date_funded_year <- extract_year(val_X$date_funded)

# 3 - earliest_cr_line_year
train_X$earliest_cr_line_year <- extract_year(train_X$earliest_cr_line)
val_X$earliest_cr_line_year <- extract_year(val_X$earliest_cr_line)

# 4 - home status
train_X$home_status <- ifelse(train_X$home_status =="NONE",NA,train_X$home_status) # fill the NONE values with NA (category not present in validation and test set!)
train_X$home_status <- ifelse(train_X$home_status =="ANY",NA,train_X$home_status) # fill the NONE values with NA (category not present in validation and test set!)
val_X$home_status <- ifelse(val_X$home_status =="NONE",NA,val_X$home_status) # fill the NONE values with NA (category not present in validation and test set!)
val_X$home_status <- ifelse(val_X$home_status =="ANY",NA,val_X$home_status) # fill the NONE values with NA (category not present in validation and test set!)

train_X$home_status<-impute(x = train_X$home_status, val = modus(train_X$home_status)) # modus inpute
val_X$home_status <- impute(x = val_X$home_status, val = modus(train_X$home_status))
dummies_train <- dummy(train_X[, c("home_status","id")])
dummies_val <- dummy(val_X[, c("home_status","id")])
train_X <- cbind(train_X, dummies_train)
val_X <- cbind(val_X, dummies_val)

# 5 - income verif status
train_X$income_verif_status <- ifelse(train_X$income_verif_status =="Not Verified",0,1)
val_X$income_verif_status <- ifelse(val_X$income_verif_status =="Not Verified",0,1)

# 6 - nr of bankrupts
train_X$num_bankrupts <- impute(x = train_X$num_bankrupts, val = modus(train_X$num_bankrupts)) # mode inpute
val_X$num_bankrupts <- impute(x = val_X$num_bankrupts, val = modus(train_X$num_bankrupts))

# 7 - num_mortgages
train_X$num_mortgages <- impute(x = train_X$num_mortgages, val = modus(train_X$num_mortgages)) # mode inpute (0) (not median or mean)
val_X$num_mortgages <- impute(x = val_X$num_mortgages, val = modus(train_X$num_mortgages))

# 8 - revol_balance
# oke -> maybe transform
 
# drop  irrelevant columns (created dummies)
train_X <- subset(train_X, select = -c(application_type,date_funded,earliest_cr_line,home_status))
val_X <- subset(val_X, select = -c(application_type,date_funded,earliest_cr_line,home_status))


# drop irrelevant columns (not used at all)
train_X <- subset(train_X, select = -c(emp_title,sub_grade))
val_X <- subset(val_X, select = -c(emp_title,sub_grade))


# chek for nan values?
colMeans(is.na(train_X))
colMeans(is.na(val_X))

# num_records - default
df <- aggregate(train$id, by = list(train$num_records,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) + geom_bar(position="fill", stat="identity")

####### TRANSFORMATIONS ########

# feature 2: amount - take sqrt()
train_X$sqrt_amount <- sqrt(train_X$amount)
val_X$sqrt_amount <- sqrt(val_X$amount)

# feature 3: annual_income - take log() 
train_X$log_annual_income <- log(train_X$annual_income)
val_X$log_annual_income <- log(val_X$annual_income)

#feature 11 - monthly_payment - take sqrt()
train_X$sqrt_monthly_payment <- sqrt(train_X$monthly_payment)
val_X$sqrt_monthly_payment <- sqrt(val_X$monthly_payment)

# feature 12: num_open_credit - take log()
#train_X$log_num_open_credit <- log(train_X$num_open_credit)
#val_X$log_num_open_credit <- log(val_X$num_open_credit)

# feature 13: num_total_credit - take sqrt()
train_X$sqrt_num_total_credit <- sqrt(train_X$num_total_credit)
val_X$sqrt_num_total_credit <- sqrt(val_X$num_total_credit)

# feature 14: date funded
idx_regexec = regexec("(\\w{3})-(\\d{4})", train$date_funded)
match <- regmatches(train$date_funded, idx_regexec)

train$date_funded_month <- sapply(match, `[`, 2)
train$date_funded_year <- sapply(match, `[`, 3)
df <- aggregate(train$id, by = list(train$date_funded_year,train$date_funded_month,train$default), FUN = length)
ggplot(df, aes(fill=Group.3, y=x, x=Group.1)) +  geom_bar(position="fill", stat="identity")
ggplot(df, aes(fill=Group.3, y=x, x=Group.2)) +  geom_bar(position="fill", stat="identity")


# feature 15: earliest_cr_line
idx_regexec = regexec("(\\w{3})-(\\d{4})", train$earliest_cr_line)
match <- regmatches(train$earliest_cr_line, idx_regexec)
train$earliest_cr_line_month <- sapply(match, `[`, 2)
train$earliest_cr_line_year <- sapply(match, `[`, 3)

df <- aggregate(train$id, by = list(train$earliest_cr_line_year,train$earliest_cr_line_month,train$default), FUN = length)
ggplot(df, aes(fill=Group.3, y=x, x=Group.1)) +  geom_bar(position="fill", stat="identity")
ggplot(df, aes(fill=Group.3, y=x, x=Group.2)) +  geom_bar(position="fill", stat="identity")

# feature 16: home status
df <- aggregate(train$id, by = list(train$home_status,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) +  geom_bar(position="fill", stat="identity")

# feature 17: income_verif_status
df <- aggregate(train$id, by = list(train$income_verif_status,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) +  geom_bar(position="fill", stat="identity")

# feature 18: num_bankrupts
df <- aggregate(train$id, by = list(train$num_bankrupts,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) +  geom_bar(position="fill", stat="identity")

# feature 19: num_mortgages
df <- aggregate(train$id, by = list(train$num_mortgages,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) + geom_bar(position="fill", stat="identity")

# feature 20: revol_balance
ggplot(data=train, aes(x=default, y=log(revol_balance), group = default))+geom_boxplot()

# feature 21: sub_grade
idx_regexec = regexec("(\\w{1})(\\d{1})", train$sub_grade)
match <- regmatches(train$sub_grade, idx_regexec)
train$sub_grade_letter <- sapply(match, `[`, 2)
train$sub_grade_digit <- sapply(match, `[`, 3)

df <- aggregate(train$id, by = list(train$sub_grade_letter, train$sub_grade_digit, train$default), FUN = length)
ggplot(df, aes(fill=Group.3, y=x, x=Group.1)) + geom_bar(position="fill", stat="identity")
ggplot(df, aes(fill=Group.3, y=x, x=Group.2)) + geom_bar(position="fill", stat="identity")

# feature 22: application_type
df <- aggregate(train$id, by = list(train$application_type,train$default), FUN = length)
ggplot(df, aes(fill=Group.2, y=x, x=Group.1)) + geom_bar(position="fill", stat="identity")


# remove unnessasary columns
train_X <- subset(train_X, select = - c(amount,annual_income,monthly_payment,
                                    num_total_credit))
val_X <- subset(val_X, select = - c(amount,annual_income,monthly_payment,
                                     num_total_credit))


######## WRITE PROCCESED DATA to file ##########

train = cbind(train_X, train_y)
val = cbind(val_X, val_y)

names(train)[names(train) == "train_y"] <- "default"
names(val)[names(val) == "val_y"] <- "default"

#write.csv(train,"./../data/2_silver/cleaned_features_train.csv", row.names = FALSE)
#write.csv(val,"./../data/2_silver/cleaned_features_val.csv", row.names = FALSE)


######## SCALING #########

# make sure the variables are in numeric format (necesary for scaling)
train_X$date_funded_year <- as.numeric(as.character(train_X$date_funded_year))
val_X$date_funded_year <- as.numeric(as.character(val_X$date_funded_year))
train_X$earliest_cr_line_year <- as.numeric(as.character(train_X$earliest_cr_line_year))
val_X$earliest_cr_line_year <- as.numeric(as.character(val_X$earliest_cr_line_year))

# initialise cols to scale
cols_to_scale <- c("debt_to_income",
                   "emp_length",
                   "grade",
                   "interest_rate",
                   "num_mortgages",
                   "num_open_credit",
                   "num_records",
                   "revol_balance",
                   "revol_util",
                   "date_funded_year",
                   "earliest_cr_line_year",
                   "sqrt_amount",
                   "log_annual_income",
                   "sqrt_monthly_payment", 
                   "sqrt_num_total_credit")

# get statistics of training set
mean_train <- colMeans(train_X[, cols_to_scale])
sd_train <- sapply(train_X[, cols_to_scale], sd)
min_train <- sapply(train_X[, cols_to_scale], min)
max_train <- sapply(train_X[, cols_to_scale], max)

# duplicate in order to prevent overwriting
train_X_standardised <- train_X
val_X_standardised <- val_X
train_X_normalised <- train_X
val_X_normalised <- val_X

# standardise  (mu = 0, sigma = 1)
standardise <- function(x, x_avg, x_sd) {  
  return((x - x_avg)/x_sd)
}

for (column in cols_to_scale){
  train_X_standardised[, column] <- sapply(train_X[, column], standardise, x_avg = mean_train[column], x_sd = sd_train[column])
  val_X_standardised[,column] <-  sapply(val_X[, column], standardise, x_avg = mean_train[column], x_sd = sd_train[column])
}

# normalise (x between [0-1])
normalise <- function(x, x_min, x_max) {  
  return((x - x_min)/(x_max - x_min))
}

for (column in cols_to_scale){
  train_X_normalised[, column] <- sapply(train_X[, column], normalise, x_min = min_train[column], x_max = max_train[column])
  val_X_normalised[,column] <-  sapply(val_X[, column], normalise, x_min = min_train[column], x_max = max_train[column])
}

# check if scaling went well
colMeans(train_X_standardised[, cols_to_scale])
sapply(train_X_standardised[, cols_to_scale], sd)
sapply(train_X_normalised[, cols_to_scale], min)
sapply(train_X_normalised[, cols_to_scale], max)

colMeans(val_X_standardised[, cols_to_scale])
sapply(val_X_standardised[, cols_to_scale], sd)
sapply(val_X_normalised[, cols_to_scale], min)
sapply(val_X_normalised[, cols_to_scale], max)

##### WRITE SCALED DATA to file #####

train_standardised = cbind(train_X_standardised, train_y)
# next line in comment when testing
val_standardised = cbind(val_X_standardised, val_y)
train_normalised = cbind(train_X_normalised, train_y)
# next line in comment when testing
val_normalised = cbind(val_X_normalised, val_y)

names(train_standardised)[names(train_standardised) == "train_y"] <- "default"
# next line in comment when testing
names(val_standardised)[names(val_standardised) == "val_y"] <- "default"
names(train_normalised)[names(train_normalised) == "train_y"] <- "default"
# next line in comment when testing
names(val_normalised)[names(val_normalised) == "val_y"] <- "default"


# when external testing
# write.csv(train_standardised,"./../data/3_gold/standardised_features_train_external.csv", row.names = FALSE)
# write.csv(train_normalised,"./../data/3_gold/normalised_features_train_external.csv", row.names = FALSE)
# write.csv(val_X_standardised,"./../data/3_gold/standardised_features_test.csv", row.names = FALSE)
# write.csv(val_X_normalised,"./../data/3_gold/normalised_features_test.csv", row.names = FALSE)

# when internal testing
write.csv(train_standardised,"./../data/3_gold/standardised_features_train.csv", row.names = FALSE)
write.csv(train_normalised,"./../data/3_gold/normalised_features_train.csv", row.names = FALSE)
write.csv(val_standardised,"./../data/3_gold/standardised_features_val.csv", row.names = FALSE)
write.csv(val_normalised,"./../data/3_gold/normalised_features_val.csv", row.names = FALSE)

