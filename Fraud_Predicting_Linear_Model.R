
# Preliminaries
# Retrieve datasets:
# Start by downloading the credit card fraud data from the course database with this query:
  
  SELECT * FROM cc_fraud;

#Retrieving from local source
fraud <- read.csv("fraud.csv", stringsAsFactors = FALSE)
fraud_original <- fraud # saving original copy for later

# Loading required libraries
library(plyr)
library(dplyr)
library(reshape2)
library(varhandle)
library(cvAUC)
library(stringr)
```
# Describing the data. 
# The dataset contains transaction information of 5000 customers over a calender year.
# It contains 29 Variables (columns) and 641914 rows which means that the customers carried out about 10 transactions per month on average
# Of the 29 columns, 9 are continous, 9 categorical and 11 binary/ logical. 
# Six of the logical columns contain only NA values. These are "echobuffer", "merchantcity", "merchantstate", "merchantzip", "posonpremises" & "recurringauthind". In addition two other columns contain some NA. These are "posentrymode" (3345 entries) & "posconditioncode" (287 entries). 
# These NA entries would be removed especailly since they do not include any of the fradulent transactions and thus should not affect our model inordinately


determine_cols_of_only_NA <- function(db){
  cols_of_only_NA <-colnames(db)[which(sapply(db, function(x){sum(is.na(x))}) == nrow(db))]
  paste("These columns are only made up of NAs:", cols_of_only_NA)
  return(cols_of_only_NA)
}

cols_of_NA <- determine_cols_of_only_NA(fraud)

print("These columns have some NA:", sapply(fraud, function(x){sum(is.na(x))}))

nrow(fraud[is.na(fraud$posentrymode) & fraud$isfraud == TRUE,])
nrow(fraud[is.na(fraud$posconditioncode) & fraud$isfraud == TRUE,])


# Three of the columns that do not contain NA values have missing data (i.e. ""). These are "acqcountry" (3913 entries), "merchantcountrycode" (624 entries) and "transactiontype" (589 entries).

# These missing entries are to be removed since they do not include any of the fradulent transaction


NA_infected <- colnames(fraud) %in% c(cols_of_NA, "posentrymode", "posconditioncode")
col_of_missing_vals <- colnames(fraud[,!NA_infected])[which(sapply(fraud[,!NA_infected], function(x){any(x == "")}))]
col_of_missing_vals

nrow(fraud[fraud$merchantcountrycode == "" & fraud$isfraud == TRUE,])
nrow(fraud[fraud$acqcountry == "" & fraud$isfraud == TRUE,])
nrow(fraud[fraud$transactiontype == "" & fraud$isfraud == TRUE,])


# Other items to note include:
# The account number and customerid columns are the same and can be treated as one.
# The credit limit though numeric and seemingly continous is actually categorical with ten categories.

# The account balance is the difference between creditlimit and availablemoney. This means that all three should not be put in the model. We would therefore not add availablemoney. Also, we shall replace available money and transactionamount by th ratio of transactionamount to availablemoney

all(fraud$accountnumber == fraud$customerid)
sapply(fraud, function(x){length(unique(x))})

# We now remove the NAs and Missing entries before developing new variables for modeling. 
# We also remove any duplicate entries

strip_na_missing_duplicate_data <- function(db, NA_cols){
  db <- db[, !colnames(db) %in% NA_cols]
  db <- db[!is.na(db$posentrymode) & !is.na(db$posconditioncode),]
  db <- db[!db$merchantcountrycode == "" & !db$acqcountry == "" & !db$transactiontype == "",]
  db <- db[!duplicated(db),]
  return(db)
}

fraud <- strip_na_missing_duplicate_data(fraud, cols_of_NA)


# I would categorize the variables I can create into three types:
#  1) Date Based variables (these would be numeric)

# - <b>account_age: </b>difference between account opening data and transaction date. This is because newer accounts may be more susceptible to fraud. This would be continous
# - <b>address_change:</b> difference between last date of address change and transaction date. This is because address change may proceed fraud as the fraudsters may want to send whatever they buy to a new address or may need the address to utilize the card. This would be continous

# 2) Variables for Particular Transaction Details 
# - <b>inaccurate_cvv:</b> Captures whether the cvv was captured accurately. The idea is that fraudsters may not know the cvv and may put in the wrong one. This is binary
# - <b>trans_amount_ratio:</b> Captures the size of the transaction amount relative to the available money. This is because fraudsters may seek to retreve the maximum they can in one transaction. This is continous
# -<b> Fixed Merchant name :</b> There are 205 merchant names (if we remove the receipt number which staarts with the # figure behind the merchant name). We can thus treat them like category variable especially if we shorten the names by replacing each with a number indicating its position when all merchant are ordered by number of transactions done for each merchant. <b> NOTE: </b> please this means that we need to remove merchantcategorycode because each is a collection of merchant names and thus may lead to singularities if both are used as variables

# 3) Variables capturing Customer behaviour/ Account Status
# - <b>days_trans: </b>Number of transactions a customer makes in a day. Multiple transactions may be indicative of fraudulent activity. This would be continous
# - <b>previous_fraud:</b> If there had been instances of previous fraud in the account.This would be binary


add_new_variables <- function(db){
  # Data conversions - Date
  db$transactiondatetime <- as.Date(db$transactiondatetime)
  db$dateoflastaddresschange <- as.Date(db$dateoflastaddresschange)
  db$accountopendate <- as.Date(db$accountopendate)
  
  # Cleaning up the merchantname column and acsribing unique numbers to each marchant so it can be used as a variable
  db$merchantname_clean <- str_trim(str_split_fixed(db$merchantname, "#",2)[,1], side = c("right"))
  mer_name_table <- as.data.frame(table(db$merchantname_clean))
  colnames(mer_name_table)[1] <- c("merchantname_clean")
  mer_name_table <- cbind(mer_name_table[order(mer_name_table$Freq, decreasing = T),], FM = c(1:nrow(mer_name_table)))
  db <- merge(db, mer_name_table[c("merchantname_clean", "FM")], by = "merchantname_clean")
  
  # Converting logicals to binary numeric (1 and 0)
  db$isfraud <- ifelse(db$isfraud == "true", 1,0)
  db$cardpresent <- ifelse(db$cardpresent == "true", 1,0)
  db$expirationdatekeyinmatch <- ifelse(db$expirationdatekeyinmatch == "true", 1,0)
  
  # Creating of date based variables 
  db$account_age <- as.numeric(db$transactiondatetime - db$accountopendate)
  db$address_change <- as.numeric(db$transactiondatetime - db$dateoflastaddresschange)
  
  # Creating the card entry variables
  db$inaccurate_cvv <- ifelse(db$cardcvv == db$enteredcvv, FALSE, TRUE)
  
  # Creating trans_amount_ratio variable
  db$trans_amount_ratio <- db$transactionamount/db$availablemoney
  
  # Creating the Number of transactions by customer per day variable
  transac <- db[c("customerid", "transactiondatetime")]
  transac$no <- 1
  transac_2 <- dcast(transac, customerid+transactiondatetime ~ .,value.var = "no", fun=sum)
  colnames(transac_2)[colnames(transac_2) == "."] <- "days_trans"
  db <- merge(db, transac_2, by = c("customerid", "transactiondatetime"))
  
  
  return(db)
}

fraud <- add_new_variables(fraud)


# Creating unique_id for each transaction based on ordering by customer_id and transaction date


add_transaction_unique_id <- function(db){
  # In order to provide a unique transaction number
  db <- db[with(db, order(db$customerid, db$transactiondatetime)),]
  db$unique_id <- c(1:nrow(db))
  return(db)
}
fraud <- add_transaction_unique_id(fraud)

# Treating Categorical variables
# The following variable are either categorical or binary and were changed to factors and each value converted to a dummy variable:
#  "acqcountry", "merchantcountrycode", "posentrymode", "posconditioncode", "transactiontype", "cardpresent","expirationdatekeyinmatch", "inaccurate_cvv", "FM"
# The caterogy_vars_handler function does this but also prints the catrgory variables in order to enable choice of defualt values to be made.

# The following default values were chosen:
#  "creditlimit.5000", "merchantcountrycode.US", "posentrymode.5",  "posconditioncode.1", "merchantcategorycode.cable/phone", "transactiontype.PC", "cardpresent.0", "expirationdatekeyinmatch.0", inaccurate_cvv.FALSE"


# Selecting and treating categorical data
category_cols <- c("acqcountry", "merchantcountrycode", "posentrymode", "posconditioncode", "transactiontype", "cardpresent","expirationdatekeyinmatch", "inaccurate_cvv", "FM")

# Function that handles categoric data for this lab
create_category_vars_dummies <- function(db, category_cols){
  # Convert category variables to factors
  db[, category_cols] <- data.frame(sapply(db[, category_cols],  as.factor))

  # Change race/ethnicity categories for space
  levels(db$transactiontype) <- c("AV", "PC", "RV")

  # For each category variable, create a set of dummies and append to the      data set
  for (i in 1:length(category_cols)) {
    dummies <- to.dummy(db[, category_cols[i]], category_cols[i])
    db <- cbind(db, dummies)
    }

  # Examine category variables by sorting by frequency and determining the     default values 
  paste(sapply(fraud[, category_cols], function(x) {
    names(table(x))[order(table(x), decreasing = T)]
    }))
  return(db)
}

# Calling the function
fraud <- create_category_vars_dummies(fraud, category_cols)

# Checking if there are rows with inordinate number of NAs 
sum(sapply(fraud, function(x) {sum(is.na(x))}))


# Divide dataset into train, test and validate sets. The function create_sets does that by creating a column in the dataset that can be used to subset the data set as required

# Subset the data: 70% training, 20% test, 10% validation

# Speficifying function to create column to enable subset of dataset
create_set_column <- function(db, vector_of_three_ratios){
  # This function takes in a vector of three ratios that must add up to 1 and uses the largest ratio for training set, the next largest for test and the smallest for validation
# Validate inputs
  numerify <- as.numeric(as.character(vector_of_three_ratios))
  stopifnot(is.vector(numerify) == TRUE)
  if(!as.logical(abs(1 - sum(numerify)) < 0.01 & length(numerify) == 3))stop("Sum of ratios must be 1 and the number of ratios must be three")
  
  # Ordering in order to select highest ratio for train, then next for test and least for validate
  ordered <- numerify[order(numerify, decreasing = T)]
  a <- ordered[1]
  b <- ordered[2]
  c <- ordered[3]
  
  # Create a dataframe of unique ids and random values
sets <- data.frame(unique_id = db$unique_id,
           rand = runif(length(db$unique_id)), isfraud = db$isfraud )

  # Assign status based on unique values and merge into data
sets$set <- ifelse(sets$rand < a, 'train', ifelse(sets$rand >= (1 - c), 'validate', 'test'))
db <- merge(sets[, c('unique_id', 'set', "isfraud")], db[,-which(colnames(db) == "isfraud")], by = 'unique_id')
return(db)
}

# Calling the function and Subset by set type
fraud <- create_set_column(fraud, c(0.1, 0.2, 0.7))
train <- fraud[fraud$set == "train",]
test <- fraud[fraud$set == "test",]
validate <- fraud[fraud$set == "validate",]

# Evaluate distributions of some variables we might want to stratify by
strats <- c("creditlimit", "acqcountry", "merchantcountrycode", "posentrymode")
for (i in 1:length(strats)){
  print(table(fraud[, strats[i]])/nrow(fraud))
  print(table(train[, strats[i]])/nrow(train))
  print(table(test[, strats[i]])/nrow(test))
  print(table(validate[, strats[i]])/nrow(validate))
}

Estimate and constrain a predictive model. Explain the performance of your new variables in the unconstrained and constrained models. Interpret the coefficients and what their value signifies relative to the probability of a fraudulent transaction.

Initial Estimation:

# removing columns which are default values, set, student_id and the original category columns
remove_cols <- c("unique_id", "set", "customerid", "transactiondatetime", "availablemoney", "accountnumber", "merchantname", "currentexpdate", "accountopendate", "dateoflastaddresschange", "cardcvv", "enteredcvv", "cardlast4digits", "currentbalance", "creditlimit", "creditlimit.5000", "acqcountry", "acqcountry.US","merchantcountrycode", "merchantcountrycode.US", "posentrymode", "posentrymode.5", "posconditioncode", "posconditioncode.1", "merchantcategorycode", "transactiontype", "transactiontype.PC", "cardpresent.0", "cardpresent", "expirationdatekeyinmatch.0", "expirationdatekeyinmatch", "inaccurate_cvv.FALSE", "inaccurate_cvv", "account_age", "merchantname_clean", "FM", "FM.1")
keep_cols <- colnames(train)[!colnames(train) %in% remove_cols ]

# Creating the model and var_full the final data used to make the model
create_logit_model_and_varfull <- function(db, keep_cols){
  model_formula <- formula(db[keep_cols])
  db <- db[which(complete.cases(db[,all.vars(model_formula)])),]
  db <- db[keep_cols]

# Estimating the model
  model <- glm(model_formula, db, family = binomial(link = "logit"))
  return( list(model, db) )
}

compendium <- create_logit_model_and_varfull(train, keep_cols)
var_full <- compendium[[2]]
logit_model <- compendium[[1]]
summary(logit_model)


# Constraining the model
constrained_logit_model <- step(logit_model, direction = 'forward')
summary(constrained_logit_model)


# Optimize a threshold for your model's predicted values. Describe your process for optimizing and what it considers. 

# To optimize a threshold for the model I determined a F1 value that in theory would provide a balance between precision and recall. The threshold that provided the maximum F1 value [where F1 is given as (precision*recall)/(precision + recall)] through trying out a list of potential values for the threshold was selected.

coefficients <- constrained_logit_model$coefficients[!is.na(constrained_logit_model$coefficients)]

pred_score_function_factory <- function(db){
  pred_score <- function(obs, coefficients) 
  {pred <- rbind.fill(obs[names(db) %in% names(coefficients)],
                      as.data.frame(t(coefficients))) %>% t %>% as.data.frame %>% subset(!is.na(V1))
  pred$product <- pred$V1 * pred$V2
  return(1/(1 + exp(-(sum(pred$product, unname(coefficients[1]))))))
  }
}

# Checking the function against model's fitted values
i <- 1
pred_score_train <- pred_score_function_factory(var_full)
pred_score_train(var_full[i,], constrained_logit_model$coefficients)
constrained_logit_model$fitted.values[i]

# Establish possible thresholds

# Create an f_function factory and initiate an f1 function
f_function_factory <- function(beta){
  fbeta <- function(preci, reca){
    ((1+beta^2)*preci*reca)/((beta^2*preci) + reca)
  }
}

esthablish_best_threshold <- function(db, model, beta){
thresh <- data.frame(threshold = seq(0, 1, 0.01))
# Add predicted values to initial dataframe
db <- data.frame(db, pred = model$fitted.values)
thresh$precision <- apply(thresh, 1, function(x) {sum(db$pred > x & db$isfraud == 1)/sum(db$pred > x)})
thresh$recall <- apply(thresh, 1, function(x) {sum(db$pred > x["threshold"] & db$isfraud == 1)/sum(db$isfraud == 1)})
#thresh$F1 <- 2 * ((thresh$precision * thresh$recall)/(thresh$precision + thresh$recall))
fbeta <- f_function_factory(beta)
thresh$fbeta <- fbeta(thresh$precision, thresh$recall)
best_thresh <- thresh[which.max(thresh$fbeta), "threshold"]
paste("The details of the best threshold are: ", thresh[which.max(thresh$fbeta),])
return(best_thresh)
}

# Calculating threshold for with F1 (ie. beta = 1)
best_thresh <- esthablish_best_threshold(var_full, constrained_logit_model, 1)

Fbeta = (1 + beta^2)(precision * recall) / ((beta^2*precision)+ recall).
F2 gives 5*(precision * recall)/((4*precision)+ recall).

# F2 give a threshold of 0.05 (test dataset) and AUC of 0.58
# Whilst F3 would be maximized by a threshold of 0.03 and AUC of 0.66
# As a result, recall increases from 19% to 42%. Higher F subscripts would result in higher recall rates with a threshold of 0.0 giving a recall of 100% and 0.01 giving a recall of 87%.
# Thus, reducing the threshold increases recall. On the other hand, increasing the threshold up until 0.37 incrased the precision for the model.

# calculating thresholds for optimized precision

# calculating for F2
threshold_F2 <- esthablish_best_threshold(var_full, constrained_logit_model, 2)
threshold_F2


threshold_F3 <- esthablish_best_threshold(var_full, constrained_logit_model, 3)
threshold_F3


# Calculating AUC (performance metrics) for the Training set
train_auc <- AUC(ifelse(constrained_logit_model$fitted.values > best_thresh, 1, 0), var_full$isfraud)
train_auc


# Performance on Test set
The AUC of the test set after the initial run was 0.5936233 (this also changes each time the program is run).


# Ensuring that all coeeficients are non-NA
coefficients <- constrained_logit_model$coefficients[!is.na(constrained_logit_model$coefficients)]

# Initializing a pre_score function for the test data
pred_score_test <- pred_score_function_factory(test)

# Creating a dataframe that includes the predictions for test
test_pred <- ldply(lapply(split(test, test$unique_id), function(x) {
  data.frame(pred = pred_score_test(x, coefficients), actual = x$isfraud)
}), rbind)


# Calcuating the AUC for the test data
test_auc <- AUC(ifelse(test_pred$pred > best_thresh, 1, 0), test_pred$actual)
test_auc


# Calculating difference between the AUC of the train set and the test set
# For the initial run, the difference between train and test AUC was 0.004571325 (0.76%)
# The differennce is recalculted below to demonstrate that they are basically equivalent.

# Train vs Test AUC difference
train_auc - test_auc

# Train vs Test AUC difference in percent
(train_auc - test_auc)*100/train_auc



# To improve this model, we would do more than just removing variables or constraing it in a different way. We would develop new variables based on changes to customer behavior (e.g. unusual behaviors for the customer) that should enable us predict fraud when these are added to our previous variables.
#  - <b>add_veri: </b> If there has been an address verification in any of the previous five transactions by a customer before the current transaction. This is created as address verification may be triggered by merchants who have these requirement for new customers and may indicate new activity. This would be binary
#  - <b>last_fraud: </b> If there was a fraudulent transaction in any of the five previous transaction before the curent transaction. This is created as fraudulent activities may come in bunches. This would be binary
#  - <b>outlier_merchant_name: If the merchant is new (an outlier when consumer previous behavior is considered) and not typical of customer. This is binary
#  - <b>outlier_merchant_country: If the merchant country is new (an outlier when consumer previous behavior is considered) and not typical of customer. This is binary
#  - <b>outlier_merchant_cat: </b> If the merchant category is new (an outlier) and not typical of customer. This is binary
#  - <b>outlier_amount: </b> When the amount requested is an outlier and very different from previous amounts. This is binary


# Create cutomer tansaction id that would enable us create the customer behavior based variables

create_customer_transaction_id <- function(db){
  temp <- db[-nrow(db), c("customerid","transactiondatetime")]
  temp <- rbind(db[1, c("customerid","transactiondatetime")], temp)
  temp <- cbind(unique_id = c(1:nrow(db)), temp)
  db <- merge(db, temp, by="unique_id")
  db$cus_change <- ifelse(!db$customerid.x == db$customerid.y, TRUE, FALSE)
  db$day_change <- ifelse(!db$transactiondatetime.x == db$transactiondatetime.y, TRUE, FALSE)

  # Lining up the customer rows
  cus_chge_db <- db[db$cus_change == TRUE,]
  cus_chge_db$temp_id <- c(1: nrow(cus_chge_db))
  temp2 <- cus_chge_db[-1, c("unique_id")]
  temp2 <- c(temp2, as.numeric(db[nrow(db), c("unique_id")])+1)
  temp2 <- cbind(temp_id = c(1:length(temp2)), dup_unique_id = temp2)
  cus_chge_db <- merge(cus_chge_db, temp2, by="temp_id")
  used <- rbind(data.frame(unique_id = c(1), "dup_unique_id" =  c(cus_chge_db[1,"unique_id"])), cus_chge_db[,c("unique_id", "dup_unique_id")])

  num_by_cus <- function(x){
    lead <- as.numeric(x["dup_unique_id"]) 
    follow <-  as.numeric(x["unique_id"])
    #print(lead)
    #print(follow)
    dif <- lead - follow
    return(data.frame(unique_id = c(follow: (lead -1) ), cus_transaction_id = c(1:dif)))
  }

  result_cus <- do.call("rbind", apply(used, 1, num_by_cus))
  return(merge(db, result_cus, by = "unique_id"))
}

create_customer_behavior_variables <- function(x){
  #print(head(x))
  row_reaper <- function(z){
    # Identifying the current row
    t11 <- as.numeric(z["cus_transaction_id"])
    
    window_frame <- 5
    tstart <- t11 - window_frame
    tstop <- t11 -1
    xseq <- seq(1:t11)
    if(t11 > window_frame){
    xseq_gt <- c(tstart: tstop)}
    
    # Selecting the variable columns from first customer transaction to the transaction of interest (i.e current row of apply function)
    test_name <- x$FM[xseq]
    test_cat <- x$merchantcategorycode[xseq]
    test_ctry <- x$merchantcountrycode[xseq]
    test_amount <- x$transactionamount[xseq]
    test_fraud <- x$isfraud[xseq]
    test_id <- x$unique_id[xseq]
    if(t11 <= window_frame){test_veri <- x$transactiontype[xseq]}else{test_veri <- x$transactiontype[xseq_gt]}
    
    # Ensuring that the fraud indicator 1 or 0 are numeric to avoid errors later in the script
    test_fraud <- as.numeric(as.character(test_fraud))
    
    # Ordering the character variables from most frequent to least frequent so that conversion to numerals can happen to enable outliers to be identified
    track_name <- names(table(test_name))[order(table(test_name), decreasing = T)]
    unq_name <- seq(1, length(unique(test_name)))
    tested_name <- unname( apply(data.frame(test_name), 1, function(x){ ifelse(any(track_name %in% x), unname(unq_name[which(track_name %in% x)]), x)}))
    
    track_cat <- names(table(test_cat))[order(table(test_cat), decreasing = T)]
    unq_cat <- seq(1, length(unique(test_cat)))
    tested_cat <- unname( apply(data.frame(test_cat), 1, function(x){ ifelse(any(track_cat %in% x), unname(unq_cat[which(track_cat %in% x)]), x)}))
    
    track_ctry <- names(table(test_ctry))[order(table(test_ctry), decreasing = T)]
    unq_ctry <- seq(1, length(unique(test_ctry)))
    tested_ctry <- unname( apply(data.frame(test_ctry), 1, function(x){ ifelse(any(track_ctry %in% x), unname(unq_ctry[which(track_ctry %in% x)]), x)}))
  
  #Identification of outliers using the boxplot function. If the entry is deemed an outlier, TRUE is returned, else FALSE is returned
  outvals_name <- boxplot(tested_name, plot=FALSE)$out
  out_name <- if(tested_name[t11] %in% outvals_name){paste0(TRUE)}else{paste0(FALSE)}
  
  outvals_cat <- boxplot(tested_cat, plot=FALSE)$out
  out_cat <- if(tested_cat[t11] %in% outvals_cat){paste0(TRUE)}else{paste0(FALSE)}

  outvals_ctry <- boxplot(tested_ctry, plot=FALSE)$out
  out_ctry <- if(sum(tested_ctry %in% tested_ctry[t11]) >= 1 & tested_ctry[t11] %in% outvals_ctry){paste(TRUE)}else{paste(FALSE)}
  
  out_veri <- ifelse(sum(ifelse(test_veri == "ADDRESS_VERIFICATION",1,0)) > 0, TRUE, FALSE)
  
  outvals_amount <- boxplot(test_amount, plot=FALSE)$out
  out_amount <- if(sum(test_amount %in% test_amount[t11]) >= 1 & test_amount[t11] %in% outvals_amount){paste(TRUE)}else{paste(FALSE)} 
  
  #Previous code left in for reference that was similar to those used for outvals_name, outvals_cat and outvals_ctry to restrict the TRUE outputs to only the first time a merchant name, country or category is utilized the first time.
  #out_amount <- if(sum(test_amount %in% test_amount[t11]) == 1 & test_amount[t11] %in% outvals_amount){paste(TRUE)}else{paste(FALSE)} 
  #print(outvals_amount)
  
  if(t11 <= window_frame){yseq <- c(1:tstop)}else{yseq <- xseq_gt}
  out_fraud <- if(t11 < 2){paste(FALSE)}else if(sum(test_fraud[yseq]) >= 1){paste(TRUE)}else{paste(FALSE)}

  
  # output <-data.frame( unique_id = c(test_id[t11]), outlier_cat = c(out_cat), outlier_cntry = c(out_ctry), veri_change = c(out_veri), outlier_amount = c(out_amount), previous_fraud = c(out_fraud))
  output <- c(test_id[t11], out_name, out_cat, out_ctry, out_veri, out_amount, out_fraud)
  print(test_id[t11])
  #print(output)
  return(output)
  }
  as.data.frame.array(t(apply(x, 1, row_reaper)))
}

# Generate a new set of data that includes the user behavior based variables
fraud <- fraud_original %>% determine_cols_of_only_NA %>% strip_na_missing_duplicate_data(fraud,.) %>% add_new_variables %>% add_transaction_unique_id %>% create_customer_transaction_id

t <- split(fraud, fraud$customerid.x)

y <-  ldply(lapply(t, create_customer_behavior_variables), rbind)
colnames(y) <- c("customer_id", "unique_id", "outlier_name", "outlier_cat", "outlier_ctry", "recent_veri", "outlier_amount", "recent_fraud")

# Merging fraud with customer behavior variables
fraud <- merge(fraud, y[,-1], by = "unique_id")

# Preparing for and creating category variables
category_cols <- c("acqcountry", "merchantcountrycode", "posentrymode", "posconditioncode", "transactiontype", "cardpresent","expirationdatekeyinmatch", "inaccurate_cvv", "outlier_name", "outlier_cat", "outlier_ctry", "recent_veri", "outlier_amount", "recent_fraud", "FM")

fraud <- create_category_vars_dummies(fraud, category_cols)

# Creating training, test and validation sets
fraud <- create_set_column(fraud, c(0.1, 0.2, 0.7))
train <- fraud[fraud$set == "train",]
test <- fraud[fraud$set == "test",]
validate <- fraud[fraud$set == "validate",]

# Preparing for initial model fitting - removing the merchantcategorycode variables
remove_cols <- c("unique_id", "set", "customerid", "transactiondatetime", "availablemoney", "accountnumber", "merchantname", "currentexpdate", "accountopendate", "dateoflastaddresschange", "cardcvv", "enteredcvv", "cardlast4digits", "currentbalance", "creditlimit", "creditlimit.5000", "acqcountry", "acqcountry.US","merchantcountrycode", "merchantcountrycode.US", "posentrymode", "posentrymode.5", "posconditioncode", "posconditioncode.1", "merchantcategorycode", "transactiontype", "transactiontype.PC", "cardpresent", "cardpresent.0",  "expirationdatekeyinmatch", "expirationdatekeyinmatch.0", "inaccurate_cvv", "inaccurate_cvv.FALSE", "account_age", "outlier_name", "outlier_name.FALSE", "outlier_cat", "outlier_cat.FALSE", "outlier_ctry", "outlier_ctry.FALSE", "recent_veri", "recent_veri.FALSE",  "outlier_amount", "outlier_amount.FALSE", "recent_fraud", "recent_fraud.FALSE", "cus_transaction_id", "day_change", "days_trans", "customerid.y", "transactiondatetime.y", "cus_change", "customerid.x", "transactiondatetime.x", "merchantname_clean", "FM", "FM.1")

keep_cols <- colnames(train)[!colnames(train) %in% remove_cols ]

# Estimating the model
compendium <- create_logit_model_and_varfull(train, keep_cols)
var_full <- compendium[[2]]
logit_model <- compendium[[1]]
summary(logit_model)

```

# Constraining the new, updated model
# I noticed that the usual backward direction takes too long to complete and the forward direction does not add any value (it seems to abort without constraining the model really) so, I in this instance, manually removed those variables that do not meet the z_value > 2 requirement. Hopefully, this would lead to a lower than 12% difference in train as against test data AUC seen in th first model

```{r}
# Constrained model - selecting the variables to constrain
#constrained_logit_model <- step(logit_model, direction = 'forward')
coeff <- summary(logit_model)
coeff <- as.data.frame.array(coeff$coefficients)
colnames(coeff)[colnames(coeff) == "z value"] <- "z_value"
coeff <- coeff[abs(coeff$z_value)>=2,]
keep_cols2 <- row.names(coeff)
keep_cols2 <- c("isfraud", keep_cols2[-1])

# Rerunning a constrained model
compendium2 <- create_logit_model_and_varfull(train, keep_cols2)
var_full2 <- compendium2[[2]]
constrained_logit_model <- compendium2[[1]]
summary(constrained_logit_model)

# Check function against model's fitted values
i <- 1
pred_score_train <- pred_score_function_factory(var_full2)
pred_score_train(var_full2[i,], constrained_logit_model$coefficients)
constrained_logit_model$fitted.values[i]

# Establish possible thresholds
# Threshold for F1 (beta = 1)
best_thresh <- esthablish_best_threshold(var_full2, constrained_logit_model, 1)

# Threshold for F2 (beta = 2)
threshold_F2 <- esthablish_best_threshold(var_full2, constrained_logit_model, 2)
threshold_F2

# Threshold for F3 (beta = 3)
threshold_F3 <- esthablish_best_threshold(var_full2, constrained_logit_model, 3)
threshold_F3

# Calculate train AUC for F1 (beta = 1)
train_auc <- AUC(ifelse(constrained_logit_model$fitted.values > best_thresh, 1, 0), var_full2$isfraud)
train_auc

# Calculate train AUC for F2 (beta = 2)
train_auc <- AUC(ifelse(constrained_logit_model$fitted.values > threshold_F2, 1, 0), var_full2$isfraud)
train_auc

# Calculate train AUC for F3 (beta = 3)
train_auc <- AUC(ifelse(constrained_logit_model$fitted.values > threshold_F3, 1, 0), var_full2$isfraud)
train_auc

# Initializing a pre_score function for the test data
pred_score_test <- pred_score_function_factory(test)

# Creating a dataframe that includes the predictions for test
test_pred <- ldply(lapply(split(test, test$unique_id), function(x) {
  data.frame(pred = pred_score_test(x, constrained_logit_model$coefficients), actual = x$isfraud)
}), rbind)


# Calcuating the AUC for the test data
test_auc <- AUC(ifelse(test_pred$pred > threshold_F3, 1, 0), test_pred$actual)
test_auc

# Train vs Test AUC difference
train_auc - test_auc

# Train vs Test AUC difference in percent
(train_auc - test_auc)*100/train_auc
