# ******************************************************************************
#                   ** Machine Learning for Business Intelligence *
#                                     Flow ID:100 
#                                   Date: 3.1.2025
# ******************************************************************************


#*******************************************************************************
#*******************************************************************************
# ============================== Problem 1 (AAT)==============================
#*******************************************************************************
#*******************************************************************************



#*******************************************************************************
## ================================= Part 1.1 =================================
#*******************************************************************************

# Load the provided dataset into R. Select columns 1 to 18, and column 57. Your selection should
# include the following variables:

### =============================== Exercise a) ===============================

#loading data
original_data = read.csv("exam_folder/Data_exams/sampled_data.csv", header = TRUE, sep = ";")

#creating a copy
data = original_data


# Select columns 1 to 18, and column 57 from data
data = data[, c(1:18, 57)]

#check if the columns were selected correctly
colnames(data)



### =============================== Exercise b) ===============================

# replace " " with NA
data[data == " "] = NA


### =============================== Exercise c) ===============================

# Remove the existing missing values from the dataset. 

data <- na.omit(data)

#check if the missing values were removed
library(DataExplorer)

plot_missing(data)
# no missing values are present

### =============================== Exercise d) ===============================


# Adapt the variable types to correspond to the types mentioned in the list above.

str(data)
# 'data.frame':	1986 obs. of  19 variables:
#   $ SEXO        : int  6 6 1 1 6 1 6 1 6 6 ...
# $ EDAD        : int  56 42 57 58 41 65 40 47 31 24 ...
# $ ESTC        : int  5 2 2 1 2 2 2 1 1 1 ...
# $ NIVELEST    : int  7 5 3 3 7 7 2 5 2 3 ...
# $ SIT_LAB     : int  3 1 1 1 1 1 1 3 2 2 ...
# $ TIP_JOR     : int  1 2 1 1 1 1 1 2 1 1 ...
# $ ACTIV       : int  9 4 2 9 6 9 4 4 4 9 ...
# $ OCUPACION1  : int  6 6 6 6 6 6 6 1 6 6 ...
# $ OCUPACION2  : int  6 6 6 6 6 6 6 6 6 6 ...
# $ ING_HOG     : int  3 3 5 3 5 5 1 5 6 3 ...
# $ TIP_H       : int  1 4 4 1 4 3 4 5 2 4 ...
# $ TOT_MH      : int  1 4 3 1 4 2 3 3 2 3 ...
# $ TOTAL10_15  : int  0 1 0 0 0 0 1 0 0 0 ...
# $ TOT_MEN16   : int  0 2 0 0 2 0 1 0 0 0 ...
# $ TOT_MAY74   : int  0 0 0 0 0 0 0 1 0 0 ...
# $ TOT_16_64_T : int  1 2 2 1 2 0 2 1 2 1 ...
# $ TOT_16_64_NT: int  0 0 1 0 0 0 0 0 0 1 ...
# $ TOT_16_24_E : int  0 0 0 0 0 0 0 0 0 0 ...
# $ PROD12      : int  6 6 2 6 1 6 6 1 6 6 ...

#all are numeric right now

#Types required
# 1. SEXO (factor)
# 2. EDAD (numeric)
# 3. ESTC (factor)
# 4. NIVELEST (factor)
# 5. SIT_LAB (factor)
# 6. TIP_JOR (factor)
# 7. ACTIV (factor)
# 8. OCUPACION1 (factor)
# 9. OCUPACION2 (factor)
# 10. ING_HOG (factor)
# 11. TIP_H (factor)
# 12. TOT_MH (numeric)
# 13. TOTAL10_15 (numeric)
# 14. TOT_MEN16 (numeric)
# 15. TOT_MAY74 (numeric)
# 16. TOT_16_64_T (numeric)
# 17. TOT_16_64_NT (numeric)
# 18. TOT_16_24_E (numeric)
# 19. PROD12 (factor)

#converting to factors with a custom function

strings_to_factors = function(dataset, columns_to_convert) {
  # Loop through the specified column names
  for (col in columns_to_convert) {
    # Check if the column exists in the dataset
    if (col %in% colnames(dataset)) {
      dataset[, col] = as.factor(dataset[, col])
    } else {
      warning(paste("Column", col, "does not exist in the dataset. Skipping."))
    }
  }
  return(dataset)
}

colums_to_factor = c("SEXO", "ESTC", "NIVELEST", "SIT_LAB", "TIP_JOR", "ACTIV", "OCUPACION1", "OCUPACION2", "ING_HOG", "TIP_H", "PROD12")

data = strings_to_factors(data, colums_to_factor)

#check if the conversion was successful
str(data)

# 'data.frame':	1986 obs. of  19 variables:
#   $ SEXO        : Factor w/ 2 levels "1","6": 2 2 1 1 2 1 2 1 2 2 ...
# $ EDAD        : int  56 42 57 58 41 65 40 47 31 24 ...
# $ ESTC        : Factor w/ 5 levels "1","2","3","4",..: 5 2 2 1 2 2 2 1 1 1 ...
# $ NIVELEST    : Factor w/ 10 levels "0","1","2","3",..: 8 6 4 4 8 8 3 6 3 4 ...
# $ SIT_LAB     : Factor w/ 3 levels "1","2","3": 3 1 1 1 1 1 1 3 2 2 ...
# $ TIP_JOR     : Factor w/ 2 levels "1","2": 1 2 1 1 1 1 1 2 1 1 ...
# $ ACTIV       : Factor w/ 11 levels "1","2","3","4",..: 9 4 2 9 6 9 4 4 4 9 ...
# $ OCUPACION1  : Factor w/ 2 levels "1","6": 2 2 2 2 2 2 2 1 2 2 ...
# $ OCUPACION2  : Factor w/ 2 levels "1","6": 2 2 2 2 2 2 2 2 2 2 ...
# $ ING_HOG     : Factor w/ 6 levels "1","2","3","4",..: 3 3 5 3 5 5 1 5 6 3 ...
# $ TIP_H       : Factor w/ 5 levels "1","2","3","4",..: 1 4 4 1 4 3 4 5 2 4 ...
# $ TOT_MH      : int  1 4 3 1 4 2 3 3 2 3 ...
# $ TOTAL10_15  : int  0 1 0 0 0 0 1 0 0 0 ...
# $ TOT_MEN16   : int  0 2 0 0 2 0 1 0 0 0 ...
# $ TOT_MAY74   : int  0 0 0 0 0 0 0 1 0 0 ...
# $ TOT_16_64_T : int  1 2 2 1 2 0 2 1 2 1 ...
# $ TOT_16_64_NT: int  0 0 1 0 0 0 0 0 0 1 ...
# $ TOT_16_24_E : int  0 0 0 0 0 0 0 0 0 0 ...
# $ PROD12      : Factor w/ 3 levels "1","2","6":

#Looks correct



### =============================== Exercise e) ================================

# Recode the values of variable PROD12:
# • Set values 1 and 2 to Buy, representing individuals who have purchased food online.
# • Set value 6 to Notbuy, representing individuals who have not purchased food online.
# Hint: data_Sel$PROD12 <- ifelse(data_Sel$PROD12 == "1" | data_Sel$PROD12 == "2", "Buy",
#                                 "Notbuy")
# After recoding, make sure PROD12 is a factor, and show explicitly its distribution.

#recoding
data$PROD12 <- ifelse(data$PROD12 == "1" | data$PROD12 == "2", "Buy", "Notbuy")

str(data$PROD12)
# chr [1:1986] "Notbuy" "Notbuy" "Buy" "Notbuy" "Buy" "Notbuy" "Notbuy" "Buy" "Notbuy" "Notbuy" "Notbuy" "Buy" "Buy" "Buy" "Notbuy" ...

#PROD12 is character, so let's convert to factor

data$PROD12 = as.factor(data$PROD12)

#Show distribution of PROD12
table(data$PROD12)
# Buy Notbuy 
# 589   1397 

prop.table(table(data$PROD12))
# Buy       Notbuy 
# 0.296576 0.703424 

#roughly 30% of the respondents belong to the Buy category, while 70% are in the Notbuy category

### =============================== Exercise f) ================================

# Split the data into training and test set using a random stratified sampling based on PROD12.
# Proceed by creating a blueprint that prepares the data for analysis in the following order:
# • Centre and scale all numeric features.
# • Handle infrequent categories in categorical variables.
# • Transform all the categorical variables into dummy variables.
# • Eliminate all features with near-zero variance.
# • Prepare and bake both training and test data using the blueprint created before.
# • Report explicitly the size of the new datasets.


# Split the data
# we are not told what proportion of the split the use, so I will go with 70%
library(rsample)
set.seed(123) # for reproducibility
split <- initial_split(data, prop = 0.7, strata = "PROD12")

train_data <- training(split)
test_data <- testing(split)

library(recipes)

#create blueprint
recipe <- recipe(PROD12 ~ ., data = train_data) %>%
  # 1. Center and scale all numeric features
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  
  # 2. Handle infrequent categories in categorical variables (I leave the threshold to be default at 0.05)
  step_other(all_nominal(), -all_outcomes()) %>%
  
  # 3. Transform all the categorical variables into dummy variables.
  step_dummy(all_nominal(), -all_outcomes(), one_hot = FALSE) %>%
  
  # 4. Eliminate all features with near-zero variance.
  step_nzv(all_predictors())

recipe

# prepare
prepare <- prep(recipe, training = train_data)

prepare

# bake
baked_train <- bake(prepare, new_data = train_data)
baked_test <- bake(prepare, new_data = test_data)

# • Report explicitly the size of the new datasets.

#size of baked_train
dim(baked_train)

# [1] 1389   34

# baked_train has 1389 rows and 34 columns

dim(baked_test)

# [1] 597  34

# baked_test has 597 rows and 34 columns


### =============================== Exercise g) ================================


# Using the package caret in R, apply k-fold cross-validation to train a set of classification models
# (minimum three) to predict the likelihood of an individual purchasing food online.

library(caret)

# Specify re-sampling strategy: k-fold cross-validation
cv <- trainControl(
  method = "cv", 
  number = 10,
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
  )

### ===== Model 1 - Logistic Regression Caret =====

lr_caret <- train(
  relevel(PROD12, ref = "Buy") ~ .,
  data = baked_train, 
  method = "glm",
  family = "binomial",
  trControl = cv,
  metric="ROC" # We are not told what metric to optimize for, so I selected ROC (could be also `Accuracy`)
)

# I selected ROC to optimize for to get more advanced metrics in the output like ROC, Sensitivity and Specificity

lr_caret
# ROC        Sens        Spec     
# 0.5629143  0.04105691  0.9692826


summary(lr_caret)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    1.71418    0.46442   3.691 0.000223 ***
#   EDAD          -0.09599    0.07532  -1.275 0.202485    
# TOT_MH         0.25292    0.23962   1.056 0.291192    
# TOTAL10_15     0.11620    0.07810   1.488 0.136797    
# TOT_MEN16     -0.21117    0.16344  -1.292 0.196351    
# TOT_16_64_T    0.11798    0.11783   1.001 0.316715    
# TOT_16_64_NT   0.13242    0.13556   0.977 0.328668    
# TOT_16_24_E   -0.12464    0.09134  -1.365 0.172388    
# SEXO_X6       -0.11559    0.13424  -0.861 0.389171    
# ESTC_X2       -0.09477    0.17791  -0.533 0.594249    
# ESTC_X5       -0.21238    0.25316  -0.839 0.401527    
# NIVELEST_X3   -0.27664    0.20861  -1.326 0.184809    
# NIVELEST_X5   -0.30509    0.22119  -1.379 0.167797    
# NIVELEST_X6   -0.56886    0.22747  -2.501 0.012391 *  
# NIVELEST_X7   -0.56833    0.21722  -2.616 0.008886 ** 
# SIT_LAB_X2    -0.05438    0.21172  -0.257 0.797289    
# SIT_LAB_X3    -0.24922    0.18192  -1.370 0.170699    
# TIP_JOR_X2    -0.18599    0.19563  -0.951 0.341741    
# ACTIV_X4       0.17897    0.23367   0.766 0.443718    
# ACTIV_X5      -0.50351    0.32715  -1.539 0.123783    
# ACTIV_X8       0.26157    0.30218   0.866 0.386698    
# ACTIV_X9       0.21370    0.20871   1.024 0.305879    
# ACTIV_X10      0.06049    0.28502   0.212 0.831916    
# ACTIV_other    0.37121    0.24657   1.506 0.132190    
# OCUPACION1_X6 -0.07601    0.19777  -0.384 0.700730    
# OCUPACION2_X6  0.23062    0.30517   0.756 0.449823    
# ING_HOG_X3    -0.19488    0.17189  -1.134 0.256890    
# ING_HOG_X4    -0.58056    0.22513  -2.579 0.009915 ** 
#   ING_HOG_X5    -0.37286    0.20903  -1.784 0.074471 .  
# ING_HOG_X6    -0.20732    0.20728  -1.000 0.317195    
# TIP_H_X2      -0.28772    0.28074  -1.025 0.305435    
# TIP_H_X3      -0.19493    0.28300  -0.689 0.490937    
# TIP_H_X4      -0.55176    0.34788  -1.586 0.112725    
# TIP_H_X5      -0.60419    0.36023  -1.677 0.093501 .  
# ---

# Calculate the average roc across the folds
lr_caret_train_roc = mean(lr_caret$resample$ROC)
lr_caret_train_roc

# predict class membership for new data
lr_caret_class = predict(lr_caret, newdata = baked_test) # default is class
lr_caret_class

# predicted probabilities for new data
lr_caret_probabilities <- predict(lr_caret, newdata = baked_test, type = "prob")
lr_caret_probabilities

# create a confusion matrix and assess performance on unseen data
confusionMatrix(lr_caret_class, baked_test$PROD12)

# Reference
# Prediction Buy Notbuy
# Buy      4     11
# Notbuy 173    409
# 
# Accuracy : 0.6918         
# 95% CI : (0.653, 0.7286)
# No Information Rate : 0.7035         
# P-Value [Acc > NIR] : 0.7503       

#accuracy
lr_caret_test_accuracy = mean(lr_caret_class == baked_test$PROD12)
lr_caret_test_accuracy
# Test accuracy is 0.6917923

### ===== Model 2 - KNN caret ===== 


# Tune a knn model using grid search to find the optimal number of neigbours
knn_fit_caret <- train(
  relevel(PROD12, ref = "Buy") ~ ., #telling caret reference level
  data = baked_train, 
  method = "knn", 
  trControl = cv,
  tuneGrid = expand.grid(k = seq(2, 50, by = 1)),
  metric = "ROC"
)

knn_fit_caret

# plot the accuracy across different number of neigbours
ggplot(knn_fit_caret)

knn_fit_caret$bestTune
#Optimal k = 33

# Calculate the average ROC for model with k=33
knn_fit_caret_train_roc = mean(knn_fit_caret$resample$ROC)
knn_fit_caret_train_roc

# [1] 0.5323871

# predict class membership for new data
knn_fit_caret_class = predict(knn_fit_caret, newdata = baked_test) # default is class
knn_fit_caret_class

# predicted probabilities for new data
knn_fit_caret_probabilities <- predict(knn_fit_caret, newdata = baked_test, type = "prob")
knn_fit_caret_probabilities

# create a confusion matrix and assess performance on unseen data
confusionMatrix(knn_fit_caret_class, baked_test$PROD12)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction Buy Notbuy
# Buy      2      2
# Notbuy 175    418
# 
# Accuracy : 0.7035          
# 95% CI : (0.6651, 0.7399)
# No Information Rate : 0.7035          
# P-Value [Acc > NIR] : 0.5203

#accuracy
knn__caret_test_accuracy = mean(knn_fit_caret_class == baked_test$PROD12)
knn__caret_test_accuracy

# Test set Accutacy for knn with 33 neighbours is 0.7035176

### ===== Model 3 - LDA caret ===== 

lda_caret = train(
  relevel(PROD12, ref = "Buy") ~ .,
  data = baked_train, 
  method="lda",
  trControl = cv, 
  metric="ROC")

lda_caret
# ROC        Sens        Spec     
# 0.5694321  0.04872242  0.9692615

summary(lda_caret)

# Calculate the average ROC
lda_caret_train_roc = mean(lda_caret$resample$ROC)
lda_caret_train_roc
# [1] 0.5694321

# predict class membership for new data
lda_caret_class = predict(lda_caret, newdata = baked_test) # default is class
lda_caret_class

# predicted probabilities for new data
lda_caret_probabilities <- predict(lda_caret, newdata = baked_test, type = "prob")
lda_caret_probabilities

# create a confusion matrix and assess performance on unseen data
confusionMatrix(lda_caret_class, baked_test$PROD12)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction Buy Notbuy
# Buy      5     13
# Notbuy 172    407
# 
# Accuracy : 0.6901  

#accuracy
lda_caret_test_accuracy = mean(lda_caret_class == baked_test$PROD12)
lda_caret_test_accuracy
# Accuracy of LDA model is 0.6901173




### =============================== Exercise h) ================================

# Discuss the output of each model, focusing on their strengths and weaknesses in the context
# of the problem.

# Logistic Regression
# The output of the logistic regression gives us the beta coefficients for each predictor. We can use these coefficients to understand the relationship between the predictors and the outcome variable. 
# So, the strength of the logistic regression model is that it provides interpretable results.

# This is not the case for knn and lda models, which do not provide interpretable results.

# KNN
# Strenght of knn is that it is extremely flexible. We can define the number of k neighbours, and it can be used for both classification and regression problems. It is also easy to understand and implement.

# The downside is that we dont really get insights on variable importance

# LDA
# LDA is a generative model that models the distribution of the predictors in each class. It is a simple model that can be used for classification problems.
# The downside is some of the assumpitions that need to be fullfiled. In our case we have 2 classes, and the predictors are assumed to be normally distributed in each class. This might not be the case in our dataset.
# It also assumes the the classes are separable by linear boundary



### =============================== Exercise i) ================================
# Compare the models based on their performance metrics, and make a few recommendations
# on how the models should be improved without changing the predictors.


prop.table(table(baked_train$PROD12))
# Buy    Notbuy 
# 0.2966163 0.7033837 

# The proportion of Buy vs Notbuy is 30% vs 70% respectively. This means that the dataset is imbalanced,
# We should keep this in mind, because we see that most of our models are just guessing the majority class, which is Notbuy. And this leads to a high specificity, but low sensitivity.

summary(
  resamples(
    list(
      model1 = lr_caret,
      model2 = knn_fit_caret,
      model3 = lda_caret
    )
  )
)$statistics

# $ROC
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# model1 0.4659034 0.5364417 0.5474057 0.5629143 0.5919111 0.6775996    0
# model2 0.4651748 0.4956231 0.5346565 0.5323871 0.5675026 0.5973121    0
# model3 0.4902818 0.5345321 0.5722278 0.5694321 0.5958485 0.6645097    0
# 
# $Sens
#        Min.     1st Qu.     Median        Mean    3rd Qu.       Max. NA's
# model1    0 0.006097561 0.04878049 0.041056911 0.06576655 0.09523810    0
# model2    0 0.000000000 0.00000000 0.007317073 0.01829268 0.02439024    0
# model3    0 0.048780488 0.04878049 0.048722416 0.04878049 0.09756098    0
# 
# $Spec
# Min.   1st Qu.    Median      Mean   3rd Qu. Max. NA's
# model1 0.9081633 0.9616558 0.9744898 0.9692826 0.9897696    1    0
# model2 0.9795918 0.9897170 0.9948980 0.9928466 1.0000000    1    0
# model3 0.9278351 0.9616558 0.9693878 0.9692615 0.9795392    1    0


# Buy is representing individuals who have purchased food online (which would be subjects of our interest)
# Those people are interesting for us in order to use their characteristics for segmentation for example

# However, our models are better at predicting the Nonbuy class, which is the majority class. This is reflected in the high specificity, but low sensitivity.

# In terms of ROC metric, Linear Discriminant model is leading with 0.5722278

# Model with the highest Sensitivity is again Linear Discriminant model with 0.048722416

# Model with the highest Specificity is KNN model with 0.9928466 (this model was guessing majority of the categories as Nonbuy)

# Therefore, the overall best model is probably Linear Disciminant model since it has the best metrics in terms of ROC and Sensitivity

##### Recommendations
# on how the models should be improved without changing the predictors

# 1) Handle the Imbalance of classes
#   - Since the Nonbuy class is the majority class, we could oversample Buy Category and undersample Nonbuy to create
# more balanced classes

# 2) Threshold adjustment
#Adjust the classification threshold to potentially increase sensitivity without changing predictors. By changing the threshold, we can make the model more sensitive to the Buy class and improve its ability to correctly identify this class.

# 3) We could run the models once more and try to optimize for Sensitivity instead of ROC in the caret, since we are more interested in correctly identifying the Buy class

# 4) Multicolinearity issues
# We could check for multicollinearity between predictors and remove highly correlated predictors to improve model performance. But I'm not really sure if this would could as not changing the predictiors. We could perhaps run Principle component analysis to combine some of them



### =============================== Exercise j) ================================

# Based on the model insights, advise the company on which segments of the population they
# should target in their next marketing campaign

# There isnt much we can use from the KNN and LDA model as their outputs do not generate insights of feature importance

# But we can use Logistic Regression where we obtained the Beta parameters + We can also generate Feature Importance insights using vip library that can help us with the segmentation

# Feature Importance (does not work for some models)
library(vip)
vip(lr_caret, num_features = 20)
varImp(lr_caret)

# NIVELEST_X7  100.00
# ING_HOG_X4    98.44
# NIVELEST_X6   95.19
# ING_HOG_X5    65.36
# TIP_H_X5      60.93
# TIP_H_X4      57.14

# Based on the Logistic Regression model, the most important features for predicting the likelihood of an individual purchasing food online are:
# 1. NIVELEST_X7
# 2. ING_HOG_X4
# 3. NIVELEST_X6
# 4. ING_HOG_X5




summary(lr_caret)
# Feature       Beta coef
# NIVELEST_X7   -0.56833    0.21722  -2.616 0.008886 **
# ING_HOG_X4    -0.58056    0.22513  -2.579 0.009915 ** 
# NIVELEST_X6   -0.56886    0.22747  -2.501 0.012391 *  
# ING_HOG_X5    -0.37286    0.20903  -1.784 0.074471 . 

# All the significant coefficients here are negative and since they represent dummies, they show as relative effect of the category compared to the reference category.

# The reference category is the first category in each of the variables. So, for example, the reference category for NIVELEST is 0, for ING_HOG is 1, etc.

# The negative coefficients can be interpreted as follows:
# NIVELEST_X7 respondetns are less likely to buy food online compared to the reference category
# ING_HOG_X4 respondetns are less likely to buy food online compared to the reference category
# NIVELEST_X6 respondetns are less likely to buy food online compared to the reference category

# This information could be used for targeting, but I'm not really sure how valuable it is and how to interpret this further

### =============================== Exercise k) ================================

# Save the predicted probability of each model as a new variable. Combine them into a new
# variable representing the average predicted probability across the respective models.
# Evaluate the performance of this stacked model, comparing it to the individual models. Reflect
# on whether using a stacked model improves performance, and under which circumstances
# stacking might be beneficial or detrimental. (7,5 points)

lr_caret_probabilities

knn_fit_caret_probabilities

lda_caret_probabilities

# Combine the probabilities
stacked_probabilities = (lr_caret_probabilities + knn_fit_caret_probabilities + lda_caret_probabilities) / 3

# create a confusion matrix and assess performance on unseen data
stacked_class = ifelse(stacked_probabilities$Buy > 0.5, "Buy", "Notbuy")

stacked_class = as.factor(stacked_class)

confusionMatrix(stacked_class, baked_test$PROD12)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction Buy Notbuy
# Buy      2      5
# Notbuy 175    415
# 
# Accuracy : 0.6985          
# 95% CI : (0.6599, 0.7351)
# No Information Rate : 0.7035          
# P-Value [Acc > NIR] : 0.6252          
# 
# Kappa : -8e-04          
# 
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.01130         
#             Specificity : 0.98810         
#          Pos Pred Value : 0.28571         
#          Neg Pred Value : 0.70339         
#              Prevalence : 0.29648         
#          Detection Rate : 0.00335         
#    Detection Prevalence : 0.01173         
#       Balanced Accuracy : 0.49970         
#                                           
#        'Positive' Class : Buy     


# The stacked model does not improve the performance compared to the individual models. The stacked model has a lower sensitivity than Logistic Regression and Linear Discriminant Model, which means it is less effective at correctly identifying the Buy class. This suggests that the combination of the individual models did not improve the overall performance. Stacking can be beneficial when the individual models have complementary strengths and weaknesses, but in this case, the individual models were not able to improve the overall performance when combined.

#Looking at Accuracy:

#Stacked model Accuracy is 0.6985

lr_caret_test_accuracy
# 0.6917923

knn__caret_test_accuracy
# 0.7035176

lda_caret_test_accuracy
#0.6901173

# We can see the Accuracy of the stacked model imporved compared to the Logistic Regression and Linear Discriminant Model, but it is still lower than the KNN model. This suggests that the stacked model did not improve the overall performance in terms of accuracy.






#*******************************************************************************
#*******************************************************************************
# ================================= Problem 2 =================================
#*******************************************************************************
#*******************************************************************************

# The goal of this task is to predict the value of purchases (variable VCOMPRAS_cont) based on socio-
#   demographic and economic factors. Proceed with the following:


#*******************************************************************************
## ================================= Part 2.1 =================================
#*******************************************************************************



### =============================== Exercise a) ===============================


# Load the dataset into R. Select columns 1 to 12, and column 70 (VCOMPRAS_cont). During this
# loading, save blank (“ “) as NA variable. (1 point)

#loading data
original_data = read.csv("exam_folder/Data_exams/sampled_data.csv", header = TRUE, sep = ";")

# I wanted to include na.strings = " " in the read.csv function, but it was causing errors later on, so I will replace the " " with NA below


#creating a copy
data = original_data

#replacing " " with NA
data[data == " "] = NA


# Select columns 1 to 12, and column 70 from data
data = data[, c(1:12, 70)]

#check if the columns were selected correctly
colnames(data)



### =============================== Exercise b) ===============================


# Remove any rows with missing (NA) values. Then, remove any rows with entries “6” in column
# 10 (ING_HOG variable)


# Remove any rows with missing variable(s).
data <- na.omit(data)

#check if it was done correctly
library(DataExplorer)
plot_missing(data)

#Remove entries "6" in the ING_HOG

data <- data[data$ING_HOG != 6,]

# check if it was done correctlyy
summary(as.factor(data$ING_HOG))

### =============================== Exercise c) ===============================


# Convert all variables excluding columns 2 (EDAD), 12 (TOT_MH) and 70 (VCOMPRAS_cont) to
# factors.

#converting to factors with a custom function

strings_to_factors = function(dataset, columns_to_convert) {
  # Loop through the specified column names
  for (col in columns_to_convert) {
    # Check if the column exists in the dataset
    if (col %in% colnames(dataset)) {
      dataset[, col] = as.factor(dataset[, col])
    } else {
      warning(paste("Column", col, "does not exist in the dataset. Skipping."))
    }
  }
  return(dataset)
}

colums_to_factor = c("SEXO", "ESTC", "NIVELEST", "SIT_LAB", "TIP_JOR", "ACTIV", "OCUPACION1", "OCUPACION2", "ING_HOG", "TIP_H")

data = strings_to_factors(data, colums_to_factor)

#check if the conversion was successful
str(data)

# data.frame':	1447 obs. of  13 variables:
#  $ SEXO         : Factor w/ 2 levels "1","6": 2 1 1 2 1 2 1 2 2 1 ...
#  $ EDAD         : int  56 57 58 41 65 40 47 24 60 37 ...
#  $ ESTC         : Factor w/ 5 levels "1","2","3","4",..: 5 2 1 2 2 2 1 1 1 2 ...
#  $ NIVELEST     : Factor w/ 8 levels "0","1","2","3",..: 7 4 4 7 7 3 5 4 4 7 ...
#  $ SIT_LAB      : Factor w/ 3 levels "1","2","3": 3 1 1 1 1 1 3 2 1 3 ...
#  $ TIP_JOR      : Factor w/ 2 levels "1","2": 1 1 1 1 1 1 2 1 1 1 ...
#  $ ACTIV        : Factor w/ 11 levels "1","2","3","4",..: 9 2 9 6 9 4 4 9 9 8 ...
#  $ OCUPACION1   : Factor w/ 2 levels "1","6": 2 2 2 2 2 2 1 2 2 2 ...
#  $ OCUPACION2   : Factor w/ 2 levels "1","6": 2 2 2 2 2 2 2 2 2 2 ...
#  $ ING_HOG      : Factor w/ 5 levels "1","2","3","4",..: 3 5 3 5 5 1 5 3 3 4 ...
#  $ TIP_H        : Factor w/ 5 levels "1","2","3","4",..: 1 4 1 4 3 4 5 4 1 4 ...
#  $ TOT_MH       : int  1 3 1 4 2 3 3 3 1 3 ...
#  $ VCOMPRAS_cont: int  160 1015 263 1268 1035 89 74 253 14 707 ...


### =============================== Exercise d) ===============================

# Provide summary statistics for each of these factor variables, and remove factor levels that
# have less than 20 entries. Which variables are modified during this stage? What is the final
# dimension of the dataset?



summary(data$SEXO, maxsum = 20)
# 1   6 
# 759 688 

summary(data$ESTC, maxsum = 20)
# 1   2   3   4   5 
# 515 772  25  23 112 

summary(data$NIVELEST, maxsum = 20)
# 0   1   2   3   5   6   7   8 
# 6  30 188 281 253 277 379  33 

# here category 0 has only 6 observations

summary(data$SIT_LAB, maxsum = 20)
# 1    2    3 
# 1099  163  185 

summary(data$TIP_JOR, maxsum = 20)
# 1    2 
# 1264  183 

summary(data$ACTIV, maxsum = 20)
# 1   2   3   4   5   6   7   8   9  10  11 
# 43 206  64 252  86  61  13 102 507 108   5 

# Here category 7 and 11 have observations under 20

summary(data[,c("OCUPACION1", "OCUPACION2", "ING_HOG", "TIP_H")], maxsum = 20)
# OCUPACION1 OCUPACION2 ING_HOG TIP_H  
# 1: 248     1:  81     1: 77   1:307  
# 6:1199     6:1366     2:422   2:157  
# 3:443   3:201  
# 4:176   4:695  
# 5:329   5: 87  



# Remove factor levels that have less than 20 entries

# First, we remove observations where NIVELEST is equal to 0
data = data[data$NIVELEST != "0", ]

# Now we use droplevels function to drop the levels that are not present in the data
data$NIVELEST = droplevels(data$NIVELEST)

levels(data$NIVELEST)

# Now we remove category 7 and 11 from ACTIV
data = data[data$ACTIV != "7" & data$ACTIV != "11", ]

# Now we use droplevels function to drop the levels that are not present in the data
data$ACTIV = droplevels(data$ACTIV)

levels(data$ACTIV)

# check if it was done correctly
summary(data[,c("NIVELEST", "ACTIV")], maxsum = 20)
# NIVELEST ACTIV   
# 1: 29    1 : 42  
# 2:187    2 :206  
# 3:277    3 : 63  
# 5:249    4 :250  
# 6:272    5 : 86  
# 7:376    6 : 61  
# 8: 33    8 :102  
# 9 :505  
# 10:108  

#looks correct

### =============================== Exercise e) ================================


# Using ggplot2 library, perform two tasks. First, plot the histogram of the VCOMPRAS_cont
# variable. Second, plot the box plot of VCOMPRAS_cont by the ING_HOG variable. Discuss the
# plots. (3 points)

library(ggplot2)

# Plot the histogram of the VCOMPRAS_cont variable

ggplot(data, aes(x = VCOMPRAS_cont)) +
  geom_histogram(binwidth = 100) +
  labs(title = "Histogram of VCOMPRAS_cont",
       x = "VCOMPRAS_cont",
       y = "Frequency")

# The histogram of VCOMPRAS_cont shows that the distribution of the variable is skewed to the right. This suggests that the majority of the respondents have lower purchases of lower values. We might consider logging this variable to make it more normally distributed.

# Plot the box plot of VCOMPRAS_cont by the ING_HOG variable

ggplot(data, aes(x = ING_HOG, y = VCOMPRAS_cont)) +
  geom_boxplot() +
  labs(title = "Boxplot of VCOMPRAS_cont by ING_HOG",
       x = "ING_HOG",
       y = "VCOMPRAS_cont")

# The box plot of VCOMPRAS_cont by the ING_HOG variable shows that there are differences in purchase values across different ING_HOG (income levels). The median purchase value increases for higher income levels, suggesting that individuals with higher income tend to have larger spend on purchases. There are also more outliers in the higher income groups, indicating that there are individuals with quite high expenditure in these income categories.


### =============================== Exercise f) ================================


# Set your seed to 25. Randomly select 80% of the units into the training set and the remaining
# units into the test set. Do this using sample() function and without stratification. Define a
# matrix of dimension 5x2 called test.error with NA variables, which has to be used in the below
# parts to store test errors of five models across two metrics.

#splitting data
set.seed(25)

train = sample(dim(data)[1],dim(data)[1] * 0.8)

train_data = data[train,]

test_data = data[-train,]

#validate split
285/(285+1138)
# 0.2002811

# Define a matrix of dimension 5x2 called test.error with NA variables, which has to be used in the below parts to store test errors of five models across two metrics.

test.error = matrix(NA, nrow = 5, ncol = 2)

test.error



### =============================== Exercise g) ===============================


# The task is to predict the VCOMPRAS_cont variable. Run an intercept only model on the
# training set. Then, store its test errors with respect to the root mean squared error (RMSE)
# and mean absolute error (MAE) metrics. What are the values of the test errors?


### ===== Model 1 - Intercept =====

intercept <- lm(VCOMPRAS_cont ~ 1, data = train_data)

summary(intercept)

intercept_predictions <- predict(intercept, test_data)

# Compute the test RMSE and MSE
RMSE = sqrt(sum((test_data$VCOMPRAS_cont - intercept_predictions)^2/dim(test_data)[1]))
RMSE
# [1] 339.8495

MSE = mean((test_data$VCOMPRAS_cont - intercept_predictions)^2)
MSE

# or simply
library(Metrics)
intercept_rmse = rmse(test_data$VCOMPRAS_cont, intercept_predictions)
intercept_mse = mse(test_data$VCOMPRAS_cont, intercept_predictions)

intercept_rmse
intercept_mse

#for incercept model, the RMSE is 339.8495 and the MSE is 115497.7



### =============================== Exercise h) ===============================

# To predict the VCOMPRAS_cont variable, run a linear regression with all remaining variables
# as predictors on the training set. Then, store its test errors with respect to RMSE and MAE
# metrics. What are the values of test errors? Which variables are statistically significant?

### ===== Model 2 - LR with all predictors =====

lr_full <- lm(VCOMPRAS_cont ~ ., data = train_data)

summary(lr_full)

lr_full_predictions <- predict(lr_full, test_data)

# test RMSE and MSE
library(Metrics)
lr_full_rmse = rmse(test_data$VCOMPRAS_cont, lr_full_predictions)
lr_full_mse = mse(test_data$VCOMPRAS_cont, lr_full_predictions)

lr_full_rmse
lr_full_mse

#for linear regression with all variables, the RMSE is 340.1897 and the MSE is 115729

summary(lr_full)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  475.8349   133.5863   3.562 0.000384 ***
#   SEXO6         -0.9363    23.0202  -0.041 0.967563    
# EDAD          -3.2326     1.2376  -2.612 0.009121 ** 
#   ESTC2        -38.3006    29.1646  -1.313 0.189371    
# ESTC3         27.0291    81.5203   0.332 0.740282    
# ESTC4         31.3701    89.4623   0.351 0.725917    
# ESTC5         58.3684    43.2485   1.350 0.177419    
# NIVELEST2    -89.0996    75.5217  -1.180 0.238339    
# NIVELEST3    -32.3046    74.3642  -0.434 0.664075    
# NIVELEST5    -50.3783    75.3949  -0.668 0.504150    
# NIVELEST6     31.5743    77.4742   0.408 0.683686    
# NIVELEST7     60.8281    77.1733   0.788 0.430748    
# NIVELEST8     58.5776   100.7669   0.581 0.561145    
# SIT_LAB2     -85.1286    34.5280  -2.465 0.013834 *  
#   SIT_LAB3     -14.2695    33.6472  -0.424 0.671581    
# TIP_JOR2     -36.1134    32.8355  -1.100 0.271647    
# ACTIV2       -85.7090    68.8556  -1.245 0.213483    
# ACTIV3       -72.0671    80.4944  -0.895 0.370819    
# ACTIV4       -85.0352    69.6211  -1.221 0.222196    
# ACTIV5       -34.2520    80.9543  -0.423 0.672303    
# ACTIV6       -30.2997    82.3038  -0.368 0.712836    
# ACTIV8      -156.9479    75.8419  -2.069 0.038740 *  
#   ACTIV9       -83.7134    68.3769  -1.224 0.221103    
# ACTIV10      -70.7414    74.4529  -0.950 0.342244    
# OCUPACION16   21.9331    34.1339   0.643 0.520643    
# OCUPACION26  -22.1500    52.8901  -0.419 0.675449    
# ING_HOG2     -22.1242    50.3875  -0.439 0.660688    
# ING_HOG3       2.9638    51.8178   0.057 0.954399    
# ING_HOG4      82.2191    56.8125   1.447 0.148124    
# ING_HOG5     176.9511    55.8483   3.168 0.001575 ** 
#   TIP_H2       -82.2897    46.6244  -1.765 0.077849 .  
# TIP_H3       -44.7454    41.4462  -1.080 0.280556    
# TIP_H4       -71.3810    52.8340  -1.351 0.176959    
# TIP_H5       -77.9458    59.0708  -1.320 0.187265    
# TOT_MH        39.9000    16.3336   2.443 0.014729 * 

# statistically significant variables are:
# EDAD, SIT_LAB2, ACTIV8, ING_HOG5, TOT_MH
# Intercept is also significant

### =============================== Exercise i) ================================

# To predict the VCOMPRAS_cont variable, run lasso and ridge regression with all remaining
# variables as predictors on the training set and with 10-fold cross validation. Then, store their
# test errors with respect to RMSE and MAE metrics. What are the values of the test errors?
#   What are the tuned lambda values (lambda.min) for both models? Which variables are
# selected by lasso? Discuss these results.

library(glmnet)

#prepare data

#get data in (x,y) format (without intercept)
x_train <- model.matrix(VCOMPRAS_cont~., train_data)[,-1]
y_train <- train_data$VCOMPRAS_cont

x_test <- model.matrix(VCOMPRAS_cont~., test_data)[,-1]
y_test <- test_data$VCOMPRAS_cont

#decreasing lambda grid from 1000 to 0.01 
l.grid <- 10^seq(3,-2,length=100)

### ===== Model 3 - RIDGE =====

#estimate CV ridge regression along grid
set.seed(1) #set seeds for replicability as folds are random
ridge <- cv.glmnet(x_train, y_train, alpha=0, nfolds = 10, lambda = l.grid) #alpha = 0 for ridge

ridge_best_lam =ridge$lambda.min
ridge_best_lam
# [1] 155.5676

# min lambda for ridge is 155.5676


# Compute the test RMSE and MSE
ridge_predictions <- predict(ridge, s = ridge_best_lam, newx = x_test)

library(Metrics)
ridge_rmse = rmse(y_test, ridge_predictions)
ridge_rmse
# [1] 337.1749


ridge_mse = mse(y_test, ridge_predictions)
ridge_mse
# [1] 113686.9

# RMSE for ridge is 337.1749 and the MSE is 113686.9

### ===== Model 4 - LASSO =====

#estimate CV lasso regression along grid

set.seed(1) #set seeds for replicability as folds are random
lasso <- cv.glmnet(x_train, y_train, alpha=1, nfolds = 10, lambda = l.grid) #alpha = 1 for lasso

lasso_best_lam = lasso$lambda.min
lasso_best_lam
# [1] 5.336699

# min lambda for lasso is 5.336699

# Compute the test RMSE and MSE

lasso_predictions <- predict(lasso, s = lasso_best_lam, newx = x_test)

lasso_rmse = rmse(y_test, lasso_predictions)
lasso_rmse
# [1] 338.1637

lasso_mse = mse(y_test, lasso_predictions)
lasso_mse
# [1] 114354.7

# RMSE for lasso is 338.1637 and the MSE is 114354.7


# Find which variables were selected by Lasso

# Get coefficients using optimal lambda
lasso_coef <- predict(lasso, type="coefficients", s=lasso$lambda.min)[-1]
lasso_coef #some of them are 0

# Find which variables were selected by Lasso
incl.lasso <- which(lasso_coef != 0)
incl.lasso # which are not 0?

# Get the names of the selected variables
colnames(x_train)[incl.lasso]

# [1] "EDAD"        "ESTC2"       "ESTC5"       "NIVELEST2"   "NIVELEST5"   "NIVELEST6"   "NIVELEST7"   "NIVELEST8"   "SIT_LAB2"   
# [10] "TIP_JOR2"    "ACTIV5"      "ACTIV6"      "ACTIV8"      "OCUPACION16" "OCUPACION26" "ING_HOG2"    "ING_HOG4"    "ING_HOG5"   
# [19] "TIP_H2"      "TOT_MH"  

#Lasso selected EDAD, ESTC2, ESTC5, NIVELEST2, NIVELEST5, NIVELEST6, NIVELEST7, NIVELEST8, SIT_LAB2, TIP_JOR2, ACTIV5, ACTIV6, ACTIV8, OCUPACION16, OCUPACION26, ING_HOG2, ING_HOG4, ING_HOG5, TIP_H2, TOT_MH


#store corresponding coefficients (parameters)
par.lasso<-lasso_coef[incl.lasso]

par.lasso

#Selected coefficients and their parameters:
lasso_selected_vars = data_frame(variable = colnames(x_train)[incl.lasso], coefficient = par.lasso)

lasso_selected_vars

# variable    coefficient
# <chr>             <dbl>
#   1 EDAD              -2.33
# 2 ESTC2            -28.7 
# 3 ESTC5             28.1 
# 4 NIVELEST2        -55.7 
# 5 NIVELEST5        -11.9 
# 6 NIVELEST6         49.0 
# 7 NIVELEST7         81.0 
# 8 NIVELEST8         53.6 
# 9 SIT_LAB2         -69.6 
# 10 TIP_JOR2         -26.8 
# 11 ACTIV5            37.5 
# 12 ACTIV6            39.5 
# 13 ACTIV8           -59.0 
# 14 OCUPACION16        9.73
# 15 OCUPACION26      -13.1 
# 16 ING_HOG2         -22.9 
# 17 ING_HOG4          57.5 
# 18 ING_HOG5         161.  
# 19 TIP_H2           -10.3 
# 20 TOT_MH            16.2 




### =============================== Exercise j) ================================

# The next task is to use three predictors, namely linear regression, lasso and ridge regression,
# from parts h) and i) and form a median-based model averaging predictor. To do this, run a for
# loop across the length of the test set to form this median-based predictor. Then, store its test
# errors with respect to RMSE and MAE metrics. What are the values of the test errors?



# Create a list to store the predictions from each model
predictions <- list()

#loop

for (i in 1:nrow(test_data)) {
  pred_lr_full <- predict(lr_full, test_data[i, ])
  pred_lasso <- predict(lasso, s = lasso_best_lam, newx = x_test[i, ])
  pred_ridge <- predict(ridge, s = ridge_best_lam, newx = x_test[i, ])
  
  predictions[[i]] <- c(pred_lr_full, pred_lasso, pred_ridge)
}


combined_preds <- numeric(nrow(test_data))

# Iterate over each test case to calculate the median-based predictor
for (i in 1:nrow(test_data)) {
  preds <- unlist(predictions[[i]])
  combined_preds[i] <- median(preds)
}

combined_preds


#calculate test rmse and mse
library(Metrics)

rmse_combined = rmse(test_data$VCOMPRAS_cont, combined_preds)
rmse_combined
# [1] 337.9687

mse_combined = mse(test_data$VCOMPRAS_cont, combined_preds)
mse_combined
# [1] 114222.8

# RMSE for the median-based model averaging predictor is 337.9687 and the MSE is 114222.8



### =============================== Exercise k) ================================

# 
# Compare the RMSE and MAE test errors of the five models from parts g), h), i) and j) by
# extracting errors stored in the test.error matrix. Discuss your results. Based on the material
# we covered in the lectures, do you find these results expected or surprising? Provide
# arguments.


test.error[1, ] = c(intercept_rmse, intercept_mse)

test.error[2, ] = c(lr_full_rmse, lr_full_mse)

test.error[3, ] = c(ridge_rmse, ridge_mse)

test.error[4, ] = c(lasso_rmse, lasso_mse)

test.error[5, ] = c(rmse_combined, mse_combined)

# The RMSE and MSE test errors for the five models are as follows:

test.error

# [,1]     [,2]
# [1,] 339.8495 115497.7
# [2,] 340.1897 115729.0
# [3,] 337.1749 113686.9
# [4,] 338.1637 114354.7
# [5,] 337.9687 114222.8

# The results show that the worse model is linear regression with all variables. The model is performing even worse than the intercept, which is surprising. This might be due to the issues with multicollinearity or overfitting in the model. The best model is Ridge regression, which has the lowest RMSE and MSE. The median-based model averaging predictor also performs well, with RMSE and MSE close to the Ridge regression model. The results are somewhat expected, as Ridge regression is known to perform well in the presence of multicollinearity and overfitting, which might be present in the dataset. The median-based model averaging predictor is also expected to perform well, as it combines the predictions from multiple models, which can help reduce the variance and improve the overall performance of the model.


