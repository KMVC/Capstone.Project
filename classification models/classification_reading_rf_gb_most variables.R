## building classification models

#classification script for 2016 read outcome variable

#load relevant libraries and data
schools14_16 <- read.csv("schools14_16.csv", na = c("", "NA"))
library(tidyverse)
library(gbm)
library(rpart)
library(Metrics)
library(randomForest)
library(caret)
library(ROCR)


#further preparing data for classification
#turning percentiles into categorical variables

schools14_16class <- schools14_16 %>% 
  mutate("Reading_16" = case_when(schools14_16$X2016.NWEA.Reading.Gr.3.Pct <= 50 ~ "Weak",
                                  schools14_16$X2016.NWEA.Reading.Gr.3.Pct >= 51 ~ "strong"))

schools14_16class <- schools14_16class %>% 
  mutate("Math_16" = case_when(X2016.NWEA.Reading.Gr.3.Pct <= 50 ~ "weak", 
                               X2016.NWEA.Reading.Gr.3.Pct >= 51 ~ "strong"))


#alter labels so they are not too long
colnames(schools14_16class)[colnames(schools14_16class)=="Environment.Rating"] <- "envi"

colnames(schools14_16class)[colnames(schools14_16class)=="Instruction.Rating"] <- "inst"

colnames(schools14_16class)[colnames(schools14_16class)=="Leadership.Rating"] <- "lead"

colnames(schools14_16class)[colnames(schools14_16class)=="Collaboration.Rating"] <- "collab"

colnames(schools14_16class)[colnames(schools14_16class)=="Safety.Rating"] <- "safe"

colnames(schools14_16class)[colnames(schools14_16class)=="Family.Rating"] <- "fam"

colnames(schools14_16class)[colnames(schools14_16class)=="Median.Income"] <- "income"

colnames(schools14_16class)[colnames(schools14_16class)=="H.S.Graduation.."] <- "grad"

colnames(schools14_16class)[colnames(schools14_16class)=="X2014.NWEA.Reading.Gr.3.Pct"] <- "2014 reading"

colnames(schools14_16class)[colnames(schools14_16class)=="X2015.NWEA.Reading.Gr.3.Pct"] <- "2015 reading "

colnames(schools14_16class)[colnames(schools14_16class)== "X2014.NWEA.Math.Gr.3.Pct"] <- "2014 math"

colnames(schools14_16class)[colnames(schools14_16class)=="X2015.NWEA.Math.Gr.3.Pct"] <- "2015 math"

colnames(schools14_16class)[colnames(schools14_16class)=="Estimate.of.Pop..At.or.Above.Poverty.Line.who.Did.Not.Receive.Food.Stamps"] <- "no stamps"

colnames(schools14_16class)[colnames(schools14_16class)=="Estimate.of.Aggregate.Earnings"] <- "earn"

#check names
schools14_16class <- data.frame(schools14_16class, check.names = TRUE)

#remove latitude, longitude, name of school and percentile variables
schools14_16class <-  schools14_16class[,(-(18:20))]
schools14_16class <-  schools14_16class[,(-(15))]
schools14_16class <-  schools14_16class[,(-(2))]

#removing 2016 math
schools14_16class_read <-  schools14_16class[,(-(17))]



#removing na values
schools14_16class_read <- na.omit(schools14_16class_read)

# Convert "strong" to 1, "weak" to 0

schools14_16class_read$Reading_16 <- ifelse(schools14_16class_read$Reading_16 == "strong", 1, 0)



#creating sample sets


# Total number of rows in the data frame

n <- nrow(schools14_16class_read)



# Number of rows for the training set (80% of the dataset)

n_train <- round(0.80 * n) 


# Create a vector of indices which is an 80% random sample

set.seed(123)

train_indices <- sample(1:n, n_train)


# Subset the schools data frame to training indices only

schoolsclass_read_train <- schools14_16class_read[train_indices, ]  


# Exclude the training indices to create the test set

schoolsclass_read_test <- schools14_16class_read[-train_indices, ]  



# boosting

#create a model using 10,000 trees

# Train a 10000-tree GBM model

set.seed(1)

class_read_model <- gbm(formula = Reading_16 ~ ., 
                        
                        distribution = "bernoulli", 
                        
                        data = schoolsclass_read_train,
                        
                        n.trees = 10000)



# Print the model object                    

print(class_read_model) 



# summary() prints variable importance

summary(class_read_model)  



# Generate predictions on the test set

preds1_read <- predict(object = class_read_model, 
                       
                       newdata = schoolsclass_read_test,
                       
                       n.trees = 10000)



# Generate predictions on the test set (scale to response)

preds2_read <- predict(object = class_read_model, 
                       
                       newdata = schoolsclass_read_test,
                       
                       n.trees = 10000,
                       
                       type = "response")



# Generate the test set AUCs using the two sets of preditions & compare

auc1 <- auc(actual = schoolsclass_read_test$Reading_16, predicted = preds1_read)  #default

auc2 <-  auc(actual = schoolsclass_read_test$Reading_16, predicted = preds2_read)  #rescaled


# Optimal ntree estimate based on OOB

ntree_opt_oob_read <- gbm.perf(object = class_read_model, 
                               
                               method = "OOB", 
                               
                               oobag.curve = TRUE)



# Train a CV GBM model

set.seed(1)

class__read_model_cv <- gbm(formula = Reading_16 ~ ., 
                            
                            distribution = "bernoulli", 
                            
                            data = schoolsclass_read_train,
                            
                            n.trees = 10000,
                            
                            cv.folds = 2)



# Optimal ntree estimate based on CV

ntree_opt_cv_read <- gbm.perf(object = class__read_model_cv, 
                              
                              method = "cv")



# Compare the estimates                         

print(paste0("Optimal n.trees (OOB Estimate): ", ntree_opt_oob_read))                         

print(paste0("Optimal n.trees (CV Estimate): ", ntree_opt_cv_read))


# Generate predictions on the test set using ntree_opt_oob number of trees

preds3_read <- predict(object = class_read_model, 
                       
                       newdata = schoolsclass_read_test,
                       
                       n.trees = ntree_opt_oob_read)



# Generate predictions on the test set using ntree_opt_cv number of trees

preds4 <- predict(object = class_read_model, 
                  
                  newdata = schoolsclass_read_test,
                  
                  n.trees = ntree_opt_cv_read)   



# Generate the test set AUCs using the two sets of preditions & compare

auc3 <- auc(actual = schoolsclass_read_test$Reading_16, predicted = preds3_read)  #OOB

auc4 <- auc(actual = schoolsclass_read_test$Reading_16, predicted = preds4)  #CV 



# Compare AUC 

print(paste0("Test set AUC (OOB): ", auc3))                         

print(paste0("Test set AUC (CV): ", auc4))


##highest AUC is for gbm model built using CV 

#plot roc curves
# List of predictions

preds_list <- list(preds2_read, preds3_read, preds4)


# List of actual values (same for all)

m <- length(preds_list)

actuals_list <- rep(list(schoolsclass_read_test$Reading_16), m)


# Plot the ROC curves

allpred <- prediction(preds_list, actuals_list)

rocs <- performance(allpred, "tpr", "fpr")

plot(rocs, col = as.list(1:m), main = "GBM Reading 2016 ROC Curves")

legend(x = "bottomright", 
       
       legend = c("10000 Trees", "OOB Error", "CVt"),
       
       fill = 1:m)

##Confusion Matrix

#change preds and Reading_16 into factors with the same levels
schoolsclass_read_test$Reading_16 <- as.factor(schoolsclass_read_test$Reading_16)

preds2_read <- round(preds2_read)

preds2_read <- as.factor(preds2_read)

confusionMatrix(data = preds2_read, reference = schoolsclass_read_test$Reading_16)


############################################################################################################################################
#random forest for reading classification

#removing 2016 math
schools14_16class_read_rf <-  schools14_16class[,(-(17))]

#removing na values
schools14_16class_read_rf <- na.omit(schools14_16class_read_rf)

#creating sample sets


# Total number of rows in the data frame

n <- nrow(schools14_16class_read_rf)



# Number of rows for the training set (80% of the dataset)

n_train <- round(0.80 * n) 


# Create a vector of indices which is an 80% random sample

set.seed(123)

train_indices <- sample(1:n, n_train)


# Subset the schools data frame to training indices only

schoolsclass_read_train_rf <- schools14_16class_read_rf[train_indices, ]  


# Exclude the training indices to create the test set

schoolsclass_read_test_rf <- schools14_16class_read_rf[-train_indices, ] 

#make Reading_16 a factor

schoolsclass_read_train_rf$Reading_16 <- as.factor(schoolsclass_read_train_rf$Reading_16)
schoolsclass_read_test_rf$Reading_16 <- as.factor(schoolsclass_read_test_rf$Reading_16)



# Train a Random Forest
set.seed(1)  # for reproducibility

class_model_read_rf <- randomForest(formula = Reading_16 ~ ., 
                                    data = schoolsclass_read_train_rf)

# Print the model output                             
print(class_model_read_rf)



# Grab OOB error matrix & take a look

err_rf_read <- class_model_read_rf$err.rate

head(err_rf_read)



# Look at final OOB error rate (last row in err matrix)

oob_err_rf_read <- err_rf_read[498, "OOB"]

print(oob_err_rf_read)


# Plot the model trained in the previous exercise

plot(class_model_read_rf)


# Add a legend since it doesn't have one by default

legend(x = "right", 
       
       legend = colnames(err_rf_read),
       
       fill = 1:ncol(err_rf_read))


# Generate predicted classes using the model object

class__read_rf_prediction <- predict(object = class_model_read_rf,   # model object 
                                     
                                     newdata = schoolsclass_read_test_rf,  # test dataset
                                     
                                     type = "class") # return classification labels



# Calculate the confusion matrix for the test set

cm_rf_read <- confusionMatrix(data = class__read_rf_prediction,       # predicted classes
                              
                              reference = schoolsclass_read_test_rf$Reading_16)  # actual classes

print(cm_rf_read)



# Compare test set accuracy to OOB accuracy

paste0("Test Accuracy: ", cm_rf_read$overall[1])

paste0("OOB Accuracy: ", 1 - oob_err_rf_read)


#Evaluate test set AUC


# Generate predictions on the test set

pred_read_rf <- predict(object = class_model_read_rf,
                        
                        newdata = schoolsclass_read_test_rf,
                        
                        type = "prob")



# `pred` is a matrix

class(pred_read_rf)



# Look at the pred format

head(pred_read_rf)



# Compute the AUC (`actual` must be a binary 1/0 numeric vector)

auc5 <- auc(actual = ifelse(schoolsclass_read_test_rf$Reading_16 == "strong", 1, 0), 
    
    predicted = pred_read_rf[,"strong"])     


# Execute the tuning process

set.seed(1)              

res_read_rf <- tuneRF(x = subset(schoolsclass_read_train_rf, select = -Reading_16), 
                      y = schoolsclass_read_train_rf$Reading_16, ntreeTry = 500)



# Look at results

print(res_read_rf)



# Find the mtry value that minimizes OOB Error

mtry_opt_read_rf <- res_read_rf[,"mtry"][which.min(res_read_rf[,"OOBError"])]

print(mtry_opt_read_rf)


# Establish a list of possible values for mtry, nodesize and sampsize

mtry_read_rf <- seq(4, ncol(schoolsclass_read_train_rf) * 0.8, 2)

nodesize <- seq(3, 8, 2)

sampsize <- nrow(schoolsclass_read_train_rf) * c(0.7, 0.8)


# Create a data frame containing all combinations 

hyper_grid <- expand.grid(mtry_read_rf = mtry_read_rf, nodesize = nodesize, sampsize = sampsize)



# Create an empty vector to store OOB error values

oob_err_r_rf <- c()


# Write a loop over the rows of hyper_grid to train the grid of models

for (i in 1:nrow(hyper_grid)) {
  
  
  
  # Train a Random Forest model
  
  model <- randomForest(formula = Reading_16 ~ ., 
                        
                        data = schoolsclass_read_train_rf,
                        
                        mtry = hyper_grid$mtry_read_rf[i],
                        
                        nodesize = hyper_grid$nodesize[i],
                        
                        sampsize = hyper_grid$sampsize[i])
  
  
  
  # Store OOB error for the model                      
  
  oob_err_r_rf[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
  
}


# Identify optimal set of hyperparmeters based on OOB error

opt_i <- which.min(oob_err_r_rf)

print(hyper_grid[opt_i,])

print(model)

#highest AUC is for the 4th gradient boosting model (employing CV)
#RF has an accuracy of about 76 %


