#linear regression script for 2016 reading outcome variable

#load relevant libraries and data
schools14_16 <- read.csv("schools14_16.csv", na = c("", "NA"))
library(tidyverse)
library(gbm)
library(randomForest)
library(rpart)
library(Metrics)



#further preparing data for linear regression
#adding dummy variables for 5essentials 

schools14_16lin <- schools14_16 %>% 
  mutate("environment scores" = case_when(Environment.Rating == "VERY STRONG" ~ 5, 
                                          Environment.Rating == "STRONG" ~ 4, 
                                          Environment.Rating == "NEUTRAL" ~ 3,
                                          Environment.Rating == "WEAK" ~ 2,
                                          Environment.Rating == "VERY WEAK" ~ 1, 
                                          Environment.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16lin <- schools14_16lin %>% 
  mutate("instruction scores" = case_when(Instruction.Rating == "VERY STRONG" ~ 5, 
                                          Instruction.Rating == "STRONG" ~ 4, 
                                          Instruction.Rating == "NEUTRAL" ~ 3, 
                                          Instruction.Rating == "WEAK" ~ 2,
                                          Instruction.Rating == "VERY WEAK" ~ 1, 
                                          Instruction.Rating == "NOT ENOUGH DATA" ~ 0))


schools14_16lin <- schools14_16lin %>% 
  mutate("collaboration scores" = case_when(Collaboration.Rating == "VERY STRONG" ~ 5, 
                                            Collaboration.Rating == "STRONG" ~ 4, 
                                            Collaboration.Rating == "NEUTRAL" ~ 3, 
                                            Collaboration.Rating == "WEAK" ~ 2,
                                            Collaboration.Rating == "VERY WEAK" ~ 1, 
                                            Collaboration.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16lin <- schools14_16lin %>% 
  mutate("family scores" = case_when(Family.Rating == "VERY STRONG" ~ 5, 
                                     Family.Rating == "STRONG" ~ 4, 
                                     Family.Rating == "NEUTRAL" ~ 3, 
                                     Family.Rating == "WEAK" ~ 2,
                                     Family.Rating == "VERY WEAK" ~ 1, 
                                     Family.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16lin <- schools14_16lin %>% 
  mutate("leadership scores" = case_when(Leadership.Rating == "VERY STRONG" ~ 5, 
                                         Leadership.Rating == "STRONG" ~ 4, 
                                         Leadership.Rating == "NEUTRAL" ~ 3, 
                                         Leadership.Rating == "WEAK" ~ 2,
                                         Leadership.Rating == "VERY WEAK" ~ 1, 
                                         Leadership.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16lin <- schools14_16lin %>% 
  mutate("safety scores" = case_when(Safety.Rating == "VERY STRONG" ~ 5, 
                                     Safety.Rating == "STRONG" ~ 4, 
                                     Safety.Rating == "NEUTRAL" ~ 3, 
                                     Safety.Rating == "WEAK" ~ 2,
                                     Safety.Rating == "VERY WEAK" ~ 1, 
                                     Safety.Rating == "NOT ENOUGH DATA" ~ 0))


#alter labels so they are not too long
colnames(schools14_16lin)[colnames(schools14_16lin)=="environment scores"] <- "envi"

colnames(schools14_16lin)[colnames(schools14_16lin)=="instruction scores"] <- "inst"

colnames(schools14_16lin)[colnames(schools14_16lin)=="leadership scores"] <- "lead"

colnames(schools14_16lin)[colnames(schools14_16lin)=="collaboration scores"] <- "collab"

colnames(schools14_16lin)[colnames(schools14_16lin)=="safety scores"] <- "safe"

colnames(schools14_16lin)[colnames(schools14_16lin)=="family scores"] <- "fam"

colnames(schools14_16lin)[colnames(schools14_16lin)=="Median.Income"] <- "income"

colnames(schools14_16lin)[colnames(schools14_16lin)=="H.S.Graduation.."] <- "grad"

colnames(schools14_16lin)[colnames(schools14_16lin)=="X2014.NWEA.Reading.Gr.3.Pct"] <- "r_2014"

colnames(schools14_16lin)[colnames(schools14_16lin)=="X2015.NWEA.Reading.Gr.3.Pct"] <- "r_2015 "

colnames(schools14_16lin)[colnames(schools14_16lin)=="X2016.NWEA.Reading.Gr.3.Pct"] <- "r_2016"

colnames(schools14_16lin)[colnames(schools14_16lin)== "X2014.NWEA.Math.Gr.3.Pct"] <- "m_2014"

colnames(schools14_16lin)[colnames(schools14_16lin)=="X2015.NWEA.Math.Gr.3.Pct"] <- "m_2015"

colnames(schools14_16lin)[colnames(schools14_16lin)=="X2016.NWEA.Math.Gr.3.Pct"] <- "m_2016"

colnames(schools14_16lin)[colnames(schools14_16lin)=="Estimate.of.Pop..At.or.Above.Poverty.Line.who.Did.Not.Receive.Food.Stamps"] <- "no_stamps"

colnames(schools14_16lin)[colnames(schools14_16lin)=="Estimate.of.Aggregate.Earnings"] <- "earn"

#check names
schools14_16lin <- data.frame(schools14_16lin, check.names = TRUE)

#remove latitude, longitude, name of school and categorical values
schools14_16lin <-  schools14_16lin[,(-(1:8))]
schools14_16lin <-  schools14_16lin[,(-(11:12))]


#removing 2016 math (as occurs simultaneously with reading (both 2016), doesn't seem logical to use it to predict reading outcomes)
schools14_16linread <-  schools14_16lin[,(-(10))]


summary(schools14_16linread)


#removing na values
schools14_16linread <- na.omit(schools14_16linread)


#creating sample sets


# Total number of rows in the schools14_16lin data frame

n <- nrow(schools14_16linread)



# Number of rows for the training set (80% of the dataset)

n_train <- round(0.80 * n) 


# Create a vector of indices which is an 80% random sample

set.seed(123)

train_indices <- sample(1:n, n_train)


# Subset the schools data frame to training indices only

schoolsread_train <- schools14_16linread[train_indices, ]  


# Exclude the training indices to create the test set

schoolsread_test <- schools14_16linread[-train_indices, ]  



# boosting

#create a model using 10,000 trees

set.seed(1)

schoolsreadgb_model <- gbm(formula = r_2016 ~ ., 
                           
                           distribution = "gaussian", 
                           
                           data = schoolsread_train,
                           
                           n.trees = 10000)



summary(schoolsreadgb_model)

# Generate prediction on the test set
pred <- predict.gbm(object = schoolsreadgb_model, 
                    
                    newdata = schoolsread_test,
                    
                    n.trees = 10000,
                    
                    type = "response")


# Look at the range of predictions

range(pred)

# Generate the RMSE 
rmse10000_gbm <- rmse(schoolsread_test$r_2016, pred)

print(rmse10000_gbm)


#plot predictions
plot(pred, main = "GBM Reading Predictions based on using 10000 trees")


# Optimal ntree estimate based on OOB
ntree_opt_oob <- gbm.perf(object = schoolsreadgb_model, 
                          
                          method = "OOB", 
                          
                          oobag.curve = TRUE)

# Train a CV GBM model
set.seed(1)

schoolsreadgb_model_cv <- gbm(formula = r_2016 ~ ., 
                              
                              distribution = "gaussian", 
                              
                              data = schoolsread_train,
                              
                              n.trees = 10000,
                              
                              cv.folds = 2)



# Optimal ntree estimate based on CV

ntree_opt_cv <- gbm.perf(object = schoolsreadgb_model_cv, 
                         
                         method = "cv")

# Compare the estimates   

print(paste0("Optimal n.trees (OOB Estimate): ", ntree_opt_oob))                         

print(paste0("Optimal n.trees (CV Estimate): ", ntree_opt_cv))


#do two different models, using different estimates of optimal trees  

# Use oob

preds1_gbm <- predict.gbm(object = schoolsreadgb_model, 
                          
                          newdata = schoolsread_test,
                          
                          n.trees = ntree_opt_oob, type = "response")

#use cv


preds2_gbm <- predict.gbm(object = schoolsreadgb_model, 
                          
                          newdata = schoolsread_test,
                          
                          n.trees = ntree_opt_cv, type = "response") 

plot(preds1_gbm, main = "Reading Predictions based on OOB Estimate of Optimal Trees")
plot(preds2_gbm, main = "Reading Predictions based on CV Estimate of Optimal Trees")


#Compare RMSE
#oob
rmse1_gbm <- rmse(schoolsread_test$r_2016, preds1_gbm)

#cv
rmse2_gbm <-  rmse(schoolsread_test$r_2016, preds2_gbm)


print(paste0("Test set RMSE (OOB): ", rmse1_gbm))                         

print(paste0("Test set RMSE (CV): ", rmse2_gbm))


##smallest gbm RMSE is for the CV model


#Plotting the RMSE error vs number of trees)
ntree1 <-  10000

#creating vectors
RMSE <- c(rmse10000_gbm, rmse1_gbm, rmse2_gbm)
Predictions <- c(pred, preds1_gbm, preds2_gbm)
Tree_Numbers <- c(ntree1, ntree_opt_oob, ntree_opt_cv)

#creating a dataframe
testsdf <- data.frame(RMSE, Predictions, Tree_Numbers)

ggplot(testsdf, aes(x = Tree_Numbers, y = RMSE, col = "red")) + 
  geom_point()+ ggtitle("Tree Numbers and RMSE")

###########################################################################################################################################

#Random forest


# Train a Random Forest
#set seed for reproducibiilty
set.seed(1) 

schoolsreadrf_model <- randomForest(formula = r_2016 ~ ., 
                                    data = schoolsread_train)

# Print the model output                             
print(schoolsreadrf_model)
summary(schoolsreadrf_model)

# Grab MSE error & take a look

err <- schoolsreadrf_model$mse

head(err)

# Look at final OOB error rate (last row in err matrix)

mse_err <- err[500]

print(mse_err)


# Plot the model trained in the previous exercise

plot(schoolsreadrf_model, main = "Random Forest Model for Schools, Reading 2016")


# Generate predicted classes using the model object

schoolsreadrf_prediction <- predict(object = schoolsreadrf_model,    
                                    
                                    newdata = schoolsread_test,  
                                    
                                    type = "response") 

#calculating the RMSE 
readrf_rmse <- rmse(schoolsread_test$r_2016, schoolsreadrf_prediction)

print(readrf_rmse)

#RMSE is lower than the CV GB Model

#tune by mtry

# Execute the tuning process

set.seed(1)              

res <- tuneRF(x = subset(schoolsread_train, select = -r_2016),
              
              y = schoolsread_train$r_2016,
              
              ntreeTry = 500)


# Look at results

print(res)


# Find the mtry value that minimizes OOB Error

mtry_opt <- res[,"mtry"][which.min(res[,"OOBError"])]

print(mtry_opt)

#mtry_opt = 8



# Train a Random Forest with tuned mtry
#set seed for reproducibiilty
set.seed(1) 

schoolsreadrf_model_mtry <- randomForest(formula = r_2016 ~ ., 
                                         data = schoolsread_train, mtry = mtry_opt)

# Print the model output                             
print(schoolsreadrf_model_mtry)
summary(schoolsreadrf_model_mtry)

# Grab MSE error & take a look

err_mtry <- schoolsreadrf_model_mtry$mse

head(err_mtry)

# Look at final OOB error rate (last row in err matrix)

mse_err_mtry <- err_mtry[500]

print(mse_err_mtry)


# Plot the model trained in the previous exercise

plot(schoolsreadrf_model_mtry, main = "Random Forest Model for Schools, Tuned Mtry, Reading 2016")


# Generate predicted classes using the model object

schoolsreadrf_prediction_mtry <- predict(object = schoolsreadrf_model_mtry,    
                                         
                                         newdata = schoolsread_test,  
                                         
                                         type = "response") 

#calculating the RMSE 
readrf_rmse_mtry <- rmse(schoolsread_test$r_2016, schoolsreadrf_prediction_mtry)

print(readrf_rmse_mtry)

#lowest RMSE for all models is the Random forest model with tuned mtry

############################################################################################################################################

##creating regression models without any of the percentile variables


#removing 2014-2015 reading and 2014-2016 math
schools14_16read2 <-  schools14_16lin[,(-(5:6))]
schools14_16read2 <- schools14_16read2[,(-(6:8))]


summary(schools14_16read2)


#removing na values
schools14_16read2 <- na.omit(schools14_16read2)


#creating sample sets


# Total number of rows in the schools14_16lin data frame

n <- nrow(schools14_16read2)



# Number of rows for the training set (80% of the dataset)

n_train <- round(0.80 * n) 


# Create a vector of indices which is an 80% random sample

set.seed(123)

train_indices <- sample(1:n, n_train)


# Subset the schools data frame to training indices only

schoolsread2_train <- schools14_16read2[train_indices, ]  


# Exclude the training indices to create the test set

schoolsread2_test <- schools14_16read2[-train_indices, ]  



# boosting

#create a model using 10,000 trees

set.seed(1)

schoolsread2_gb_model <- gbm(formula = r_2016 ~ ., 
                           
                           distribution = "gaussian", 
                           
                           data = schoolsread2_train,
                           
                           n.trees = 10000)



summary(schoolsread2_gb_model)

# Generate prediction on the test set
pred_read2 <- predict.gbm(object = schoolsread2_gb_model, 
                    
                    newdata = schoolsread2_test,
                    
                    n.trees = 10000,
                    
                    type = "response")


# Look at the range of predictions

range(pred_read2)

# Generate the RMSE 
rmse10000_gbm_read2 <- rmse(schoolsread2_test$r_2016, pred_read2)

print(rmse10000_gbm_read2)


#plot predictions
plot(pred_read2, main = "GBM Reading Predictions based on using 10000 trees, Other Year Math and Reading Variables Not Included")


# Optimal ntree estimate based on OOB
ntree_opt_oob_read2 <- gbm.perf(object = schoolsread2_gb_model, 
                          
                          method = "OOB", 
                          
                          oobag.curve = TRUE)

# Train a CV GBM model
set.seed(1)

schoolsread2_gb_model_cv <- gbm(formula = r_2016 ~ ., 
                              
                              distribution = "gaussian", 
                              
                              data = schoolsread2_train,
                              
                              n.trees = 10000,
                              
                              cv.folds = 2)



# Optimal ntree estimate based on CV

ntree_opt_cv_read2 <- gbm.perf(object = schoolsread2_gb_model_cv, 
                         
                         method = "cv")

# Compare the estimates   

print(paste0("Optimal n.trees (OOB Estimate): ", ntree_opt_oob_read2))                         

print(paste0("Optimal n.trees (CV Estimate): ", ntree_opt_cv_read2))


#build two different models, using different estimates of optimal trees  

# Use oob

preds1_gbm_read2 <- predict.gbm(object = schoolsread2_gb_model, 
                          
                          newdata = schoolsread2_test,
                          
                          n.trees = ntree_opt_oob_read2, type = "response")

#use cv


preds2_gbm_read2 <- predict.gbm(object = schoolsread2_gb_model, 
                          
                          newdata = schoolsread2_test,
                          
                          n.trees = ntree_opt_cv_read2, type = "response") 

plot(preds1_gbm_read2, main = "Reading Predictions based on OOB Estimate of Optimal Trees, All Math and Reading Variables from Other Years
     Not Included")
plot(preds2_gbm_read2, main = "Reading Predictions based on CV Estimate of Optimal Trees, All Math and Reading Variables from Other Years 
     Not Included")


#Compare RMSE
#oob
rmse1_gbm_read2 <- rmse(schoolsread2_test$r_2016, preds1_gbm_read2)

#cv
rmse2_gbm_read2 <-  rmse(schoolsread2_test$r_2016, preds2_gbm_read2)


print(paste0("Test set RMSE (OOB): ", rmse1_gbm_read2))                         

print(paste0("Test set RMSE (CV): ", rmse2_gbm_read2))


##smallest gbm RMSE is for the 10,000 tree model


#Plotting the RMSE error vs number of trees)
ntree_read2 <-  10000

#creating vectors
RMSE_read2 <- c(rmse10000_gbm_read2, rmse1_gbm_read2, rmse2_gbm_read2)
Predictions_read2 <- c(pred_read2, preds1_gbm_read2, preds2_gbm_read2)
Tree_Numbers_read2 <- c(ntree_read2, ntree_opt_oob_read2, ntree_opt_cv_read2)

#creating a dataframe
testsdf_read2 <- data.frame(RMSE_read2, Predictions_read2, Tree_Numbers_read2)

ggplot(testsdf_read2, aes(x = Tree_Numbers_read2, y = RMSE_read2, col = "red")) + 
  geom_point()+ ggtitle("Tree Numbers and RMSE, All Math and Reading Variables from Other Years Not Included")

###########################################################################################################################################

#Random forest without math variables and reading variables from other years


# Train a Random Forest
#set seed for reproducibiilty
set.seed(1) 

schoolsreadrf_model_2 <- randomForest(formula = r_2016 ~ ., 
                                    data = schoolsread2_train)

# Print the model output                             
print(schoolsreadrf_model_2)
summary(schoolsreadrf_model_2)

# Grab MSE error & take a look

err_2 <- schoolsreadrf_model_2$mse


head(err_2)


# Look at final OOB error rate (last row in err matrix)

mse_err_2 <- err_2[500]

print(mse_err_2)


# Plot the model trained in the previous exercise

plot(schoolsreadrf_model_2, main = "Random Forest Model for Schools, Reading 2016, Not Including Math Variables
     and Reading variables from Other Years")


# Generate predicted classes using the model object

schoolsreadrf_prediction_2 <- predict(object = schoolsreadrf_model_2,    
                                    
                                    newdata = schoolsread2_test,  
                                    
                                    type = "response") 

#calculating the RMSE 
readrf_rmse_2 <- rmse(schoolsread2_test$r_2016, schoolsreadrf_prediction_2)

print(readrf_rmse_2)

#RMSE is higher than the 10000 tree GB Model

#tune by mtry

# Execute the tuning process

set.seed(1)              

res_2 <- tuneRF(x = subset(schoolsread2_train, select = -r_2016),
              
              y = schoolsread2_train$r_2016,
              
              ntreeTry = 500)


# Look at results

print(res_2)


# Find the mtry value that minimizes OOB Error

mtry_opt_2 <- res_2[,"mtry"][which.min(res_2[,"OOBError"])]

print(mtry_opt_2)

#mtry_opt_2 = 2



# Train a Random Forest with tuned mtry
#set seed for reproducibiilty
set.seed(1) 

schoolsreadrf_model_mtry2 <- randomForest(formula = r_2016 ~ ., 
                                         data = schoolsread2_train, mtry = mtry_opt_2)

# Print the model output                             
print(schoolsreadrf_model_mtry2)
summary(schoolsreadrf_model_mtry2)

# Grab MSE error & take a look

err_mtry2 <- schoolsreadrf_model_mtry2$mse

head(err_mtry2)

# Look at final OOB error rate (last row in err matrix)

mse_err_mtry2 <- err_mtry2[500]

print(mse_err_mtry2)


# Plot the model trained in the previous exercise

plot(schoolsreadrf_model_mtry2, main = "Random Forest Model for Schools, Tuned Mtry, Reading 2016, Math Variables and Reading Variables
     From Other Years Not Included")


# Generate predicted classes using the model object

schoolsreadrf_prediction_mtry2 <- predict(object = schoolsreadrf_model_mtry2,    
                                         
                                         newdata = schoolsread2_test,  
                                         
                                         type = "response") 

#calculating the RMSE 
readrf_rmse_mtry2 <- rmse(schoolsread2_test$r_2016, schoolsreadrf_prediction_mtry2)

print(readrf_rmse_mtry2)


#smallest RMSE from random forest and gradient boosting (for models not including reading variables from other years and math variables)
# is for GB model that used 10,000 trees




