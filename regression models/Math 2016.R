#regression script for 2016 math outcome variable

#load relevant libraries and data
schools14_16 <- read.csv("schools14_16.csv", na = c("", "NA"))
library(tidyverse)
library(gbm)
library(rpart)
library(Metrics)
library(randomForest)


#further preparing data for  regression
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

colnames(schools14_16lin)[colnames(schools14_16lin)=="X2014.NWEA.Reading.Gr.3.Pct"] <- "2014 reading"

colnames(schools14_16lin)[colnames(schools14_16lin)=="X2015.NWEA.Reading.Gr.3.Pct"] <- "2015 reading "

colnames(schools14_16lin)[colnames(schools14_16lin)=="X2016.NWEA.Reading.Gr.3.Pct"] <- "2016 reading"

colnames(schools14_16lin)[colnames(schools14_16lin)== "X2014.NWEA.Math.Gr.3.Pct"] <- "2014 math"

colnames(schools14_16lin)[colnames(schools14_16lin)=="X2015.NWEA.Math.Gr.3.Pct"] <- "2015 math"

colnames(schools14_16lin)[colnames(schools14_16lin)=="X2016.NWEA.Math.Gr.3.Pct"] <- "2016 Math"

colnames(schools14_16lin)[colnames(schools14_16lin)=="Estimate.of.Pop..At.or.Above.Poverty.Line.who.Did.Not.Receive.Food.Stamps"] <- "no stamps"

colnames(schools14_16lin)[colnames(schools14_16lin)=="Estimate.of.Aggregate.Earnings"] <- "earn"

#check names
schools14_16lin <- data.frame(schools14_16lin, check.names = TRUE)

#remove latitude, longitude, name of school and categorical values
schools14_16lin <-  schools14_16lin[,(-(1:8))]
schools14_16lin <-  schools14_16lin[,(-(11:12))]

#removing 2016 reading
schools14_16linmath <-  schools14_16lin[,(-(7))]


summary(schools14_16linmath)


#removing na values
schools14_16linmath <- na.omit(schools14_16linmath)


#creating sample sets


# Total number of rows in the schools14_16lin data frame

n <- nrow(schools14_16linmath)



# Number of rows for the training set (80% of the dataset)

n_train <- round(0.80 * n) 


# Create a vector of indices which is an 80% random sample

set.seed(123)

train_indices <- sample(1:n, n_train)


# Subset the schools data frame to training indices only

schoolsmath_train <- schools14_16linmath[train_indices, ]  


# Exclude the training indices to create the test set

schoolsmath_test <- schools14_16linmath[-train_indices, ]  


# boosting

#create a model using 10,000 trees

set.seed(1)

schoolsmathgb_model <- gbm(formula = X2016.Math ~ ., 
                           
                           distribution = "gaussian", 
                           
                           data = schoolsmath_train,
                           
                           n.trees = 10000)



summary(schoolsmathgb_model)

# Generate prediction on the test set
predm_gb <- predict.gbm(object = schoolsmathgb_model, 
                    
                    newdata = schoolsmath_test,
                    
                    n.trees = 10000,
                    
                    type = "response")


# Examine the range of predictions

range(predm_gb)

# Generate the RMSE 
rmse10000m_gb <- rmse(schoolsmath_test$X2016.Math, predm_gb)

print(rmse10000m_gb)


#plot predictions
plot(predm_gb, main = "Math Predictions based on using 10000 trees, GB Model")


# Optimal ntree estimate based on OOB
ntree_opt_oob <- gbm.perf(object = schoolsmathgb_model, 
                          
                          method = "OOB", 
                          
                          oobag.curve = TRUE)

# Train a CV GBM model
set.seed(1)

schoolsmathgb_model_cv <- gbm(formula = X2016.Math ~ ., 
                              
                              distribution = "gaussian", 
                              
                              data = schoolsmath_train,
                              
                              n.trees = 10000,
                              
                              cv.folds = 2)



# Optimal ntree estimate based on CV

ntree_opt_cv <- gbm.perf(object = schoolsmathgb_model_cv, 
                         
                         method = "cv")

# Compare the estimates   

print(paste0("Optimal n.trees (OOB Estimate): ", ntree_opt_oob))                         

print(paste0("Optimal n.trees (CV Estimate): ", ntree_opt_cv))


# two different models, using different estimates of optimal trees  

# Use oob

predsm1_gbm <- predict.gbm(object = schoolsmathgb_model, 
                      
                      newdata = schoolsmath_test,
                      
                      n.trees = ntree_opt_oob, type = "response")

#use cv


predsm2_gbm <- predict.gbm(object = schoolsmathgb_model, 
                      
                      newdata = schoolsmath_test,
                      
                      n.trees = ntree_opt_cv, type = "response") 

#plot predictions

plot(predsm1_gbm, main = "Math Predictions based on OOB Estimate of Optimal Trees")
plot(predsm2_gbm, main = "Math Predictions based on CV Estimate of Optimal Trees")


#Compare RMSE
#oob
rmse1_gb <- rmse(schoolsmath_test$X2016.Math, predsm1_gbm)

#cv
rmse2_gb <-  rmse(schoolsmath_test$X2016.Math, predsm2_gbm)



# Compare RMSE 

print(paste0("Test set RMSE (OOB): ", rmse1_gb))                         

print(paste0("Test set RMSE (CV): ", rmse2_gb))

##smallest RMSE is for the CV model


#Plotting the RMSE error vs number of trees)
ntree1 <-  10000

#creating vectors
RMSEmath <- c(rmse10000m_gb, rmse1_gb, rmse2_gb)
Predictionsmath <- c(predm_gb, predsm1_gbm, predsm2_gbm)
Tree_Numbersmath <- c(ntree1, ntree_opt_oob, ntree_opt_cv)

#creating a dataframe
testsdf <- data.frame(RMSEmath, Predictionsmath, Tree_Numbersmath)

ggplot(testsdf, aes(x = Tree_Numbersmath, y = RMSEmath, col = "red")) + 
  geom_point()+ ggtitle("Tree Numbers and RMSE")

##########################################################################################################################################
#Random Forest
# Train a Random Forest
#set seed for reproducibiilty
set.seed(1) 

schoolsmathrf_model <- randomForest(formula = X2016.Math ~ ., 
                                    data = schoolsmath_train)

# Print the model output                             
print(schoolsmathrf_model)
summary(schoolsmathrf_model)

# Grab MSE error & take a look

err <- schoolsmathrf_model$mse

head(err)

# Look at final OOB error rate (last row in err matrix)

mse_err <- err[500]

print(mse_err)


# Plot the model trained in the previous exercise

plot(schoolsmathrf_model, main = "Random Forest Model for Schools, Math, 2016")


# Generate predicted classes using the model object

schoolsmathrf_prediction <- predict(object = schoolsmathrf_model,    
                                    
                                    newdata = schoolsmath_test,  
                                    
                                    type = "response") 

#calculating the RMSE 
mathrf_rmse <- rmse(schoolsmath_test$X2016.Math, schoolsmathrf_prediction)

print(mathrf_rmse)

#RMSE is a bit smaller than the CV GB Model

#tune by mtry

# Execute the tuning process

set.seed(1)              

res_math <- tuneRF(x = subset(schoolsmath_train, select = -X2016.Math),
              
              y = schoolsmath_train$X2016.Math,
              
              ntreeTry = 500)


# Look at results

print(res_math)


# Find the mtry value that minimizes OOB Error

mtry_opt_m <- res_math[,"mtry"][which.min(res_math[,"OOBError"])]

print(mtry_opt_m)


# Train a Random Forest with optimized mtry
#set seed for reproducibiilty
set.seed(1) 

schoolsmathrf_model_mtry <- randomForest(formula = X2016.Math ~ ., 
                                         data = schoolsmath_train, mtry = mtry_opt_m )

# Print the model output                             
print(schoolsmathrf_model_mtry)
summary(schoolsmathrf_model_mtry)

# Grab MSE error & take a look

err_mtry_m <- schoolsmathrf_model_mtry$mse

head(err_mtry_m)

# Look at final OOB error rate (last row in err matrix)

mse_err_mtry_m <- err_mtry_m[500]

print(mse_err_mtry_m)

# Plot the model trained in the previous exercise

plot(schoolsmathrf_model_mtry, main = "Random Forest Model for Schools, Tuned Mtry, Math, 2016")


# Generate predicted classes using the model object

schoolsmathrf_prediction_mtry <- predict(object = schoolsmathrf_model_mtry,    
                                         
                                         newdata = schoolsmath_test,  
                                         
                                         type = "response") 

#calculating the RMSE 
mathrf_rmse_mtry <- rmse(schoolsmath_test$X2016.Math, schoolsmathrf_prediction_mtry)

print(mathrf_rmse_mtry)

#lowest RMSE for all models is the Random forest model with tuned mtry
#############################################################################################################################################
#building gb and random forest models without the reading variables and math variables from other years

#removing 2014-2016 reading variables and other math variables
schools14_16math2 <-  schools14_16lin[,(-(5:9))]


#removing na values
schools14_16math2 <- na.omit(schools14_16math2)


#creating sample sets


# Total number of rows in the schools14_16lin data frame

n <- nrow(schools14_16math2)



# Number of rows for the training set (80% of the dataset)

n_train <- round(0.80 * n) 


# Create a vector of indices which is an 80% random sample

set.seed(123)

train_indices <- sample(1:n, n_train)


# Subset the schools data frame to training indices only

schoolsmath2_train <- schools14_16math2[train_indices, ]  


# Exclude the training indices to create the test set

schoolsmath2_test <- schools14_16math2[-train_indices, ]  


# boosting

#create a model using 10,000 trees

set.seed(1)

schoolsmathgb_model_2 <- gbm(formula = X2016.Math ~ ., 
                           
                           distribution = "gaussian", 
                           
                           data = schoolsmath2_train,
                           
                           n.trees = 10000)



summary(schoolsmathgb_model_2)

# Generate prediction on the test set
predm_gb_2 <- predict.gbm(object = schoolsmathgb_model_2, 
                        
                        newdata = schoolsmath2_test,
                        
                        n.trees = 10000,
                        
                        type = "response")


# Examine the range of predictions

range(predm_gb_2)

# Generate the RMSE 
rmse10000m_gb2 <- rmse(schoolsmath2_test$X2016.Math, predm_gb_2)

print(rmse10000m_gb2)


#plot predictions
plot(predm_gb_2, main = "Math Predictions based on using 10000 trees, GB Model, Reading Variables and Math Variables from other years
     Not Included")


# Optimal ntree estimate based on OOB
ntree_opt_oob2 <- gbm.perf(object = schoolsmathgb_model_2, 
                          
                          method = "OOB", 
                          
                          oobag.curve = TRUE)

# Train a CV GBM model
set.seed(1)

schoolsmathgb_model_cv2 <- gbm(formula = X2016.Math ~ ., 
                              
                              distribution = "gaussian", 
                              
                              data = schoolsmath2_train,
                              
                              n.trees = 10000,
                              
                              cv.folds = 2)



# Optimal ntree estimate based on CV

ntree_opt_cv2 <- gbm.perf(object = schoolsmathgb_model_cv2, 
                         
                         method = "cv")

# Compare the estimates   

print(paste0("Optimal n.trees (OOB Estimate): ", ntree_opt_oob2))                         

print(paste0("Optimal n.trees (CV Estimate): ", ntree_opt_cv2))


# two different models, using different estimates of optimal trees  

# Use oob

predsm1_gbm2 <- predict.gbm(object = schoolsmathgb_model_2, 
                           
                           newdata = schoolsmath2_test,
                           
                           n.trees = ntree_opt_oob2, type = "response")

#use cv


predsm2_gbm2 <- predict.gbm(object = schoolsmathgb_model_2, 
                           
                           newdata = schoolsmath2_test,
                           
                           n.trees = ntree_opt_cv2, type = "response") 

#plot predictions

plot(predsm1_gbm2, main = "Math Predictions based on OOB Estimate of Optimal Trees, Reading and Other Math Variables Not Included")
plot(predsm2_gbm2, main = "Math Predictions based on CV Estimate of Optimal Trees, Reading and Other Math Variables Not Included")


#Compare RMSE
#oob
rmse1_gb2 <- rmse(schoolsmath2_test$X2016.Math, predsm1_gbm2)

#cv
rmse2_gb2 <-  rmse(schoolsmath2_test$X2016.Math, predsm2_gbm2)



# Compare RMSE 

print(paste0("Test set RMSE (OOB): ", rmse1_gb2))                         

print(paste0("Test set RMSE (CV): ", rmse2_gb2))

##smallest RMSE is for the CV model


#Plotting the RMSE error vs number of trees)
ntree2 <-  10000

#creating vectors
RMSEmath2 <- c(rmse10000m_gb2, rmse1_gb2, rmse2_gb2)
Predictionsmath2 <- c(predm_gb_2, predsm1_gbm2, predsm2_gbm2)
Tree_Numbersmath2 <- c(ntree2, ntree_opt_oob2, ntree_opt_cv2)

#creating a dataframe
testsdf2 <- data.frame(RMSEmath2, Predictionsmath2, Tree_Numbersmath2)

ggplot(testsdf2, aes(x = Tree_Numbersmath2, y = RMSEmath2, col = "red")) + 
  geom_point()+ ggtitle("Tree Numbers and RMSE, Reading and other Math Variables Not Included")

##########################################################################################################################################
#Random Forest
# Train a Random Forest
#set seed for reproducibiilty
set.seed(1) 

schoolsmathrf_model2 <- randomForest(formula = X2016.Math ~ ., 
                                    data = schoolsmath2_train)

# Print the model output                             
print(schoolsmathrf_model2)
summary(schoolsmathrf_model2)

# Grab MSE error & take a look

err2 <- schoolsmathrf_model2$mse

head(err2)

# Look at final OOB error rate (last row in err matrix)

mse_err2 <- err2[500]

print(mse_err2)


# Plot the model trained in the previous exercise

plot(schoolsmathrf_model2, main = "Random Forest Model for Schools, Math, 2016, Reading & Other Math Variables Not Included")


# Generate predicted classes using the model object

schoolsmathrf_prediction2 <- predict(object = schoolsmathrf_model2,    
                                    
                                    newdata = schoolsmath2_test,  
                                    
                                    type = "response") 

#calculating the RMSE 
mathrf_rmse2 <- rmse(schoolsmath2_test$X2016.Math, schoolsmathrf_prediction2)

print(mathrf_rmse2)

#RMSE is larger than the CV GB Model

#tune by mtry

# Execute the tuning process

set.seed(1)              

res_math2 <- tuneRF(x = subset(schoolsmath2_train, select = -X2016.Math),
                   
                   y = schoolsmath2_train$X2016.Math,
                   
                   ntreeTry = 500)


# Look at results

print(res_math2)


# Find the mtry value that minimizes OOB Error

mtry_opt_m2 <- res_math2[,"mtry"][which.min(res_math2[,"OOBError"])]

print(mtry_opt_m2)


# Train a Random Forest with optimized mtry
#set seed for reproducibiilty
set.seed(1) 

schoolsmathrf_model_mtry2 <- randomForest(formula = X2016.Math ~ ., 
                                         data = schoolsmath2_train, mtry = mtry_opt_m2 )

# Print the model output                             
print(schoolsmathrf_model_mtry2)
summary(schoolsmathrf_model_mtry2)

# Grab MSE error & take a look

err_mtry_m2 <- schoolsmathrf_model_mtry2$mse

head(err_mtry_m2)

# Look at final OOB error rate (last row in err matrix)

mse_err_mtry_m2 <- err_mtry_m2[500]

print(mse_err_mtry_m2)

# Plot the model trained in the previous exercise

plot(schoolsmathrf_model_mtry2, main = "Random Forest Model for Schools, Tuned Mtry, Math, 2016,
     Reading & Other Math Variables Not Included")


# Generate predicted classes using the model object

schoolsmathrf_prediction_mtry2 <- predict(object = schoolsmathrf_model_mtry2,    
                                         
                                         newdata = schoolsmath2_test,  
                                         
                                         type = "response") 

#calculating the RMSE 
mathrf_rmse_mtry2 <- rmse(schoolsmath2_test$X2016.Math, schoolsmathrf_prediction_mtry2)

print(mathrf_rmse_mtry2)

#lowest RMSE for all models is the CV GB model for models that don't include reading and other math variables
#lowest RMSE for all models built is the mtry tuned random forest that includes reading and math variables from other years

