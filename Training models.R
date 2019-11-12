################################################################
# Sales Conversion Optimization
# How to Cluster Customer data for campaign marketing

# Model for approved conversions

# Created by Eirik Espe
################################################################

# Load the data we explored in the first script
source("Initial exploration.R")


#--- Creating a training and test set ----

#Set seed
set.seed(123)


#Define a 75%/25% train/test split of the sample dataset
inTraining <- createDataPartition(campaignAd$Approved_Conversion, 
                                  p = .75, 
                                  list = FALSE)
training <- campaignAd[inTraining,]
testing <- campaignAd[-inTraining,]


#--- Cross validation ----

#Setting a 4-fold cross validation to use on the model
Control <- trainControl(method = "repeatedcv", 
                        number = 10, 
                        repeats = 3)


#--- 1st Training model ----

#Train a linear regression model
mod_lm1 <- train(Approved_Conversion ~ ., data = training %>% 
                     select(Approved_Conversion, xyz_campaign_id:Total_Conversion),
                   method = "lm",
                   trControl = Control, 
                   tuneLength = 5)


#---Results 1st Training model model----

#Check results on the training set
train_results_1st_lm <- predict(object = mod_lm1, newdata = training)
postResample(train_results_1st_lm, training$Approved_Conversion)
# RMSE  0.5732
# R2    90.04 %
# MAE   0.2858


#Results on testing set
test_results_1st_lm <- predict(object = mod_lm1, newdata = testing)
postResample(test_results_1st_lm, testing$Approved_Conversion)
# RMSE  1.1222
# R2    57.40 %
# MAE   0.7850

summary(mod_lm1)