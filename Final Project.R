## Final Project
## Delaney Helgeson
#####################

#Load libraries
library(VIM)
library(caret)
library(tidyr)
library(splines)
library(caret)
library(glmnet)
library(ranger)
library(OptimalCutpoints)
library(kernlab)
library(pROC)
library(dplyr)
library(klaR)

###########################
## Initial PreProcessing ##
###########################

setwd("C:\\Users\\delan\\Downloads\\STAT6302\\Final Project")

# Upload raw data
mushroom <- read.csv("secondary_data.csv", sep=";",  na.strings = "", stringsAsFactors = T)
str(mushroom)

# See which columns have missing values
colSums(is.na(mushroom))*100/dim(mushroom)[1]

# Remove spore.print.color (89.595%) and veil.type (94.798%)
mushroom_r <- subset(mushroom, select=-c(spore.print.color,veil.type))

# Remove Near Zero Variance
nearZero <- nearZeroVar(mushroom_r, saveMetrics = T)
mushroom_r[,17]
# Remove ring.type
mushroom_r2 <- mushroom_r[,-nearZero]

# Train Validation Test Rows
set.seed(425)
train_valid_Rows <- createDataPartition(mushroom_r2$class, p = 0.8, list = F)
mushroom_train_valid <- mushroom_r2[train_valid_Rows,]
set.seed(425)
train_Rows <- createDataPartition(mushroom_train_valid$class, p = 0.8, list = F)

# Create Train, Validation, and Test splits
mushroom_train <- mushroom_train_valid[train_Rows,]
mushroom_valid <- mushroom_train_valid[-train_Rows,]
mushroom_test <- mushroom_r2[-train_valid_Rows,]

# Impute Validation Set
set.seed(425)
mushroom_valid_imp <- kNN(mushroom_valid, variable=c("cap.surface", "gill.attachment", "gill.spacing", "stem.root",
                                                     "stem.surface","veil.color"), imp_var=FALSE)
# Write file to csv for later use
write.csv(mushroom_valid_imp, "mushroom_valid_imp.csv")
# Cross your fingers
colSums(is.na(mushroom_valid_imp))*100/dim(mushroom_valid_imp)[1]

length(colnames(mushroom_train_ns))-9-6+3
# Impute Test Set
set.seed(425)
mushroom_test_imp <- kNN(mushroom_test, variable=c("cap.surface", "gill.attachment", "gill.spacing", "stem.root",
                                                   "stem.surface","veil.color"), imp_var=FALSE)

# Write file to csv for later use
write.csv(mushroom_test_imp, "mushroom_test_imp.csv")
# Cross your fingers
colSums(is.na(mushroom_test_imp))*100/dim(mushroom_test_imp)[1]

# Impute Train Set
set.seed(425)
mushroom_train_imp <- kNN(mushroom_train, variable=c("cap.surface", "gill.attachment", "gill.spacing", "stem.root",
                                                     "stem.surface","veil.color"), imp_var=FALSE)
# Write file to csv for later use
write.csv(mushroom_train_imp, "mushroom_train_imp.csv")
# Cross your fingers
colSums(is.na(mushroom_train_imp))*100/dim(mushroom_train_imp)[1]


# Used this block of code for creating the table in the report; enter the names of the 
# variables to see their distribtion
mushroom_summary <- rbind(mushroom_train_imp, mushroom_valid_imp, mushroom_test_imp)
variable <- mushroom_summary %>% group_by(gill.spacing) %>% summarize(count_each = n())
variable <- data.frame(variable)
variable$percentage <- round(variable$count_each*100/ sum(variable$count_each),2)
variable
# For numeric variables
summary(mushroom_summary$cap.diameter)


#############
## Splines ##
#############

# Upload imputed data
mushroom_train_imp <- read.csv("mushroom_train_imp.csv", stringsAsFactors = T)
mushroom_valid_imp <- read.csv("mushroom_valid_imp.csv", stringsAsFactors = T)
mushroom_test_imp <- read.csv("mushroom_test_imp.csv", stringsAsFactors = T)

# Drop ID column
mushroom_train_imp <- mushroom_train_imp[,-1]
mushroom_valid_imp <- mushroom_valid_imp[,-1]
mushroom_test_imp <- mushroom_test_imp[,-1]

# Create Splines for Numeric variables (cap.diameter, stem.height, stem.width)
cap.diameter_spline <- ns(mushroom_train_imp$cap.diameter, df=3)
stem.height_spline <- ns(mushroom_train_imp$stem.height, df=3)
stem.width_spline <- ns(mushroom_train_imp$stem.width, df=3)
cap.diameter_spline_valid <- ns(mushroom_valid_imp$cap.diameter, df=3)
stem.height_spline_valid <- ns(mushroom_valid_imp$stem.height, df=3)
stem.width_spline_valid <- ns(mushroom_valid_imp$stem.width, df=3)
cap.diameter_spline_test <- ns(mushroom_test_imp$cap.diameter, df=3)
stem.height_spline_test <- ns(mushroom_test_imp$stem.height, df=3)
stem.width_spline_test <- ns(mushroom_test_imp$stem.width, df=3)

# Rename columns
colnames(cap.diameter_spline)[1:3] <- c('cap.diameter.N1', 'cap.diameter.N2', 'cap.diameter.N3')
colnames(stem.height_spline)[1:3] <- c('stem.height.N1', 'stem.height.N2', 'stem.height.N3')
colnames(stem.width_spline)[1:3] <- c('stem.width.N1', 'stem.width.N2', 'stem.width.N3')
colnames(cap.diameter_spline_valid)[1:3] <- c('cap.diameter.N1', 'cap.diameter.N2', 'cap.diameter.N3')
colnames(stem.height_spline_valid)[1:3] <- c('stem.height.N1', 'stem.height.N2', 'stem.height.N3')
colnames(stem.width_spline_valid)[1:3] <- c('stem.width.N1', 'stem.width.N2', 'stem.width.N3')
colnames(cap.diameter_spline_test)[1:3] <- c('cap.diameter.N1', 'cap.diameter.N2', 'cap.diameter.N3')
colnames(stem.height_spline_test)[1:3] <- c('stem.height.N1', 'stem.height.N2', 'stem.height.N3')
colnames(stem.width_spline_test)[1:3] <- c('stem.width.N1', 'stem.width.N2', 'stem.width.N3')

# Join to data set 
mushroom_train_ns1 <- cbind(mushroom_train_imp, cap.diameter_spline, stem.height_spline, stem.width_spline)
mushroom_valid_ns1 <- cbind(mushroom_valid_imp, cap.diameter_spline_valid, stem.height_spline_valid, stem.width_spline_valid)
mushroom_test_ns1 <- cbind(mushroom_test_imp, cap.diameter_spline_test, stem.height_spline_test, stem.width_spline_test)
# Drop original variables
mushroom_train_ns <- subset(mushroom_train_ns1, select = -c(cap.diameter, stem.height, stem.width))
mushroom_valid_ns <- subset(mushroom_valid_ns1, select = -c(cap.diameter, stem.height, stem.width))
mushroom_test_ns <- subset(mushroom_test_ns1, select = -c(cap.diameter, stem.height, stem.width))

#####################################
## Logistic Regression: Tune Model ##
#####################################

# Define predictors/response split
X_train <- model.matrix(class ~., mushroom_train_ns) # dummy encoded
y_train <- mushroom_train_ns[,1]
X_valid <- model.matrix(class ~., mushroom_valid_ns) 
y_valid <- mushroom_valid_ns[,1]
y_valid <- ifelse(y_valid == 'e', 0, 1)
X_test <- model.matrix(class ~., mushroom_test_ns)
y_test <- mushroom_test_ns[,1]
y_test <- ifelse(y_test == 'e', 0, 1)

# Remove R objects to save space
rm(mushroom_train_imp)
rm(mushroom_valid_imp)
rm(mushroom_test_imp)
rm(mushroom_train_ns1)
rm(mushroom_valid_ns1)
rm(mushroom_test_ns1)
rm(cap.diameter_spline)
rm(stem.height_spline)
rm(stem.width_spline)
rm(cap.diameter_spline_valid)
rm(stem.height_spline_valid)
rm(stem.width_spline_valid)
rm(cap.diameter_spline_test)
rm(stem.height_spline_test)
rm(stem.width_spline_test)


# hyperparameter grid
glmnGrid <- expand.grid(alpha = c(0, 0.5, 1),
                        lambda = seq(0.01, 0.2, length = 10))

# Set train control: 10-fold cross validation repeated five times. 
ctrl <- trainControl(method = "repeatedcv", 
                     classProbs = TRUE, 
                     repeats=5,
                     savePredictions = TRUE)
# Train model
set.seed(425)
Logistic_glm <- train(x = X_train, 
                      y = y_train,
                      method = "glmnet",
                      tuneGrid = glmnGrid,
                      metric = "Accuracy",
                      family = "binomial",
                      trControl = ctrl)
saveRDS(Logistic_glm, "Logistic_glm.rds")

# Optimal Parameters and results
Logistic_glm$bestTune
Logistic_glm$results

#####################
## Cutpoint Tuning ##
#####################

# Read in model
Logistic_glm <- readRDS("Logistic_glm.rds")

# Coefficients of final model
coef(Logistic_glm$finalModel, s=Logistic_glm$bestTune$lambda)

# Preds: Logistic regression on Validation data; Used for choosing a cutpoint
logistic_predict_valid <- predict(Logistic_glm$finalModel, newx=X_valid, s=Logistic_glm$bestTune$lambda, type='response')

# Logistic Validation Results 
Logistic_results <- cbind(y_valid, logistic_predict_valid)
Logistic_results <- data.frame(Logistic_results)
Logistic_results$y_valid <- factor(Logistic_results$y_valid)
Logistic_results$predclass <- factor(ifelse(Logistic_results$s1 > 0.5, 1, 0))

# See what is the sensitivity of un-adjusted cut-point model
confusionMatrix(data = Logistic_results$predclass, 
                reference = Logistic_results$y_valid, positive="1")

# Tune model for optimal cut point
SeValues <- seq(0.89, 0.99, length = 10)

# Test all desired sensitivities: Find a balance between Accuracy, Sensitivity and Specificity
# It's not letting me access the optimal.cutpoints objects from a list properly... so we will do it individually
# Se = 0.89
Opt.cpt.1 <- optimal.cutpoints(X="s1", status="y_valid", data=data.frame(Logistic_results), tag.healthy='0', methods='MinValueSe',
                               control = control.cutpoints(valueSe = SeValues[1]))
# Se = 0.90
Opt.cpt.2 <- optimal.cutpoints(X="s1", status="y_valid", data=data.frame(Logistic_results), tag.healthy='0', methods='MinValueSe',
                               control = control.cutpoints(valueSe = SeValues[2]))
# Se = 0.91
Opt.cpt.3 <- optimal.cutpoints(X="s1", status="y_valid", data=data.frame(Logistic_results), tag.healthy='0', methods='MinValueSe',
                               control = control.cutpoints(valueSe = SeValues[3]))
# Se = 0.92
Opt.cpt.4 <- optimal.cutpoints(X="s1", status="y_valid", data=data.frame(Logistic_results), tag.healthy='0', methods='MinValueSe',
                               control = control.cutpoints(valueSe = SeValues[4]))
# Se = 0.93
Opt.cpt.5 <- optimal.cutpoints(X="s1", status="y_valid", data=data.frame(Logistic_results), tag.healthy='0', methods='MinValueSe',
                               control = control.cutpoints(valueSe = SeValues[5]))
# Se = 0.945
Opt.cpt.6 <- optimal.cutpoints(X="s1", status="y_valid", data=data.frame(Logistic_results), tag.healthy='0', methods='MinValueSe',
                               control = control.cutpoints(valueSe = SeValues[6]))
# Se = 0.956
Opt.cpt.7 <- optimal.cutpoints(X="s1", status="y_valid", data=data.frame(Logistic_results), tag.healthy='0', methods='MinValueSe',
                               control = control.cutpoints(valueSe = SeValues[7]))
# Se = 0.967
Opt.cpt.8 <- optimal.cutpoints(X="s1", status="y_valid", data=data.frame(Logistic_results), tag.healthy='0', methods='MinValueSe',
                               control = control.cutpoints(valueSe = SeValues[8]))
# Se = 0.978
Opt.cpt.9 <- optimal.cutpoints(X="s1", status="y_valid", data=data.frame(Logistic_results), tag.healthy='0', methods='MinValueSe',
                               control = control.cutpoints(valueSe = SeValues[9]))
# Se = 0.99
Opt.cpt.10 <- optimal.cutpoints(X="s1", status="y_valid", data=data.frame(Logistic_results), tag.healthy='0', methods='MinValueSe',
                                control = control.cutpoints(valueSe = SeValues[10]))

summary(Opt.cpt.1)
summary(Opt.cpt.2)
summary(Opt.cpt.3)
summary(Opt.cpt.4)
summary(Opt.cpt.5)
summary(Opt.cpt.6)
summary(Opt.cpt.7)
summary(Opt.cpt.8)
summary(Opt.cpt.9)
summary(Opt.cpt.10)

Specificities <- c(summary(Opt.cpt.1)$p.table$Global$MinValueSe[[1]]["Sp",],
                   summary(Opt.cpt.2)$p.table$Global$MinValueSe[[1]]["Sp",],
                   summary(Opt.cpt.3)$p.table$Global$MinValueSe[[1]]["Sp",],
                   summary(Opt.cpt.4)$p.table$Global$MinValueSe[[1]]["Sp",],
                   summary(Opt.cpt.5)$p.table$Global$MinValueSe[[1]]["Sp",],
                   summary(Opt.cpt.6)$p.table$Global$MinValueSe[[1]]["Sp",],
                   summary(Opt.cpt.7)$p.table$Global$MinValueSe[[1]]["Sp",],
                   summary(Opt.cpt.8)$p.table$Global$MinValueSe[[1]]["Sp",],
                   summary(Opt.cpt.9)$p.table$Global$MinValueSe[[1]]["Sp",],
                   summary(Opt.cpt.10)$p.table$Global$MinValueSe[[1]]["Sp",])

Cutoff <- c(summary(Opt.cpt.1)$p.table$Global$MinValueSe[[1]]["cutoff",],
            summary(Opt.cpt.2)$p.table$Global$MinValueSe[[1]]["cutoff",],
            summary(Opt.cpt.3)$p.table$Global$MinValueSe[[1]]["cutoff",],
            summary(Opt.cpt.4)$p.table$Global$MinValueSe[[1]]["cutoff",],
            summary(Opt.cpt.5)$p.table$Global$MinValueSe[[1]]["cutoff",],
            summary(Opt.cpt.6)$p.table$Global$MinValueSe[[1]]["cutoff",],
            summary(Opt.cpt.7)$p.table$Global$MinValueSe[[1]]["cutoff",],
            summary(Opt.cpt.8)$p.table$Global$MinValueSe[[1]]["cutoff",],
            summary(Opt.cpt.9)$p.table$Global$MinValueSe[[1]]["cutoff",],
            summary(Opt.cpt.10)$p.table$Global$MinValueSe[[1]]["cutoff",])

SeTuningMetrics <- data.frame(cbind(SeValues, Specificities, Cutoff))
colnames(SeTuningMetrics)[1:2] <- c("Sensitivity", "Specificity")

# Append outcomes to Logistic results of different cutpoints
Logistic_results$predclass_se1 <- factor(ifelse(Logistic_results$s1 > Cutoff[1], 1, 0))
Logistic_results$predclass_se2 <- factor(ifelse(Logistic_results$s1 > Cutoff[2], 1, 0))
Logistic_results$predclass_se3 <- factor(ifelse(Logistic_results$s1 > Cutoff[3], 1, 0))
Logistic_results$predclass_se4 <- factor(ifelse(Logistic_results$s1 > Cutoff[4], 1, 0))
Logistic_results$predclass_se5 <- factor(ifelse(Logistic_results$s1 > Cutoff[5], 1, 0))
Logistic_results$predclass_se6 <- factor(ifelse(Logistic_results$s1 > Cutoff[6], 1, 0))
Logistic_results$predclass_se7 <- factor(ifelse(Logistic_results$s1 > Cutoff[7], 1, 0))
Logistic_results$predclass_se8 <- factor(ifelse(Logistic_results$s1 > Cutoff[8], 1, 0))
Logistic_results$predclass_se9 <- factor(ifelse(Logistic_results$s1 > Cutoff[9], 1, 0))
Logistic_results$predclass_se10 <- factor(ifelse(Logistic_results$s1 > Cutoff[10], 1, 0))

# Construct Confusion Matrices to Extract Metrics
conf_matrix1 <- confusionMatrix(data = Logistic_results$predclass_se1, 
                                reference = Logistic_results$y_valid, positive="1")
conf_matrix2 <- confusionMatrix(data = Logistic_results$predclass_se2, 
                                reference = Logistic_results$y_valid, positive="1")
conf_matrix3 <- confusionMatrix(data = Logistic_results$predclass_se3, 
                                reference = Logistic_results$y_valid, positive="1")
conf_matrix4 <- confusionMatrix(data = Logistic_results$predclass_se4, 
                                reference = Logistic_results$y_valid, positive="1")
conf_matrix5 <- confusionMatrix(data = Logistic_results$predclass_se5, 
                                reference = Logistic_results$y_valid, positive="1")
conf_matrix6 <- confusionMatrix(data = Logistic_results$predclass_se6, 
                                reference = Logistic_results$y_valid, positive="1")
conf_matrix7 <- confusionMatrix(data = Logistic_results$predclass_se7, 
                                reference = Logistic_results$y_valid, positive="1")
conf_matrix8 <- confusionMatrix(data = Logistic_results$predclass_se8, 
                                reference = Logistic_results$y_valid, positive="1")
conf_matrix9 <- confusionMatrix(data = Logistic_results$predclass_se9, 
                                reference = Logistic_results$y_valid, positive="1")
conf_matrix10 <- confusionMatrix(data = Logistic_results$predclass_se10, 
                                 reference = Logistic_results$y_valid, positive="1")

SeTuningMetrics$Kappa <- c(conf_matrix1$overall["Kappa"], conf_matrix2$overall["Kappa"], conf_matrix3$overall["Kappa"],
                           conf_matrix4$overall["Kappa"], conf_matrix5$overall["Kappa"], conf_matrix6$overall["Kappa"],
                           conf_matrix7$overall["Kappa"], conf_matrix8$overall["Kappa"], conf_matrix9$overall["Kappa"],
                           conf_matrix10$overall["Kappa"])

SeTuningMetrics$Accuracy <- c(conf_matrix1$overall["Accuracy"], conf_matrix2$overall["Accuracy"], conf_matrix3$overall["Accuracy"],
                              conf_matrix4$overall["Accuracy"], conf_matrix5$overall["Accuracy"], conf_matrix6$overall["Accuracy"],
                              conf_matrix7$overall["Accuracy"], conf_matrix8$overall["Accuracy"], conf_matrix9$overall["Accuracy"],
                              conf_matrix10$overall["Accuracy"])

SeTuningMetrics$PPV <- c(conf_matrix1$byClass["Pos Pred Value"], conf_matrix2$byClass["Pos Pred Value"], 
                         conf_matrix3$byClass["Pos Pred Value"], conf_matrix4$byClass["Pos Pred Value"],
                         conf_matrix5$byClass["Pos Pred Value"], conf_matrix6$byClass["Pos Pred Value"], 
                         conf_matrix7$byClass["Pos Pred Value"], conf_matrix8$byClass["Pos Pred Value"],
                         conf_matrix9$byClass["Pos Pred Value"], conf_matrix10$byClass["Pos Pred Value"])

SeTuningMetrics$NPV <- c(conf_matrix1$byClass["Neg Pred Value"], conf_matrix2$byClass["Neg Pred Value"], 
                         conf_matrix3$byClass["Neg Pred Value"], conf_matrix4$byClass["Neg Pred Value"],
                         conf_matrix5$byClass["Neg Pred Value"], conf_matrix6$byClass["Neg Pred Value"], 
                         conf_matrix7$byClass["Neg Pred Value"], conf_matrix8$byClass["Neg Pred Value"],
                         conf_matrix9$byClass["Neg Pred Value"], conf_matrix10$byClass["Neg Pred Value"])

# Metrics dataframe for various cutoffs
SeTuningMetrics
#write.csv(SeTuningMetrics, "SeTuningMetrics.csv")
SeTuningMetrics_long <- gather(SeTuningMetrics, Metric, Value, c(1,2,4,5,6,7))

# Plot Model Metrics for Various cutoffs
SeTuningPlot <- ggplot(SeTuningMetrics_long, aes(x = Cutoff, y = Value, colour = Metric)) + geom_line() +
  ggtitle("Figure 1: Model Performance Metrics across Cutpoints")+
  theme(plot.title=element_text(hjust=0.5, face='bold'))
SeTuningPlot

# Grab optimal cutoff
SeTuningMetrics <- read.csv("SeTuningMetrics.csv")
opt_cpt_final <- SeTuningMetrics$Cutoff[6]

###########################################
## Model Evaluation: Logistic Regression ##
###########################################

# Read model to save time
Logistic_glm_final <- readRDS("Logistic_glm.rds") # same model as before, just gave it a different name

# Predict on test data
logistic_predict_test <- predict(Logistic_glm_final$finalModel, newx=X_test,
                                 s=Logistic_glm_final$bestTune$lambda, type='response')
# Logistic Validation Results 
Logistic_results_final <- data.frame(cbind(y_test, logistic_predict_test))
Logistic_results_final$y_test <- factor(Logistic_results_final$y_test)
Logistic_results_final$predclass <- factor(ifelse(Logistic_results_final$s1 > opt_cpt_final, 1, 0))

## Calibration analysis
Cal_Logistic <- calibration(y_test ~ s1, data = Logistic_results_final, cuts = 10, class="1")
xyplot(Cal_Logistic, auto.key = list(columns = 2), main="Figure 2: Calibration Plot for Logistic Regression Model")
Cal_Logistic$data

# ROC Plot
ROC_Logistic <- roc(Logistic_results_final$y_test, Logistic_results_final$s1)

# AUC
auc(ROC_Logistic)
# AUC confidence interval
ci.auc(ROC_Logistic)

# Plot ROC
plot(ROC_Logistic, legacy.axes = TRUE, main="Figure 3: ROC Curve for Logistic Regression Model")

# Confusion Matrix
confusionMatrix(data = Logistic_results_final$predclass, 
                reference = Logistic_results_final$y_test, positive="1")

# Calculate brier score
(1/length(Logistic_results_final$s1)) * sum((Logistic_results_final$s1 - as.numeric(Logistic_results_final$y_test))^2)

# Predicted Probabilities Faceted by true outcome
ggnames <- c("1" = "True Outcome: Poisonous", "0" = "True Outcome: Edible")
hist_Logistic <- ggplot(data=Logistic_results_final, aes(x=s1)) + geom_histogram() + 
  facet_grid(~factor(y_test, levels = c("1", "0")),
             labeller = as_labeller(ggnames)) + xlab("Probability of Poisonous") + ylab("Frequency") +
  ggtitle("Figure 4: Fitted Probabilities By True Outcome for Logistic Regression")+
  theme(plot.title=element_text(hjust=0.5, face='bold'))
hist_Logistic

coef(Logistic_glm_final$finalModel, s=Logistic_glm_final$bestTune$lambda)

#######################################
## Random Forest: Tune Initial Model ##
#######################################

# Specify tuning grid
rf_grid <- expand.grid(mtry = c(3, 5, 7),
                       splitrule = c("gini", "hellinger"),
                       min.node.size = c(1, 3, 5))
# train model
set.seed(425)
rf_fit_caret <- train(x = mushroom_train_ns[,-1], 
                      y = mushroom_train_ns[,1],
                      method = "ranger",
                      trControl = trainControl(method="repeatedcv", number = 10, classProbs = T),
                      metric = "Kappa",
                      tuneGrid = rf_grid,
                      num.trees = 500,
                      importance="impurity"
)
saveRDS(rf_fit_caret, "rf_fit_caret.rds")
rf_fit_caret <- readRDS("rf_fit_caret.rds")

# Results and best tuning parameters
rf_fit_caret$results
rf_fit_caret$bestTune

# Predict on validation set
rf_predict <- predict(rf_fit_caret$finalModel, data=mushroom_valid_ns[,-1], s1=rf_fit_caret$bestTune$mtry,
                      s2=rf_fit_caret$bestTune$splitrule, s3=rf_fit_caret$bestTune$min.node.size)

# RF Validation Results 
Rf_results <- data.frame(cbind(y_valid, rf_predict$predictions[,"p"]))
Rf_results$y_valid <- factor(Rf_results$y_valid)
Rf_results$predclass <- factor(ifelse(Rf_results$V2 > 0.5, 1, 0))
str(Rf_results)

# See what is the sensitivity of un-adjusted cut-point model
confusionMatrix(data = Rf_results$predclass, 
                reference = Rf_results$y_valid, positive="1")
# Model performed suspiciously well, no need to tune the cutpoint. 

# Investigation.. for funsies
qualities_of_poison <- cbind(mushroom_test_ns$veil.color, mushroom_test_ns$stem.surface, data.frame(Rf_results_final$predclass))
qualities_of_poison[which(qualities_of_poison$Rf_results_final.predclass==0),1]


#####################################
## Model Evaluation: Random Forest ##
#####################################

# Predict on test set
rf_predict_test <- predict(rf_fit_caret$finalModel, data=mushroom_test_ns[,-1], s1=rf_fit_caret$bestTune$mtry,
                           s2=rf_fit_caret$bestTune$splitrule, s3=rf_fit_caret$bestTune$min.node.size)


# RF Test Results 
Rf_results_final <- data.frame(cbind(y_test, rf_predict_test$predictions[,'p']))
Rf_results_final$y_test <- factor(Rf_results_final$y_test)
Rf_results_final$predclass <- factor(ifelse(Rf_results_final$V2 > 0.5, 1, 0))

## Calibration analysis
Cal_Rf <- calibration(y_test ~ V2, data = Rf_results_final, cuts = 10, class="1")
xyplot(Cal_Rf, auto.key = list(columns = 2), main="Figure 5: Calibration Plot for Random Forest")
Cal_Rf$data

# Recalibrate
Cal_Rf_recal <- NaiveBayes(y_test ~ V2, data = Rf_results_final, usekernel = TRUE)
lrCal <- glm(y_test ~ V2, data = Rf_results_final, family = binomial)
Rf_results_final$recalibrated <- predict(Cal_Rf_recal, Rf_results_final[,"V2",drop=F])$posterior[,2] 

# RF Test Results 
Rf_results_final_re <- data.frame(cbind(y_test, Rf_results_final$recalibrated))
Rf_results_final_re$y_test <- factor(Rf_results_final_re$y_test)
Cal_Rf_re_plot <- calibration(y_test ~ V2, data = Rf_results_final_re, cuts = 10, class="1")
xyplot(Cal_Rf_re_plot, auto.key = list(columns = 2), main="Figure 5: Calibration Plot for Random Forest")
Cal_Rf_re_plot$data # When you call data, the counts column shows the number of positive counts. 

# ROC
ROC_Rf <- roc(Rf_results_final$y_test, Rf_results_final$V2)

# AUC
auc(ROC_Rf)
# AUC confidence interval
ci.auc(ROC_Rf)

# Plot ROC
plot(ROC_Rf, legacy.axes = TRUE, main="Figure 6: ROC Curve for Random Forest Model")


# Confusion matrix
confusionMatrix(data = Rf_results_final$predclass, 
                reference = Rf_results_final$y_test, positive="1")

# Calculate brier score
(1/length(Rf_results_final$V2)) * sum((Rf_results_final$V2 - as.numeric(Rf_results_final$y_test))^2)

# Predicted Probabilities faceted by true outcome 
ggnames <- c("1" = "True Outcome: Poisonous", "0" = "True Outcome: Edible")
hist_Rf <- ggplot(data=Rf_results_final, aes(x=V2)) + geom_histogram() + 
  facet_grid(~factor(y_test, levels = c("1", "0")),
             labeller = as_labeller(ggnames)) + xlab("Probability of Poisonous") + ylab("Frequency") +
  ggtitle("Figure 7: Fitted Probabilities By True Outcome for Random Forest")+
  theme(plot.title=element_text(hjust=0.5, face='bold'))
hist_Rf

# Variable Importance
varImp(rf_fit_caret)
Rf_importance <- data.frame(varImp(rf_fit_caret)$importance)

# Rename for plot
rownames(Rf_importance)[1:23] <- c("Cap Shape", "Cap Surface", "Cap Color", "Bruise or Bleed", "Gill Attachment",
                                   "Gill Spacing", "Gill Color", "Stem Root", "Stem Surface", "Stem Color", "Viel Color", 
                                   "Has Ring", "Habitat", "Season", "Cap Diameter N1", "Cap Diameter N2", "Cap Diameter N3",
                                   "Stem Height N1", "Stem Height N2", "Stem Height N3", "Stem Width N1", "Stem Width N2",
                                   "Stem Width N3")
# Variable importance plot
Rf_importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  ggtitle("Variable Importance for Random Forest")+
  xlab("Variable")+
  ylab("Overall Importance")+
  coord_flip()+
  theme(plot.title=element_text(hjust=0.5, face='bold'))

