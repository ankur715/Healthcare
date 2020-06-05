library(e1071)
library(tidyverse)
library(mlbench)
library(Boruta)
library(caret)
library(psych)
library(pROC)
library(MASS)
library(skimr)
library(gridExtra)
library(corrplot)
setwd("~/Desktop")
df <- read.csv(file.choose())
df <- read.csv("SA_heart_dataset.csv")
str(df)
dim(df)
df$chd <-  as.factor(df$chd)
df$row.names <- NULL
library(skimr)
overview <- skim_to_wide(df)
anyNA(df)
####################### EDA #####################
df
all_plots <- lapply(X = 1:ncol(df), function(x) ggplot() + 
                      geom_bar(data = df, aes(x = df[,x]), fill = "skyblue", alpha = 0.5) + theme_linedraw() + 
                      xlab(colnames(df)[x]))
marrangeGrob(grobs = all_plots, nrow = 2, ncol = 2)
ptlist_bi <- list()
for(var in colnames(df)[-11]){
  if (class(df[,var]) %in% c("factor","logical")){
    ptlist_bi[[var]] <- ggplot(data = df) + geom_bar(aes_string(x = var, fill = "chd")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. chd")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5))
  } else if (class(df[,var]) %in% c("numeric","double","integer")){
    ptlist_bi[[var]]  <- ggplot(data = df) + geom_boxplot(aes_string(y = var, x = "chd")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. chd")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5))
  }
}
ptlist_bi
str(df)
not_num <- df[,  which(names(df) %in% c("famhist", "chd"))]  
num <- df[, -which(names(df) %in% c("famhist"))] 
###########
mean <- sapply(num[-9], tapply ,INDEX = num$chd, mean)
median <- sapply(num[-9], tapply ,INDEX = num$chd, median)
sd <- sapply(num[-9], tapply ,INDEX = num$chd, sd)
num_info <- list(Mean = mean, Median = median, SD = sd)
num_info
apply(num[-9], 2, skewness)
apply(num[-9] , 2, kurtosis)
ggplot(num, aes(x = tobacco)) + geom_density()
############## correlation 
w <- cor(num[,-9])
w
corrplot(w, method = "circle")
corrplot(w, method = "pie")
corrplot(w, method = "number")
cor.test(df$obesity, df$adiposity  , method = "pearson", alternative = "two.sided", conf.level = 0.95)
cor.test(df$age, df$typea, method = "pearson", alternative = "two.sided", conf.level = 0.95)
cor.test(df$alcohol, df$adiposity, method = "pearson", alternative = "two.sided", conf.level = 0.95)
######################
require(lsr)
library(gmodels)
CrossTable(df$famhist , df$chd,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df$famhist, df$chd)
##################
############# sbp
require(car)
leveneTest(df$sbp, df$chd)
t.test(df$sbp~df$chd, var.equal = T)
aov1 = aov(sbp ~ chd, data = df)
summary(aov1)
TukeyHSD(aov1)
######### tabacco
leveneTest(df$tobacco, df$chd)
t.test(df$tobacco~df$chd, var.equal = T)
aov1 = aov(tobacco ~ chd, data = df)
summary(aov1)
TukeyHSD(aov1)
###### ldl
leveneTest(df$ldl, df$chd)
t.test(df$ldl~df$chd, var.equal = T)
aov1 = aov(ldl ~ chd, data = df)
summary(aov1)
TukeyHSD(aov1)
##### adiposity
leveneTest(df$adiposity, df$chd)
oneway.test(adiposity~chd, data=df, var.equal=F)
########### type a
leveneTest(df$typea, df$chd)
oneway.test(typea~chd, data=df, var.equal=F)
####### obesity
leveneTest(df$obesity, df$chd)
oneway.test(obesity~chd, data=df, var.equal=F)
######### alcohol
leveneTest(df$alcohol, df$chd)
oneway.test(alcohol~chd, data=df, var.equal=F)
######## age
leveneTest(df$age, df$chd)
t.test(df$age~df$chd, var.equal = T)
aov1 = aov(age ~ chd, data = df)
summary(aov1)
TukeyHSD(aov1)
###########################
##### split data
set.seed(123)
s <- sample(1:nrow(df), nrow(df)*0.8)
train <- df[s,]
test <- df[-s,]
### Accuracy Function for models  #########################################
acc <- function(x){
  w <- sum(diag(x))/ sum(x)*100
  return(w)
}
########### logistic regression Models #################
model <- glm(chd ~ ., data = train, family = "binomial")
summary(model)  
pred1 <- predict(model, train, type = "response")
roccurve <- roc(train$chd ~ pred1)
pred1 <- ifelse(pred1 > 0.5, 1,0)
tab1 <- table(Pred =pred1, Actual = train$chd)
tab1
acc(tab1)
################## Feature selection
## Used stepwise regression to get our best models 
# Model with the lowest AIC 
step_1 <- step(model, scope = list(upper = model), direction = "both")
step_1$formula
step_2 <- step(model, scope = list(upper = model), direction = "backward")
step_2$formula
list(Both = step_1$formula, Backward = step_2$formula)
####### Variable Importance 
set.seed(123)
ctrl <- trainControl(method = "cv", number = 10)
model_caret_glm <- train(chd ~ ., train, trControl = ctrl, method = "glm")
model_caret_glm$results
model_caret_glm$resample
var_glm <- varImp(model_caret_glm)
df_vi <- data.frame(var_glm$importance)
df_vi
plot(var_glm, main = "Variable Importance GLM")
########## Bortua 
set.seed(123)
bor_output <- Boruta(chd ~., data = train, doTrace = 2)
bor_sign <- names(bor_output$finalDecision[bor_output$finalDecision %in% c("Confirmed", "Tentative")])
print(bor_sign)
bor_output$finalDecision
plot(bor_output, cex.axis =.7, las=2, xlab = "", main = "Vriable Importance")
############  Recursive Feature Elimination ####
set.seed(123)
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
res <- rfe(train[,1:9], train$chd, sizes = c(1:13), rfeControl=control)
predictors(res)
res$variables
res$results
plot(res, type= c("g","o"))
################################################ 
## Model 2 #Stepwise
model2 <- glm(chd ~ sbp + tobacco + ldl + famhist + typea + obesity + age , data = train, family = "binomial")
summary(model2)  
pred2 <- predict(model2, train, type = "response")
roccurve2 <- roc(train$chd ~ pred2)
pred2 <- ifelse(pred2 > 0.5, 1,0)
tab2 <- table(Pred = pred2, Actual = train$chd)
acc(tab2)
tab2
auc(roccurve2)
ggroc(roccurve2) + ggtitle("Step Formula ROC")
#######
# Model 3 #Variable Importance 
model3 <- glm(chd ~ famhist + ldl + typea + obesity + age + tobacco + sbp ,data =  train, family = "binomial")
summary(model3)
pred3 <- predict(model3, train, type = "response")
roccurve3 <- roc(train$chd~ pred3)
pred3 <- ifelse(pred3 > 0.5, 1,0)
tab3 <- table(Pred = pred3, Actual = train$chd)
acc(tab3)
tab3
auc(roccurve3)
ggroc(roccurve3) + ggtitle("Vriable Importance ROC")
###########################
# Model 4 trial and error
model4 <- glm(chd~ .-alcohol , data = train, family = "binomial")
summary(model4)
pred4 <- predict(model4, train, type = "response")
roccurve4 <- roc(train$chd ~ pred4)
pred4 <- ifelse(pred4 > 0.5, 1,0)
tab4 <- table(Pred = pred4, Actual = train$chd)
acc(tab4)
tab4
auc(roccurve4)
ggroc(roccurve4) + ggtitle("Best Model from Project")
######################
#Model 5 #P_value
model5 <- glm(chd ~ tobacco + ldl + famhist + typea + age , data = train, family = "binomial")
summary(model5)
pred5 <- predict(model5, train, type = "response")
roccurve5 <- roc(train$chd ~ pred5)
pred5 <- ifelse(pred5 > 0.5, 1,0)
tab5 <- table(Pred = pred5, Actual = train$chd)
acc(tab5)
auc(roccurve5)
ggroc(roccurve5) + ggtitle("P - Value")
#######################
###### RFE
model6 <- glm(chd ~ age +famhist, data = train, family = "binomial")
summary(model6)
pred6 <- predict(model6, train, type = "response")
roccurve6 <- roc(train$chd ~ pred6)
pred6 <- ifelse(pred6 > 0.5, 1,0)
tab6 <- table(Pred = pred6, Actual = train$chd)
acc(tab5)
tab5
auc(roccurve6)
ggroc(roccurve6) + ggtitle("Recursive Feature Elimination (RFE)")
#############################
# Model 7 Bortua 
model7 <- glm(chd ~ sbp + tobacco + ldl +  adiposity + famhist + typea + age, train,  family =  "binomial")
summary(model7)
pred7 <- predict(model7, train, type = "response")
roccurve7 <- roc(train$chd ~ pred7)
pred7 <- ifelse(pred7 > 0.5, 1,0)
tab7 <- table(Pred = pred7, Actual = train$chd)
acc(tab7)
auc(roccurve7)
ggroc(roccurve7) + ggtitle("Bortua Importance ROC")
############
info_list <- list(All = list(Accuracy = acc(tab1), Null_Deviance = model$null.deviance, Residual_Deviance = model$deviance, AIC = model$aic, AUC = auc(roccurve)),
                  Step = list(Accuracy = acc(tab2), Null_Deviance = model2$null.deviance, Residual_Deviance = model2$deviance, AIC = model2$aic, AUC = auc(roccurve2)),
                  variable_importance = list(Accuracy = acc(tab3), Null_Deviance = model3$null.deviance, Residual_Deviance = model3$deviance, AIC = model3$aic, AUC = auc(roccurve3)),
                  trial_and_error = list(Accuracy = acc(tab4), Null_Deviance = model4$null.deviance, Residual_Deviance = model4$deviance, AIC = model4$aic, AUC = auc(roccurve4)),
                  P_value = list(Accuracy = acc(tab5), Null_Deviance = model5$null.deviance, Residual_Deviance = model5$deviance, AIC = model5$aic, AUC = auc(roccurve5)),
                  RFE = list(Accuracy = acc(tab6), Null_Deviance = model6$null.deviance, Residual_Deviance = model6$deviance, AIC = model6$aic, AUC = auc(roccurve6)),
                  Bortua = list(Accuracy = acc(tab7), Null_Deviance = model7$null.deviance, Residual_Deviance = model7$deviance, AIC = model7$aic, AUC = auc(roccurve7)))
create_table <- function(x){
  info2 <- do.call(rbind, x)
  info2 <- data.frame(info2)
  info1 <- rownames_to_column(info2, "Models")
  return(info1)
}
round_df <- function(x, y){
  w <- round(as.numeric(x),y)
  return(w)
}
info <- create_table(info_list)
info
info$Accuracy <- round_df(info$Accuracy, 2)
info$Null_Deviance <- round_df(info$Null_Deviance, 2)
info$Residual_Deviance <- round_df(info$Residual_Deviance, 2)
info$AIC <- round_df(info$AIC, 2)
info$AUC <- round_df(info$AUC, 3)
info
#############
ggroc(list(All = roccurve, Step = roccurve2,variable_importance = roccurve3,trial_and_error = roccurve4, P_value = roccurve5, RFE = roccurve6,
           Bortua = roccurve7)) + ggtitle("ROC Curve") + theme(plot.title = element_text(hjust = 0.5))
#### tables of models. 
# Looking for models with smallest false negative rate, that means predicited no but Actual is yes 
tables <- list(All = tab1,
               Step = tab2,
               variable_importance = tab3,
               trial_and_error = tab4,
               P_value = tab5,
               RFE = tab6,
               Bortua = tab7)
tables
#######################################
############
##################################
#### K-fold Cross Validation 
set.seed(123)
train_control <- trainControl(method = "cv", number = 5)
model_cv <- train(chd~., data = train, trControl = train_control, method = "glm")
model_cv$resample
print(model_cv)
cm_all <- confusionMatrix(model_cv, reference = train$AHD, positive = "1")
cm_all$table
########## Model 2 # Stepwise
set.seed(123)
model2_cv <- train(chd ~ sbp + tobacco + ldl + famhist + typea + obesity + age , 
                   data = train, trControl = train_control, method = "glm")
model2_cv$resample
print(model2_cv)
cm_step <- confusionMatrix(model2_cv, reference = train$AHD, positive = "1")
cm_step$table
######### Model 3 # Variable Importance 
model3_cv <- train(chd ~ famhist + ldl + typea + obesity + age + tobacco + sbp,
                   data = train, trControl = train_control, method = "glm")
model3_cv$resample
print(model3_cv)
cm_vi <- confusionMatrix(model3_cv, reference = train$AHD, positive = "1")
cm_vi$table
######## Model 4 # trial and error
set.seed(123)
model4_cv <- train(chd ~ . -alcohol,
                   data = train, trControl = train_control, method = "glm")
model4_cv$resample
print(model4_cv)
cm_te <- confusionMatrix(model4_cv, reference = train$AHD, positive = "1")
cm_te$table
####### Model 5 #P_value  
set.seed(123)
model5_cv <- train(chd ~ tobacco + ldl + famhist + typea + age ,
                   data = train, trControl = train_control, method = "glm")
model5_cv$resample
print(model5_cv)
cm_p <- confusionMatrix(model5_cv, reference = train$AHD, positive = "1")
cm_p$table
######## Model 6 RFE
set.seed(123)
model6_cv <- train(chd ~ age +famhist,data = train, trControl = train_control, method = "glm")
model6_cv$resample
print(model6_cv)
cm_rfe <- confusionMatrix(model6_cv, reference = train$AHD, positive = "1")
cm_rfe$table
###### Model 7 Bortua 
set.seed(123)
model7_cv <- train(chd ~ sbp + tobacco + ldl +  adiposity + famhist + typea + age,
                   data = train, trControl = train_control, method = "glm")
model7_cv$resample
print(model7_cv)
cm_b <- confusionMatrix(model7_cv, reference = train$AHD, positive = "1")
cm_b$table
#######
####################
cv2= list(All = model_cv$results,
          Step = model2_cv$results, 
          variable_importance = model3_cv$results,
          trial_and_error = model4_cv$results,
          P_value = model5_cv$results,
          RFE = model6_cv$results,
          Bortua = model7_cv$results)
cv2
cv_table_2 <- create_table(cv2)
cv_table_2$parameter <- NULL
cv_table_2
##########
######################
cm_info <- function(x){
  Sen = sensitivity(x, positive = "1", negative = "0")
  Spef = specificity(x, positive = "1", negative = "0")
  ppv = posPredValue(x, positive = "1", negative = "0")
  npv =  negPredValue(x, positive = "1", negative = "0")
  return(rbind(list( Sensitivity = Sen,Specificity = Spef, Pos_Pred_Value = ppv, Neg_Pred_Value = npv )))
}
cm_table= list(All = cm_info(cm_all$table),
               Step = cm_info(cm_step$table), 
               variable_importance = cm_info(cm_vi$table),
               trial_and_error = cm_info(cm_te$table),
               P_value = cm_info(cm_p$table),
               RFE = cm_info(cm_rfe$table),
               Bortua = cm_info(cm_p$table))
cm_table 
#####
cv2_table= list(All = cm_all$table,
                Step = cm_step$table, 
                variable_importance = cm_vi$table,
                trial_and_error = cm_te$table,
                P_value = cm_p$table,
                RFE = cm_rfe$table,
                Bortua = cm_p$table)
cv2_table
#######################
########################################### SVM ##################################
set.seed(123)
svm1 <-  svm(chd ~ ., data = train, kernel = "linear")
summary(svm1)
pred_svm <- predict(svm1, train, type = "class")
tab <- table(Predicited = pred_svm, Actual = train$chd)
tab
acc(tab)
############# Variable importance svm ###################
colnames(df)
str(train)
train$famhist <-  as.factor(train$famhist)
ctrl <- trainControl(method = "cv", number = 10)
model_svm <- train(chd ~ age + ldl + sbp + tobacco + adiposity + typea + obesity + alcohol + age,
                   data = train, trainControl = ctrl, method = "svmLinear")
var_svm <- varImp(model_svm)
var_svm
plot(var_svm, main = "Variable Importance SVM")
###### SVM2 Step wise ########################
svm2 <- svm(chd ~ sbp + tobacco + ldl + famhist + typea + obesity + age, train, kernel = "linear")
summary(svm2)
pred_svm2 <- predict(svm2, train, type = "class")
plot(pred_svm2, main = "Step Formula ROC")
tab2 <- table(Predicited = pred_svm2, Actual = train$chd)
tab2
acc(tab2)
######################### Svm 3  Variable Importance ################
svm3 <- svm(chd ~ age + tobacco + ldl + adiposity + sbp + typea, data = train, kernel = "linear")
summary(svm3)
pred3 <- predict(svm3, train, type = "class")
plot(pred3, main = "Varibale Importance ")
tab3 <- table(Predicited = pred3, Actual = train$chd)
tab3
acc(tab3)
############## SVM 4  trial and error ################
svm4 <- svm(chd ~. -alcohol, data = train, kernel = "linear")
summary(svm4)
pred4 <- predict(svm4, train, type = "class")
plot(pred4, main = "Best from Project")
tab4 <- table(Predicited = pred4, Actual = train$chd)
tab4
acc(tab4)
##################### SVM 5  P-value ##############
svm5 <- svm(chd ~ tobacco + ldl + famhist + typea + age , data = train, kernel = "linear")
summary(svm5)
pred5 <- predict(svm5, train, type = "class")
plot(pred5, main = "P-value")
tab5 <- table(Predicited = pred5, Actual = train$chd)
tab5
acc(tab5)
##################### SVM 6 RFE ##################
svm6 <- svm(chd ~ age + famhist, data = train, family = "binomial")
summary(svm6)
pred6 <- predict(svm6, train, type = "class")
plot(pred6 , main = "Recursive Feature Elimination (RFE)")
tab6 <- table(Pred = pred6, Actual = train$chd)
acc(tab6)
##################### SVM 7 Bortua
svm7 <- svm(chd~sbp + tobacco + ldl +  adiposity + famhist + typea + age , data = train, kernel = "linear")
summary(svm7)
pred7 <- predict(svm7, train, type = "class")
plot(pred7, main = "Bortua")
tab7 <- table(Predicited = pred7, Actual = train$chd)
tab7
acc(tab7)
###################################################
svm_list <- list(All = list(svm1$tot.nSV,acc(tab)),
                 Step = list(svm2$tot.nSV,acc(tab2)),
                 variable_importance = list(svm3$tot.nSV,acc(tab3)),
                 trial_and_error = list(svm4$tot.nSV ,acc(tab4)),
                 P_value = list(svm5$tot.nSV ,acc(tab5)),
                 RFE = list(svm6$tot.nSV ,acc(tab6)),
                 Bortua = list(svm7$tot.nSV ,acc(tab7)))

svm_info <- create_table(svm_list)
names(svm_info)[2] <- "Total Support Vectors"
names(svm_info)[3] <- "Accuracy"
svm_info
svm_info$Accuracy <- round(as.numeric(svm_info$Accuracy), 2)
svm_info
###############
tables_svm <- list(All = tab,
                   Step = tab2,
                   variable_importance = tab3,
                   trial_and_error = tab4,
                   P_value = tab5,
                   RFE = tab6,
                   Bortua = tab7)
tables_svm
###############
#### K-fold Cross Validation 
set.seed(123)
train_control <- trainControl(method = "cv", number = 5)
svm_cv <- train(chd~., data = train, trControl = train_control, method = "svmLinear")
svm_cv$resample
print(svm_cv)
svm_cm <- confusionMatrix(svm_cv, reference = train$AHD, positive = "1")
svm_cm$table
########## SVM 2 # Stepwise 
set.seed(123)
svm2_cv <- train(chd ~ sbp + tobacco + ldl + famhist + typea + obesity + age, 
                 data = train, trControl = train_control, method = "svmLinear")
svm2_cv$resample
print(svm2_cv)
svm2_cm <- confusionMatrix(svm2_cv, reference = train$AHD, positive = "1")
svm2_cm$table
###### SVM 3 # Variable Importance 
set.seed(123)
svm3_cv <- train(chd ~ age + tobacco + ldl + adiposity + sbp + typea, 
                 data = train, trControl = train_control, method = "svmLinear")
svm3_cv$resample
print(svm3_cv)
svm3_cm <- confusionMatrix(svm3_cv, reference = train$AHD, positive = "1")
svm3_cm$table
########## SVM 4 #trial and error 
set.seed(123)
svm4_cv <- train(chd ~. -alcohol, 
                 data = train, trControl = train_control, method = "svmLinear")
svm4_cv$resample
print(svm4_cv)
svm4_cm <- confusionMatrix(svm4_cv, reference = train$AHD, positive = "1")
svm4_cm$table
###### SVM 5  # P_values
set.seed(123)
svm5_cv <- train(chd ~ tobacco + ldl + famhist + typea + age, 
                 data = train, trControl = train_control, method = "svmLinear")
svm5_cv$resample
print(svm5_cv)
svm5_cm <- confusionMatrix(svm5_cv, reference = train$AHD, positive = "1")
svm5_cm$table
######  SVM 6 #RFE
set.seed(123)
svm6_cv <- train(chd ~ age + famhist, data = train, 
                 trControl = train_control, method = "svmLinear")
svm6_cv$resample
print(svm6_cv)
svm6_cm <- confusionMatrix(svm6_cv, reference = train$AHD, positive = "1")
svm6_cm$table
###### SVM 7 #Bortua
set.seed(123)
svm7_cv <- train(chd~sbp + tobacco + ldl +  adiposity + famhist + typea + age, 
                 data = train, trControl = train_control, method = "svmLinear")
svm7_cv$resample
print(svm7_cv)
svm7_cm <- confusionMatrix(svm7_cv, reference = train$AHD, positive = "1")
svm7_cm$table
####################
svm_cv_2= list(All = svm_cv$results,
               Step = svm2_cv$results, 
               variable_importance = svm3_cv$results,
               trial_and_error = svm4_cv$results,
               P_value = svm5_cv$results,
               RFE = svm6_cv$results,
               Bortua = svm7_cv$results)
svm_cv_2
svm_cv_table2 <- create_table(svm_cv_2)
svm_cv_table2$C <- NULL
svm_cv_table2
###########
svm_table2= list(All = cm_info(svm_cm$table),
                 Step = cm_info(svm2_cm$table), 
                 variable_importance = cm_info(svm3_cm$table),
                 trial_and_error = cm_info(svm4_cm$table),
                 P_value = cm_info(svm5_cm$table),
                 RFE = cm_info(svm6_cm$table),
                 Bortua = cm_info(svm7_cm$table))
svm_table2 

#####
svm2_table= list(All = svm_cm$table,
                 Step = svm2_cm$table, 
                 variable_importance = svm3_cm$table,
                 trial_and_error = svm4_cm$table,
                 P_value = svm5_cm$table,
                 RFE = svm6_cm$table,
                 Bortua = svm7_cm$table)
svm2_table
##################
library(randomForest)
############ RF 1 ###########################
set.seed(123)
rf <-  randomForest(chd ~., train, mtry = 2)
rf
importance(rf)
acc(rf$confusion)
############## RF 2 Stepwise Regression ###########################
set.seed(123)
rf2 <- randomForest(chd ~ sbp + tobacco + ldl + famhist + typea + obesity + age, train, mtry = 2)
rf2
acc(rf2$confusion)
############# RF 3  Variable Importance ##################
set.seed(123)
rf3 <- randomForest(chd ~ sbp + tobacco + ldl + adiposity + typea + obesity + age, train, mtry = 2)
rf3
acc(rf3$confusion)
########################## RF 4 trial_and_error ##############
set.seed(123)
rf4 <- randomForest(chd ~ .-alcohol, train, mtry = 2)
rf4
acc(rf4$confusion)
##################### RF 5  P- value ##############
set.seed(123)
rf5 <- randomForest(chd ~ tobacco + ldl + famhist + typea + age, data = train, mtry = 2)
rf5
acc(rf5$confusion)
##################### SVM 6 RFE #########
set.seed(123)
rf6 <- randomForest(chd ~ age + famhist, data = train, mtry = 2)
rf6
acc(rf6$confusion)
##################### RF 7  Bortua ##################
set.seed(123)
rf7 <- randomForest(chd~sbp + tobacco + ldl +  adiposity + famhist + typea + age, train, mtry = 2)
rf7
acc(rf7$confusion)
###################################################
rf_list <- list(All = acc(rf$confusion),
                Step = acc(rf2$confusion),
                Variable_Importance  = acc(rf3$confusion),
                trial_and_error = acc(rf4$confusion),
                P_value =  acc(rf5$confusion),
                RFE = acc(rf6$confusion),
                Boruta = acc(rf7$confusion))


rf_info <- create_table(rf_list)
names(rf_info)[2] <-  "Accuracy"
rf_info
###############
rf_table <- list(All = rf$confusion,
                 Step = rf2$confusion,
                 Variable_Importance  = rf3$confusion,
                 trial_and_error = rf4$confusion,
                 P_value =  rf5$confusion,
                 RFE = rf6$confusion,
                 Boruta = rf7$confusion)
rf_table
#### K-fold Cross Validation #################
set.seed(123)
train_control <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry=2)
rf_cv <- train(chd~., data = train,method = "rf", metric=metric, tuneGrid=tunegrid, trControl=train_control)
rf_cv$resample
print(rf_cv)
rf_cm <- confusionMatrix(rf_cv, reference = train$AHD, positive = "1")
rf_cm$table
########## RF 2 Stepwise
set.seed(123)
rf2_cv <- train(chd ~ sbp + tobacco + ldl + famhist + typea + obesity + age, 
                data = train, trControl = train_control, method = "rf", metric=metric, tuneGrid=tunegrid)
rf2_cv$resample
print(rf2_cv)
rf2_cm <- confusionMatrix(rf2_cv, reference = train$AHD, positive = "1")
rf2_cm$table
############ RF 3 Variable Importance 
set.seed(123)
rf3_cv <- train(chd ~ sbp + tobacco + ldl + adiposity + typea + obesity + age, 
                data = train, trControl = train_control, method = "rf",metric=metric, tuneGrid=tunegrid)
rf3_cv$resample
print(rf3_cv)
rf3_cm <- confusionMatrix(rf3_cv, reference = train$AHD, positive = "1")
rf3_cm$table
########### RF 4 trial_and_error
set.seed(123)
rf4_cv <- train(chd ~ .-alcohol, 
                data = train, trControl = train_control, method = "rf",metric=metric, tuneGrid=tunegrid)
rf4_cv$resample
print(rf4_cv)
rf4_cm <- confusionMatrix(rf4_cv, reference = train$AHD, positive = "1")
rf4_cm$table
###### RF 5  P-value
set.seed(123)
rf5_cv <- train(chd ~ tobacco + ldl + famhist + typea + age , 
                data = train, trControl = train_control, method = "rf", metric=metric, tuneGrid=tunegrid)
rf5_cv$resample
print(rf5_cv)
rf5_cm <- confusionMatrix(rf5_cv, reference = train$AHD, positive = "1")
rf5_cm$table
######  RF 6 RFE
set.seed(123)
rf6_cv <- train(chd ~ age + famhist, data = train, 
                trControl = train_control, method = "rf", metric=metric, tuneGrid=tunegrid)
rf6_cv$resample
print(rf6_cv)
rf6_cm <- confusionMatrix(rf6_cv, reference = train$AHD, positive = "1")
rf6_cm$table
######## RF 7 Bortua 
set.seed(123)
rf7_cv <- train(chd~sbp + tobacco + ldl +  adiposity + famhist + typea + age, 
                data = train, trControl = train_control, method = "rf", metric=metric, tuneGrid=tunegrid)
rf7_cv$resample
print(rf7_cv)
rf7_cm <- confusionMatrix(rf7_cv, reference = train$AHD, positive = "1")
rf7_cm$table
###########################
rf_cv_2_mt= list(All = rf_cv$results,
                 Step = rf2_cv$results,
                 Variable_Importance = rf3_cv$results,
                 trial_and_error = rf4_cv$results,
                 P_value = rf5_cv$results,
                 RFE = rf6_cv$results,
                 Bortua = rf7_cv$results)
rf_cv_2_mt
rf_cv_table_mt <- create_table(rf_cv_2_mt)
rf_cv_table_mt
############
rf_table2= list(All = cm_info(rf_cm$table),
                Step = cm_info(rf2_cm$table), 
                variable_importance = cm_info(rf3_cm$table),
                trial_and_error = cm_info(rf4_cm$table),
                P_value = cm_info(rf5_cm$table),
                RFE = cm_info(rf6_cm$table),
                Bortua = cm_info(rf7_cm$table))
rf_table2 
#####
rf2_table= list(All = rf_cm$table,
                Step = rf2_cm$table, 
                variable_importance = rf3_cm$table,
                trial_and_error = rf4_cm$table,
                P_value = rf5_cm$table,
                RFE = rf6_cm$table,
                Bortua = rf7_cm$table)
rf2_table
########################
#######################################################################
###################### Test Data ##########################
test_info <- function(x){
  w <- predict(x, test)
  cf <- confusionMatrix(data =  w, reference = test$chd,positive = "1" )
  return(cf)
}

######## GLM Test
########### All
set.seed(123)
glm_all <- test_info(model_cv)
glm_all
##################### Stepwise
set.seed(123)
glm_step <- test_info(model2_cv)
glm_step

##################### Variable Importance 
set.seed(123)
glm_vi <- test_info(model3_cv)
glm_vi
##################### trail and error 
set.seed(123)
glm_te <- test_info(model4_cv)
glm_te$table
##################### P value
set.seed(123)
glm_p <- test_info(model5_cv)
glm_p
##################### RFE
set.seed(123)
glm_rfe <- test_info(model6_cv)
glm_rfe
##################### Bortua
set.seed(123)
glm_b <- test_info(model7_cv)
glm_b
############# 
glm_list <- list(All = glm_all, Step = glm_step,variable_importance = glm_vi, trial_and_error = glm_te,
                 P_value  = glm_p, RFE = glm_rfe, Bortua = glm_b)
glm_tables <- lapply(1:length(glm_list), function(i){
  Model <- names(glm_list[i])
  Accuracy <- round(glm_list[[i]]$overall[1],2)
  Sensitivity = round(glm_list[[i]]$byClass[1],2)
  Specificity = round(glm_list[[i]]$byClass[2],2)
  Pos_Pred_Value = round(glm_list[[i]]$byClass[3],2)
  Neg_Pred_Value =  round(glm_list[[i]]$byClass[4],2)
  c(Model, Accuracy, Sensitivity,Specificity,Pos_Pred_Value,   Neg_Pred_Value )
})
glm <- as.data.frame(do.call(rbind, glm_tables))
names(glm)[1] <- "Model"
glm
###############
#### tables of models. 
# Looking for models with smallest false negative rate, that means predicited no but Actual is yes 
tables_test <- list(All = glm_all$table,
                    Step = glm_step$table,
                    variable_importance = glm_vi$table,
                    trial_and_error = glm_te$table,
                    P_value = glm_p$table,
                    RFE = glm_rfe$table,
                    Bortua = glm_b$table)
tables_test
#######################
####### SVM TEST
set.seed(123)
svm_all <- test_info(svm_cv)
svm_all
##################### Stepwise
set.seed(123)
svm_step <- test_info(svm2_cv)
svm_step
##################### Variable Importance
set.seed(123)
svm_vi <- test_info(svm3_cv)
svm_vi
##################### trail and error 
set.seed(123)
svm_te <- test_info(svm4_cv)
svm_te
##################### P value
set.seed(123)
svm_p <- test_info(svm5_cv)
svm_p
##################### RFE
set.seed(123)
svm_rfe <- test_info(svm6_cv)
svm_rfe
##################### Bortua
set.seed(123)
svm_b <- test_info(svm7_cv)
svm_b
############# 
svm_list <- list(All = svm_all, Step = svm_step,variable_importance = svm_vi, trial_and_error = svm_te,
                 P_value  = svm_p, RFE = svm_rfe, Bortua = svm_b)
svm_tables <- lapply(1:length(svm_list), function(i){
  Model <- names(svm_list[i])
  Accuracy <- round(svm_list[[i]]$overall[1],3)
  Sensitivity = round(svm_list[[i]]$byClass[1],3)
  Specificity = round(svm_list[[i]]$byClass[2],3)
  Pos_Pred_Value = round(svm_list[[i]]$byClass[3],3)
  Neg_Pred_Value =  round(svm_list[[i]]$byClass[4],3)
  c(Model, Accuracy, Sensitivity,Specificity,Pos_Pred_Value,   Neg_Pred_Value )
})
svm <- as.data.frame(do.call(rbind, svm_tables))
names(svm)[1] <- "Model"
svm
###############
#### tables of models. 
# Looking for models with smallest false negative rate, that means predicited no but Actual is yes 
tables_test2 <- list(All = svm_all$table,
                     Step = svm_step$table,
                     variable_importance = svm_vi$table,
                     trial_and_error = svm_te$table,
                     P_value = svm_p$table,
                     RFE = svm_rfe$table,
                     Bortua = svm_b$table)
tables_test2
#######################
####### RF TEST
set.seed(123)
rf_all <- test_info(rf_cv)
rf_all
##################### Stepwise
set.seed(123)
rf_step <- test_info(rf2_cv)
rf_step
##################### Variable Importance 
set.seed(123)
rf_vi <- test_info(rf3_cv)
rf_vi
##################### trail and error 
set.seed(123)
rf_te <- test_info(rf4_cv)
rf_te
##################### P value
set.seed(123)
rf_p <- test_info(rf5_cv)
rf_p
##################### RFE
set.seed(123)
rf_rfe <- test_info(rf6_cv)
rf_rfe
##################### Bortua
set.seed(123)
rf_b <- test_info(rf7_cv)
rf_b
############# 
rf_list <- list(All = rf_all, Step = rf_step,variable_importance = rf_vi, trial_and_error = rf_te,
                P_value  = rf_p, RFE = rf_rfe, Bortua = rf_b)
rf_tables <- lapply(1:length(rf_list), function(i){
  Model <- names(rf_list[i])
  Accuracy <- round(rf_list[[i]]$overall[1],3)
  Sensitivity = round(rf_list[[i]]$byClass[1],3)
  Specificity = round(rf_list[[i]]$byClass[2],3)
  Pos_Pred_Value = round(rf_list[[i]]$byClass[3],3)
  Neg_Pred_Value =  round(rf_list[[i]]$byClass[4],3)
  c(Model, Accuracy, Sensitivity,Specificity,Pos_Pred_Value,   Neg_Pred_Value )
})
rf <- as.data.frame(do.call(rbind, rf_tables))
names(rf)[1] <- "Model"
rf
###############
#### tables of models. 
# Looking for models with smallest false negative rate, that means predicited no but Actual is yes 
tables_test3 <- list(All = rf_all$table,
                     Step = rf_step$table,
                     variable_importance = rf_vi$table,
                     trial_and_error = rf_te$table,
                     P_value = rf_p$table,
                     RFE = rf_rfe$table,
                     Bortua = rf_b$table)
tables_test3
