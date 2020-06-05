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
df <- read.csv(file.choose())
setwd("~/Desktop")
df <- read.csv("New_fram_data.csv")
anyNA(df)
str(df)
dim(df)
summary(df)
overview <- skim_to_wide(df)
####################### EDA #####################
all_plots <- lapply(X = 1:ncol(df), function(x) ggplot() + 
                      geom_bar(data = df, aes(x = df[,x]), fill = "skyblue", alpha = 0.5) + theme_linedraw() + 
                      xlab(colnames(df)[x]))
marrangeGrob(grobs = all_plots, nrow = 2, ncol = 2)
#
ptlist_bi <- list()
for(var in colnames(df)[-14]){
  if (class(df[,var]) %in% c("factor","logical")){
    ptlist_bi[[var]] <- ggplot(data = df) + geom_bar(aes_string(x = var, fill = "TenYearCHD"), position = "fill") + 
      theme_linedraw() + ggtitle(paste0(var, " vs. TenYearCHD")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5))
  } else if (class(df[,var]) %in% c("numeric","double","integer")){
    ptlist_bi[[var]]  <- ggplot(data = df) + geom_boxplot(aes_string(y = var, x = "TenYearCHD")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. TenYearCHD")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5))
  }
}
ptlist_bi
label_val <- function(x){
  w <- table( x , TenYearCHD= df$TenYearCHD)
  return(w)
}
label_val_percent <- function(x){
  w <- round(prop.table(table(x , TenYearCHD= df$TenYearCHD)),3)
  return(w)
}
############ categorical variable
not_num <- df[, - which(names(df) %in% c("age","cigsPerDay","totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose"))]  
num <- df[, which(names(df) %in% c("age","cigsPerDay","totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose"))] 
apply(not_num, 2, label_val)
apply(not_num, 2, label_val_percent)
############## continuous variable
num <- df1[, c("age","cigsPerDay","totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose")]
mean <- sapply(df[, c("age","cigsPerDay","totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose")], tapply ,INDEX = df$TenYearCHD, mean)
median <- sapply(df[, c("age","cigsPerDay","totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose")], tapply ,INDEX = df$TenYearCHD, median)
sd <- sapply(df[, c("age","cigsPerDay","totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose")], tapply ,INDEX = df$TenYearCHD, sd)
num_info <- list(Mean = mean, Median = median, SD = sd)
num_info
apply(num, 2, skewness)
apply(num , 2, kurtosis)
ggplot(num, aes(x = glucose)) + geom_density()
############## correlation 
w <- cor(num[,-9])
w
corrplot(w, method = "circle")
corrplot(w, method = "pie")
corrplot(w, method = "number")
cor.test(df$age, df$heartRate, method = "pearson", alternative = "two.sided", conf.level = 0.95)
cor.test(df$diaBP, df$sysBP, method = "pearson", alternative = "two.sided", conf.level = 0.95)
library(lsr)
require(lsr)
CrossTable(not_num$male , not_num$TenYearCHD,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(not_num$male , not_num$TenYearCHD)
CrossTable(not_num$education , not_num$TenYearCHD,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(not_num$education, not_num$TenYearCHD)
CrossTable(not_num$diabetes , not_num$TenYearCHD,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(not_num$diabetes, not_num$TenYearCHD)
CrossTable(not_num$currentSmoker , not_num$TenYearCHD,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(not_num$currentSmoker, not_num$TenYearCHD)
############
require(car)
leveneTest(df$diaBP, df$TenYearCHD)
t.test(df$diaBP~df$TenYearCHD, var.equal = T)
leveneTest(df$age, df$TenYearCHD)
t.test(df$age~df$TenYearCHD, var.equal = F)
aov1 = aov(diaBP ~ TenYearCHD, data = df)
summary(aov1)
TukeyHSD(aov1)
oneway.test(age ~ TenYearCHD, data = df, var.equal = F)
leveneTest(df$heartRate, df$TenYearCHD)
oneway.test(heartRate ~ TenYearCHD , data = df, var.equal = F)
#########################
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
model <- glm(TenYearCHD ~ ., data = train, family = "binomial")
summary(model)  
pred1 <- predict(model, train, type = "response")
roccurve <- roc(train$TenYearCHD ~ pred1)
pred1 <- ifelse(pred1 > 0.5, 1,0)
tab1 <- table(Pred =pred1, Actual = train$TenYearCHD)
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
model_caret_glm <- train(TenYearCHD ~ ., train, trControl = ctrl, method = "glm")
model_caret_glm$results
model_caret_glm$resample
var_glm <- varImp(model_caret_glm)
df_vi <- data.frame(var_glm$importance)
df_vi
plot(var_glm, main = "Variable Importance GLM")
########## Bortua 
set.seed(123)
bor_output <- Boruta(TenYearCHD ~., data = train, doTrace = 2)
bor_sign <- names(bor_output$finalDecision[bor_output$finalDecision %in% c("Confirmed", "Tentative")])
print(bor_sign)
bor_output$finalDecision
plot(bor_output, cex.axis =.7, las=2, xlab = "", main = "Vriable Importance")
############  Recursive Feature Elimination ####
set.seed(123)
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
res <- rfe(train[,1:15], train$TenYearCHD, sizes = c(1:13), rfeControl=control)
predictors(res)
res$variables
res$results
plot(res, type= c("g","o"))
################################################ 
colnames(df)
## Model 2 #Stepwise
model2 <- glm(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + sysBP + glucose  , data = train, family = "binomial")
summary(model2)  
pred2 <- predict(model2, train, type = "response")
roccurve2 <- roc(train$TenYearCHD ~ pred2)
pred2 <- ifelse(pred2 > 0.5, 1,0)
tab2 <- table(Pred = pred2, Actual = train$TenYearCHD)
acc(tab2)
tab2
auc(roccurve2)
ggroc(roccurve2) + ggtitle("Step Formula ROC")
#######
# Model 3 #Variable Importance 
model3 <- glm(TenYearCHD ~ male + age + cigsPerDay + sysBP + glucose + prevalentStroke + prevalentHyp ,data =  train, family = "binomial")
summary(model3)
pred3 <- predict(model3, train, type = "response")
roccurve3 <- roc(train$TenYearCHD ~ pred3)
pred3 <- ifelse(pred3 > 0.5, 1,0)
tab3 <- table(Pred = pred3, Actual = train$TenYearCHD)
acc(tab3)
tab3
auc(roccurve3)
ggroc(roccurve3) + ggtitle("Vriable Importance ROC")
###########################
# Model 4 trial and error
model4 <- glm(TenYearCHD~ glucose + BMI + diabetes + sysBP + totChol + BPMeds + cigsPerDay + male + age , data = train, family = "binomial")
summary(model4)
pred4 <- predict(model4, train, type = "response")
roccurve4 <- roc(train$TenYearCHD ~ pred4)
pred4 <- ifelse(pred4 > 0.5, 1,0)
tab4 <- table(Pred = pred4, Actual = train$TenYearCHD)
acc(tab4)
tab4
auc(roccurve4)
ggroc(roccurve4) + ggtitle("Best Model from Project")
######################
#Model 5 #P_value
model5 <- glm(TenYearCHD ~ male +age + cigsPerDay +  sysBP + glucose , data = train, family = "binomial")
summary(model5)
pred5 <- predict(model5, train, type = "response")
roccurve5 <- roc(train$TenYearCHD ~ pred5)
pred5 <- ifelse(pred5 > 0.5, 1,0)
tab5 <- table(Pred = pred5, Actual = train$TenYearCHD)
acc(tab5)
auc(roccurve5)
ggroc(roccurve5) + ggtitle("P - Value")
#######################
###### RFE
model6 <- glm(TenYearCHD ~ age + sysBP + diaBP + prevalentHyp + male + glucose + BMI + totChol + currentSmoker + cigsPerDay  + diabetes
              , data = train, family = "binomial")
summary(model6)
pred6 <- predict(model6, train, type = "response")
roccurve6 <- roc(train$TenYearCHD ~ pred6)
pred6 <- ifelse(pred6 > 0.5, 1,0)
tab6 <- table(Pred = pred6, Actual = train$TenYearCHD)
acc(tab5)
tab5
auc(roccurve6)
ggroc(roccurve6) + ggtitle("Recursive Feature Elimination (RFE)")
#############################
# Model 7 Bortua 
model7 <- glm(TenYearCHD ~ male + age + cigsPerDay + BPMeds +  prevalentHyp  + diabetes + totChol + sysBP + diaBP + BMI +glucose, train,  family =  "binomial")
summary(model7)
pred7 <- predict(model7, train, type = "response")
roccurve7 <- roc(train$TenYearCHD ~ pred7)
pred7 <- ifelse(pred7 > 0.5, 1,0)
tab7 <- table(Pred = pred7, Actual = train$TenYearCHD)
acc(tab7)
auc(roccurve7)
ggroc(roccurve7) + ggtitle("Bortua Importance ROC")
###################
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
train_control <- trainControl(method = "cv", number = 10)
model_cv <- train(TenYearCHD~., data = train, trControl = train_control, method = "glm")
model_cv$resample
print(model_cv)
cm_all <- confusionMatrix(model_cv, reference = train$AHD, positive = "Yes")
cm_all$table
########## Model 2 # Stepwise
set.seed(123)
model2_cv <- train(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + sysBP + glucose , 
                   data = train, trControl = train_control, method = "glm")
model2_cv$resample
print(model2_cv)
cm_step <- confusionMatrix(model2_cv, reference = train$AHD, positive = "Yes")
cm_step$table
######### Model 3 # Variable Importance 
model3_cv <- train(TenYearCHD ~ male + age + cigsPerDay + sysBP + glucose + prevalentStroke + prevalentHyp ,
                   data = train, trControl = train_control, method = "glm")
model3_cv$resample
print(model3_cv)
cm_vi <- confusionMatrix(model3_cv, reference = train$AHD, positive = "Yes")
cm_vi$table
######## Model 4 # trial and error
set.seed(123)
model4_cv <- train(TenYearCHD~ glucose + BMI + diabetes + sysBP + totChol + BPMeds + cigsPerDay + male + age,
                   data = train, trControl = train_control, method = "glm")
model4_cv$resample
print(model4_cv)
cm_te <- confusionMatrix(model4_cv, reference = train$AHD, positive = "Yes")
cm_te$table
####### Model 5 #P_value  
set.seed(123)
model5_cv <- train(TenYearCHD ~ male +age + cigsPerDay +  sysBP + glucose ,
                   data = train, trControl = train_control, method = "glm")
model5_cv$resample
print(model5_cv)
cm_p <- confusionMatrix(model5_cv, reference = train$AHD, positive = "Yes")
cm_p$table
######## Model 6 RFE
set.seed(123)
model6_cv <- train(TenYearCHD ~ age + sysBP + diaBP + prevalentHyp + male + glucose + BMI + totChol + currentSmoker + cigsPerDay  + diabetes,
                   data = train, trControl = train_control, method = "glm")
model6_cv$resample
print(model6_cv)
cm_rfe <- confusionMatrix(model6_cv, reference = train$AHD, positive = "Yes")
cm_rfe$table
###### Model 7 Bortua 
set.seed(123)
model7_cv <- train(TenYearCHD ~ male + age + cigsPerDay + BPMeds +  prevalentHyp  + diabetes + totChol + sysBP + diaBP + BMI +glucose,
                   data = train, trControl = train_control, method = "glm")
model7_cv$resample
print(model7_cv)
cm_b <- confusionMatrix(model7_cv, reference = train$AHD, positive = "Yes")
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
cm_table= list(All = cm_info(cm_all$table),
               Step = cm_info(cm_step$table), 
               variable_importance = cm_info(cm_vi$table),
               trial_and_error = cm_info(cm_te$table),
               P_value = cm_info(cm_p$table),
               RFE = cm_info(cm_rfe$table),
               Bortua = cm_info(cm_p$table))
cm_table 
glm_more <- do.call(rbind, cm_table)
glm_more2 <- data.frame(glm_more) 
glm_more2
#####
cv2_table= list(All = cm_all$table,
                Step = cm_step$table, 
                variable_importance = cm_vi$table,
                trial_and_error = cm_te$table,
                P_value = cm_p$table,
                RFE = cm_rfe$table,
                Bortua = cm_p$table)
cv2_table
###############
########################################### SVM ##################################
set.seed(123)
svm1 <-  svm(TenYearCHD ~ ., data = train, kernel = "linear")
summary(svm1)
pred_svm <- predict(svm1, train, type = "class")
tab <- table(Predicited = pred_svm, Actual = train$TenYearCHD)
tab
acc(tab)
############# Variable importance svm ###################
ctrl <- trainControl(method = "cv", number = 10)
model_svm <- train(TenYearCHD ~., train,trainControl  = ctrl, method = "svmLinear")
var_svm <- varImp(model_svm)
var_svm
plot(var_svm, main = "Variable Importance SVM")
###### SVM2 Step wise ########################
svm2 <- svm(TenYearCHD ~ age , train, kernel = "polynomial")
summary(svm2)
pred_svm2 <- predict(svm2, train, type = "class")
plot(pred_svm2, main = "Step Formula ROC")
tab2 <- table(Predicited = pred_svm2, Actual = train$TenYearCHD)
tab2
acc(tab2)

