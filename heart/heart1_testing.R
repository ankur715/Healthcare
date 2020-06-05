library(e1071)
library(tidyverse)
library(mlbench)
library(psych)
library(skimr)
library(corrplot)
library(effects)
library(multcomp)
library(car)
library(gmodels)
library(lsr)
#######################
setwd("~/Desktop")
df <- read.csv("heart1.csv")
overview <- skim_to_wide(df)
df$Ca <- as.factor(df$Ca)
######################## correlation 
colnames(df)
num_list <- list()
not_num_list <- list()
for(var in colnames(df)){
  if (class(df[,var]) %in% c("factor","logical")){
    not_num_list[[var]] <- df[,var]
  } else if (class(df[,var]) %in% c("numeric","double","integer")){
    num_list[[var]]  <- df[,var]
  }
}
num_list[1]
num <- as.data.frame(num_list)
not_num <- as.data.frame(not_num_list)
not_num
num
w <- cor(num)
w
corrplot(w, method = "number")
############ Chol
cor.test(df$Chol, df$Age, method = "pearson", alternative = "two.side", conf.level = 0.95)
cor.test(df$Chol, df$RestBP, method = "pearson", alternative = "two.side", conf.level = 0.95)
cor.test(df$Chol, df$MaxHR, method = "pearson", alternative = "two.side", conf.level = 0.95) # No cor
cor.test(df$Chol, df$Oldpeak, method = "pearson", alternative = "two.side", conf.level = 0.95) # No cor
############ RestBP
cor.test(df$RestBP, df$MaxHR, method = "pearson", alternative = "two.side", conf.level = 0.95) # no cor
################### Cross table 
##### AHD
CrossTable(df$Sex , df$AHD,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F) # yes 
cramersV(df$Sex, df$AHD) # Low
CrossTable(df$ChestPain , df$AHD,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F) # yes
cramersV(df$ChestPain , df$AHD) # middle
CrossTable(df$Fbs , df$AHD,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F) # no 
CrossTable(df$RestECG , df$AHD,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F) #yes
cramersV(df$RestECG , df$AHD) #low
CrossTable(df$ExAng , df$AHD,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)#yes
cramersV(df$ExAng , df$AHD) # middle
CrossTable(df$Slope , df$AHD,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df$Slope , df$AHD) # middle
CrossTable(df$Ca , df$AHD,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df$Ca, df$AHD) # middle 
CrossTable(df$Thal , df$AHD,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df$Thal, df$AHD) # middle 
##################### ANOVA TEST WITH AHD
anova_func <- function(x){
  r <- leveneTest(x ~ df$AHD)
  if(r$`Pr(>F)`[1] > 0.05){
    aov1 <- aov(x~AHD, df)
    w <-  summary(aov1)
    t <- TukeyHSD(aov1)
    return(list(r,w, t))
  } else{
    c <- oneway.test(x~AHD, data=df, var.equal=F)
    return(list(r,c))
  }
}
################## AHD
anova_ahd <- apply(df[,c("Age","RestBP", "Chol", "Oldpeak","MaxHR")], 2, anova_func)
anova_ahd$Age
anova_ahd$RestBP
anova_ahd$Chol
anova_ahd$Oldpeak
anova_ahd$MaxHR
################### Sex
anova_func_sex <- function(x){
  r <- leveneTest(x ~ df$Sex)
  if(r$`Pr(>F)`[1] > 0.05){
    aov1 <- aov(x~Sex, df)
    w <-  summary(aov1)
    t <- TukeyHSD(aov1)
    return(list(r,w, t))
  } else{
    c <- oneway.test(x~Sex, data=df, var.equal=F)
    return(list(r,c))
  }
}

anova_func_sex(df$Age)
anova_func_sex(df$RestBP)
anova_func_sex(df$Chol)
anova_func_sex(df$Oldpeak)
anova_func_sex(df$MaxHR)
############### anova simple main effects
aov1 <- aov(Age~AHD + Thal + AHD*Thal, df)
summary(aov1)
####################
df_y <- df[df$AHD == "Yes",]
aov1 <- aov(Age ~ Thal, df_y)
summary(aov1)
TukeyHSD(aov1)
df_n <- df[df$AHD == "No",]
aov2 <- aov(Age ~ Thal, df_n)
summary(aov2)
TukeyHSD(aov2)
df_nor <- df[df$Thal == "normal",]
aov3 <- aov(Age ~ AHD, df_nor)
summary(aov3)
TukeyHSD(aov3)
df_fix <- df[df$Thal == "fixed",]
aov4 <- aov(Age ~ AHD, df_fix)
summary(aov4)
TukeyHSD(aov4)
df_r<- df[df$Thal == "reversable",]
aov5 <- aov(Age ~ AHD, df_r)
summary(aov5)
TukeyHSD(aov5)
#############
aov1 <- aov(RestBP~AHD + Sex + AHD*Sex, df)
summary(aov1)
################
df_y <- df[df$AHD == "Yes",]
aov1 <- aov(RestBP ~ Sex, df_y)
summary(aov1)
TukeyHSD(aov1)
df_n <- df[df$AHD == "No",]
aov2 <- aov(RestBP ~ Sex, df_n)
summary(aov2)
TukeyHSD(aov2)
df_male <- df[df$Sex == "male",]
aov3 <- aov(RestBP ~ AHD, df_male)
summary(aov3)
TukeyHSD(aov3)
df_female <- df[df$Sex == "female",]
aov4 <- aov(RestBP ~ AHD, df_female)
summary(aov4)
TukeyHSD(aov4)
#############################
#Binomial Regression
df$AHD <- relevel(df$AHD, ref = "Yes")
model <- glm(AHD ~ ., data = df, family = binomial())
summary(model)
expb <-  exp(coef(model))
expb
df4 <- data.frame(expb)
df4 <- rownames_to_column(df4, "Type")
df4 <- df4 %>%
  mutate(prob = expb/(expb + 1))
df4

table(df$Sex, df$AHD)
