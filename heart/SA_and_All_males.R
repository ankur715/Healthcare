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
df <- read.csv("SA_heart_dataset.csv")
str(df)
dim(df)
df$chd <-  as.factor(df$chd)
df$row.names <- NULL
library(skimr)
overview <- skim_to_wide(df)
anyNA(df)
str(df)
df$chd <- as.factor(df$chd)
##################
num_list <- list()
not_num_list <- list()
for(var in colnames(df)){
  if (class(df[,var]) %in% c("factor","logical")){
    not_num_list[[var]] <- df[,var]
  } else if (class(df[,var]) %in% c("numeric","double","integer")){
    num_list[[var]]  <- df[,var]
  }
}
num <- as.data.frame(num_list)
not_num <- as.data.frame(not_num_list)
not_num
num
w <- cor(num)
w
corrplot(w, method = "number")
cor.test(df$obesity, df$adiposity  , method = "pearson", alternative = "two.sided", conf.level = 0.95)
cor.test(df$age, df$typea, method = "pearson", alternative = "two.sided", conf.level = 0.95)
cor.test(df$alcohol, df$adiposity, method = "pearson", alternative = "two.sided", conf.level = 0.95)
############
require(lsr)
library(gmodels)
library(car)
CrossTable(df$famhist , df$chd,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df$famhist, df$chd)
#################
anova_func <- function(x){
  r <- leveneTest(x ~ df$chd)
  if(r$`Pr(>F)`[1] > 0.05){
    aov1 <- aov(x~chd, df)
    w <-  summary(aov1)
    t <- TukeyHSD(aov1)
    return(list(r,w, t))
  } else{
    c <- oneway.test(x~chd, data=df, var.equal=F)
    return(list(r,c))
  }
}
sa_anova <- apply(df[,c("sbp","tobacco","ldl","adiposity","typea","obesity","alcohol","age")], 2, anova_func)
sa_anova$sbp
sa_anova$tobacco
sa_anova$ldl
sa_anova$adiposity
sa_anova$typea
sa_anova$obesity
sa_anova$alcohol
sa_anova$age
#############################
df$chd <- relevel(df$chd, ref = "1")
model <- glm(chd~., df, family = binomial())
summary(model)
odds <- exp(model$coefficients)
odds
#################################################### All Males
setwd("~/Desktop")
df <- read.csv("heart_disease_male.csv")
str(df)
dim(df)
names(df)[8] <- "CHD"
library(skimr)
overview <- skim_to_wide(df)
anyNA(df)
unique(df$rest_electro)
df1 <- filter(df, rest_electro != "?")
df1$rest_electro
############################################
num_list <- list()
not_num_list <- list()
for(var in colnames(df1)){
  if (class(df1[,var]) %in% c("factor","logical")){
    not_num_list[[var]] <- df1[,var]
  } else if (class(df1[,var]) %in% c("numeric","double","integer")){
    num_list[[var]]  <- df1[,var]
  }
}
num <- as.data.frame(num_list)
not_num <- as.data.frame(not_num_list)
not_num
num
w <- cor(num)
w
corrplot(w, method = "number")
########################
CrossTable(df1$chest_pain , df1$CHD,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df1$chest_pain , df1$CHD)
CrossTable(df1$rest_electro , df1$CHD,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F) #No
CrossTable(df1$blood_sugar , df1$CHD,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df1$blood_sugar , df1$CHD)
CrossTable(df1$exercice_angina , df1$CHD,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df1$exercice_angina , df1$CHD)
#######################
anova_func2 <- function(x){
  r <- leveneTest(x ~ df1$CHD)
  if(r$`Pr(>F)`[1] > 0.05){
    aov1 <- aov(x~CHD, df1)
    w <-  summary(aov1)
    t <- TukeyHSD(aov1)
    return(list(r,w, t))
  } else{
    c <- oneway.test(x~CHD, data=df1, var.equal=F)
    return(list(r,c))
  }
}
colnames(num)
males_anova <- apply(df1[,c("age","rest_bpress" ,"max_heart_rate")], 2, anova_func2)
males_anova$age
males_anova$rest_bpress
males_anova$max_heart_rate
###############
df1$CHD <- relevel(df1$CHD, ref = "positive")
model <- glm(CHD~., df1, family = binomial())
summary(model)
odds_2 <- exp(model$coefficients)
odds_2
df3 <- data.frame(odds_2)
df3 <- rownames_to_column(df3, "Type")
df3 <- df3 %>%
  mutate(prob = odds_2/(odds_2 + 1))
df3
