library(tidyverse)
library(survAUC)
# library(timeROC)
dfbase <- read_csv("dfbase.csv")
dfbase$totalTime <- dfbase$totalTime/365
dfbase <- dfbase%>%filter(totalTime!=0)

dfbase[which(dfbase$totalTime%%1==0&dfbase$totalTime!=0),"totalTime"] <-
  dfbase[which(dfbase$totalTime%%1==0&dfbase$totalTime!=0),"totalTime"]-0.01

col_list <- c("id","sex","age","residenc","edu","occ","marriage","totalTime","outcome")

set.seed(1)

dfbase2 <-  dfbase %>% select(col_list,"emo")
dfbase2 <- dfbase2 %>% drop_na()
V<-10
library(caret)
folds <- createFolds(factor(dfbase2$outcome), k = V)

# FUN <- function(j){
    
# }

#j<-folds$Fold05
FUN1 <- function (j) {
    i <- dfbase2$id[j]
    TR <- dfbase2[which(!dfbase2$id %in% i), ]
    TE <- dfbase2[which(dfbase2$id %in% i), ]
    train.fit <- coxph(Surv(totalTime,event=outcome)~sex+age+residenc+edu+occ+marriage+emo,
                    data=TR,x=TRUE)
    lp <- predict(train.fit)
    lpnew <- predict(train.fit, newdata=TE)
    Surv.rsp <- Surv(TR$totalTime, TR$outcome)
    Surv.rsp.new <- Surv(TE$totalTime, TE$outcome)
    times <- c(2,5,7,10,13,15,17)
    AUC_SH <- AUC.sh(Surv.rsp, Surv.rsp.new, lp, lpnew, times,type="incident")
    AUC_SH2 <- AUC.sh(Surv.rsp, Surv.rsp.new, lp, lpnew, times,type="cumulative")
    re1 <- AUC_SH$auc
    re2 <- AUC_SH2$auc
    return(c(re1,re2))
}
re = sapply(folds,FUN1)
re
p = rowMeans(re)
p

dfbase2 <-  dfbase %>% select(col_list,"ADL")
dfbase2 <- dfbase2 %>% drop_na()
V<-10
library(caret)
folds <- createFolds(factor(dfbase2$outcome), k = V)

# FUN <- function(j){
    
# }

#j<-folds$Fold05
FUN1 <- function (j) {
    i <- dfbase2$id[j]
    TR <- dfbase2[which(!dfbase2$id %in% i), ]
    TE <- dfbase2[which(dfbase2$id %in% i), ]
    train.fit <- coxph(Surv(totalTime,event=outcome)~sex+age+residenc+edu+occ+marriage+ADL,
                    data=TR,x=TRUE)
    lp <- predict(train.fit)
    lpnew <- predict(train.fit, newdata=TE)
    Surv.rsp <- Surv(TR$totalTime, TR$outcome)
    Surv.rsp.new <- Surv(TE$totalTime, TE$outcome)
    times <- c(2,5,7,10,13,15,17)
    AUC_SH <- AUC.sh(Surv.rsp, Surv.rsp.new, lp, lpnew, times,type="incident")
    AUC_SH2 <- AUC.sh(Surv.rsp, Surv.rsp.new, lp, lpnew, times,type="cumulative")
    re1 <- AUC_SH$auc
    re2 <- AUC_SH2$auc
    return(c(re1,re2))
}
re = sapply(folds,FUN1)
re
p = rowMeans(re)
p

dfbase2 <-  dfbase %>% select(col_list,"bmi")
dfbase2 <- dfbase2 %>% drop_na()
V<-10
library(caret)
folds <- createFolds(factor(dfbase2$outcome), k = V)

# FUN <- function(j){
    
# }

#j<-folds$Fold05
FUN1 <- function (j) {
    i <- dfbase2$id[j]
    TR <- dfbase2[which(!dfbase2$id %in% i), ]
    TE <- dfbase2[which(dfbase2$id %in% i), ]
    train.fit <- coxph(Surv(totalTime,event=outcome)~sex+age+residenc+edu+occ+marriage+bmi,
                    data=TR,x=TRUE)
    lp <- predict(train.fit)
    lpnew <- predict(train.fit, newdata=TE)
    Surv.rsp <- Surv(TR$totalTime, TR$outcome)
    Surv.rsp.new <- Surv(TE$totalTime, TE$outcome)
    times <- c(2,5,7,10,13,15,17)
    AUC_SH <- AUC.sh(Surv.rsp, Surv.rsp.new, lp, lpnew, times,type="incident")
    AUC_SH2 <- AUC.sh(Surv.rsp, Surv.rsp.new, lp, lpnew, times,type="cumulative")
    re1 <- AUC_SH$auc
    re2 <- AUC_SH2$auc
    return(c(re1,re2))
}
re = sapply(folds,FUN1)
re
p = rowMeans(re)
p
q = apply(re,1,sd)
q