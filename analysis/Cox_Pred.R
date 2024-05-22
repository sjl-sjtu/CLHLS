library(tidyverse)
library(survAUC)
# library(timeROC)
dfbase <- read_csv("dfbase_filtered.csv")
dfbase <- dfbase %>% drop_na()

# dfbase[which(dfbase$totalTime%%1==0&dfbase$totalTime!=0),"totalTime"] <-
#   dfbase[which(dfbase$totalTime%%1==0&dfbase$totalTime!=0),"totalTime"]-0.01

col_list <- c("id","sex","age","res","edu","occ","mar","totalTime","outcome")

set.seed(1)

dfbase2 <-  dfbase %>% select(col_list,"SWB")
dfbase2 <- dfbase2 %>% drop_na()
V<-10
library(caret)
folds <- createFolds(factor(dfbase2$outcome), k = V)


#j<-folds$Fold05
FUN1 <- function (j) {
    i <- dfbase2$id[j]
    TR <- dfbase2[which(!dfbase2$id %in% i), ]
    TE <- dfbase2[which(dfbase2$id %in% i), ]
    train.fit <- coxph(Surv(totalTime,event=outcome)~sex+age+res+edu+occ+mar+SWB,
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
print(re)
p = rowMeans(re)
print(p)

df1 <- tibble(index=rep("SWB",14),type=rep(c("incident","cumulative"),each=7),
             time=rep(c(2,5,7,10,13,15,17),2),mean=p)

dfbase2 <-  dfbase %>% select(col_list,"ADL")
dfbase2 <- dfbase2 %>% drop_na()
V<-10
library(caret)
folds <- createFolds(factor(dfbase2$outcome), k = V)


FUN1 <- function (j) {
    i <- dfbase2$id[j]
    TR <- dfbase2[which(!dfbase2$id %in% i), ]
    TE <- dfbase2[which(dfbase2$id %in% i), ]
    train.fit <- coxph(Surv(totalTime,event=outcome)~sex+age+res+edu+occ+mar+ADL,
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
print(re)
p = rowMeans(re)
print(p)

df2 <- tibble(index=rep("ADL",14),type=rep(c("incident","cumulative"),each=7),
             time=rep(c(2,5,7,10,13,15,17),2),mean=p)

dfbase2 <-  dfbase %>% select(col_list,"BMI")
dfbase2 <- dfbase2 %>% drop_na()
V<-10
library(caret)
folds <- createFolds(factor(dfbase2$outcome), k = V)


FUN1 <- function (j) {
    i <- dfbase2$id[j]
    TR <- dfbase2[which(!dfbase2$id %in% i), ]
    TE <- dfbase2[which(dfbase2$id %in% i), ]
    train.fit <- coxph(Surv(totalTime,event=outcome)~sex+age+res+edu+occ+mar+BMI,
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
print(re)
p = rowMeans(re)
print(p)

df3 <- tibble(index=rep("BMI",14),type=rep(c("incident","cumulative"),each=7),
             time=rep(c(2,5,7,10,13,15,17),2),mean=p)

df <- purrr::reduce(list(df1,df2,df3),rbind)
df %>% write_csv("base_pred.csv")
