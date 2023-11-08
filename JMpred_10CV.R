library(JMbayes)
library(tidyverse)
library(parallel)
library(pbapply)

dfsur <- read_csv("dfsur.csv")
dflongi <-read_csv(("dflongi2.csv"))

dfsur$totalTime <- dfsur$totalTime/365
dflongi$totalTime <- dflongi$totalTime/365
dflongi$times <- dflongi$times/365
dflongi$ADL <- dflongi$ADL/12
dfsur[which(dfsur$totalTime%%1==0&dfsur$totalTime!=0),"totalTime"] <-
  dfsur[which(dfsur$totalTime%%1==0&dfsur$totalTime!=0),"totalTime"]-0.01
dflongi[which(dflongi$totalTime%%1==0&dflongi$totalTime!=0),"totalTime"] <-
  dflongi[which(dflongi$totalTime%%1==0&dflongi$totalTime!=0),"totalTime"]-0.01
dflongi[which(dflongi$times%%1==0&dflongi$times!=0),"times"] <-
  dflongi[which(dflongi$times%%1==0&dflongi$times!=0),"times"]-0.01

#dflongi[which(dflongi$times>dflongi$totalTime &dflongi$outcome==0),"totalTime"] <- dflongi[which(dflongi$times>dflongi$totalTime &dflongi$outcome==0),"times"]

set.seed(0)

ctr <- lmeControl(maxIter = 50000, msMaxIter = 50000, tolerance = 1e-6, niterEM = 25000,
                  msMaxEval = 200000,opt="optim")

dflongi1 <- subset(dflongi,times!="NA" &  emo!="NA" & ADL!="NA" & bmi!="NA")
dflongi1<-dflongi1[-which(dflongi1$times>dflongi1$totalTime),] #删去在事件发生之后的纵向数据记录
dfsur1 <- subset(dfsur,marriage!="NA" & sex!="NA" & age!="NA" & residenc!="NA" & 
                   edu!="NA" & occ!="NA" & totalTime!="NA")
dftimes <- as.data.frame(table(dflongi1$id))
p <- dfsur1$id
q <- dflongi1$id
w <- intersect(p,q)
s <- dftimes$Var1[which(dftimes$Freq>=2)]
w <- intersect(s,w)
dfsur2 <- dfsur1[which(dfsur1$id %in% w),]
dflongi2 <- dflongi1[which(dflongi1$id %in% w),]

V <- 10
library(caret)
folds <- createFolds(factor(dfsur2$outcome), k = V)
# j<-folds$`Fold5`
CrossValJM <- function (j) {
  library("JMbayes")
  library("splines")
  library(Metrics)
  i <- dfsur2$id[j]
  trainingData <- dflongi2[which(!dflongi2$id %in% i), ]
  trainingData.id <- dfsur2[which(!dfsur2$id %in% i), ]
  testingData <- dflongi2[which(dflongi2$id %in% i), ]
  testingData.id <- dfsur2[which(dfsur2$id %in% i), ]
  lmeFit <- lme(emo~times, data = trainingData,random = ~ times | id,control = ctr)
  coxFit <- coxph(Surv(totalTime,event=outcome)~sex+age+residenc+edu+occ+marriage,
                data=trainingData.id,x=TRUE)
  jointFit <- jointModelBayes(lmeFit, coxFit , timeVar = "times",n.iter = 20000,seed=0)
  auc.jointFit <- aucJM(jointFit, newdata = testingData, Tstart = 4, Dt = 2,idVar='id')
  auc.jointFit2 <- aucJM(jointFit, newdata = testingData, Tstart = 4, Dt = 5,idVar='id')
  auc.jointFit3 <- aucJM(jointFit, newdata = testingData, Tstart = 4, Dt = 7,idVar='id')
  auc.jointFit4 <- aucJM(jointFit, newdata = testingData, Tstart = 4, Dt = 10,idVar='id')
  auc.jointFit5 <- aucJM(jointFit, newdata = testingData, Tstart = 8, Dt = 2,idVar='id')
  auc.jointFit6 <- aucJM(jointFit, newdata = testingData, Tstart = 8, Dt = 5,idVar='id')
  auc.jointFit7 <- aucJM(jointFit, newdata = testingData, Tstart = 8, Dt = 7,idVar='id')
  auc.jointFit8 <- aucJM(jointFit, newdata = testingData, Tstart = 8, Dt = 10,idVar='id')
  auc.jointFit9 <- aucJM(jointFit, newdata = testingData, Tstart = 10, Dt = 2,idVar='id')
  auc.jointFit10 <- aucJM(jointFit, newdata = testingData, Tstart = 10, Dt = 5,idVar='id')
  auc.jointFit11 <- aucJM(jointFit, newdata = testingData, Tstart = 10, Dt = 7,idVar='id')
  auc.jointFit12 <- aucJM(jointFit, newdata = testingData, Tstart = 10, Dt = 10,idVar='id')
  # dyn.jointFit <- dynCJM(jointFit, newdata = testingData, Dt = 2,t.max=120,idVar='id',simulate=TRUE)
  # ipe.jointFit <- prederrJM(jointFit, newdata = testingData, Tstart = 2, Thoriz = 20,idVar='id',interval = TRUE)
  list(auc1=auc.jointFit$auc,auc2=auc.jointFit2$auc,auc3=auc.jointFit3$auc,auc4=auc.jointFit4$auc,
       auc5=auc.jointFit5$auc,auc6=auc.jointFit6$auc,auc7=auc.jointFit7$auc,auc8=auc.jointFit8$auc,
       auc9=auc.jointFit9$auc,auc10=auc.jointFit10$auc,auc11=auc.jointFit11$auc,auc12=auc.jointFit12$auc)
       #dyn=dyn.jointFit$dynC,ipe=ipe.jointFit$prederr)
}
cl <- makeCluster(40)
clusterExport(cl,varlist=c("dfsur2","dflongi2","ctr"))
# res_ <- parLapply(cl, folds, CrossValJM)
op <- pboptions(type = "timer")
res_ <- pblapply(X=folds, FUN=CrossValJM, cl=cl)
pboptions(op)

stopCluster(cl)
dfre_ <- data.frame(t(matrix(unlist(res_),ncol=V)))
colnames(dfre_) <- c("auc1","auc2","auc3","auc4","auc5","auc6","auc7","auc8",
                     "auc9","auc10","auc11","auc12")
write_csv(dfre_,"predict_detail_new.csv",col_names=TRUE)
p_ <- data.frame(t(colMeans(dfre_,na.rm = TRUE)))
p_$Variabel <- "emo"
write_csv(p_,"predict_new.csv",col_names=TRUE)

q_ <- data.frame(t(apply(dfre_,2,sd,na.rm=TRUE)))
q_$Variabel <- "emo"
write_csv(q_,"predict_new_sd.csv",col_names=TRUE)

V <- 10
library(caret)
folds <- createFolds(factor(dfsur2$outcome), k = V)
# j<-folds$`Fold5`
CrossValJM <- function (j) {
  library("JMbayes")
  library("splines")
  library(Metrics)
  i <- dfsur2$id[j]
  trainingData <- dflongi2[which(!dflongi2$id %in% i), ]
  trainingData.id <- dfsur2[which(!dfsur2$id %in% i), ]
  testingData <- dflongi2[which(dflongi2$id %in% i), ]
  testingData.id <- dfsur2[which(dfsur2$id %in% i), ]
  lmeFit <- lme(ADL~times, data = trainingData,random = ~ times | id,control = ctr)
  coxFit <- coxph(Surv(totalTime,event=outcome)~sex+age+residenc+edu+occ+marriage,
                data=trainingData.id,x=TRUE)
  jointFit <- jointModelBayes(lmeFit, coxFit , timeVar = "times",n.iter = 20000,seed=0)
  auc.jointFit <- aucJM(jointFit, newdata = testingData, Tstart = 4, Dt = 2,idVar='id')
  auc.jointFit2 <- aucJM(jointFit, newdata = testingData, Tstart = 4, Dt = 5,idVar='id')
  auc.jointFit3 <- aucJM(jointFit, newdata = testingData, Tstart = 4, Dt = 7,idVar='id')
  auc.jointFit4 <- aucJM(jointFit, newdata = testingData, Tstart = 4, Dt = 10,idVar='id')
  auc.jointFit5 <- aucJM(jointFit, newdata = testingData, Tstart = 8, Dt = 2,idVar='id')
  auc.jointFit6 <- aucJM(jointFit, newdata = testingData, Tstart = 8, Dt = 5,idVar='id')
  auc.jointFit7 <- aucJM(jointFit, newdata = testingData, Tstart = 8, Dt = 7,idVar='id')
  auc.jointFit8 <- aucJM(jointFit, newdata = testingData, Tstart = 8, Dt = 10,idVar='id')
  auc.jointFit9 <- aucJM(jointFit, newdata = testingData, Tstart = 10, Dt = 2,idVar='id')
  auc.jointFit10 <- aucJM(jointFit, newdata = testingData, Tstart = 10, Dt = 5,idVar='id')
  auc.jointFit11 <- aucJM(jointFit, newdata = testingData, Tstart = 10, Dt = 7,idVar='id')
  auc.jointFit12 <- aucJM(jointFit, newdata = testingData, Tstart = 10, Dt = 10,idVar='id')
  # dyn.jointFit <- dynCJM(jointFit, newdata = testingData, Dt = 2,t.max=120,idVar='id',simulate=TRUE)
  # ipe.jointFit <- prederrJM(jointFit, newdata = testingData, Tstart = 2, Thoriz = 20,idVar='id',interval = TRUE)
  list(auc1=auc.jointFit$auc,auc2=auc.jointFit2$auc,auc3=auc.jointFit3$auc,auc4=auc.jointFit4$auc,
       auc5=auc.jointFit5$auc,auc6=auc.jointFit6$auc,auc7=auc.jointFit7$auc,auc8=auc.jointFit8$auc,
       auc9=auc.jointFit9$auc,auc10=auc.jointFit10$auc,auc11=auc.jointFit11$auc,auc12=auc.jointFit12$auc)
       #dyn=dyn.jointFit$dynC,ipe=ipe.jointFit$prederr)
}
cl <- makeCluster(40)
clusterExport(cl,varlist=c("dfsur2","dflongi2","ctr"))
# res_ <- parLapply(cl, folds, CrossValJM)
op <- pboptions(type = "timer")
res_ <- pblapply(X=folds, FUN=CrossValJM, cl=cl)
pboptions(op)

stopCluster(cl)
dfre_ <- data.frame(t(matrix(unlist(res_),ncol=V)))
colnames(dfre_) <- c("auc1","auc2","auc3","auc4","auc5","auc6","auc7","auc8",
                     "auc9","auc10","auc11","auc12")
write_csv(dfre_,"predict_detail_new.csv",append = TRUE,col_names=TRUE)
p_ <- data.frame(t(colMeans(dfre_,na.rm = TRUE)))
p_$Variabel <- "ADL"
write_csv(p_,"predict_new.csv",append = TRUE)

q_ <- data.frame(t(apply(dfre_,2,sd,na.rm=TRUE)))
q_$Variabel <- "ADL"
write_csv(q_,"predict_new_sd.csv",append = TRUE)


dflongi2$bmi <- scale(dflongi2$bmi)
V <- 10
library(caret)
folds <- createFolds(factor(dfsur2$outcome), k = V)
# j<-folds$`Fold5`
CrossValJM <- function (j) {
  library("JMbayes")
  library("splines")
  library(Metrics)
  i <- dfsur2$id[j]
  trainingData <- dflongi2[which(!dflongi2$id %in% i), ]
  trainingData.id <- dfsur2[which(!dfsur2$id %in% i), ]
  testingData <- dflongi2[which(dflongi2$id %in% i), ]
  testingData.id <- dfsur2[which(dfsur2$id %in% i), ]
  lmeFit <- lme(bmi~times, data = trainingData,random = ~ times | id,control = ctr)
  coxFit <- coxph(Surv(totalTime,event=outcome)~sex+age+residenc+edu+occ+marriage,
                data=trainingData.id,x=TRUE)
  jointFit <- jointModelBayes(lmeFit, coxFit , timeVar = "times",n.iter = 20000,seed=0)
  auc.jointFit <- aucJM(jointFit, newdata = testingData, Tstart = 4, Dt = 2,idVar='id')
  auc.jointFit2 <- aucJM(jointFit, newdata = testingData, Tstart = 4, Dt = 5,idVar='id')
  auc.jointFit3 <- aucJM(jointFit, newdata = testingData, Tstart = 4, Dt = 7,idVar='id')
  auc.jointFit4 <- aucJM(jointFit, newdata = testingData, Tstart = 4, Dt = 10,idVar='id')
  auc.jointFit5 <- aucJM(jointFit, newdata = testingData, Tstart = 8, Dt = 2,idVar='id')
  auc.jointFit6 <- aucJM(jointFit, newdata = testingData, Tstart = 8, Dt = 5,idVar='id')
  auc.jointFit7 <- aucJM(jointFit, newdata = testingData, Tstart = 8, Dt = 7,idVar='id')
  auc.jointFit8 <- aucJM(jointFit, newdata = testingData, Tstart = 8, Dt = 10,idVar='id')
  auc.jointFit9 <- aucJM(jointFit, newdata = testingData, Tstart = 10, Dt = 2,idVar='id')
  auc.jointFit10 <- aucJM(jointFit, newdata = testingData, Tstart = 10, Dt = 5,idVar='id')
  auc.jointFit11 <- aucJM(jointFit, newdata = testingData, Tstart = 10, Dt = 7,idVar='id')
  auc.jointFit12 <- aucJM(jointFit, newdata = testingData, Tstart = 10, Dt = 10,idVar='id')
  # dyn.jointFit <- dynCJM(jointFit, newdata = testingData, Dt = 2,t.max=120,idVar='id',simulate=TRUE)
  # ipe.jointFit <- prederrJM(jointFit, newdata = testingData, Tstart = 2, Thoriz = 20,idVar='id',interval = TRUE)
  list(auc1=auc.jointFit$auc,auc2=auc.jointFit2$auc,auc3=auc.jointFit3$auc,auc4=auc.jointFit4$auc,
       auc5=auc.jointFit5$auc,auc6=auc.jointFit6$auc,auc7=auc.jointFit7$auc,auc8=auc.jointFit8$auc,
       auc9=auc.jointFit9$auc,auc10=auc.jointFit10$auc,auc11=auc.jointFit11$auc,auc12=auc.jointFit12$auc)
       #dyn=dyn.jointFit$dynC,ipe=ipe.jointFit$prederr)
}
cl <- makeCluster(40)
clusterExport(cl,varlist=c("dfsur2","dflongi2","ctr"))
# res_ <- parLapply(cl, folds, CrossValJM)
op <- pboptions(type = "timer")
res_ <- pblapply(X=folds, FUN=CrossValJM, cl=cl)
pboptions(op)

stopCluster(cl)
dfre_ <- data.frame(t(matrix(unlist(res_),ncol=V)))
colnames(dfre_) <- c("auc1","auc2","auc3","auc4","auc5","auc6","auc7","auc8",
                     "auc9","auc10","auc11","auc12")
write_csv(dfre_,"predict_detail_new.csv",append = TRUE,col_names=TRUE)
p_ <- data.frame(t(colMeans(dfre_,na.rm = TRUE)))
p_$Variabel <- "bmi"
write_csv(p_,"predict_new.csv",append = TRUE)

q_ <- data.frame(t(apply(dfre_,2,sd,na.rm=TRUE)))
q_$Variabel <- "bmi"
write_csv(q_,"predict_new_sd.csv",append = TRUE)