library(JMbayes)
library(tidyverse)

dfsur <- read_csv("dfsur.csv")
dflongi <-read_csv(("dflongi2.csv"))

dfsur$totalTime <- dfsur$totalTime/365
dflongi$totalTime <- dflongi$totalTime/365
dflongi$times <- dflongi$times/365
dflongi$ADL <- dflongi$ADL/12

set.seed(1)

ctr <- lmeControl(maxIter = 50000, msMaxIter = 50000, tolerance = 1e-6, niterEM = 25000,
                  msMaxEval = 200000,opt="optim")

#emotion
print("emo")
dflongi1 <- subset(dflongi,times!="NA" &  emo!="NA")
dflongi1<-dflongi1[-which(dflongi1$times>dflongi1$totalTime),] #删去在事件发生之后的纵向数据记录
dfsur1 <- subset(dfsur,marriage!="NA" & sex!="NA" & age!="NA" & residenc!="NA" & 
                   edu!="NA" & occ!="NA" & totalTime!="NA")
dftimes <- as.data.frame(table(dflongi1$id))
p <- dfsur1$id
q <- dflongi1$id
w <- intersect(p,q)
# s <- dftimes$Var1[which(dftimes$Freq>=2)]
# w <- intersect(s,w)
dfsur2 <- dfsur1[which(dfsur1$id %in% w),]
dflongi2 <- dflongi1[which(dflongi1$id %in% w),]
coxFit <- coxph(Surv(totalTime,event=outcome)~sex+age+residenc+edu+occ+marriage,
                data=dfsur2,x=TRUE)
coxFit
lmeFit <- lme(emo~times, data = dflongi2,random = ~ times | id,control = ctr)
lmeFit
JMFit <- jointModelBayes(lmeFit, coxFit ,timeVar="times",n.iter=20000,seed=0,verbose=FALSE)
JMFit
summary(JMFit)
print("#######")

#bmi
print("bmi")
dfbmi1 <- subset(dflongi,times!="NA" &  bmi!="NA")
dfbmi1<-dfbmi1[-which(dfbmi1$times>dfbmi1$totalTime),] #删去在事件发生之后的纵向数据记录
dfsur1 <- subset(dfsur,marriage!="NA" & sex!="NA" & age!="NA" & residenc!="NA" & 
                   edu!="NA" & occ!="NA" & totalTime != "NA")
p <- dfsur1$id
q <- dfbmi1$id
w <- intersect(p,q)
dftimes <- as.data.frame(table(dfbmi1$id))
# s <- dftimes$Var1[which(dftimes$Freq>=2)]
# w <- intersect(s,w)
dfsur2 <- dfsur1[which(dfsur1$id %in% w),]
dfbmi2 <- dfbmi1[which(dfbmi1$id %in% w),]
dfbmi2$bmi <- scale(dfbmi2$bmi) #标准化
coxFit <- coxph(Surv(totalTime,event=outcome)~sex+age+residenc+edu+occ+marriage,
                data=dfsur2,x=TRUE)
coxFit
lmeFit <- lme(bmi~times, data = dfbmi2, random = ~ times | id,control=ctr)
lmeFit
JMFit2 <- jointModelBayes(lmeFit, coxFit ,timeVar="times",n.iter=20000,seed=0,verbose=FALSE)#,functional_forms = fForm,n_iter=8000)
JMFit2
summary(JMFit2)
print("#######")

#ADL
print("ADL")
dflongi1 <- subset(dflongi,times!="NA" &  ADL!="NA")
dflongi1<-dflongi1[-which(dflongi1$times>dflongi1$totalTime),] #删去在事件发生之后的纵向数据记录
dfsur1 <- subset(dfsur,marriage!="NA" & sex!="NA" & age!="NA" & residenc!="NA" & 
                   edu!="NA" & occ!="NA" & totalTime!="NA")


p <- dfsur1$id
q <- dflongi1$id
w <- intersect(p,q)
dftimes <- as.data.frame(table(dflongi1$id))
# s <- dftimes$Var1[which(dftimes$Freq>=2)]
# w <- intersect(s,w)
dfsur2 <- dfsur1[which(dfsur1$id %in% w),]
dflongi2 <- dflongi1[which(dflongi1$id %in% w),]
coxFit <- coxph(Surv(totalTime,event=outcome)~sex+age+residenc+edu+occ+marriage,
                data=dfsur2,x=TRUE)
coxFit
lmeFit <- lme(ADL~times, data = dflongi2, random = ~ times | id,control=ctr)
lmeFit
JMFit3 <- jointModelBayes(lmeFit, coxFit ,timeVar="times",n.iter=20000,seed=0,verbose=FALSE)#,functional_forms = fForm)
JMFit3
summary(JMFit3)