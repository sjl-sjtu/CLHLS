library(JMbayes)
library(tidyverse)

dfsur <- read_csv("dfsur.csv")
dflongi <-read_csv(("dflongi2.csv"))

dfsur$totalTime <- dfsur$totalTime/365
dflongi$totalTime <- dflongi$totalTime/365
dflongi$times <- dflongi$times/365
dflongi$ADL <- dflongi$ADL/12
# dfsur <- dfsur[-which(dfsur$totalTime==0),] #删去入组即患病
# dfsur <- dfsur[-which(dfsur$totalTime==0&dfsur$outcome==1),] #删去入组即患病
dfsur <- dfsur[-which(dfsur$totalTime==0),] #删去入组即患病或者删失
set.seed(1)

ctr <- lmeControl(maxIter = 50000, msMaxIter = 50000, tolerance = 1e-6, niterEM = 25000,
                  msMaxEval = 200000,opt="optim")

print("younger")

#emotion
print("emo")
dfsur1 <- dfsur%>%
               drop_na("sex","age","residenc","edu","occ","marriage","totalTime")%>%
               filter(age<80)


dflongi2 <- dflongi%>%
                   drop_na(times,emo)%>%
                   filter(times<=totalTime)%>%
                   filter(id %in% dfsur1$id)

dfsur2 <- dfsur1%>%
                filter(id %in% dflongi2$id)
        


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
dfsur1 <- dfsur%>%
               drop_na("sex","age","residenc","edu","occ","marriage","totalTime")%>%filter(age<80)


dfbmi2 <- dflongi%>%
                   drop_na("times","bmi")%>%
                   filter(times<=totalTime)%>%
                   filter(id %in% dfsur1$id)

dfsur2 <- dfsur1%>%
                filter(id %in% dfbmi2$id)

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

dfsur1 <- dfsur%>%
               drop_na("sex","age","residenc","edu","occ","marriage","totalTime")%>%filter(age<80)

dflongi2 <- dflongi%>%
                   drop_na("times","ADL")%>%
                   filter(times<=totalTime)%>%
                   filter(id %in% dfsur1$id)

dfsur2 <- dfsur1%>%
                filter(id %in% dflongi2$id)

coxFit <- coxph(Surv(totalTime,event=outcome)~sex+age+residenc+edu+occ+marriage,
                data=dfsur2,x=TRUE)
coxFit
lmeFit <- lme(ADL~times, data = dflongi2, random = ~ times | id,control=ctr)
lmeFit
JMFit3 <- jointModelBayes(lmeFit, coxFit ,timeVar="times",n.iter=20000,seed=0,verbose=FALSE)#,functional_forms = fForm)
JMFit3
summary(JMFit3)

print("#######")

print("elder")

#emotion
print("emo")


dfsur1 <- dfsur%>%
               drop_na("sex","age","residenc","edu","occ","marriage","totalTime")%>%filter(age>=80)
dflongi2 <- dflongi%>%
                   drop_na("times","emo")%>%
                   filter(times<=totalTime)%>%
                   filter(id %in% dfsur1$id)

dfsur2 <- dfsur1%>%
                filter(id %in% dflongi2$id)


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

dfsur1 <- dfsur%>%
               drop_na("sex","age","residenc","edu","occ","marriage","totalTime")%>%filter(age>=80)

dfbmi2 <- dflongi%>%
                   drop_na("times","bmi")%>%
                   filter(times<=totalTime)%>%
                   filter(id %in% dfsur1$id)

dfsur2 <- dfsur1%>%
                filter(id %in% dfbmi2$id)

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


dfsur1 <- dfsur%>%
               drop_na("sex","age","residenc","edu","occ","marriage","totalTime")%>%filter(age>=80)

dflongi2 <- dflongi%>%
                   drop_na("times","ADL")%>%
                   filter(times<=totalTime)%>%
                   filter(id %in% dfsur1$id)

dfsur2 <- dfsur1%>%
                filter(id %in% dflongi2$id)

coxFit <- coxph(Surv(totalTime,event=outcome)~sex+age+residenc+edu+occ+marriage,
                data=dfsur2,x=TRUE)
coxFit
lmeFit <- lme(ADL~times, data = dflongi2, random = ~ times | id,control=ctr)
lmeFit
JMFit3 <- jointModelBayes(lmeFit, coxFit ,timeVar="times",n.iter=20000,seed=0,verbose=FALSE)#,functional_forms = fForm)
JMFit3
summary(JMFit3)
