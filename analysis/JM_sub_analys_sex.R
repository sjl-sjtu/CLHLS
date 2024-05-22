library(JMbayes)
library(tidyverse)

dfsur <- read_csv("dfbase_filtered.csv")
dflongi <-read_csv(("dflongi_filtered.csv"))

dflongi$BMI <- scale(dflongi$BMI) #标准化
dflongi$ADL <- dflongi$ADL/12 #标准化
dflongi$SWB <- dflongi$SWB/40 #标准化


set.seed(1)

ctr <- lmeControl(maxIter = 50000, msMaxIter = 50000, tolerance = 1e-6, niterEM = 25000,
                  msMaxEval = 200000,opt="optim")

print("female")

#SWBtion
print("SWB")
dfsur1 <- dfsur%>%
               drop_na("sex","age","res","edu","occ","mar","totalTime")%>%
               filter(sex==0)


dflongi2 <- dflongi%>%
                   drop_na(times,SWB)%>%
                   filter(times<=totalTime)%>%
                   filter(id %in% dfsur1$id)

dfsur2 <- dfsur1%>%
                filter(id %in% dflongi2$id)


coxFit <- coxph(Surv(totalTime,event=outcome)~age+res+edu+occ+mar,
                data=dfsur2,x=TRUE)
coxFit
lmeFit <- lme(SWB~times, data = dflongi2,random = ~ times | id,control = ctr)
lmeFit
JMFit <- jointModelBayes(lmeFit, coxFit ,timeVar="times",n.iter=20000,seed=0,verbose=FALSE)
JMFit
summary(JMFit)
summary(JMFit)$`CoefTable-Event` %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "covariate") %>% 
  write_csv("SWB_assoc_female_scale.csv")
print("#######")

#BMI
print("BMI")
dfsur1 <- dfsur%>%
               drop_na("sex","age","res","edu","occ","mar","totalTime")%>%filter(sex==0)


dflongi2 <- dflongi%>%
                   drop_na("times","BMI")%>%
                   filter(times<=totalTime)%>%
                   filter(id %in% dfsur1$id)

dfsur2 <- dfsur1%>%
                filter(id %in% dflongi2$id)

coxFit <- coxph(Surv(totalTime,event=outcome)~age+res+edu+occ+mar,
                data=dfsur2,x=TRUE)
coxFit
lmeFit <- lme(BMI~times, data = dflongi2, random = ~ times | id,control=ctr)
lmeFit
JMFit2 <- jointModelBayes(lmeFit, coxFit ,timeVar="times",n.iter=20000,seed=0,verbose=FALSE)#,functional_forms = fForm,n_iter=8000)
JMFit2
summary(JMFit2)
summary(JMFit2)$`CoefTable-Event` %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "covariate") %>% 
  write_csv("BMI_assoc_female_scale.csv")
print("#######")

#ADL
print("ADL")

dfsur1 <- dfsur%>%
               drop_na("sex","age","res","edu","occ","mar","totalTime")%>%filter(sex==0)

dflongi2 <- dflongi%>%
                   drop_na("times","ADL")%>%
                   filter(times<=totalTime)%>%
                   filter(id %in% dfsur1$id)

dfsur2 <- dfsur1%>%
                filter(id %in% dflongi2$id)

coxFit <- coxph(Surv(totalTime,event=outcome)~age+res+edu+occ+mar,
                data=dfsur2,x=TRUE)
coxFit
lmeFit <- lme(ADL~times, data = dflongi2, random = ~ times | id,control=ctr)
lmeFit
JMFit3 <- jointModelBayes(lmeFit, coxFit ,timeVar="times",n.iter=20000,seed=0,verbose=FALSE)#,functional_forms = fForm)
JMFit3
summary(JMFit3)
summary(JMFit3)$`CoefTable-Event` %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "covariate") %>% 
  write_csv("ADL_assoc_female_scale.csv")

print("#######")

print("male")

#SWBtion
print("SWB")


dfsur1 <- dfsur%>%
               drop_na("sex","age","res","edu","occ","mar","totalTime")%>%filter(sex==1)
dflongi2 <- dflongi%>%
                   drop_na("times","SWB")%>%
                   filter(times<=totalTime)%>%
                   filter(id %in% dfsur1$id)

dfsur2 <- dfsur1%>%
                filter(id %in% dflongi2$id)

coxFit <- coxph(Surv(totalTime,event=outcome)~age+res+edu+occ+mar,
                data=dfsur2,x=TRUE)
coxFit
lmeFit <- lme(SWB~times, data = dflongi2,random = ~ times | id,control = ctr)
lmeFit
JMFit <- jointModelBayes(lmeFit, coxFit ,timeVar="times",n.iter=20000,seed=0,verbose=FALSE)
JMFit
summary(JMFit)
summary(JMFit)$`CoefTable-Event` %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "covariate") %>% 
  write_csv("SWB_assoc_male_scale.csv")
print("#######")

#BMI
print("BMI")

dfsur1 <- dfsur%>%
               drop_na("sex","age","res","edu","occ","mar","totalTime")%>%filter(sex==1)

dflongi2 <- dflongi%>%
                   drop_na("times","BMI")%>%
                   filter(times<=totalTime)%>%
                   filter(id %in% dfsur1$id)

dfsur2 <- dfsur1%>%
                filter(id %in% dflongi2$id)

coxFit <- coxph(Surv(totalTime,event=outcome)~age+res+edu+occ+mar,
                data=dfsur2,x=TRUE)
coxFit
lmeFit <- lme(BMI~times, data = dflongi2, random = ~ times | id,control=ctr)
lmeFit
JMFit2 <- jointModelBayes(lmeFit, coxFit ,timeVar="times",n.iter=20000,seed=0,verbose=FALSE)#,functional_forms = fForm,n_iter=8000)
JMFit2
summary(JMFit2)
summary(JMFit2)$`CoefTable-Event` %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "covariate") %>% 
  write_csv("BMI_assoc_male_scale.csv")
print("#######")

#ADL
print("ADL")


dfsur1 <- dfsur%>%
               drop_na("sex","age","res","edu","occ","mar","totalTime")%>%filter(sex==1)

dflongi2 <- dflongi%>%
                   drop_na("times","ADL")%>%
                   filter(times<=totalTime)%>%
                   filter(id %in% dfsur1$id)

dfsur2 <- dfsur1%>%
                filter(id %in% dflongi2$id)

coxFit <- coxph(Surv(totalTime,event=outcome)~age+res+edu+occ+mar,
                data=dfsur2,x=TRUE)
coxFit
lmeFit <- lme(ADL~times, data = dflongi2, random = ~ times | id,control=ctr)
lmeFit
JMFit3 <- jointModelBayes(lmeFit, coxFit ,timeVar="times",n.iter=20000,seed=0,verbose=FALSE)#,functional_forms = fForm)
JMFit3
summary(JMFit3)
summary(JMFit3)$`CoefTable-Event` %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "covariate") %>% 
  write_csv("ADL_assoc_male_scale.csv")
