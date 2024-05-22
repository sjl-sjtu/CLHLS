library(JMbayes)
library(tidyverse)

dfsur <- read_csv("dfbase_filtered.csv")
dflongi <-read_csv(("dflongi_filtered.csv"))

set.seed(1)

ctr <- lmeControl(maxIter = 50000, msMaxIter = 50000, tolerance = 1e-6, niterEM = 25000,
                  msMaxEval = 200000,opt="optim")

#swb
print("SWB")
dflongi1 <- subset(dflongi,times!="NA" &  SWB!="NA")
dfsur1 <- subset(dfsur,mar!="NA" & sex!="NA" & age!="NA" & res!="NA" & 
                   edu!="NA" & occ!="NA" & totalTime!="NA")
dftimes <- as.data.frame(table(dflongi1$id))
p <- dfsur1$id
q <- dflongi1$id
w <- intersect(p,q)
dfsur2 <- dfsur1[which(dfsur1$id %in% w),]
dflongi2 <- dflongi1[which(dflongi1$id %in% w),]
dflongi2$SWB <- dflongi2$SWB/40 #标准化
coxFit <- coxph(Surv(totalTime,event=outcome)~sex+age+res+edu+occ+mar,
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
  write_csv("SWB_assoc_scale.csv")

print("#######")

#bmi
print("bmi")
dflongi1 <- subset(dflongi,times!="NA" & BMI!="NA")
dfsur1 <- subset(dfsur,mar!="NA" & sex!="NA" & age!="NA" & res!="NA" & 
                   edu!="NA" & occ!="NA" & totalTime != "NA")
p <- dfsur1$id
q <- dflongi1$id
w <- intersect(p,q)
dftimes <- as.data.frame(table(dflongi1$id))
dfsur2 <- dfsur1[which(dfsur1$id %in% w),]
dflongi2 <- dflongi1[which(dflongi1$id %in% w),]
dflongi2$BMI <- scale(dflongi2$BMI) #标准化
coxFit <- coxph(Surv(totalTime,event=outcome)~sex+age+res+edu+occ+mar,
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
  write_csv("BMI_assoc_scale.csv")

print("#######")

#ADL
print("ADL")
dflongi1 <- subset(dflongi,times!="NA" &  ADL!="NA")
dfsur1 <- subset(dfsur,mar!="NA" & sex!="NA" & age!="NA" & res!="NA" & 
                   edu!="NA" & occ!="NA" & totalTime!="NA")


p <- dfsur1$id
q <- dflongi1$id
w <- intersect(p,q)
dftimes <- as.data.frame(table(dflongi1$id))
dfsur2 <- dfsur1[which(dfsur1$id %in% w),]
dflongi2 <- dflongi1[which(dflongi1$id %in% w),]
dflongi2$ADL <- dflongi2$ADL/12 #标准化
coxFit <- coxph(Surv(totalTime,event=outcome)~sex+age+res+edu+occ+mar,
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
  write_csv("ADL_assoc_scale.csv")