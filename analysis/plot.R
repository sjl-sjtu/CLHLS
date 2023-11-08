setwd("D:/OneDrive/new")
setwd("C:/Users/DELL/OneDrive/new")
setwd("F:/OneDrive/new")
library("tidyverse")
#df <- read_csv("./predict_10f.csv")
df <- read_csv("predict_new.csv")
df1 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),auc=t(df[1,1:12]))
df2 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),auc=t(df[2,1:12]))
df3 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),auc=t(df[3,1:12]))
df1$variable <- "EMO"
df2$variable <- "ADL"
df3$variable <- "BMI"
dfa <- bind_rows(df1,df2,df3)
dfa$t <- factor(dfa$t,labels = c("t=4","t=8","t=10"))

dfsd <- read_csv("./predict_new_sd.csv")
df1 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),sd=t(dfsd[1,1:12]))
df2 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),sd=t(dfsd[2,1:12]))
df3 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),sd=t(dfsd[3,1:12]))
df1$variable <- "EMO"
df2$variable <- "ADL"
df3$variable <- "BMI"
dfb <- bind_rows(df1,df2,df3)
dfb$t <- factor(dfa$t,labels = c("t=4","t=8","t=10"))

dfa$sd <- dfb$sd
library(latex2exp)

library(ggthemes)
library(ggsci)
ggplot(dfa)+geom_point(aes(x=deltat,y=auc,colour=variable),size=1)+
  geom_line(aes(x=deltat,y=auc,colour=variable),linewidth=0.5)+
  facet_grid(~t)+
  scale_x_continuous(breaks = c(2,5,7,10))+
  #scale_y_continuous(breaks = seq(0.74,0.86,0.02))+
  xlab(TeX("$\\Delta t$"))+
  ylab("AUC")+
  ylim(c(0.65,0.95))+
 # geom_errorbar(aes(x=deltat,y=auc,colour=variable,ymin=auc-sd, ymax=auc+sd), width=.2,
 #               position=position_dodge(0.05))+
  theme_few()+
  scale_color_lancet()+
  theme(plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(face = "bold.italic"),
        strip.background = element_rect(fill="grey"))+
  ggtitle("Bayesian joint model")
  
ggsave("bjm.png", width = 5, height = 5, dpi = 600)



t<-c(c(2,5,7,10))
z1=c(0.7887408,0.7766274,0.7667983,0.7557138)
z2=c(0.7944512,0.8211321,0.8358516,0.8535598)
z3=c(0.7915430,0.7734627,0.7621138,0.7473239)
z4=c(0.7983705,0.8243510,0.8358798,0.8511404)
z5=c(0.7899332,0.7792529,0.7672427,0.7550594)
z6=c(0.7945496,0.8245858,0.8414416,0.8578185)
dfb <- data.frame(t=as.character(rep(t,3)),incident=c(z1,z3,z5),cumulative=c(z2,z4,z6))
library(reshape2)
dfb <- melt(dfb,id.vars="t",variable.name="type",value.name = "AUC")
dfb$t <- as.numeric(as.character(dfb$t))
dfb$variable <- rep(c("EMO","ADL","BMI"),each=4)
ggplot(dfb)+geom_point(aes(x=t,y=AUC,colour=variable),size=1)+
  geom_line(aes(x=t,y=AUC,colour=variable,linetype=type),linewidth=0.5)+
  scale_x_continuous(breaks = t)+
  ylim(c(0.65,0.95))+
  scale_linetype_manual(values=c("solid", "dashed"))+
  #scale_y_continuous(breaks = seq(0.74,0.86,0.02))+
  xlab("t")+
  ylab("AUC")+
  theme_few()+
  scale_color_lancet()+
  theme(plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(face = "bold.italic"),
        strip.background = element_rect(fill="grey"))+
  ggtitle("baseline Cox model")

ggsave("y.png", width = 4, height = 5, dpi = 600)

dflongi <- read_csv("./dflongi2.csv")
library(car)
dflongi$outcomes <- recode(dflongi$outcomes,"'illness'='CI';'health'='non-CI'")
dflongi$totalTime <- dflongi$totalTime/365
dflongi$times <- dflongi$times/365
dflongi$ADL <- dflongi$ADL/12
dflongi$outcomes <- factor(dflongi$outcomes,levels = c("non-CI","CI"))

dflongi1 <- dflongi %>% drop_na(times) %>% 
  group_by(id) %>%
  summarise(n=n()) %>%
  ungroup()

dflongi <- dflongi%>%filter(id %in% (dflongi1%>%filter(n>=3)%>%pull(id)))

library(RColorBrewer)
ggplot(dflongi,aes(times,ADL))+
  facet_wrap( ~ outcomes,ncol=2)+
  geom_path(aes(group=id),linewidth=0.1,colour=brewer.pal(9,"Greys")[4])+
  geom_smooth(method="lm",linewidth=0.2,fill=brewer.pal(9,"GnBu")[5])+
  theme_few()+
  scale_color_lancet()+
  theme(plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(face = "bold.italic"),
        strip.background = element_rect(fill=brewer.pal(11,"RdYlBu")[8]))+
  xlab("Years after baseline")
ggsave("g4.png", width = 5, height = 4, dpi = 600)

ggplot(dflongi,aes(times,emo))+
  facet_wrap( ~ outcomes,ncol=2)+
  geom_path(aes(group=id),linewidth=0.1,colour=brewer.pal(9,"Greys")[4])+
  geom_smooth(method="lm",linewidth=0.2,fill=brewer.pal(9,"GnBu")[5])+
  theme_few()+
  scale_color_lancet()+
  theme(plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(face = "bold.italic"),
        strip.background = element_rect(fill=brewer.pal(11,"RdYlBu")[8]))+
  xlab("Years after baseline")+
  ylab("EMO")
ggsave("g5.png", width = 5, height = 4, dpi = 600)

ggplot(dflongi,aes(times,bmi))+
  facet_wrap( ~ outcomes,ncol=2)+
  geom_path(aes(group=id),linewidth=0.1,colour=brewer.pal(9,"Greys")[4])+
  geom_smooth(method="lm",linewidth=0.2,fill=brewer.pal(9,"GnBu")[5])+
  theme_few()+
  scale_color_lancet()+
  theme(plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(face = "bold.italic"),
        strip.background = element_rect(fill=brewer.pal(11,"RdYlBu")[8]))+
  xlab("Years after baseline")+
  ylab("BMI")
ggsave("g6.png", width = 5, height = 4, dpi = 600)
#getwd()


#Granger
library(plm)
dflongi1 <- dflongi %>% drop_na(times,ADL,MSE) %>% 
  group_by(id) %>%
  summarise(n=n()) %>%
  ungroup()
table(dflongi1$n)
dflongi2 <- dflongi %>% drop_na(times,ADL,MSE) %>% 
  filter(id %in% (dflongi1%>%filter(n>=3)%>%pull(id)))
pdf_longi <- pdata.frame(dflongi2, index = c("id", "year"), 
                         drop.index = TRUE)
pgrangertest(MSE ~ ADL, data = pdf_longi, test = "Wbar")


###
devtools::install_github("seonjoo/lcca")

require(rmcorr)

dflongi <- read_csv("./dflongi2.csv")
library(car)
dflongi$outcomes <- recode(dflongi$outcomes,"'illness'='CI';'health'='non-CI'")
dflongi$totalTime <- dflongi$totalTime/365
dflongi$times <- dflongi$times/365
dflongi$ADL <- dflongi$ADL/12
dflongi$outcomes <- factor(dflongi$outcomes,levels = c("non-CI","CI"))

dflongi1 <- dflongi %>% drop_na(times,MSE,ADL) %>% 
  group_by(id) %>%
  summarise(n=n()) %>%
  ungroup()
dflongi2 <- dflongi %>% drop_na(times,MSE,ADL) %>% 
  filter(id %in% (dflongi1%>%filter(n>=4)%>%pull(id)))
rc <- rmcorr(id,ADL,MSE,dflongi2)
png( 
  filename = "ADL_MSE.png", # 文件名称
  width = 3500,           # 宽
  height = 3000,          # 高
  units = "px",          # 单位
  bg = "white",          # 背景颜色
  res = 600)              # 分辨率
plot(rc, ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5)
dev.off()
rc

dflongi1 <- dflongi %>% drop_na(times,MSE,emo) %>% 
  group_by(id) %>%
  summarise(n=n()) %>%
  ungroup()
dflongi2 <- dflongi %>% drop_na(times,MSE,emo) %>% 
  filter(id %in% (dflongi1%>%filter(n>=4)%>%pull(id)))
rc1 <- rmcorr(id,emo,MSE,dflongi2)
png( 
  filename = "emo_MSE.png", # 文件名称
  width = 3500,           # 宽
  height = 3000,          # 高
  units = "px",          # 单位
  bg = "white",          # 背景颜色
  res = 600)              # 分辨率
plot(rc1,xlab="EMO", ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5)
dev.off()
rc1

dflongi1 <- dflongi %>% drop_na(times,MSE,bmi) %>% 
  group_by(id) %>%
  summarise(n=n()) %>%
  ungroup()
dflongi2 <- dflongi %>% drop_na(times,MSE,bmi) %>% 
  filter(id %in% (dflongi1%>%filter(n>=4)%>%pull(id)))
rc3 <- rmcorr(id,bmi,MSE,dflongi2)
png( 
  filename = "BMI_MSE.png", # 文件名称
  width = 3500,           # 宽
  height = 3000,          # 高
  units = "px",          # 单位
  bg = "white",          # 背景颜色
  res = 600)              # 分辨率
plot(rc3, xlab="BMI",ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5)
dev.off()
rc3


##########
df <- read_csv("deephit.csv")
df <- df%>%pivot_longer(-c(model,pred),names_to = "eval",values_to = "cindex")
df$model <- factor(df$model,labels = c("ADL+EMO+BMI","ADL+BMI","ADL+EMO",
                                       "EMO+BMI","BMI","ADL","EMO","benchmark"))#,
df
library(data.table)
df <- as.data.table(df)
df
df[,model:=fct_relevel(df$model,"ADL","BMI","EMO","ADL+BMI","ADL+EMO",
                       "EMO+BMI","ADL+EMO+BMI","benchmark")]
df
df$pred <- factor(df$pred,labels = c("t=4","t=6","t=8","t=10"))
df$eval <- factor(df$eval,levels = c(2,5,7,10))
library(latex2exp)
library(ggthemes)
library(ggsci)
ggplot(df)+geom_point(aes(x=eval,y=cindex,colour=model),size=1)+
  geom_line(aes(x=eval,y=cindex,colour=model,group=model),linewidth=0.5)+
  facet_grid(~pred)+
  #scale_x_continuous(breaks = c(2,5,7,10))+
  #scale_y_continuous(breaks = seq(0.74,0.86,0.02))+
  xlab(TeX("$\\Delta t$"))+
  ylab("C-index")+
  ylim(c(0.65,0.85))+
  # geom_errorbar(aes(x=deltat,y=auc,colour=variable,ymin=auc-sd, ymax=auc+sd), width=.2,
  #               position=position_dodge(0.05))+
  theme_few()+
  scale_color_lancet()+
  theme(plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(face = "bold.italic"),
        strip.background = element_rect(fill="grey"))+
  ggtitle("Dynamic-DeepHit")
ggsave("deephit.png", width = 9, height = 5, dpi = 600)




##########
dflongi <- read_csv("./dflongi2.csv")
library(car)
dflongi$outcomes <- recode(dflongi$outcomes,"'illness'='CI';'health'='non-CI'")
dflongi$totalTime <- dflongi$totalTime/365
dflongi$times <- dflongi$times/365
dflongi$ADL <- dflongi$ADL/12
dflongi$outcomes <- factor(dflongi$outcomes,levels = c("non-CI","CI"))


df <- dflongi %>% 
  drop_na(times) %>%
  # group_by(id) %>%
  # mutate(t=seq(1,n())) %>%
  # ungroup() %>%
  mutate(outc = as.integer(outcome==1&times>=totalTime)) %>%
  select(id,times,sex,age,residenc,edu,occ,marriage,ADL,emo,bmi,outc) %>%
  rename(EMO=emo,BMI=bmi,res=residenc,mar=marriage)


df$tstart <- ipw::tstartfun(id, times, as.data.frame(df[,c("id","times")]))

df
library(survival)
fit.tdc <- coxph(Surv(tstart,times,outc)~
                   sex+age+res+edu+occ+mar+ADL+EMO+BMI+cluster(id),df)
fit.tdc
library(survminer)
ggforest(fit.tdc,df,main = "Time-dependent")
ggsave("tv.tiff",width = 6, height = 3.5,dpi=600)

dfbase <- dflongi%>%filter(times==0) %>% 
  rename(EMO=emo,BMI=bmi,res=residenc,mar=marriage)
fitbase <- coxph(Surv(totalTime, outcome) ~
                sex+age+res+edu+occ+mar+ADL+EMO+BMI,
              data=dfbase)
ggforest(fitbase,dfbase,main = "Baseline")
ggsave("base.tiff",width = 6, height = 3.5,dpi=600)

dfn <- dflongi %>% 
  drop_na(times) %>%
  # group_by(id) %>%
  # mutate(t=seq(1,n())) %>%
  # ungroup() %>%
  mutate(outc = as.integer(outcome==1&times>=totalTime)) %>%
  select(id,times,sex,age,residenc,edu,occ,marriage,ADL,emo,bmi,outc,totalTime) %>%
  rename(EMO=emo,BMI=bmi,res=residenc,mar=marriage)
fit2 <- coxph(Surv(totalTime, outcome) ~
                   sex+age+res+edu+occ+mar+ADL+EMO+BMI,
                 data=dfn)

zph <- cox.zph(fitbase)
zph[7]

plot(zph[2],lwd=2)
abline(0,0, col=1,lty=3,lwd=2)
abline(h= fit2$coef[7], col=3, lwd=2, lty=2)



############
library(JMbayes)
dflongi1 <- dflongi %>% drop_na(times) %>% 
  group_by(id) %>%
  summarise(n=n()) %>%
  ungroup()
dflongi2 <- dflongi%>%filter(id %in% (dflongi1%>%filter(n>=2)%>%pull(id)))
dflongi2.id <- dfbase%>%filter(id %in% (dflongi1%>%filter(n>=2)%>%pull(id)))

MixedModelFit <- mvglmer(list(emo ~ times + (times | id),
                              bmi ~ times + (times | id),
                              ADL ~ times + (times | id)), data = dflongi2,
                         families = list(gaussian, gaussian,gaussian))
summary(MixedModelFit)
coxFit <- coxph(Surv(totalTime,event=outcome)~sex+age+residenc+edu+occ+marriage,
                data=dflongi2.id,model = TRUE)
coxFit
JMFit <- mvJointModelBayes(MixedModelFit,coxFit,timeVar = "times",n.iter=30,seed=0)
JMFit

############
dflongi <- read_csv("./dflongi2.csv")
library(car)
dflongi$outcomes <- recode(dflongi$outcomes,"'illness'='CI';'health'='non-CI'")
dflongi$totalTime <- dflongi$totalTime/365
dflongi$times <- dflongi$times/365
dflongi$ADL <- dflongi$ADL/12
dflongi$outcomes <- factor(dflongi$outcomes,levels = c("non-CI","CI"))

library(ipw)
dflongi1 <- dflongi %>% drop_na() %>% 
  group_by(id) %>%
  summarise(n=n()) %>%
  ungroup()
dflongi2 <- dflongi%>%filter(id %in% (dflongi1%>%filter(n>=1)%>%pull(id))) %>%
  mutate_at(vars(id),as.character)
df <- dflongi2 %>% 
  drop_na() %>%
  arrange(id,year) %>%
  group_by(id) %>%
  mutate(t=seq(0,n()-1)) %>%
  ungroup() %>%
  mutate(outc = as.integer(outcome==1&times>=totalTime)) %>%
  select(id,t,sex,age,residenc,edu,occ,marriage,ADL,emo,bmi,outc)


df$tstart <- ipw::tstartfun(id, t, as.data.frame(df[,c("id","t")]))
# 
# dfid <- df %>% drop_na() %>% filter(t==0)%>% pull(id)
# df1 <- df%>% drop_na()%>%filter(id%in%dfid)

temp <- ipwtm(exposure = ADL, family = "gaussian",
              numerator = ~sex+age+residenc+edu+occ+marriage, 
              denominator = ~emo+bmi+sex+age+residenc+edu+occ+marriage,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df),
              corstr = "ar1")

ipwplot(weights = temp$ipw.weights, timevar = df$t,
         binwidth = 1, main = "Stabilized weights for ADL",
         xlab = "follow-up",ylab="stabilized weights")

library(survival)
coxm <- coxph(Surv(tstart, t, outc) ~ ADL + cluster(id),
              data = df, weights = temp$ipw.weights)
summary(coxm)

library(ggpubr)
data.frame(ipw=temp$ipw.weights,timevar=df$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
library(tableone)
ShowRegTable(coxm,digits = 3)


temp <- ipwtm(exposure = emo, family = "gaussian",
              numerator = ~sex+age+residenc+edu+occ+marriage, 
              denominator = ~ADL+bmi+sex+age+residenc+edu+occ+marriage,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df),
              corstr = "ar1")

ipwplot(weights = temp$ipw.weights, timevar = df$t,
        binwidth = 1, main = "Stabilized weights for Emotion Index",
        xlab = "follow-up",ylab="stabilized weights")

coxm <- coxph(Surv(tstart, t, outc) ~ emo + cluster(id),
              data = df, weights = temp$ipw.weights)
summary(coxm)

data.frame(ipw=temp$ipw.weights,timevar=df$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

temp <- ipwtm(exposure = bmi, family = "gaussian",
              numerator = ~sex+age+residenc+edu+occ+marriage, 
              denominator = ~ADL+emo+sex+age+residenc+edu+occ+marriage,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df),
              corstr = "ar1")

ipwplot(weights = temp$ipw.weights, timevar = df$t,
        binwidth = 1, main = "Stabilized weights for BMI",
        xlab = "follow-up",ylab="stabilized weights")

coxm <- coxph(Surv(tstart, t, outc) ~ bmi + cluster(id),
              data = df, weights = temp$ipw.weights)
summary(coxm)

data.frame(ipw=temp$ipw.weights,timevar=df$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)


### subset
df1 <- df %>% filter(sex==0)
temp <- ipwtm(exposure = ADL, family = "gaussian",
              numerator = ~age+residenc+edu+occ+marriage, 
              denominator = ~emo+bmi+age+residenc+edu+occ+marriage,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p1 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
        binwidth = 1, main = "Stabilized weights for ADL (Female)",
        xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ ADL + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

temp <- ipwtm(exposure = emo, family = "gaussian",
              numerator = ~age+residenc+edu+occ+marriage, 
              denominator = ~ADL+bmi+age+residenc+edu+occ+marriage,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p2 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for EMO (Female)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ emo + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

temp <- ipwtm(exposure = bmi, family = "gaussian",
              numerator = ~age+residenc+edu+occ+marriage, 
              denominator = ~ADL+emo+age+residenc+edu+occ+marriage,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p3 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for BMI (Female)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ bmi + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

df1 <- df %>% filter(sex==1)
temp <- ipwtm(exposure = ADL, family = "gaussian",
              numerator = ~age+residenc+edu+occ+marriage, 
              denominator = ~emo+bmi+age+residenc+edu+occ+marriage,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p1 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for ADL (Male)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ ADL + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

temp <- ipwtm(exposure = emo, family = "gaussian",
              numerator = ~age+residenc+edu+occ+marriage, 
              denominator = ~ADL+bmi+age+residenc+edu+occ+marriage,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p2 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for EMO (Male)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ emo + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

temp <- ipwtm(exposure = bmi, family = "gaussian",
              numerator = ~age+residenc+edu+occ+marriage, 
              denominator = ~ADL+emo+age+residenc+edu+occ+marriage,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p3 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for BMI (Male)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ bmi + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

### age subset
df1 <- df %>% filter(age<80)
temp <- ipwtm(exposure = ADL, family = "gaussian",
              numerator = ~sex+age+residenc+edu+occ+marriage, 
              denominator = ~emo+bmi+sex+age+residenc+edu+occ+marriage,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p1 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for ADL (<80)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ ADL + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

temp <- ipwtm(exposure = emo, family = "gaussian",
              numerator = ~sex+age+residenc+edu+occ+marriage, 
              denominator = ~ADL+bmi+sex+age+residenc+edu+occ+marriage,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p2 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for EMO (<80)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ emo + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

temp <- ipwtm(exposure = bmi, family = "gaussian",
              numerator = ~sex+age+residenc+edu+occ+marriage, 
              denominator = ~ADL+emo+sex+age+residenc+edu+occ+marriage,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p3 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for BMI (<80)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ bmi + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

df1 <- df %>% filter(age>=80)
temp <- ipwtm(exposure = ADL, family = "gaussian",
              numerator = ~sex+age+residenc+edu+occ+marriage, 
              denominator = ~emo+bmi+sex+age+residenc+edu+occ+marriage,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p1 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for ADL (>=80)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ ADL + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

temp <- ipwtm(exposure = emo, family = "gaussian",
              numerator = ~sex+age+residenc+edu+occ+marriage, 
              denominator = ~ADL+bmi+sex+age+residenc+edu+occ+marriage,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p2 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for EMO (>=80)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ emo + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

temp <- ipwtm(exposure = bmi, family = "gaussian",
              numerator = ~sex+age+residenc+edu+occ+marriage, 
              denominator = ~ADL+emo+sex+age+residenc+edu+occ+marriage,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p3 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for BMI (>=80)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ bmi + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)






# df <- read_delim("causal.txt") %>%
#   mutate(CI=sprintf("%.3f (%.3f,%.3f)",HR,L,U))
# library(forestplot)
# dt |> forestplot(labeltext = c(variable,HR,CI,P),
#            mean=HR,
#            lower=L,
#            upper=U,
#            zero=1,
#            boxsize=0.01,
#            lineheight = unit(7,'mm'),
#            colgap=unit(7,'mm'),
#            lwd.zero=0.5,
#            lwd.ci=0.5, 
#            col=fpColors(box='#458B00',
#                         summary='#8B008B',
#                         lines = 'black',
#                         zero = 'red'),
#            xlab="beta",
#            lwd.xaxis = 0.5,
#            txt_gp = fpTxtGp(ticks = gpar(cex = 0.85),
#                             xlab  = gpar(cex = 0.8),
#                             cex = 1),
#            lty.ci = "solid",
#            title = "Forestplot", 
#            line.margin = 0.2,
#            ci.vertices.height = 0.05,
#            graph.pos=4) #|>
#   # fp_set_style(box = "royalblue",
#   #              line = "darkblue",
#   #              summary = "royalblue") #|> 
#   # fp_add_header(study = c("", "Study"),
#   #               deaths_steroid = c("Deaths", "(steroid)"),
#   #               deaths_placebo = c("Deaths", "(placebo)"),
#   #               OR = c("", "OR"))
# 
# install.packages("forestploter")
# 
# library(grid)
# library(forestploter)
# 
# tm <- forest_theme(base_size = 20,
#                    refline_col = "red",
#                    arrow_type = "closed",
#                    footnote_col = "blue")
# 
# dt <- read_delim("causal.txt") %>%
#   mutate(CI=sprintf("(%.3f,%.3f)",L,U))
# dt$` ` <- paste(rep(" ", nrow(dt)), collapse = " ")
# p <- forest(dt[,c(1,2,6,7,5)],
#             est = dt$HR,
#             lower = dt$L, 
#             upper = dt$U,
#             #sizes = dt$U-dt$L,
#             ci_column = 4,
#             ref_line = 1,
#             # arrow_lab = c("Placebo Better", "Treatment Better"),
#             #xlim = c(0, 4),
#             #ticks_at = c(0.5, 1, 2, 3),
#             # footnote = "This is the demo data. Please feel free to change\nanything you want.",
#             theme = tm)
# plot(p)


# data("basdat")
# data("timedat")
# basdat[1:4,]
# timedat$cd4.sqrt <- sqrt(timedat$cd4count)
# timedat <- merge(timedat, basdat[,c("id","Ttb")], by = "id", all.x = TRUE)
# timedat$tb.lag <- ifelse(with(timedat, !is.na(Ttb) & fuptime > Ttb), 1, 0)
# times <- sort(unique(c(basdat$Ttb, basdat$Tend)))
# startstop <- data.frame(
#   id = rep(basdat$id, each = length(times)),
#   fuptime = rep(times, nrow(basdat)))
# startstop <- merge(startstop, basdat, by = "id", all.x = TRUE)
# startstop <- startstop[with(startstop, fuptime <= Tend), ]
# startstop$tstart <- tstartfun(id, fuptime, startstop)
# startstop$tb <- ifelse(with(startstop, !is.na(Ttb) & fuptime >= Ttb),
#                        + 1, 0)
# startstop$tb.lag <- ifelse(with(startstop, !is.na(Ttb) & fuptime > Ttb),
#                               + 1, 0)
# startstop$event <- ifelse(with(startstop, !is.na(Tdeath) & fuptime >=
#                                     + Tdeath), 1, 0)
# cd4.lme <- lme(cd4.sqrt ~ fuptime + tb.lag, random = ~ fuptime | id,
#                 data = timedat)
# startstop$cd4.sqrt <- predict(cd4.lme, newdata = data.frame(id =
#                                                                  + startstop$id, fuptime = startstop$fuptime, tb.lag = startstop$tb.lag))
