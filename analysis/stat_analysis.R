setwd("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS")
# setwd("C:/Users/DELL/OneDrive/new")
# setwd("F:/OneDrive/new")
library(tidyverse)
library(latex2exp)
library(ggthemes)
library(ggsci)
library(RColorBrewer)
library(patchwork)


dfbase <- read_csv("dfbase_filtered.csv")
dflongi <- read_csv("dflongi_filtered.csv")

#####################
# baseline descriptive
df <- read_csv("dfbase_filtered.csv")
feat=c("sex","age","res","edu","occ","mar","SWB","ADL","BMI","outcome")

feat
for (col in feat[c(1,2,3,4,5,6)]) {
  freq <- prop.table(table(df[[col]]))
  crs <- prop.table(table(df[[col]],df$outcomes))
  crs <- apply(crs,2,function(x) x/sum(x))
  print(paste("Column:", col))
  print(round(freq,4))
  print(round(crs,4))
  print("--------------------")
}
colMeans(df[,c("age_y","edu_y","SWB","ADL","BMI")],na.rm=T) |> round(2)
aggregate(. ~ outcome, data = df[,c("age_y","edu_y","SWB","ADL","BMI","outcome")], FUN = mean) |> round(2)

table(df$outcomes)
prop.table(table(df$outcome))
nrow(df)

table(dflongi$outcomes)
nrow(dflongi)


### correlation
library("corrplot")
dat <- df%>%select(c(any_of(feat)))
dat
# png("sug1.png",res =300)
mcor <- cor(dat,use="pairwise.complete.obs")   #计算相关系数
q=wrap_elements(~corrplot(mcor,tl.cex=1,
                          method="circle",
                          type="upper",shade.col=NA,tl.col="black", 
                          tl.offset = 0.7,tl.srt=90,cl.cex=1,
                          cl.offset = 10,addCoef.col="black"))  #作图，调节图例字???

mcor <- cor(dat,use="pairwise.complete.obs",method="spearman")   #计算相关系数
q1 <- wrap_elements(~corrplot(mcor,tl.cex=1,method="circle",
                              type="upper",shade.col=NA,
                              tl.col="black", tl.offset = 0.7,
                              tl.srt=90,cl.cex=1,cl.offset = 10,
                              addCoef.col="black"))  #作图，调节图例字???
q+q1+plot_annotation(tag_levels = 'A')
ggsave("cor.png",dpi=600,width = 12,height=7)

## baseline test
chisq.test(df$sex,df$outcome)
t.test(age_y~outcome,data=df)
chisq.test(df$age,df$outcome)
chisq.test(df$res,df$outcome)
t.test(edu_y~outcome,data=df)
chisq.test(df$edu,df$outcome)
chisq.test(df$occ,df$outcome)
chisq.test(df$mar,df$outcome)
t.test(SWB~outcome,data=df)
t.test(ADL~outcome,data=df)
t.test(BMI~outcome,data=df)

### enroll and censoring
dft <- read_csv("dftimes.csv")
table(dft$enT)
table(dft$enT)|>sum()
table(dft%>%filter(outcome==1)%>%pull(ouT))
table(dft%>%filter(outcome==1)%>%pull(ouT)) |> sum()
table(dft%>%filter(outcome==0)%>%pull(ouT))
table(dft%>%filter(outcome==0)%>%pull(ouT)) |> sum()
df <- read_csv("dfbase_filtered.csv")
dft <- dft %>% left_join(df[,c("id",feat[-10])],by="id") %>%
  mutate(age_p = case_when(age_y<=70~'60~70',age_y>70&age_y<=80~'71~80',
                           age_y>80&age_y<=90~'81~90',age_y>90&age_y<=100~'91~100',
                           age_y>100~'above 100'))
tt <- table(dft$age_p,dft$num) |> t()
tt
margin.table(tt, 1)
margin.table(tt, 2)

# dft <- read_csv("dftimes.csv")%>%
#   filter(id%in%df$id)
# lapply(dft%>%select(eN,iN), table)
# table(dft%>%filter(id%in%(df%>%filter(outcome==0)%>%pull(id)))%>%pull(oN))
# table(dft$eN) |> sum()
# table(dft$iN) |> sum()
# table(dft%>%filter(id%in%(df%>%filter(outcome==0)%>%pull(id)))%>%pull(oN)) |> sum()

#############
### KM estimator
library(survival)
library(survminer)
fit <- survfit(Surv(totalTime, outcome) ~ sex, data = df)
q1 <- ggsurvplot(fit, data = df, risk.table = F, pval = TRUE,
           pval.method = TRUE, surv.median.line="v",conf.int=T)+
  xlab("Time (Years)")
fit <- survfit(Surv(totalTime, outcome) ~ age, data = df)
q2 <- ggsurvplot(fit, data = df, risk.table = F, pval = TRUE,
                 pval.method = TRUE, surv.median.line="v",conf.int=T)+
  xlab("Time (Years)")
fit <- survfit(Surv(totalTime, outcome) ~ res, data = df)
q3 <- ggsurvplot(fit, data = df, risk.table = F, pval = TRUE,
                                pval.method = TRUE, surv.median.line="v",conf.int=T)+
                      xlab("Time (Years)")
fit <- survfit(Surv(totalTime, outcome) ~ edu, data = df)
q4 <- ggsurvplot(fit, data = df, risk.table = F, pval = TRUE,
                 pval.method = TRUE, surv.median.line="v",conf.int=T)+
  xlab("Time (Years)")
fit <- survfit(Surv(totalTime, outcome) ~ occ, data = df)
q5 <- ggsurvplot(fit, data = df, risk.table = F, pval = TRUE,
                                pval.method = TRUE, surv.median.line="v",conf.int=T)+
                      xlab("Time (Years)")
fit <- survfit(Surv(totalTime, outcome) ~ mar, data = df)
q6 <- ggsurvplot(fit, data = df, risk.table = F, pval = TRUE,
                                pval.method = TRUE, surv.median.line="v",conf.int=T)+
                      xlab("Time (Years)")

q <- arrange_ggsurvplots(list(q1,q2,q3,q4,q5,q6),print = TRUE,  nrow=3)
ggsave("KM.png",q,dpi=600,width = 11,height = 11)

####
# longitudinal trend
dfti <- dflongi %>% drop_na(times) %>% 
  group_by(id) %>%
  summarise(n=n()) %>%
  ungroup()
p1 <- ggplot(dflongi,aes(times,ADL))+
  facet_wrap( ~ outcomes,ncol=2)+
  geom_path(aes(group=id),linewidth=0.1,colour=brewer.pal(9,"Greys")[4])+
  geom_smooth(method="lm",linewidth=0.2,fill=brewer.pal(9,"GnBu")[5])+
  # geom_smooth(method="loess",linewidth=0.2,color=brewer.pal(9,"PuRd")[8],fill=brewer.pal(9,"PuRd")[5])+
  theme_few()+
  scale_color_lancet()+
  theme(plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(face = "bold.italic"),
        strip.background = element_rect(fill=brewer.pal(11,"RdYlBu")[8]))+
  xlab("Years after baseline")
# ggsave("g4.png", width = 5, height = 4, dpi = 600)

p2 <- ggplot(dflongi,aes(times,SWB))+
  facet_wrap( ~ outcomes,ncol=2)+
  geom_path(aes(group=id),linewidth=0.1,colour=brewer.pal(9,"Greys")[4])+
  geom_smooth(method="lm",linewidth=0.2,fill=brewer.pal(9,"GnBu")[5])+
  # geom_smooth(method="gam",linewidth=0.2,color=brewer.pal(9,"PuRd")[8],fill=brewer.pal(9,"PuRd")[5])+
  theme_few()+
  scale_color_lancet()+
  theme(plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(face = "bold.italic"),
        strip.background = element_rect(fill=brewer.pal(11,"RdYlBu")[8]))+
  xlab("Years after baseline")+
  ylab("SWB")
# ggsave("g5.png", width = 5, height = 4, dpi = 600)

p3 <- ggplot(dflongi,aes(times,BMI))+
  facet_wrap( ~ outcomes,ncol=2)+
  geom_path(aes(group=id),linewidth=0.1,colour=brewer.pal(9,"Greys")[4])+
  geom_smooth(method="lm",linewidth=0.2,fill=brewer.pal(9,"GnBu")[5])+
  # geom_smooth(method="loess",linewidth=0.2,color=brewer.pal(9,"PuRd")[8],fill=brewer.pal(9,"PuRd")[5])+
  theme_few()+
  scale_color_lancet()+
  theme(plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(face = "bold.italic"),
        strip.background = element_rect(fill=brewer.pal(11,"RdYlBu")[8]))+
  xlab("Years after baseline")+
  ylab("BMI")
# ggsave("g6.png", width = 5, height = 4, dpi = 600)
#getwd()

p1+p2+p3+plot_annotation(tag_levels = 'a')
ggsave("Fig1.pdf",width=12,height=5)
# ggsave("longiChange.tiff",dpi=600)


###
# repeated-measurement correlation
library(rmcorr)

dflongi1 <- dflongi %>% drop_na(times,MSE,ADL) %>% 
  group_by(id) %>%
  summarise(n=n()) %>%
  ungroup()
dflongi2 <- dflongi %>% drop_na(times,MSE,ADL) %>% 
  filter(id %in% (dflongi1%>%filter(n>=3)%>%pull(id)))
rc <- rmcorr(id,ADL,MSE,dflongi2)
q1 <- plot(rc, ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5)
rc

dflongi1 <- dflongi %>% drop_na(times,MSE,SWB) %>% 
  group_by(id) %>%
  summarise(n=n()) %>%
  ungroup()
dflongi2 <- dflongi %>% drop_na(times,MSE,SWB) %>% 
  filter(id %in% (dflongi1%>%filter(n>=3)%>%pull(id)))
rc1 <- rmcorr(id,SWB,MSE,dflongi2)
q2 <- plot(rc1,xlab="SWB", ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5)
rc1

dflongi1 <- dflongi %>% drop_na(times,MSE,BMI) %>% 
  group_by(id) %>%
  summarise(n=n()) %>%
  ungroup()
dflongi2 <- dflongi %>% drop_na(times,MSE,BMI) %>% 
  filter(id %in% (dflongi1%>%filter(n>=3)%>%pull(id)))
rc3 <- rmcorr(id,BMI,MSE,dflongi2)
q3 <- plot(rc3, xlab="BMI",ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5)
rc3

par(mfrow = c(1,3))
plot(rc, xlab="ADL",ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5)
plot(rc1,xlab="SWB", ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5)
plot(rc3, xlab="BMI",ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5)

# q1 <- wrap_elements(~plot(rc, xlab="ADL",ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5))
# q2 <- wrap_elements(~plot(rc1, xlab="SWB",ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5))
# q3 <- wrap_elements(~plot(rc3, xlab="BMI",ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5))
# q1 + q2 + q3 +  plot_annotation(tag_levels = 'A')
# ggsave("rmcorr.pdf",width=16, height = 8,dpi=600)
# ggsave("rmcorr.png",width=16, height = 8,dpi=600)

##########
# Baseline Cox
library(survival)
library(survminer)


fitbase <- coxph(Surv(totalTime, outcome) ~
                   sex+age+res+edu+occ+mar+ADL+SWB+BMI,
                 data=dfbase)
fitbase
p1 <- ggforest(fitbase,dfbase,main = "Baseline")
# ggsave("base.tiff",width = 6, height = 3.5,dpi=600)

# time-varying cox
df <- dflongi %>% 
  mutate(outc = as.integer(outcome==1&times>=totalTime)) %>%
  select(id,times,sex,age,res,edu,occ,mar,ADL,SWB,BMI,outc) %>%
  rename(SWB=SWB,BMI=BMI,res=res,mar=mar)
df$tstart <- ipw::tstartfun(id, times, as.data.frame(df[,c("id","times")]))
df
fit.tdc <- coxph(Surv(tstart,times,outc)~
                   sex+age+res+edu+occ+mar+ADL+SWB+BMI+cluster(id),df)
fit.tdc
p2 <- ggforest(fit.tdc,df,main = "Time-dependent")
# ggsave("tv.tiff",width = 6, height = 3.5,dpi=600)

(p1/p2)#+plot_annotation(tag_levels = "a")
# p1/p2
ggsave("surv.pdf",width = 8, height = 10,dpi=600)

############
# Cox-MSMs causal inference
library(ipw)
library(survival)
library(ggpubr)
library(tableone)

df <- dflongi %>% 
  drop_na() %>%
  arrange(id,year) %>%
  group_by(id) %>%
  mutate(t=seq(0,n()-1)) %>%
  ungroup() %>%
  mutate(outc = as.integer(outcome==1&times>=totalTime)) %>%
  select(id,t,sex,age,res,edu,occ,mar,ADL,SWB,BMI,outc)


df$tstart <- ipw::tstartfun(id, t, as.data.frame(df[,c("id","t")]))
temp <- ipwtm(exposure = ADL, family = "gaussian",
              numerator = ~sex+age+res+edu+occ+mar, 
              denominator = ~SWB+BMI+sex+age+res+edu+occ+mar,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df),
              corstr = "ar1")
ipwplot(weights = temp$ipw.weights, timevar = df$t,
        binwidth = 1, main = "Stabilized weights for ADL",
        xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ ADL + cluster(id),
              data = df, weights = temp$ipw.weights)
data.frame(ipw=temp$ipw.weights,timevar=df$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

temp <- ipwtm(exposure = SWB, family = "gaussian",
              numerator = ~sex+age+res+edu+occ+mar, 
              denominator = ~ADL+BMI+sex+age+res+edu+occ+mar,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df),
              corstr = "ar1")
ipwplot(weights = temp$ipw.weights, timevar = df$t,
        binwidth = 1, main = "Stabilized weights for SWB",
        xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ SWB + cluster(id),
              data = df, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

temp <- ipwtm(exposure = BMI, family = "gaussian",
              numerator = ~sex+age+res+edu+occ+mar, 
              denominator = ~ADL+SWB+sex+age+res+edu+occ+mar,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df),
              corstr = "ar1")
ipwplot(weights = temp$ipw.weights, timevar = df$t,
        binwidth = 1, main = "Stabilized weights for BMI",
        xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ BMI + cluster(id),
              data = df, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

### subgroup causal inference
# sex subgroup
df1 <- df %>% filter(sex==0)
temp <- ipwtm(exposure = ADL, family = "gaussian",
              numerator = ~age+res+edu+occ+mar, 
              denominator = ~SWB+BMI+age+res+edu+occ+mar,
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

temp <- ipwtm(exposure = SWB, family = "gaussian",
              numerator = ~age+res+edu+occ+mar, 
              denominator = ~ADL+BMI+age+res+edu+occ+mar,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p2 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for SWB (Female)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ SWB + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

temp <- ipwtm(exposure = BMI, family = "gaussian",
              numerator = ~age+res+edu+occ+mar, 
              denominator = ~ADL+SWB+age+res+edu+occ+mar,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p3 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for BMI (Female)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ BMI + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

df1 <- df %>% filter(sex==1)
temp <- ipwtm(exposure = ADL, family = "gaussian",
              numerator = ~age+res+edu+occ+mar, 
              denominator = ~SWB+BMI+age+res+edu+occ+mar,
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

temp <- ipwtm(exposure = SWB, family = "gaussian",
              numerator = ~age+res+edu+occ+mar, 
              denominator = ~ADL+BMI+age+res+edu+occ+mar,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p2 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for SWB (Male)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ SWB + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

temp <- ipwtm(exposure = BMI, family = "gaussian",
              numerator = ~age+res+edu+occ+mar, 
              denominator = ~ADL+SWB+age+res+edu+occ+mar,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p3 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for BMI (Male)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ BMI + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

### age subset
df1 <- df %>% filter(age%in%c(1,2))
temp <- ipwtm(exposure = ADL, family = "gaussian",
              numerator = ~sex+age+res+edu+occ+mar, 
              denominator = ~SWB+BMI+sex+age+res+edu+occ+mar,
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

temp <- ipwtm(exposure = SWB, family = "gaussian",
              numerator = ~sex+age+res+edu+occ+mar, 
              denominator = ~ADL+BMI+sex+age+res+edu+occ+mar,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p2 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for SWB (<80)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ SWB + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

temp <- ipwtm(exposure = BMI, family = "gaussian",
              numerator = ~sex+age+res+edu+occ+mar, 
              denominator = ~ADL+SWB+sex+age+res+edu+occ+mar,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p3 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for BMI (<80)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ BMI + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

df1 <- df %>% filter(age%in%c(3,4))
temp <- ipwtm(exposure = ADL, family = "gaussian",
              numerator = ~sex+age+res+edu+occ+mar, 
              denominator = ~SWB+BMI+sex+age+res+edu+occ+mar,
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

temp <- ipwtm(exposure = SWB, family = "gaussian",
              numerator = ~sex+age+res+edu+occ+mar, 
              denominator = ~ADL+BMI+sex+age+res+edu+occ+mar,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p2 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for SWB (>=80)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ SWB + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)

temp <- ipwtm(exposure = BMI, family = "gaussian",
              numerator = ~sex+age+res+edu+occ+mar, 
              denominator = ~ADL+SWB+sex+age+res+edu+occ+mar,
              id = id, tstart = tstart,
              timevar = t,type = "all",data=as.data.frame(df1),
              corstr = "ar1")
p3 <- ipwplot(weights = temp$ipw.weights, timevar = df1$t,
              binwidth = 1, main = "Stabilized weights for BMI (>=80)",
              xlab = "follow-up",ylab="stabilized weights")
coxm <- coxph(Surv(tstart, t, outc) ~ BMI + cluster(id),
              data = df1, weights = temp$ipw.weights)
summary(coxm)
data.frame(ipw=temp$ipw.weights,timevar=df1$t) %>% 
  ggviolin(y="ipw",x="timevar",palette = "lancet",fill="timevar")
ShowRegTable(coxm,digits = 3)



##############
# subgroup analysis on age and times

dfbase <- read_csv("dfbase_filtered.csv")
dflongi <- read_csv("dflongi_filtered.csv") #original age

dfage <- dflongi %>% 
  drop_na(times) %>%
  mutate(outc = as.integer(outcome==1&times>=totalTime)) %>%
  filter(times<=totalTime) %>%
  select(id,times,sex,age_y,res,edu,occ,mar,ADL,SWB,BMI,outc,outcome) %>%
  mutate(dyn_age = round(age_y+times))%>%
  mutate_at(vars(outc),as.factor)
# ggplot(data, aes(x = dyn_age, y = SWB, color = outcome))
dfage <- dfage %>%
  drop_na(dyn_age) %>%
  mutate(age_group = case_when(
    dyn_age >= 65 & dyn_age <= 70 ~ "60~70",
    dyn_age >= 71 & dyn_age < 80 ~ "71~80",
    dyn_age >= 81 & dyn_age < 90 ~ "81~90",
    dyn_age >= 91 & dyn_age < 100 ~ "91~100",
    dyn_age >= 100 ~ "above 100"
  )) %>%
  drop_na(age_group)
dfage$outc <- factor(dfage$outc,labels = c("Control","Case"))
dfage$outcome <- factor(dfage$outcome,labels = c("Control","Case"))
p1 <- ggplot(dfage,aes(x = dyn_age, y = BMI, color = outcome)) +
  facet_wrap(~age_group,scale="free_x",nrow=1)+
  geom_smooth(method = "loess",aes(group = outcome))+
  theme_classic()+
  scale_color_lancet()+
  xlab("age (dynamic)")
p2 <- ggplot(dfage,aes(x = dyn_age, y = ADL, color = outcome)) +
  facet_wrap(~age_group,scale="free_x",nrow=1)+
  geom_smooth(method = "loess",aes(group = outcome))+
  theme_classic()+
  scale_color_lancet()+
  xlab("age (dynamic)")
p3 <- ggplot(dfage,aes(x = dyn_age, y = SWB, color = outcome)) +
  facet_wrap(~age_group,scale="free_x",nrow=1)+
  geom_smooth(method = "loess",aes(group = outcome))+
  theme_classic()+
  scale_color_lancet()+
  xlab("age (dynamic)")
p1+p2+p3+ 
  plot_layout(ncol = 1)+
  plot_annotation(tag_levels = "a")
ggsave("age1.png",height=8,width=10,dpi=600)

p1 <- ggplot(dfage,aes(x = dyn_age, y = BMI, color = outc)) +
  facet_wrap(~age_group,scale="free_x",nrow=1)+
  geom_smooth(method = "loess",aes(group = outc))+
  theme_classic()+
  scale_color_lancet()+
  xlab("age (dynamic)")
p2 <- ggplot(dfage,aes(x = dyn_age, y = ADL, color = outc)) +
  facet_wrap(~age_group,scale="free_x",nrow=1)+
  geom_smooth(method = "loess",aes(group = outc))+
  theme_classic()+
  scale_color_lancet()+
  xlab("age (dynamic)")
p3 <- ggplot(dfage,aes(x = dyn_age, y = SWB, color = outc)) +
  facet_wrap(~age_group,scale="free_x",nrow=1)+
  geom_smooth(method = "loess",aes(group = outc))+
  theme_classic()+
  scale_color_lancet()+
  xlab("age (dynamic)")
p1+p2+p3+ 
  plot_layout(ncol = 1)+
  plot_annotation(tag_levels = "a")
ggsave("age2.png",height=8,width=10,dpi=600)


dfage$id <- as.character(dfage$id)
dfage_A <- subset(dfage, outcome == "Control")
dfage_B <- subset(dfage, outcome == "Case")

library(lme4)
library(npmlreg)
model_A <- lmer(ADL ~ dyn_age+(1|id), data = dfage_A)
model_B <- lmer(ADL ~ dyn_age+(1|id), data = dfage_B)
new_dfage_A <- data.frame(dyn_age = seq(min(dfage_A$dyn_age), max(dfage_A$dyn_age), length.out = 100))
new_dfage_B <- data.frame(dyn_age = seq(min(dfage_B$dyn_age), max(dfage_B$dyn_age), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$dyn_age, y = predictions_A, outcome = "Control")
plot_dfage_B <- data.frame(x = new_dfage_B$dyn_age, y = predictions_B, outcome = "Case")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p1 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("age (dynamic)")+
  ylab("ADL")+
  scale_x_continuous(breaks = seq(65,115,10))
model_A <- lmer(SWB ~ dyn_age+(1|id), data = dfage_A)
model_B <- lmer(SWB ~ dyn_age+(1|id), data = dfage_B)
new_dfage_A <- data.frame(dyn_age = seq(min(dfage_A$dyn_age), max(dfage_A$dyn_age), length.out = 100))
new_dfage_B <- data.frame(dyn_age = seq(min(dfage_B$dyn_age), max(dfage_B$dyn_age), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$dyn_age, y = predictions_A, outcome = "Control")
plot_dfage_B <- data.frame(x = new_dfage_B$dyn_age, y = predictions_B, outcome = "Case")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p2 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("age (dynamic)")+
  ylab("SWB")+
  scale_x_continuous(breaks = seq(65,115,10))
model_A <- lmer(BMI ~ dyn_age+(1|id), data = dfage_A)
model_B <- lmer(BMI ~ dyn_age+(1|id), data = dfage_B)
new_dfage_A <- data.frame(dyn_age = seq(min(dfage_A$dyn_age), max(dfage_A$dyn_age), length.out = 100))
new_dfage_B <- data.frame(dyn_age = seq(min(dfage_B$dyn_age), max(dfage_B$dyn_age), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$dyn_age, y = predictions_A, outcome = "Control")
plot_dfage_B <- data.frame(x = new_dfage_B$dyn_age, y = predictions_B, outcome = "Case")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p3 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("age (dynamic)")+
  ylab("BMI")
p1+p2+p3+plot_layout(guides = "collect")+
  scale_x_continuous(breaks = seq(65,115,10))+
  plot_annotation(tag_levels = "a")
ggsave("para.png",dpi=600)
ggsave("para.pdf",dpi=600)

##
model_A <- allvc(ADL ~ dyn_age, random=~1|id, data = dfage_A)
model_B <- allvc(ADL ~ dyn_age, random=~1|id, data = dfage_B)
new_dfage_A <- data.frame(dyn_age = seq(min(dfage_A$dyn_age), max(dfage_A$dyn_age), length.out = 100))
new_dfage_B <- data.frame(dyn_age = seq(min(dfage_B$dyn_age), max(dfage_B$dyn_age), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$dyn_age, y = predictions_A, outcome = "Control")
plot_dfage_B <- data.frame(x = new_dfage_B$dyn_age, y = predictions_B, outcome = "Case")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p1 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("age (dynamic)")+
  ylab("ADL")+
  scale_x_continuous(breaks = seq(65,115,10))
model_A <- allvc(SWB ~ dyn_age, random=~1|id, data = dfage_A)
model_B <- allvc(SWB ~ dyn_age, random=~1|id, data = dfage_B)
new_dfage_A <- data.frame(dyn_age = seq(min(dfage_A$dyn_age), max(dfage_A$dyn_age), length.out = 100))
new_dfage_B <- data.frame(dyn_age = seq(min(dfage_B$dyn_age), max(dfage_B$dyn_age), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$dyn_age, y = predictions_A, outcome = "Control")
plot_dfage_B <- data.frame(x = new_dfage_B$dyn_age, y = predictions_B, outcome = "Case")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p2 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("age (dynamic)")+
  ylab("SWB")+
  scale_x_continuous(breaks = seq(65,115,10))
model_A <- allvc(BMI ~ dyn_age, random=~1|id, data = dfage_A)
model_B <- allvc(BMI ~ dyn_age, random=~1|id, data = dfage_B)
new_dfage_A <- data.frame(dyn_age = seq(min(dfage_A$dyn_age), max(dfage_A$dyn_age), length.out = 100))
new_dfage_B <- data.frame(dyn_age = seq(min(dfage_B$dyn_age), max(dfage_B$dyn_age), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$dyn_age, y = predictions_A, outcome = "Control")
plot_dfage_B <- data.frame(x = new_dfage_B$dyn_age, y = predictions_B, outcome = "Case")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p3 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("age (dynamic)")+
  ylab("BMI")
p1+p2+p3+plot_layout(guides = "collect")+
  scale_x_continuous(breaks = seq(65,115,10))+
  plot_annotation(tag_levels = "a")
ggsave("non-para.png",dpi=600)
ggsave("non-para.pdf",dpi=600)

#####
library(lme4)
library(npmlreg)
model_A <- lmer(ADL ~ times+(1|id), data = dfage_A)
model_B <- lmer(ADL ~ times+(1|id), data = dfage_B)
new_dfage_A <- data.frame(times = seq(min(dfage_A$times), max(dfage_A$times), length.out = 100))
new_dfage_B <- data.frame(times = seq(min(dfage_B$times), max(dfage_B$times), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$times, y = predictions_A, outcome = "Control")
plot_dfage_B <- data.frame(x = new_dfage_B$times, y = predictions_B, outcome = "Case")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p1 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("Years after baseline")+
  ylab("ADL")+
  scale_x_continuous(breaks = seq(0,20,5))
model_A <- lmer(SWB ~ times+(1|id), data = dfage_A)
model_B <- lmer(SWB ~ times+(1|id), data = dfage_B)
new_dfage_A <- data.frame(times = seq(min(dfage_A$times), max(dfage_A$times), length.out = 100))
new_dfage_B <- data.frame(times = seq(min(dfage_B$times), max(dfage_B$times), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$times, y = predictions_A, outcome = "Control")
plot_dfage_B <- data.frame(x = new_dfage_B$times, y = predictions_B, outcome = "Case")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p2 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("Years after baseline")+
  ylab("SWB")+
  scale_x_continuous(breaks = seq(0,20,5))
model_A <- lmer(BMI ~ times+(1|id), data = dfage_A)
model_B <- lmer(BMI ~ times+(1|id), data = dfage_B)
new_dfage_A <- data.frame(times = seq(min(dfage_A$times), max(dfage_A$times), length.out = 100))
new_dfage_B <- data.frame(times = seq(min(dfage_B$times), max(dfage_B$times), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$times, y = predictions_A, outcome = "Control")
plot_dfage_B <- data.frame(x = new_dfage_B$times, y = predictions_B, outcome = "Case")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p3 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("Years after baseline")+
  ylab("BMI")+
  scale_x_continuous(breaks = seq(0,20,5))
p1+p2+p3+plot_layout(guides = "collect")+
  #scale_x_continuous(breaks = seq(65,115,10))+
  plot_annotation(tag_levels = "a")
ggsave("para2.png",dpi=600)
ggsave("para2.pdf",dpi=600)

##
model_A <- allvc(ADL ~ times, random=~1|id, data = dfage_A)
model_B <- allvc(ADL ~ times, random=~1|id, data = dfage_B)
new_dfage_A <- data.frame(times = seq(min(dfage_A$times), max(dfage_A$times), length.out = 100))
new_dfage_B <- data.frame(times = seq(min(dfage_B$times), max(dfage_B$times), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$times, y = predictions_A, outcome = "Control")
plot_dfage_B <- data.frame(x = new_dfage_B$times, y = predictions_B, outcome = "Case")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p1 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("Years after baseline")+
  ylab("ADL")+
  scale_x_continuous(breaks = seq(0,20,5))
model_A <- allvc(SWB ~ times, random=~1|id, data = dfage_A)
model_B <- allvc(SWB ~ times, random=~1|id, data = dfage_B)
new_dfage_A <- data.frame(times = seq(min(dfage_A$times), max(dfage_A$times), length.out = 100))
new_dfage_B <- data.frame(times = seq(min(dfage_B$times), max(dfage_B$times), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$times, y = predictions_A, outcome = "Control")
plot_dfage_B <- data.frame(x = new_dfage_B$times, y = predictions_B, outcome = "Case")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p2 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("Years after baseline")+
  ylab("SWB")+
  scale_x_continuous(breaks = seq(0,20,5))
model_A <- allvc(BMI ~ times, random=~1|id, data = dfage_A)
model_B <- allvc(BMI ~ times, random=~1|id, data = dfage_B)
new_dfage_A <- data.frame(times = seq(min(dfage_A$times), max(dfage_A$times), length.out = 100))
new_dfage_B <- data.frame(times = seq(min(dfage_B$times), max(dfage_B$times), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$times, y = predictions_A, outcome = "Control")
plot_dfage_B <- data.frame(x = new_dfage_B$times, y = predictions_B, outcome = "Case")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p3 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("Years after baseline")+
  ylab("BMI")+
  scale_x_continuous(breaks = seq(0,20,5))
p1+p2+p3+plot_layout(guides = "collect")+
  plot_annotation(tag_levels = "a")
ggsave("non-para2.png",dpi=600)
ggsave("non-para2.pdf",dpi=600)


# ##
# library(gamm4)
# par(mfrow=c(3,2))
# model_A <- gamm4::gamm4(ADL ~ 1+s(dyn_age), random=~(1|id), data = dfage_A)
# model_B <- gamm4::gamm4(ADL ~ 1+s(dyn_age), random=~(1|id), data = dfage_B)
# mgcv::plot.gam(model_A$gam,xlab="age (dynamic)",ylim=c(-0.3,0.2),ylab="ADL",main="Control")
# mgcv::plot.gam(model_B$gam,xlab="age (dynamic)",ylim=c(-0.3,0.2),ylab="ADL",main="Case")
# model_A <- gamm4::gamm4(SWB ~ 1+s(dyn_age), random=~(1|id), data = dfage_A)
# model_B <- gamm4::gamm4(SWB ~ 1+s(dyn_age), random=~(1|id), data = dfage_B)
# mgcv::plot.gam(model_A$gam,xlab="age (dynamic)",ylim=c(-0.02,0.03),ylab="SWB",main="Control")
# mgcv::plot.gam(model_B$gam,xlab="age (dynamic)",ylim=c(-0.02,0.03),ylab="SWB",main="Case")
# model_A <- gamm4::gamm4(BMI ~ 1+s(dyn_age), random=~(1|id), data = dfage_A)
# model_B <- gamm4::gamm4(BMI ~ 1+s(dyn_age), random=~(1|id), data = dfage_B)
# mgcv::plot.gam(model_A$gam,xlab="age (dynamic)",ylim=c(-3,2),ylab="BMI",main="Control")
# mgcv::plot.gam(model_B$gam,xlab="age (dynamic)",ylim=c(-3,2),ylab="BMI",main="Case")


###############
# jm
ass <- list.files(pattern = '[ADL|BMI|SWB].*_scale.csv$')
ass
df <- map_dfr(set_names(ass),read_csv,.id="source")
df
df <- df %>% mutate(Assoc = paste0(sprintf("%.3f", Value)," (",
                                   sprintf("%.3f", `2.5%`),", ",
                                   sprintf("%.3f", `97.5%`),")"),
                    p_val = if_else(P==0,'<0.001',as.character(P)))
df %>% select(source,covariate,Assoc,p_val) %>% write_csv("assoc.csv")



################
## plot performance
##
dfb <- read_csv("base_pred.csv")
colnames(dfb) <- c("variable","type","t","AUC")
dfb <- dfb %>% filter(t<=10)
# library(reshape2)
# dfb <- melt(dfb,id.vars="t",variable.name="type",value.name = "AUC")
# dfb$t <- as.numeric(as.character(dfb$t))
# dfb$variable <- rep(c("SWB","ADL","BMI"),each=4)
p1 <- ggplot(dfb)+geom_point(aes(x=t,y=AUC,colour=variable),size=1)+
  geom_line(aes(x=t,y=AUC,colour=variable,linetype=type),linewidth=0.5)+
  scale_x_continuous(breaks = unique(dfb$t))+
  ylim(c(0.75,0.9))+
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
p1
# ggsave("y.png", p1,width = 4, height = 5, dpi = 600)


df <- read_csv("cv10_predict.csv")
df1 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),auc=t(df[1,1:12]))
df2 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),auc=t(df[2,1:12]))
df3 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),auc=t(df[3,1:12]))
df1$variable <- "SWB"
df2$variable <- "ADL"
df3$variable <- "BMI"
dfa <- bind_rows(df1,df2,df3)
dfa$t <- factor(dfa$t,labels = c("t=4","t=8","t=10"))

# dfsd <- read_csv("cv10_predict_full_new_scale.csv")
# df1 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),sd=t(dfsd[1,1:12]))
# df2 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),sd=t(dfsd[2,1:12]))
# df3 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),sd=t(dfsd[3,1:12]))
# df1$variable <- "SWB"
# df2$variable <- "ADL"
# df3$variable <- "BMI"
# dfb <- bind_rows(df1,df2,df3)
# dfb$t <- factor(dfa$t,labels = c("t=4","t=8","t=10"))

# dfa$sd <- dfb$sd

p2 <- ggplot(dfa)+geom_point(aes(x=deltat,y=auc,colour=variable),size=1)+
  geom_line(aes(x=deltat,y=auc,colour=variable),linewidth=0.5)+
  facet_grid(~t)+
  scale_x_continuous(breaks = c(2,5,7,10))+
  #scale_y_continuous(breaks = seq(0.74,0.86,0.02))+
  xlab(TeX("$\\Delta t$"))+
  ylab("AUC")+
  ylim(c(0.7,0.9))+
  # geom_errorbar(aes(x=deltat,y=auc,colour=variable,ymin=auc-sd, ymax=auc+sd), width=.2,
  #               position=position_dodge(0.05))+
  theme_few()+
  scale_color_lancet()+
  theme(plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(face = "bold.italic"),
        strip.background = element_rect(fill="grey"))+
  ggtitle("Bayesian joint model")
p2
# ggsave("bjm.png", width = 5, height = 5, dpi = 600)

##########
ass <- list.files(pattern = "^C_index.*.csv$")
df <- map_dfr(set_names(ass),read_csv,.id = "model")
colnames(df)[2] <- "pred"
df <- df%>%pivot_longer(-c(model,pred),names_to = "eval",values_to = "cindex")
df <- df %>% mutate(pred=case_match(pred, "pred_time 4: event_1"~4,"pred_time 6: event_1"~6,
                              "pred_time 8: event_1"~8,"pred_time 10: event_1"~10),
              eval=case_match(eval,"eval_time 2"~2,"eval_time 5"~5, "eval_time 7"~7,
                              "eval_time 10"~10),
              model=str_extract(model, "(?<=C_index_)[^.]+"))
df$model <- gsub("_", "+", df$model)
df$model <- factor(df$model,levels = c("SWB+BMI+ADL","SWB+ADL",
                                       "SWB+BMI","BMI+ADL",
                                       "BMI","ADL","SWB","base"))#,
df
# library(data.table)
# df <- as.data.table(df)
# df
# df[,model:=fct_relevel(df$model,"ADL","BMI","SWB","ADL+BMI","ADL+SWB",
#                        "SWB+BMI","ADL+SWB+BMI","base")]
# df
df$pred <- factor(df$pred,labels = c("t=4","t=6","t=8","t=10"))
df$eval <- factor(df$eval,levels = c(2,5,7,10))
library(latex2exp)
library(ggthemes)
library(ggsci)
p3 <- ggplot(df)+geom_point(aes(x=eval,y=cindex,colour=model),size=1)+
  geom_line(aes(x=eval,y=cindex,colour=model,group=model),linewidth=0.5)+
  facet_grid(~pred)+
  #scale_x_continuous(breaks = c(2,5,7,10))+
  #scale_y_continuous(breaks = seq(0.74,0.86,0.02))+
  xlab(TeX("$\\Delta t$"))+
  ylab("C-index")+
  ylim(c(0.8,0.88))+
  # geom_errorbar(aes(x=deltat,y=auc,colour=variable,ymin=auc-sd, ymax=auc+sd), width=.2,
  #               position=position_dodge(0.05))+
  theme_few()+
  scale_color_lancet()+
  theme(plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(face = "bold.italic"),
        strip.background = element_rect(fill="grey"))+
  ggtitle("Dynamic-DeepHit")
p3
# ggsave("deephit.png", width = 9, height = 5, dpi = 600)

((p1+p2+plot_layout(widths = c(1.5, 2)))/p3) +plot_annotation(tag_levels = "a")
ggsave("Fig2.pdf",dpi=600,width=9,height=9)
ggsave("Fig2.png",dpi=600,width=9,height=9)
