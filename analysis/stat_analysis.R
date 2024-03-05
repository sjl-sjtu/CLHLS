setwd("D:/OneDrive/new")
# setwd("C:/Users/DELL/OneDrive/new")
# setwd("F:/OneDrive/new")
library(tidyverse)
library(latex2exp)
library(ggthemes)
library(ggsci)
library(RColorBrewer)
library(patchwork)


#####

df <- read_csv("dfbase.csv")%>%
  mutate(totalTime=totalTime/365,ADL=ADL/12)%>%
  rename(res=residenc,mar=marriage,BMI=bmi,EMO=emo)%>% 
  filter(totalTime!=0) #|outcome!=1)
feat=c("sex","age","res","edu","occ","mar","EMO","ADL","BMI","outcome")

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

t.test(age~outcome,data=df)
chisq.test(df$sex,df$outcome)
chisq.test(df$res,df$outcome)
t.test(edu~outcome,data=df)
chisq.test(df$occ,df$outcome)
chisq.test(df$mar,df$outcome)
t.test(EMO~outcome,data=df)
t.test(ADL~outcome,data=df)
t.test(BMI~outcome,data=df)

feat
for (col in feat[c(1,3,5,6)]) {
  freq <- prop.table(table(df[[col]]))
  crs <- prop.table(table(df[[col]],df$outcome))
  crs <- apply(crs,2,function(x) x/sum(x))
  print(paste("Column:", col))
  print(freq)
  print(crs)
  print("--------------------")
}
colMeans(df[,feat[-c(1,3,5,6,10)]],na.rm=T)
aggregate(. ~ outcome, data = df[,feat[-c(1,3,5,6)]], FUN = mean)

table(df$outcome)
prop.table(table(df$outcome))
nrow(df)

dflongi <- read_csv("./dflongi2.csv")
library(car)
dflongi$outcomes <- recode(dflongi$outcomes,"'illness'='CI';'health'='non-CI'")
dflongi$totalTime <- dflongi$totalTime/365
dflongi$times <- dflongi$times/365
dflongi$ADL <- dflongi$ADL/12
dflongi$outcomes <- factor(dflongi$outcomes,levels = c("non-CI","CI"))
dflongi <- dflongi %>% filter(totalTime!=0) %>%filter(times <= totalTime)
dflongi <- dflongi%>%drop_na(times)
table(dflongi$outcome)
# dflongi1 <- dflongi %>% drop_na(times) %>% 
#   group_by(id) %>%
#   summarise(n=n()) %>%
#   ungroup()
# dflongi1 <- dflongi1%>%left_join(dflongi%>%select(id,outcome),by="id")
# table(dflongi1$n,dflongi1$outcome)


dft <- read_csv("dftimes.csv")%>%
  filter(id%in%df$id)
lapply(dft%>%select(eN,iN), table)
table(dft%>%filter(id%in%(df%>%filter(outcome==0)%>%pull(id)))%>%pull(oN))
table(dft$eN) |> sum()
table(dft$iN) |> sum()
table(dft%>%filter(id%in%(df%>%filter(outcome==0)%>%pull(id)))%>%pull(oN)) |> sum()

####
fit <- survfit(Surv(totalTime, outcome) ~ sex, data = df)
q1 <- ggsurvplot(fit, data = df, risk.table = F, pval = TRUE,
           pval.method = TRUE, surv.median.line="v",conf.int=T)+
  xlab("Time (Years)")
fit <- survfit(Surv(totalTime, outcome) ~ res, data = df)
q2 <- ggsurvplot(fit, data = df, risk.table = F, pval = TRUE,
                                pval.method = TRUE, surv.median.line="v",conf.int=T)+
                      xlab("Time (Years)")
fit <- survfit(Surv(totalTime, outcome) ~ occ, data = df)
q3 <- ggsurvplot(fit, data = df, risk.table = F, pval = TRUE,
                                pval.method = TRUE, surv.median.line="v",conf.int=T)+
                      xlab("Time (Years)")
fit <- survfit(Surv(totalTime, outcome) ~ mar, data = df)
q4 <- ggsurvplot(fit, data = df, risk.table = F, pval = TRUE,
                                pval.method = TRUE, surv.median.line="v",conf.int=T)+
                      xlab("Time (Years)")

q <- arrange_ggsurvplots(list(q1,q2,q3,q4),print = TRUE,  nrow=2)
ggsave("KM.png",q,dpi=600,width = 11,height = 7)

####
dflongi <- read_csv("./dflongi2.csv")
library(car)
dflongi$outcomes <- recode(dflongi$outcomes,"'illness'='CI';'health'='non-CI'")
dflongi$totalTime <- dflongi$totalTime/365
dflongi$times <- dflongi$times/365
dflongi$ADL <- dflongi$ADL/12
dflongi$outcomes <- factor(dflongi$outcomes,levels = c("non-CI","CI"))

dflongi <- dflongi %>% filter(totalTime!=0) %>%filter(times <= totalTime)
dflongi1 <- dflongi %>% drop_na(times) %>% 
  group_by(id) %>%
  summarise(n=n()) %>%
  ungroup()
dflongi <- dflongi%>%filter(id %in% (dflongi1%>%filter(n>=3)%>%pull(id)))
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
ggsave("g4.png", width = 5, height = 4, dpi = 600)

p2 <- ggplot(dflongi,aes(times,emo))+
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
  ylab("EMO")
ggsave("g5.png", width = 5, height = 4, dpi = 600)

p3 <- ggplot(dflongi,aes(times,bmi))+
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
ggsave("g6.png", width = 5, height = 4, dpi = 600)
#getwd()

p1+p2+p3+plot_annotation(tag_levels = 'a')
ggsave("longiChange.pdf",dpi=600)
# ggsave("longiChange.tiff",dpi=600)

# #Granger
# library(plm)
# dflongi1 <- dflongi %>% drop_na(times,ADL,MSE) %>% 
#   group_by(id) %>%
#   summarise(n=n()) %>%
#   ungroup()
# table(dflongi1$n)
# dflongi2 <- dflongi %>% drop_na(times,ADL,MSE) %>% 
#   filter(id %in% (dflongi1%>%filter(n>=3)%>%pull(id)))
# pdf_longi <- pdata.frame(dflongi2, index = c("id", "year"), 
#                          drop.index = TRUE)
# pgrangertest(MSE ~ ADL, data = pdf_longi, test = "Wbar")


###
library(rmcorr)
dflongi <- read_csv("./dflongi2.csv")
# dfl <- dflongi %>% filter(times==0) 
# table(dfl$year)
library(car)
dflongi$outcomes <- recode(dflongi$outcomes,"'illness'='CI';'health'='non-CI'")
dflongi$totalTime <- dflongi$totalTime/365
dflongi$times <- dflongi$times/365
dflongi$ADL <- dflongi$ADL/12
dflongi$outcomes <- factor(dflongi$outcomes,levels = c("non-CI","CI"))
dflongi <- dflongi %>% filter(totalTime!=0) %>%filter(times <= totalTime)

dflongi1 <- dflongi %>% drop_na(times,MSE,ADL) %>% 
  group_by(id) %>%
  summarise(n=n()) %>%
  ungroup()
dflongi2 <- dflongi %>% drop_na(times,MSE,ADL) %>% 
  filter(id %in% (dflongi1%>%filter(n>=3)%>%pull(id)))
rc <- rmcorr(id,ADL,MSE,dflongi2)
# png( 
#   filename = "ADL_MSE.png", # 文件名称
#   width = 3500,           # 宽
#   height = 3000,          # 高
#   units = "px",          # 单位
#   bg = "white",          # 背景颜色
#   res = 600)              # 分辨率
q1 <- plot(rc, ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5)
# dev.off()
rc

dflongi1 <- dflongi %>% drop_na(times,MSE,emo) %>% 
  group_by(id) %>%
  summarise(n=n()) %>%
  ungroup()
dflongi2 <- dflongi %>% drop_na(times,MSE,emo) %>% 
  filter(id %in% (dflongi1%>%filter(n>=3)%>%pull(id)))
rc1 <- rmcorr(id,emo,MSE,dflongi2)
# png( 
#   filename = "emo_MSE.png", # 文件名称
#   width = 3500,           # 宽
#   height = 3000,          # 高
#   units = "px",          # 单位
#   bg = "white",          # 背景颜色
#   res = 600)              # 分辨率
q2 <- plot(rc1,xlab="EMO", ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5)
# dev.off()
rc1

dflongi1 <- dflongi %>% drop_na(times,MSE,bmi) %>% 
  group_by(id) %>%
  summarise(n=n()) %>%
  ungroup()
dflongi2 <- dflongi %>% drop_na(times,MSE,bmi) %>% 
  filter(id %in% (dflongi1%>%filter(n>=3)%>%pull(id)))
rc3 <- rmcorr(id,bmi,MSE,dflongi2)
png( 
  filename = "BMI_MSE.png", # 文件名称
  width = 3500,           # 宽
  height = 3000,          # 高
  units = "px",          # 单位
  bg = "white",          # 背景颜色
  res = 600)              # 分辨率
q3 <- plot(rc3, xlab="BMI",ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5)
dev.off()
rc3

q1 <- wrap_elements(~plot(rc, xlab="ADL",ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5))
q2 <- wrap_elements(~plot(rc1, xlab="EMO",ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5))
q3 <- wrap_elements(~plot(rc3, xlab="BMI",ylab = "CMMSE",cex.axis=1.5, cex.lab=1.5))
q1 + q2 + q3 +  plot_annotation(tag_levels = 'A')
ggsave("rmcorr.pdf",width=16, height = 8,dpi=600)
ggsave("rmcorr.png",width=16, height = 8,dpi=600)

##########
library(survival)
library(survminer)

dflongi <- read_csv("./dflongi2.csv")
library(car)
dflongi$outcomes <- car::recode(dflongi$outcomes,"'illness'='CI';'health'='non-CI'")
dflongi$totalTime <- dflongi$totalTime/365
dflongi$times <- dflongi$times/365
dflongi$ADL <- dflongi$ADL/12
dflongi$outcomes <- factor(dflongi$outcomes,levels = c("non-CI","CI"))
  
#dflongi[which(dflongi$totalTime==0&dflongi$outcome==0),"totalTime"] <- 2
# dflongi <- dflongi%>%filter(totalTime!=0)
dflongi <- dflongi[-which(dflongi$totalTime==0),]

dfbase <- dflongi%>%filter(times==0) %>% 
  rename(EMO=emo,BMI=bmi,res=residenc,mar=marriage) %>%
  filter(totalTime!=0)
fitbase <- coxph(Surv(totalTime, outcome) ~
                   sex+age+res+edu+occ+mar+ADL+EMO+BMI,
                 data=dfbase)
fitbase
p1 <- ggforest(fitbase,dfbase,main = "Baseline")
ggsave("base.tiff",width = 6, height = 3.5,dpi=600)

dflongi <- dflongi%>%drop_na(times)%>%filter(times <= totalTime) %>% filter(totalTime!=0|outcome!=1)

df <- dflongi %>% 
  drop_na(times) %>%
  mutate(outc = as.integer(outcome==1&times>=totalTime)) %>%
  filter(times<=totalTime) %>%
  select(id,times,sex,age,residenc,edu,occ,marriage,ADL,emo,bmi,outc) %>%
  rename(EMO=emo,BMI=bmi,res=residenc,mar=marriage)
df$tstart <- ipw::tstartfun(id, times, as.data.frame(df[,c("id","times")]))
df
fit.tdc <- coxph(Surv(tstart,times,outc)~
                   sex+age+res+edu+occ+mar+ADL+EMO+BMI+cluster(id),df)
fit.tdc
p2 <- ggforest(fit.tdc,df,main = "Time-dependent")
ggsave("tv.tiff",width = 6, height = 3.5,dpi=600)

(p1/p2)+plot_annotation(tag_levels = "a")
# p1/p2
ggsave("surv.pdf",width = 8, height = 10,dpi=600)

###
dflongi <- read_csv("./dflongi2.csv")
library(car)
dflongi$outcomes <- car::recode(dflongi$outcomes,"'illness'='CI';'health'='non-CI'")
dflongi$totalTime <- dflongi$totalTime/365
dflongi$times <- dflongi$times/365
dflongi$ADL <- dflongi$ADL/12
dflongi$outcomes <- factor(dflongi$outcomes,levels = c("non-CI","CI"))
dflongi <- dflongi[-which(dflongi$totalTime==0),]
dflongi <- dflongi%>%drop_na(times)%>%filter(times <= totalTime) %>% filter(totalTime!=0|outcome!=1)


dfage <- dflongi %>% 
  drop_na(times) %>%
  mutate(outc = as.integer(outcome==1&times>=totalTime)) %>%
  filter(times<=totalTime) %>%
  select(id,times,sex,age,residenc,edu,occ,marriage,ADL,emo,bmi,outc,outcome) %>%
  rename(EMO=emo,BMI=bmi,res=residenc,mar=marriage)%>% 
  mutate(dyn_age = round(age+times))%>%
  mutate_at(vars(outc),as.factor)
# ggplot(data, aes(x = dyn_age, y = EMO, color = outcome))
dfage <- dfage %>%
  drop_na(dyn_age) %>%
  mutate(age_group = case_when(
    dyn_age >= 65 & dyn_age <= 70 ~ "65~70",
    dyn_age >= 71 & dyn_age < 80 ~ "71~80",
    dyn_age >= 81 & dyn_age < 90 ~ "81~90",
    dyn_age >= 91 & dyn_age < 100 ~ "91~100",
    dyn_age >= 100 ~ "above 100"
  )) %>%
  drop_na(age_group)
dfage$outc <- factor(dfage$outc,labels = c("non-CI","CI"))
dfage$outcome <- factor(dfage$outcome,labels = c("non-CI","CI"))
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
p3 <- ggplot(dfage,aes(x = dyn_age, y = EMO, color = outcome)) +
  facet_wrap(~age_group,scale="free_x",nrow=1)+
  geom_smooth(method = "loess",aes(group = outcome))+
  theme_classic()+
  scale_color_lancet()+
  xlab("age (dynamic)")
p1+p2+p3+ 
  plot_layout(ncol = 1)+
  plot_annotation(tag_levels = "a")
ggsave("age1.png",height=8,width=10,dpi=600)


dfage$id <- as.character(dfage$id)
dfage_A <- subset(dfage, outcome == "non-CI")
dfage_B <- subset(dfage, outcome == "CI")

library(lme4)
library(npmlreg)
model_A <- lmer(ADL ~ dyn_age+(1|id), data = dfage_A)
model_B <- lmer(ADL ~ dyn_age+(1|id), data = dfage_B)
new_dfage_A <- data.frame(dyn_age = seq(min(dfage_A$dyn_age), max(dfage_A$dyn_age), length.out = 100))
new_dfage_B <- data.frame(dyn_age = seq(min(dfage_B$dyn_age), max(dfage_B$dyn_age), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$dyn_age, y = predictions_A, outcome = "non-CI")
plot_dfage_B <- data.frame(x = new_dfage_B$dyn_age, y = predictions_B, outcome = "CI")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p1 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("age (dynamic)")+
  ylab("ADL")+
  scale_x_continuous(breaks = seq(65,115,10))
model_A <- lmer(EMO ~ dyn_age+(1|id), data = dfage_A)
model_B <- lmer(EMO ~ dyn_age+(1|id), data = dfage_B)
new_dfage_A <- data.frame(dyn_age = seq(min(dfage_A$dyn_age), max(dfage_A$dyn_age), length.out = 100))
new_dfage_B <- data.frame(dyn_age = seq(min(dfage_B$dyn_age), max(dfage_B$dyn_age), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$dyn_age, y = predictions_A, outcome = "non-CI")
plot_dfage_B <- data.frame(x = new_dfage_B$dyn_age, y = predictions_B, outcome = "CI")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p2 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("age (dynamic)")+
  ylab("EMO")+
  scale_x_continuous(breaks = seq(65,115,10))
model_A <- lmer(BMI ~ dyn_age+(1|id), data = dfage_A)
model_B <- lmer(BMI ~ dyn_age+(1|id), data = dfage_B)
new_dfage_A <- data.frame(dyn_age = seq(min(dfage_A$dyn_age), max(dfage_A$dyn_age), length.out = 100))
new_dfage_B <- data.frame(dyn_age = seq(min(dfage_B$dyn_age), max(dfage_B$dyn_age), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$dyn_age, y = predictions_A, outcome = "non-CI")
plot_dfage_B <- data.frame(x = new_dfage_B$dyn_age, y = predictions_B, outcome = "CI")
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
plot_dfage_A <- data.frame(x = new_dfage_A$dyn_age, y = predictions_A, outcome = "non-CI")
plot_dfage_B <- data.frame(x = new_dfage_B$dyn_age, y = predictions_B, outcome = "CI")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p1 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("age (dynamic)")+
  ylab("ADL")+
  scale_x_continuous(breaks = seq(65,115,10))
model_A <- allvc(EMO ~ dyn_age, random=~1|id, data = dfage_A)
model_B <- allvc(EMO ~ dyn_age, random=~1|id, data = dfage_B)
new_dfage_A <- data.frame(dyn_age = seq(min(dfage_A$dyn_age), max(dfage_A$dyn_age), length.out = 100))
new_dfage_B <- data.frame(dyn_age = seq(min(dfage_B$dyn_age), max(dfage_B$dyn_age), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$dyn_age, y = predictions_A, outcome = "non-CI")
plot_dfage_B <- data.frame(x = new_dfage_B$dyn_age, y = predictions_B, outcome = "CI")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p2 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("age (dynamic)")+
  ylab("EMO")+
  scale_x_continuous(breaks = seq(65,115,10))
model_A <- allvc(BMI ~ dyn_age, random=~1|id, data = dfage_A)
model_B <- allvc(BMI ~ dyn_age, random=~1|id, data = dfage_B)
new_dfage_A <- data.frame(dyn_age = seq(min(dfage_A$dyn_age), max(dfage_A$dyn_age), length.out = 100))
new_dfage_B <- data.frame(dyn_age = seq(min(dfage_B$dyn_age), max(dfage_B$dyn_age), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$dyn_age, y = predictions_A, outcome = "non-CI")
plot_dfage_B <- data.frame(x = new_dfage_B$dyn_age, y = predictions_B, outcome = "CI")
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
plot_dfage_A <- data.frame(x = new_dfage_A$times, y = predictions_A, outcome = "non-CI")
plot_dfage_B <- data.frame(x = new_dfage_B$times, y = predictions_B, outcome = "CI")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p1 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("Years after baseline")+
  ylab("ADL")+
  scale_x_continuous(breaks = seq(0,20,5))
model_A <- lmer(EMO ~ times+(1|id), data = dfage_A)
model_B <- lmer(EMO ~ times+(1|id), data = dfage_B)
new_dfage_A <- data.frame(times = seq(min(dfage_A$times), max(dfage_A$times), length.out = 100))
new_dfage_B <- data.frame(times = seq(min(dfage_B$times), max(dfage_B$times), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$times, y = predictions_A, outcome = "non-CI")
plot_dfage_B <- data.frame(x = new_dfage_B$times, y = predictions_B, outcome = "CI")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p2 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("Years after baseline")+
  ylab("EMO")+
  scale_x_continuous(breaks = seq(0,20,5))
model_A <- lmer(BMI ~ times+(1|id), data = dfage_A)
model_B <- lmer(BMI ~ times+(1|id), data = dfage_B)
new_dfage_A <- data.frame(times = seq(min(dfage_A$times), max(dfage_A$times), length.out = 100))
new_dfage_B <- data.frame(times = seq(min(dfage_B$times), max(dfage_B$times), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$times, y = predictions_A, outcome = "non-CI")
plot_dfage_B <- data.frame(x = new_dfage_B$times, y = predictions_B, outcome = "CI")
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
plot_dfage_A <- data.frame(x = new_dfage_A$times, y = predictions_A, outcome = "non-CI")
plot_dfage_B <- data.frame(x = new_dfage_B$times, y = predictions_B, outcome = "CI")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p1 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("Years after baseline")+
  ylab("ADL")+
  scale_x_continuous(breaks = seq(0,20,5))
model_A <- allvc(EMO ~ times, random=~1|id, data = dfage_A)
model_B <- allvc(EMO ~ times, random=~1|id, data = dfage_B)
new_dfage_A <- data.frame(times = seq(min(dfage_A$times), max(dfage_A$times), length.out = 100))
new_dfage_B <- data.frame(times = seq(min(dfage_B$times), max(dfage_B$times), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$times, y = predictions_A, outcome = "non-CI")
plot_dfage_B <- data.frame(x = new_dfage_B$times, y = predictions_B, outcome = "CI")
plot_dfage <- rbind(plot_dfage_A, plot_dfage_B)
p2 <- ggplot(plot_dfage, aes(x = x, y = y, color = outcome))+ 
  geom_line(data = plot_dfage, aes(x = x, y = y, group = outcome), size = 1)+
  theme_classic()+
  xlab("Years after baseline")+
  ylab("EMO")+
  scale_x_continuous(breaks = seq(0,20,5))
model_A <- allvc(BMI ~ times, random=~1|id, data = dfage_A)
model_B <- allvc(BMI ~ times, random=~1|id, data = dfage_B)
new_dfage_A <- data.frame(times = seq(min(dfage_A$times), max(dfage_A$times), length.out = 100))
new_dfage_B <- data.frame(times = seq(min(dfage_B$times), max(dfage_B$times), length.out = 100))
predictions_A <- predict(model_A, newdata = new_dfage_A, re.form = NA)
predictions_B <- predict(model_B, newdata = new_dfage_B, re.form = NA)
plot_dfage_A <- data.frame(x = new_dfage_A$times, y = predictions_A, outcome = "non-CI")
plot_dfage_B <- data.frame(x = new_dfage_B$times, y = predictions_B, outcome = "CI")
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


##
library(gamm4)
par(mfrow=c(3,2))
model_A <- gamm4::gamm4(ADL ~ 1+s(dyn_age), random=~(1|id), data = dfage_A)
model_B <- gamm4::gamm4(ADL ~ 1+s(dyn_age), random=~(1|id), data = dfage_B)
mgcv::plot.gam(model_A$gam,xlab="age (dynamic)",ylim=c(-0.3,0.2),ylab="ADL",main="non-CI")
mgcv::plot.gam(model_B$gam,xlab="age (dynamic)",ylim=c(-0.3,0.2),ylab="ADL",main="CI")
model_A <- gamm4::gamm4(EMO ~ 1+s(dyn_age), random=~(1|id), data = dfage_A)
model_B <- gamm4::gamm4(EMO ~ 1+s(dyn_age), random=~(1|id), data = dfage_B)
mgcv::plot.gam(model_A$gam,xlab="age (dynamic)",ylim=c(-0.02,0.03),ylab="EMO",main="non-CI")
mgcv::plot.gam(model_B$gam,xlab="age (dynamic)",ylim=c(-0.02,0.03),ylab="EMO",main="CI")
model_A <- gamm4::gamm4(BMI ~ 1+s(dyn_age), random=~(1|id), data = dfage_A)
model_B <- gamm4::gamm4(BMI ~ 1+s(dyn_age), random=~(1|id), data = dfage_B)
mgcv::plot.gam(model_A$gam,xlab="age (dynamic)",ylim=c(-3,2),ylab="BMI",main="non-CI")
mgcv::plot.gam(model_B$gam,xlab="age (dynamic)",ylim=c(-3,2),ylab="BMI",main="CI")

# dfn <- dflongi %>% 
#   drop_na(times) %>%
#   # group_by(id) %>%
#   # mutate(t=seq(1,n())) %>%
#   # ungroup() %>%
#   mutate(outc = as.integer(outcome==1&times>=totalTime)) %>%
#   select(id,times,sex,age,residenc,edu,occ,marriage,ADL,emo,bmi,outc,totalTime) %>%
#   rename(EMO=emo,BMI=bmi,res=residenc,mar=marriage)
# fit2 <- coxph(Surv(totalTime, outcome) ~
#                    sex+age+res+edu+occ+mar+ADL+EMO+BMI,
#                  data=dfn)
# 
# zph <- cox.zph(fitbase)
# zph[7]
# 
# plot(zph[2],lwd=2)
# abline(0,0, col=1,lty=3,lwd=2)
# abline(h= fit2$coef[7], col=3, lwd=2, lty=2)



############
# library(JMbayes)
# dflongi1 <- dflongi %>% drop_na(times) %>% 
#   group_by(id) %>%
#   summarise(n=n()) %>%
#   ungroup()
# dflongi2 <- dflongi%>%filter(id %in% (dflongi1%>%filter(n>=2)%>%pull(id)))
# dflongi2.id <- dfbase%>%filter(id %in% (dflongi1%>%filter(n>=2)%>%pull(id)))
# 
# MixedModelFit <- mvglmer(list(emo ~ times + (times | id),
#                               bmi ~ times + (times | id),
#                               ADL ~ times + (times | id)), data = dflongi2,
#                          families = list(gaussian, gaussian,gaussian))
# summary(MixedModelFit)
# coxFit <- coxph(Surv(totalTime,event=outcome)~sex+age+residenc+edu+occ+marriage,
#                 data=dflongi2.id,model = TRUE)
# coxFit
# JMFit <- mvJointModelBayes(MixedModelFit,coxFit,timeVar = "times",n.iter=30,seed=0)
# JMFit

############
dflongi <- read_csv("./dflongi2.csv")
library(car)
dflongi$outcomes <- recode(dflongi$outcomes,"'illness'='CI';'health'='non-CI'")
dflongi$totalTime <- dflongi$totalTime/365
dflongi$times <- dflongi$times/365
dflongi$ADL <- dflongi$ADL/12
dflongi$outcomes <- factor(dflongi$outcomes,levels = c("non-CI","CI"))

dflongi <- dflongi%>%filter(totalTime!=0)
dflongi <- dflongi%>%filter(times <= totalTime)

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
#df <- read_csv("./predict_10f.csv")
df <- read_csv("predict_full_new.csv")
df1 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),auc=t(df[1,1:12]))
df2 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),auc=t(df[2,1:12]))
df3 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),auc=t(df[3,1:12]))
df1$variable <- "EMO"
df2$variable <- "ADL"
df3$variable <- "BMI"
dfa <- bind_rows(df1,df2,df3)
dfa$t <- factor(dfa$t,labels = c("t=4","t=8","t=10"))

dfsd <- read_csv("./predict_full_sd_new.csv")
df1 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),sd=t(dfsd[1,1:12]))
df2 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),sd=t(dfsd[2,1:12]))
df3 <- data.frame(t=rep(c(4,8,10),each=4),deltat=rep(c(2,5,7,10),times=3),sd=t(dfsd[3,1:12]))
df1$variable <- "EMO"
df2$variable <- "ADL"
df3$variable <- "BMI"
dfb <- bind_rows(df1,df2,df3)
dfb$t <- factor(dfa$t,labels = c("t=4","t=8","t=10"))

dfa$sd <- dfb$sd

p2 <- ggplot(dfa)+geom_point(aes(x=deltat,y=auc,colour=variable),size=1)+
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
p1 <- ggplot(dfb)+geom_point(aes(x=t,y=AUC,colour=variable),size=1)+
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
p3 <- ggplot(df)+geom_point(aes(x=eval,y=cindex,colour=model),size=1)+
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

((p1+p2+plot_layout(widths = c(1.5, 2)))/p3) +plot_annotation(tag_levels = "a")
ggsave("perform.pdf",dpi=600,width=9,height=9)
ggsave("perform.png",dpi=600,width=9,height=9)
