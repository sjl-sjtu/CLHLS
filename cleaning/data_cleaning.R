setwd("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS")
library(tidyverse)
library(data.table)
library(foreign)
library(stringr)
Sys.setlocale("LC_ALL", "English")

#read data set from sav files
#######################################################################
dataset_1998_2018=read.spss("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/clhls_1998_2018_longitudinal_dataset_released_version1.sav")
fwrite(dataset_1998_2018,"dataset_1998_2018.csv")
dataset_2000_2018=read.spss("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/clhls_2000_2018_longitudinal_dataset_released_version1.sav")
fwrite(dataset_2000_2018,"dataset_2000_2018.csv")
dataset_2002_2018=read.spss("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/clhls_2002_2018_longitudinal_dataset_released_version1.sav")
fwrite(dataset_2002_2018,"dataset_2002_2018.csv")
dataset_2005_2018=read.spss("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/clhls_2005_2018_longitudinal_dataset_released_version1.sav")
fwrite(dataset_2005_2018,"dataset_2005_2018.csv")
dataset_2008_2018=read.spss("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/clhls_2008_2018_longitudinal_dataset_released_version1.sav")
fwrite(dataset_2008_2018,"dataset_2008_2018.csv")
dataset_2011_2018=read.spss("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/clhls_2011_2018_longitudinal_dataset_released_version1.sav")
fwrite(dataset_2011_2018,"dataset_2011_2018.csv")
dataset_2014_2018=read.spss("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/clhls_2014_2018_longitudinal_dataset_released_version1.sav")
fwrite(dataset_2014_2018,"dataset_2014_2018.csv")


#######################################
dataset_1998_2018=fread("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_1998_2018.csv")
dataset_2000_2018=fread("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2000_2018.csv")
dataset_2002_2018=fread("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2002_2018.csv")
dataset_2005_2018=fread("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2005_2018.csv")
dataset_2008_2018=fread("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2008_2018.csv")
dataset_2011_2018=fread("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2011_2018.csv")
dataset_2014_2018=fread("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2014_2018.csv")

mergeTwo <- function(x,y){
  xname <- colnames(x)
  yname <- colnames(y)
  coname <- intersect(xname,yname)
  z <- merge(x,y,by=coname,all=TRUE)
  return(z)
}

#心理情绪指标
p1 <- colnames(dataset_1998_2018)
p2 <- colnames(dataset_2000_2018)
p3 <- colnames(dataset_2002_2018)
p4 <- colnames(dataset_2005_2018)
p5 <- colnames(dataset_2008_2018)
p6 <- colnames(dataset_2011_2018)
p7 <- colnames(dataset_2014_2018)
p <- reduce(list(p1,p2,p3,p4,p5,p6,p7),union)
p <- sort(p)

q98 <- c("b11","b21","b22","b23","b24","b25","b26","b27")
q00 <- c("b11_0","b21_0","b22_0","b23_0","b24_0","b25_0","b26_0","b27_0")
q02 <- c("b11_2","b21_2","b22_2","b23_2","b24_2","b25_2","b26_2","b27_2")
q05 <- c("b11_5","b21_5","b22_5","b23_5","b24_5","b25_5","b26_5","b27_5")
q08 <- c("b11_8","b21_8","b22_8","b23_8","b24_8","b25_8","b26_8","b27_8")
q11 <- c("b11_11","b21_11","b22_11","b23_11","b24_11","b25_11","b26_11",
         "b27_11")
q14 <- c("b11_14","b21_14","b22_14","b23_14","b24_14","b25_14","b26_14",
         "b27_14")
q18 <- c("b11_18","b21_18","b22_18","b26_18","b34_18","b36_18","b37_18",
         "b38_18")
q <- c(q98,q00,q02,q05,q08,q11,q14,q18)
q1 <- intersect(q,p1)
q2 <- intersect(q,p2)
q3 <- intersect(q,p3)
q4 <- intersect(q,p4)
q5 <- intersect(q,p5)
q6 <- intersect(q,p6)
q7 <- intersect(q,p7)

dff1 <- dataset_1998_2018[,c("id",q1),with=F]
dff2 <- dataset_2000_2018[,c("id",q2),with=F]
dff3 <- dataset_2002_2018[,c("id",q3),with=F]
dff4 <- dataset_2005_2018[,c("id",q4),with=F]
dff5 <- dataset_2008_2018[,c("id",q5),with=F]
dff6 <- dataset_2011_2018[,c("id",q6),with=F]
dff7 <- dataset_2014_2018[,c("id",q7),with=F]

baseq <- c("b11","b21","b22","b23","b24","b25","b26","b27")
dff2 <- dff2 %>% rename_at(vars(all_of(baseq)), ~ paste0(., "_0"))
dff3 <- dff3 %>% rename_at(vars(all_of(baseq)), ~ paste0(., "_2"))
dff4 <- dff4 %>% rename_at(vars(all_of(baseq)), ~ paste0(., "_5"))
dff5 <- dff5 %>% rename_at(vars(all_of(baseq)), ~ paste0(., "_8"))
dff6 <- dff6 %>% rename_at(vars(all_of(baseq)), ~ paste0(., "_11"))
dff7 <- dff7 %>% rename_at(vars(all_of(baseq)), ~ paste0(., "_14"))

fwrite(dff1,"dff1.csv")
fwrite(dff2,"dff2.csv")
fwrite(dff3,"dff3.csv")
fwrite(dff4,"dff4.csv")
fwrite(dff5,"dff5.csv")
fwrite(dff6,"dff6.csv")
fwrite(dff7,"dff7.csv")

dff2 <- dff2[-which(dff2$id %in% dff1$id),]
dff3 <- dff3[-which(dff3$id %in% c(dff1$id,dff2$id)),]
dff4 <- dff4[-which(dff4$id %in% c(dff1$id,dff2$id,dff3$id)),]
dff5 <- dff5[-which(dff5$id %in% c(dff1$id,dff2$id,dff3$id,dff4$id)),]
dff6 <- dff6[-which(dff6$id %in% c(dff1$id,dff2$id,dff3$id,dff4$id,dff5$id)),]
dff7 <- dff7[-which(dff7$id %in% c(dff1$id,dff2$id,dff3$id,dff4$id,dff5$id,dff6$id)),]

dff <- reduce(list(dff1,dff2,dff3,dff4,dff5,dff6,dff7),mergeTwo)
dff <- dff %>% mutate_all(~str_replace_all(., " ", ""))
fwrite(dff,"dffeel.csv",row.names = FALSE)

qall <- colnames(dff)
sort(qall)


library(car)
#1998
dff$b11 <- recode(dff$b11,'"verygood"=5;"good"=4;"soso"=3;"bad"=2;"verybad"=1;else=NA')
dff$b21 <- recode(dff$b21,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b22 <- recode(dff$b22,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b23 <- recode(dff$b23,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b24 <- recode(dff$b24,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b25 <- recode(dff$b25,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b26 <- recode(dff$b26,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b27 <- recode(dff$b27,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')

#2000
dff$b11_0 <- recode(dff$b11_0,'"verygood"=5;"good"=4;"soso"=3;"bad"=2;"verybad"=1;else=NA')
dff$b21_0 <- recode(dff$b21_0,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b22_0 <- recode(dff$b22_0,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b23_0 <- recode(dff$b23_0,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b24_0 <- recode(dff$b24_0,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b25_0 <- recode(dff$b25_0,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b26_0 <- recode(dff$b26_0,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b27_0 <- recode(dff$b27_0,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')

#2002
dff$b11_2 <- recode(dff$b11_2,'"verygood"=5;"good"=4;"soso"=3;"bad"=2;"verybad"=1;else=NA')
dff$b21_2 <- recode(dff$b21_2,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b22_2 <- recode(dff$b22_2,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b23_2 <- recode(dff$b23_2,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b24_2 <- recode(dff$b24_2,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b25_2 <- recode(dff$b25_2,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b26_2 <- recode(dff$b26_2,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b27_2 <- recode(dff$b27_2,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')

#2005
dff$b11_5 <- recode(dff$b11_5,'"verygood"=5;"good"=4;"soso"=3;"bad"=2;"verybad"=1;else=NA')
dff$b21_5 <- recode(dff$b21_5,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b22_5 <- recode(dff$b22_5,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b23_5 <- recode(dff$b23_5,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b24_5 <- recode(dff$b24_5,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b25_5 <- recode(dff$b25_5,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b26_5 <- recode(dff$b26_5,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b27_5 <- recode(dff$b27_5,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')

#2008
dff$b11_8 <- recode(dff$b11_8,'"verygood"=5;"good"=4;"soso"=3;"bad"=2;"verybad"=1;else=NA')
dff$b21_8 <- recode(dff$b21_8,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b22_8 <- recode(dff$b22_8,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b23_8 <- recode(dff$b23_8,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b24_8 <- recode(dff$b24_8,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b25_8 <- recode(dff$b25_8,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b26_8 <- recode(dff$b26_8,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b27_8 <- recode(dff$b27_8,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')

#2011
dff$b11_11 <- recode(dff$b11_11,'"verygood"=5;"good"=4;"soso"=3;"bad"=2;"verybad"=1;else=NA')
dff$b21_11 <- recode(dff$b21_11,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b22_11 <- recode(dff$b22_11,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b23_11 <- recode(dff$b23_11,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b24_11 <- recode(dff$b24_11,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b25_11 <- recode(dff$b25_11,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b26_11 <- recode(dff$b26_11,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b27_11 <- recode(dff$b27_11,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')

#2014
dff$b11_14 <- recode(dff$b11_14,'"verygood"=5;"good"=4;"soso"=3;"bad"=2;"verybad"=1;else=NA')
dff$b21_14 <- recode(dff$b21_14,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b22_14 <- recode(dff$b22_14,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b23_14 <- recode(dff$b23_14,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b24_14 <- recode(dff$b24_14,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b25_14 <- recode(dff$b25_14,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b26_14 <- recode(dff$b26_14,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"never"=5;else=NA') #负面问题
dff$b27_14 <- recode(dff$b27_14,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')

#2018
dff$b11_18 <- recode(dff$b11_18,'"verygood"=5;"good"=4;"soso"=3;"bad"=2;"verybad"=1;else=NA')
dff$b21_18 <- recode(dff$b21_18,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b22_18 <- recode(dff$b22_18,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b26_18 <- recode(dff$b26_18,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"never"=1;else=NA')
dff$b34_18 <- recode(dff$b34_18,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"rarelyornever"=5;else=NA') #负面问题
dff$b36_18 <- recode(dff$b36_18,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"rarelyornever"=5;else=NA') #负面问题
dff$b37_18 <- recode(dff$b37_18,'"always"=5;"often"=4;"sometimes"=3;"seldom"=2;"rarelyornever"=1;else=NA')
dff$b38_18 <- recode(dff$b38_18,'"always"=1;"often"=2;"sometimes"=3;"seldom"=4;"rarelyornever"=5;else=NA') #负面问题

dff$emo98 <- rowSums(dff[,q98,with=F])
dff$emo00 <- rowSums(dff[,q00,with=F])
dff$emo02 <- rowSums(dff[,q02,with=F])
dff$emo05 <- rowSums(dff[,q05,with=F])
dff$emo08 <- rowSums(dff[,q08,with=F])
dff$emo11 <- rowSums(dff[,q11,with=F])
dff$emo14 <- rowSums(dff[,q14,with=F])
dff$emo18 <- rowSums(dff[,q18,with=F])
vemo <- c("emo98","emo00","emo02","emo05","emo08","emo11","emo14","emo18")
dffemo <- dff[,c("id",vemo),with=F]

library(reshape2)
dffemo <- data.table::melt(dffemo,measure=vemo,variable.name = "year",value.name = "emo")
dffemo$year <- recode(dffemo$year,'"emo98"=0;"emo00"=2;"emo02"=4;"emo05"=7;
                      "emo08"=10;"emo11"=13;"emo14"=16;"emo18"=20')
fwrite(dffemo,"dffemo.csv")

#######################################################################

vid=c("id")

# baseline
vsex=c("a1")
vage=c("trueage")
vresid=c("residenc")
vedu=c("f1")
vocc=c("f2")
vmar=c("f41")

#1998-2018
vcog=c()
for (i in c("","_0","_2","_5","_8","_11","_14","_18")){
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vADL=c()
for (i in c("","_0","_2","_5","_8","_11","_14","_18")){
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vstatus=c("dth98_00","dth00_02","dth02_05","dth05_08","dth08_11","dth11_14","dth14_18")
vdoi=c("year9899","month98","date98","month_0","day_0","month_2","day_2","month_5","day_5",
       "year_8","month_8","day_8","yearin_11","monthin_11","dayin_11","yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18") #日期
df1an <- dataset_1998_2018[,c(vid,vsex,vage,vresid,vedu,vocc,vmar,vcog,vADL,vstatus,vdoi),with=F]
df1an <- df1an %>% mutate_at(vars(all_of(vdoi)), as.numeric)


#2000-2018
vcog=c()
for (i in c("","_2","_5","_8","_11","_14","_18")){
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vADL=c()
for (i in c("","_2","_5","_8","_11","_14","_18")){
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vstatus=c("dth00_02","dth02_05","dth05_08","dth08_11","dth11_14","dth14_18")
vdoi=c("month_0","day_0","month_2","day_2","month_5","day_5",
       "year_8","month_8","day_8","yearin_11","monthin_11","dayin_11","yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18") #日期
colnames(dataset_2000_2018)[which(colnames(dataset_2000_2018)%in% c("month00","day00"))] <- c("month_0","day_0")
df2an <- dataset_2000_2018[,c(vid,vsex,vage,vresid,vedu,vocc,vmar,vcog,vADL,vstatus,vdoi),with=F]
L <- sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
               "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
               "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s","e1%s","e2%s",
               "e3%s","e4%s","e5%s","e6%s"),"")
Lchange <- sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
                     "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
                     "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s","e1%s","e2%s",
                     "e3%s","e4%s","e5%s","e6%s"),"_0")
colnames(df2an)[which(colnames(df2an) %in% L)] <- Lchange
df2an <- df2an %>% mutate_at(vars(all_of(vdoi)), as.numeric)

#2002-2018
vcog=c()
for (i in c("","_5","_8","_11","_14","_18")){
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vADL=c()
for (i in c("","_5","_8","_11","_14","_18")){
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vstatus=c("dth02_05","dth05_08","dth08_11","dth11_14","dth14_18")
vdoi=c("month_2","day_2","month_5","day_5",
       "year_8","month_8","day_8","yearin_11","monthin_11","dayin_11","yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18") #日期
colnames(dataset_2002_2018)[which(colnames(dataset_2002_2018)%in% c("month02","day02"))] <- c("month_2","day_2")
df3an <- dataset_2002_2018[,c(vid,vsex,vage,vresid,vedu,vocc,vmar,vcog,vADL,vstatus,vdoi),with=F]
Lchange<-sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
                   "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
                   "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s","e1%s","e2%s",
                   "e3%s","e4%s","e5%s","e6%s"),"_2")
colnames(df3an)[which(colnames(df3an) %in% L)] <- Lchange
df3an <- df3an %>% mutate_at(vars(all_of(vdoi)), as.numeric)

#2005-2018
vcog=c()
for (i in c("","_8","_11","_14","_18")){
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vADL=c()
for (i in c("","_8","_11","_14","_18")){
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vstatus=c("dth05_08","dth08_11","dth11_14","dth14_18")
vdoi=c("month_5","day_5","year_8","month_8","day_8","yearin_11","monthin_11","dayin_11","yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18") #日期
colnames(dataset_2005_2018)[which(colnames(dataset_2005_2018)%in% c("monthin","dayin"))] <- c("month_5","day_5")
df4an <- dataset_2005_2018[,c(vid,vsex,vage,vresid,vedu,vocc,vmar,vcog,vADL,vstatus,vdoi),with=F]
Lchange<-sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
                   "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
                   "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s","e1%s","e2%s",
                   "e3%s","e4%s","e5%s","e6%s"),"_5")
colnames(df4an)[which(colnames(df4an) %in% L)] <- Lchange
df4an <- df4an %>% mutate_at(vars(all_of(vdoi)), as.numeric)

#2008-2018
vcog=c()
for (i in c("","_11","_14","_18")){
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vADL=c()
for (i in c("","_11","_14","_18")){
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vstatus=c("dth08_11","dth11_14","dth14_18")
vdoi=c("year_8","month_8","day_8","yearin_11","monthin_11","dayin_11","yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18") #日期
colnames(dataset_2008_2018)[which(colnames(dataset_2008_2018)%in% c("yearin","monthin","dayin"))] <- c("year_8","month_8","day_8")
df5an <- dataset_2008_2018[,c(vid,vsex,vage,vresid,vedu,vocc,vmar,vcog,vADL,vstatus,vdoi),with=F]
Lchange<-sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
                   "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
                   "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s","e1%s","e2%s",
                   "e3%s","e4%s","e5%s","e6%s"),"_8")
colnames(df5an)[which(colnames(df5an) %in% L)] <- Lchange
df5an <- df5an %>% mutate_at(vars(all_of(vdoi)), as.numeric)

#2011-2018
vcog=c()
for (i in c("","_14","_18")){
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vADL=c()
for (i in c("","_14","_18")){
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vstatus=c("dth11_14","dth14_18")
vdoi=c("yearin_11","monthin_11","dayin_11","yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18") #日期
colnames(dataset_2011_2018)[which(colnames(dataset_2011_2018)%in% c("yearin","monthin","dayin"))] <- c("yearin_11","monthin_11","dayin_11")
df6an <- dataset_2011_2018[,c(vid,vsex,vage,vresid,vedu,vocc,vmar,vcog,vADL,vstatus,vdoi),with=F]
Lchange<-sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
                   "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
                   "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s","e1%s","e2%s",
                   "e3%s","e4%s","e5%s","e6%s"),"_11")
colnames(df6an)[which(colnames(df6an) %in% L)] <- Lchange
df6an <- df6an %>% mutate_at(vars(all_of(vdoi)), as.numeric)

#2014-2018
vcog=c()
for (i in c("","_18")){
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vADL=c()
for (i in c("","_18")){
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vstatus=c("dth14_18")
vdoi=c("yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18") #日期
colnames(dataset_2014_2018)[which(colnames(dataset_2014_2018)%in% c("yearin","monthin","dayin"))] <- c("yearin_14","monthin_14","dayin_14")
df7an <- dataset_2014_2018[,c(vid,vsex,vage,vresid,vedu,vocc,vmar,vcog,vADL,vstatus,vdoi),with=F]
Lchange<-sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
                   "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
                   "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s","e1%s","e2%s",
                   "e3%s","e4%s","e5%s","e6%s"),"_14")
colnames(df7an)[which(colnames(df7an) %in% L)] <- Lchange
df7an <- df7an %>% mutate_at(vars(all_of(vdoi)), as.numeric)


fwrite(df1an,"df1an.csv")
fwrite(df2an,"df2an.csv")
fwrite(df3an,"df3an.csv")
fwrite(df4an,"df4an.csv")
fwrite(df5an,"df5an.csv")
fwrite(df6an,"df6an.csv")
fwrite(df7an,"df7an.csv")

df2an <- df2an[-which(df2an$id %in% df1an$id),]
df3an <- df3an[-which(df3an$id %in% c(df1an$id,df2an$id)),]
df4an <- df4an[-which(df4an$id %in% c(df1an$id,df2an$id,df3an$id)),]
df5an <- df5an[-which(df5an$id %in% c(df1an$id,df2an$id,df3an$id,df4an$id)),]
df6an <- df6an[-which(df6an$id %in% c(df1an$id,df2an$id,df3an$id,df4an$id,df5an$id)),]
df7an <- df7an[-which(df7an$id %in% c(df1an$id,df2an$id,df3an$id,df4an$id,df5an$id,df6an$id)),]

dfall <- reduce(list(df1an,df2an,df3an,df4an,df5an,df6an,df7an),mergeTwo)
dfall <- dfall %>% mutate_all(~str_replace_all(., " ", ""))
fwrite(dfall,"dfall.csv")


dfall <- fread("dfall.csv")

dfall$a1 <- recode(dfall$a1,"'male'=1;'female'=0")  #性别???1-男；0-???
dfall$residenc <- recode(dfall$residenc,"c('city','town','urban(cityandtown)')=1;
                         'rural'=0")  #居住地：1-城；0-???
mentalOcc <- c("governmental,institutionalormanagerialpersonnel",
               "professionalandtechnicalpersonnel")
physiOcc <- c("agriculture,forest,animalhusbandry","agriculture,forestry,animalhusbandryorfisheryworker",
              "commercial,serviceorindustrialworker","commercialorserviceworker","fisheryworker",
              "housework","houseworker","industrialworker","militarypersonnel","personnelinagriculturefisheryforestryanimalhusbandry",
              "self-employed","staff/serviceworkers/industrialworkers")
dfall$f2 <- recode(dfall$f2,'mentalOcc = 1;physiOcc = 0;else=NA')  #职业???0-体力???1-脑力
dfall$f41 <- recode(dfall$f41,"'nevermarried'=0;c('divorced','separated','widowed','marriedbutnotlivingwithspouse')=1;
                    'currentlymarriedandlivingwithspouse'=2;else=NA") #婚姻状况
dfall$trueage <- as.numeric(dfall$trueage)
dfall$f1 <- as.numeric(dfall$f1)
colnames(dfall)[which(colnames(dfall) %in% c("a1","trueage","residenc","f1","f2","f41"))] <- c("sex","age","residenc","edu","occ","marriage")

# MSE
vcog=c()
for (i in c("","_0","_2","_5","_8","_11","_14","_18")){
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i) #"c16%s"是列举题，暂时除???
  vcog=c(vcog,a)
}

for(i in vcog){
  dfall[[i]] <- recode(dfall[[i]],'"correct"=1;c("can\'tusepentodrawthefigure","notabletoanswer",
                      "notabletodo","notabletodothis(disabled)","unabletoanswer","unabletodo",
                      "wrong")=0;else=NA')
}

dfnew <- dfall

#score for 
a <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"c16")
b <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"c16trans")
for(i in 1:length(a)){
  dfnew[[a[i]]] <- as.numeric(dfnew[[a[i]]])
  dfnew[[b[i]]] <- dfnew[[a[i]]]
  dfnew[[b[i]]][which(dfnew[[b[i]]]>=7)] <- 7
}

#ADL
a <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"e1")
for(i in a){
  dfnew[[i]] <- recode(dfnew[[i]],"'morethanonepartassistance'=0;
                      'onepartassistance'=1;'withoutassistance'=2;else=NA")
}

a <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"e2")
for(i in a){
  dfnew[[i]] <- recode(dfnew[[i]],"c('assistanceingettingclothesandgettingdressed','morethanonepartassistance')=0;
                      c('needassistancefortryingshoes','onepartassistance')=1;
                      'withoutassistance'=2;else=NA")
}

a <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"e3")
for(i in a){
  dfnew[[i]] <- recode(dfnew[[i]],'c("don\'tusetoilet","morethanonepartassistance")=0;
                      c("assistanceincleaningorarrangingclothes","onepartassistance")=1;
                      "withoutassistance"=2;else=NA')
}

a <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"e4")
for(i in a){
  dfnew[[i]] <- recode(dfnew[[i]],'c("bedridden","morethanonepartassistance")=0;
                      c("withassistance","onepartassistance")=1;"withoutassistance"=2;else=NA')
}

a <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"e5")
for(i in a){
  dfnew[[i]] <- recode(dfnew[[i]],'c("incontinent","morethanonepartassistance")=0;
                      c("occasionalaccidents","onepartassistance")=1;"withoutassistance"=2;else=NA')
}

a <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"e6")
for(i in a){
  dfnew[[i]] <- recode(dfnew[[i]],'c("needfeeding","morethanonepartassistance")=0;
                      c("withsomehelp","onepartassistance")=1;"withoutassistance"=2;else=NA')
}

#计算量表得分
vmse <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"mse")
mmselist <- list()
p <- c("","_0","_2","_5","_8","_11","_14","_18")
for (i in 1:length(p)){
  k <- p[i]
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16trans%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),k)
  dfmmse <- dfnew[,..a]
  mmse <- c(unlist(apply(dfmmse,1,sum)))
  mmselist[i] <- list(mmse)
}
for(i in 1:length(vmse)){
  dfnew[[vmse[i]]] <- mmselist[[i]]
}
dfnew[,..vmse]

#判断是否精神疾病
vmmse <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"mmse")
for(i in 1:length(vmmse)){
  dfnew[[vmmse[i]]] <- ifelse(dfnew[[vmse[i]]]<18,1,0)  #CI-1,non-0
}
dfnew[,..vmmse]

#计算ADL自评
adllist <- list()
p <- c("","_0","_2","_5","_8","_11","_14","_18")
for (i in 1:length(p)){
  k <- p[i]
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),k)
  dfadl <- dfnew[,..a]
  adl <- c(unlist(apply(dfadl,1,sum)))
  adllist[i] <- list(adl)
}
vadl <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"adl")
for(i in 1:length(vadl)){
  dfnew[[vadl[i]]] <- adllist[[i]]
}
dfnew[,..vadl]

#状???
dfnew$dth98_00 <- recode(dfnew$dth98_00,'"diedbefore2000survey"=0;"stillaliveat2000survey"=1;
                         "losttofollow-upinthe2000survey"=NA') 
dfnew$dth00_02 <- recode(dfnew$dth00_02,'"diedbeforethe2002survey"=0;"survivingatthe2002survey"=1;
                         c("diedorlosttofollow-upinpreviouswaves","losttofollow-upatthe2002survey")=NA') 
dfnew$dth02_05 <- recode(dfnew$dth02_05,'"diedbeforethe2005survey"=0;"survivingatthe2005survey"=1;
                         c("diedorlosttofollow-upinpreviouswaves","losttofollow-upatthe2005survey","losttofollow-upatthe2005wave",
                         "losttofollow-upinthe2005survey")=NA')
dfnew$dth05_08 <- recode(dfnew$dth05_08,'"diedbeforethe2008survey"=0;"survivingatthe2008survey"=1;
                         c("diedorlosttofollow-upinpreviouswaves","losttofollow-upinthe2008survey")=NA')
dfnew$dth08_11 <- recode(dfnew$dth08_11,'"diedbeforethe2011/2012survey"=0;c("survivingat2011surveybutdiedbefore2012survey","survivingatthe2011/2012survey")=1;
                         c("diedorlosttofollow-upinpreviouswaves","losttofollow-upinthe2011/2012survey")=NA')
dfnew$dth11_14 <- recode(dfnew$dth11_14,'"diedbeforethe2014survey"=0;"survivingatthe2014survey"=1;
                         c("diedorlosttofollow-upinpreviouswaves","losttofollow-upinthe2014survey")=NA')
dfnew$dth14_18 <- recode(dfnew$dth14_18,'"diedbeforethe2018survey"=0;"survivingatthe2018survey"=1;
                         "losttofollow-upinthe2018survey"=NA')

idstart <- unique(df1an$id)

vstatus=c("dth98_00","dth00_02","dth02_05","dth05_08","dth08_11","dth11_14","dth14_18")
dfifdie <- dfnew[,c(vid,vstatus),with = F]
dfifdie <- dfifdie[, dth98 := ifelse(id %in% idstart, 1, NA)]
vstatus=c("dth98","dth98_00","dth00_02","dth02_05","dth05_08","dth08_11","dth11_14","dth14_18")
dfifdie <- melt(dfifdie,measure=vstatus,variable.name = "year",value.name = "status")
dfifdie$year <- recode(dfifdie$year,'"dth98"=0;"dth98_00"=2;"dth00_02"=4;"dth02_05"=7;"dth05_08"=10;
                       "dth08_11"=13;"dth11_14"=16;"dth14_18"=20')
dfifdie$year <- as.character(dfifdie$year)
dfifhea <- dfnew[,c(vid,vmmse),with=F]
dfifhea <- melt(dfifhea,measure=vmmse,variable.name = "year",value.name = "mental")
dfifhea$year <- recode(dfifhea$year,'"mmse"=0;"mmse_0"=2;"mmse_2"=4;"mmse_5"=7;"mmse_8"=10;
                       "mmse_11"=13;"mmse_14"=16;"mmse_18"=20')
dfifhea$year <- as.character(dfifhea$year)
dfif <- merge(dfifdie,dfifhea,by=c("id","year"))
dfif$year <- as.numeric(dfif$year)
dfif <- dfif[-which(is.na(dfif$mental)),]
dfif <- arrange(dfif,id,year)
dfif2 <-split(dfif,dfif$id)

#入组时间、患病时间、出组时间
id1998 <- df1an$id
id2000 <- df2an$id
id2002 <- df3an$id
id2005 <- df4an$id
id2008 <- df5an$id
id2011 <- df6an$id
id2014 <- df7an$id
enterTime <- function(x){
  if(x$id[1] %in% id1998){
    et <- c(x$id[1],0)
  }else if(x$id[1] %in% id2000){
    et <- c(x$id[1],2)
  }else if(x$id[1] %in% id2002){
    et <- c(x$id[1],4)
  }else if(x$id[1] %in% id2005){
    et <- c(x$id[1],7)
  }else if(x$id[1] %in% id2008){
    et <- c(x$id[1],10)
  }else if(x$id[1] %in% id2011){
    et <- c(x$id[1],13)
  }else if(x$id[1] %in% id2014){
    et <- c(x$id[1],16)
  }
  return(et)
}
illTime <- function(x){
  it <- c(x$id[1],NA)
  for(i in 1:nrow(x)){
    #if(is.na(x$status[i])){
    #  next;
    #}
    if(!is.na(x$mental[i]) & x$mental[i] == 1){ #(x$status[i]==1 & x$mental[i]==1){
      it <- c(x$id[i],x$year[i])
      break;
    }
  }
  return(it)
}
outTime <- function(x){
  ot <- c(x$id[1],NA)
  for(i in 1:nrow(x)){
    if(is.na(x$mental[i]) & x$year[i]>x$enterT[i]){#((is.na(x$status[i])|x$status[i]==0) & x$year[i]>=x$enterT[i]){
      ot <- c(x$id[i],x[i-1,2])
      break;
    }else{
      ot <- c(x$id[i],20)
    }
  }
  return(ot)
}

et <- lapply(dfif2,enterTime)
et <- unlist(et)
et <- matrix(et,nrow=2) #第一行id，第二行year
et <- tibble(id=et[1,],enterT=et[2,])
dfnew2 <- merge(dfnew,et,by="id")

it <- lapply(dfif2,illTime)
it <- unlist(it)
it <- matrix(it,nrow=2) #第一行id，第二行year
it <- tibble(id=it[1,],illT=it[2,])

dfifhea <- dfnew2[,c(vid,vmmse,"enterT"),with=F]
dfifhea <- melt(dfifhea,measure=vmmse,variable.name = "year",value.name = "mental")
dfifhea$year <- recode(dfifhea$year,'"mmse"=0;"mmse_0"=2;"mmse_2"=4;"mmse_5"=7;"mmse_8"=10;
                       "mmse_11"=13;"mmse_14"=16;"mmse_18"=20')
dfifhea$year <- as.character(dfifhea$year)
dfif <- merge(dfifdie,dfifhea,by=c("id","year"))
dfif$year <- as.numeric(dfif$year)
dfif <- arrange(dfif,id,year)
dfif2 <-split(dfif,dfif$id)

ot <- lapply(dfif2,outTime)
ot <- unlist(ot)
ot <- matrix(ot,nrow=2) #第一行id，第二行year
ot <- tibble(id=ot[1,],outT=ot[2,])

dfsur <- dfnew[,c("id","sex","age","residenc","edu","occ","marriage"),with=F]
dfsur <- merge(dfsur,et,by="id")
dfsur <- merge(dfsur,it,by="id")
dfsur <- merge(dfsur,ot,by="id")

#具体时间
vdoi=c("year9899","month98","date98","month_0","day_0","month_2","day_2","month_5","day_5",
       "year_8","month_8","day_8","yearin_11","monthin_11","dayin_11","yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18")
dfdate <- dfall[,c(vid,vdoi),with=F]
dfdate$year_8 <- recode(dfdate$year_8,'c("-9","2009")=2009;"-7"=2007;c("-8","2008")=2008')
for(i in vdoi){
  dfdate[[i]]<-replace(dfdate[[i]],which(dfdate[[i]]==99),NA)
  dfdate[[i]]<- as.numeric(dfdate[[i]])
}

#
dfdate[, time98 := fifelse(rowSums(is.na(.SD))==0,
                           paste(year9899, month98, date98, sep="-"),
                           NA), .SDcols = c("year9899", "month98", "date98")]

dfdate[, time00 := fifelse(rowSums(is.na(.SD))==0,
    paste("2000", month_0, day_0, sep="-"),
    NA), .SDcols = c("month_0", "day_0")]

dfdate[, time02 := fifelse(rowSums(is.na(.SD))==0,
    paste("2002", month_2, day_2, sep="-"),
    NA), .SDcols = c("month_2", "day_2")]

dfdate[, time05 := fifelse(rowSums(is.na(.SD))==0,
    paste("2005", month_5, day_5, sep="-"),
    NA), .SDcols = c("month_5", "day_5")]

dfdate[, time08 := fifelse(rowSums(is.na(.SD))==0,
    paste(year_8, month_8, day_8, sep="-"),
    NA), .SDcols = c("year_8", "month_8", "day_8")]

dfdate[, time11 := fifelse(rowSums(is.na(.SD))==0,
    paste(yearin_11, monthin_11, dayin_11, sep="-"),
    NA), .SDcols = c("yearin_11", "monthin_11", "dayin_11")]

dfdate[, time14 := fifelse(rowSums(is.na(.SD))==0,
    paste(yearin_14, monthin_14, dayin_14, sep="-"),
    NA), .SDcols = c("yearin_14", "monthin_14", "dayin_14")]

dfdate[, time18 := fifelse(rowSums(is.na(.SD))==0,
    paste(yearin_18, monthin_18, dayin_18, sep="-"),
    NA), .SDcols = c("yearin_18", "monthin_18", "dayin_18")]

vtime <- paste0("time",c("98","00","02","05","08","11","14","18"))
dfdate2 <- dfdate[,c("id",vtime),with=F]
dfdate2[, (vtime) := lapply(.SD, as.Date,format = "%Y-%m-%d"), .SDcols = vtime]
dfdate2[, (vtime) := lapply(.SD, as.numeric), .SDcols = vtime]

dfdate2[, baseTime := case_when(
  id %in% id1998 ~ time98,
  id %in% id2000 ~ time00,
  id %in% id2002 ~ time02,
  id %in% id2005 ~ time05,
  id %in% id2008 ~ time08,
  id %in% id2011 ~ time11,
  id %in% id2014 ~ time14,
  TRUE ~ NA
)]

#确诊时间
illid1998 <- dfsur$id[which(dfsur$illT==0)]
illid2000 <- dfsur$id[which(dfsur$illT==2)]
illid2002 <- dfsur$id[which(dfsur$illT==4)]
illid2005 <- dfsur$id[which(dfsur$illT==7)]
illid2008 <- dfsur$id[which(dfsur$illT==10)]
illid2011 <- dfsur$id[which(dfsur$illT==13)]
illid2014 <- dfsur$id[which(dfsur$illT==16)]
illid2018 <- dfsur$id[which(dfsur$illT==20)]
dfdate2[, illTime := case_when(
  id %in% illid1998 ~ time98,
  id %in% illid2000 ~ time00,
  id %in% illid2002 ~ time02,
  id %in% illid2005 ~ time05,
  id %in% illid2008 ~ time08,
  id %in% illid2011 ~ time11,
  id %in% illid2014 ~ time14,
  id %in% illid2018 ~ time18,
  TRUE ~ NA
)]

#出组时间
outid1998 <- dfsur$id[which(dfsur$outT==0)]
outid2000 <- dfsur$id[which(dfsur$outT==2)]
outid2002 <- dfsur$id[which(dfsur$outT==4)]
outid2005 <- dfsur$id[which(dfsur$outT==7)]
outid2008 <- dfsur$id[which(dfsur$outT==10)]
outid2011 <- dfsur$id[which(dfsur$outT==13)]
outid2014 <- dfsur$id[which(dfsur$outT==16)]
outid2018 <- dfsur$id[which(dfsur$outT==20)]
dfdate2[, outTime := case_when(
  id %in% outid1998 ~ time98,
  id %in% outid2000 ~ time00,
  id %in% outid2002 ~ time02,
  id %in% outid2005 ~ time05,
  id %in% outid2008 ~ time08,
  id %in% outid2011 ~ time11,
  id %in% outid2014 ~ time14,
  id %in% outid2018 ~ time18
)]

dfsur <- merge(dfsur,dfdate2[,c("id","baseTime","illTime","outTime"),with=F],by="id")

dfsur[, outcome := ifelse(is.na(illT), 0, 1)]
dfsur[, totalT := ifelse(is.na(illT), outT - enterT, illT - enterT)]
dfsur[, totalTime := ifelse(
  !is.na(outTime) & !is.na(baseTime),
  outTime - baseTime,
  ifelse(
    !is.na(totalT),
    totalT * 365,
    NA
  )
)]

dfdate3 <- merge(dfdate2,dfsur[,c("id","enterT"),with=F],by="id")
dfdate3 <- data.table::melt(dfdate3,id=c("id","baseTime","illTime","outTime","enterT"),variable.name = "year",value.name = "time")
dfdate3$year <- recode(dfdate3$year,'"time98"=0;"time00"=2;
                       "time02"=4;"time05"=7;"time08"=10;"time11"=13;
                       "time14"=16;"time18"=20')
dfdate3$year <- as.character(dfdate3$year)
dfdate3$year <- as.numeric(dfdate3$year)
dfdate3[, times := ifelse(!is.na(baseTime) & !is.na(time), time - baseTime, NA)]
fwrite(dfdate3,"dfdate3.csv")

dflongi <- dfnew[,c("id","sex","age","residenc","edu","occ","marriage",vadl),with=F]
dflongi <- merge(dflongi,dfsur[,c("id","outcome","totalTime"),with=F],by="id")

dflongi <- data.table::melt(dflongi,measure=vadl,variable.name = "year",value.name = "ADL")
dflongi$year <- recode(dflongi$year,'"adl"=0;"adl_0"=2;"adl_2"=4;"adl_5"=7;"adl_8"=10;"adl_11"=13;
                     "adl_14"=16;"adl_18"=20')
dflongi$year <- as.character(dflongi$year)
dflongi$year <- as.numeric(dflongi$year)

dflongi <- merge(dflongi,dfdate3[,c("id","baseTime","illTime","outTime","year","times")],by=c("id","year"))
dflongi$id <- as.character(dflongi$id)
dffemo$year <- as.numeric(as.character(dffemo$year))
dflongi <- merge(dflongi,dffemo,by=c("id","year"))

dfmse2 <- dfnew[,c(vid,vmse),with=F]
dfmse2 <- data.table::melt(dfmse2,id="id",variable.name = "year",value.name = "MSE")
dfmse2$year <- recode(dfmse2$year,'"mse"=0;"mse_0"=2;"mse_2"=4;"mse_5"=7;"mse_8"=10;
                      "mse_11"=13;"mse_14"=16;"mse_18"=20')
dfmse2$year <- as.numeric(as.character(dfmse2$year))
dfmse2$id <- as.character(dfmse2$id)
dflongi <- merge(dflongi,dfmse2,by=c("id","year"))
dflongi$outcomes <- factor(dflongi$outcome, levels = 0:1,labels = c("health","illness"))

dflongi
dfsur

fwrite(dflongi,"dflongi.csv")
fwrite(dfsur,"dfsur.csv")

########
#BMI
vweight <- c("g12","g10_0","g101_2","g101_5","g101_8","g101_11","g101_14","g101_18")
vheight <- c("g1021_8","g1021_11","g1021_14","g1021_18")
vknee <- c("g82","g102b_2")
dfbmi1 <- dataset_1998_2018[,c("id",vweight,vheight,vknee),with=F]

vweight <- c("g10","g101_2","g101_5","g101_8","g101_11","g101_14","g101_18")
vknee <- c("g102b_2")
dfbmi2 <- dataset_2000_2018[,c("id",vweight,vheight,vknee),with=F]
colnames(dfbmi2)[which(colnames(dfbmi2) %in% c("g10"))] <- c("g10_0")

vweight <- c("g101","g101_5","g101_8","g101_11","g101_14","g101_18")
vknee <- c("g102b")
dfbmi3 <- dataset_2002_2018[,c("id",vweight,vheight,vknee),with=F]
colnames(dfbmi3)[which(colnames(dfbmi3) %in% c("g101","g102b"))] <- c("g101_2","g102b_2")

vweight <- c("g101","g101_8","g101_11","g101_14","g101_18")
dfbmi4 <- dataset_2005_2018[,c("id",vweight,vheight),with=F]
colnames(dfbmi4)[which(colnames(dfbmi4) %in% c("g101"))] <- c("g101_5")

vweight <- c("g101","g101_11","g101_14","g101_18")
vheight <- c("g1021","g1021_11","g1021_14","g1021_18")
dfbmi5 <- dataset_2008_2018[,c("id",vweight,vheight),with=F]
colnames(dfbmi5)[which(colnames(dfbmi5) %in% c("g101","g1021"))] <- c("g101_8","g1021_8")

vweight <- c("g101","g101_14","g101_18")
vheight <- c("g1021","g1021_14","g1021_18")
dfbmi6 <- dataset_2011_2018[,c("id",vweight,vheight),with=F]
colnames(dfbmi6)[which(colnames(dfbmi6) %in% c("g101","g1021"))] <- c("g101_11","g1021_11")

vweight <- c("g101","g101_18")
vheight <- c("g1021","g1021_18")
dfbmi7 <- dataset_2014_2018[,c("id",vweight,vheight),with=F]
colnames(dfbmi7)[which(colnames(dfbmi7) %in% c("g101","g1021"))] <- c("g101_14","g1021_14")

library(car)
library(reshape2)

v <- c("g12","g10_0","g101_2","g101_5","g101_8","g101_11","g101_14",
       "g101_18","g1021_8","g1021_11","g1021_14","g1021_18","g82","g102b_2")
transNum <- function(df){
  df <- as.data.table(df)
  colN <- intersect(colnames(df),v)
  df[, (colN) := lapply(.SD, as.numeric), .SDcols = colN]
  df[df==888] <- NA
  df[df==999] <- NA
  return(df)
}
dfbmi1 <- transNum(dfbmi1)
dfbmi2 <- transNum(dfbmi2)
dfbmi3 <- transNum(dfbmi3)
dfbmi4 <- transNum(dfbmi4)
dfbmi5 <- transNum(dfbmi5)
dfbmi6 <- transNum(dfbmi6)
dfbmi7 <- transNum(dfbmi7)

dfbmi <- reduce(list(dfbmi1,dfbmi2,dfbmi3,dfbmi4,dfbmi5,dfbmi6,dfbmi7),mergeTwo)

q=dfbmi$id[which(duplicated(dfbmi$id))]
for(i in q){
    dfbmi[dfbmi$id == i, ] <- dfbmi[dfbmi$id == i, ][, lapply(.SD, function(x) mean(x, na.rm = TRUE))]
  }
dfbmi[which(dfbmi$id==i),]
dfbmi[which(dfbmi$id%in%q),]
dfbmi <- dfbmi[!duplicated(dfbmi$id),]

dfbmi$g12 <- as.numeric(dfbmi$g12)
dfbmi$g82 <- as.numeric(dfbmi$g82)
dfbmi$g10_0 <- as.numeric(dfbmi$g10_0)
dfbmi$g101_2 <- as.numeric(dfbmi$g101_2)
dfbmi$g102b_2 <- as.numeric(dfbmi$g102b_2)
dfbmi$g101_5 <- as.numeric(dfbmi$g101_5)
dfbmi$g101_8 <- as.numeric(dfbmi$g101_8)
dfbmi$g1021_8 <- as.numeric(dfbmi$g1021_8)
dfbmi$g101_11 <- as.numeric(dfbmi$g101_11)
dfbmi$g1021_11 <- as.numeric(dfbmi$g1021_11)
dfbmi$g101_14 <- as.numeric(dfbmi$g101_14)
dfbmi$g1021_14 <- as.numeric(dfbmi$g1021_14)
dfbmi$g101_18 <- as.numeric(dfbmi$g101_18)
dfbmi$g1021_18 <- as.numeric(dfbmi$g1021_18)

dfbmi$g101_11 <- recode(dfbmi$g101_11,"c(6,10,11)=NA")
dfbmi$g101_14 <- recode(dfbmi$g101_14,"c(1,2,3,4,5,888,999)=NA")
dfbmi$g101_18 <- recode(dfbmi$g101_18,"c(1,2,3,4,5,15,627,664,999)=NA")
dfbmi[which(dfbmi$g1021_11==16),"g1021_11"] <- 160
dfbmi[which(dfbmi$g1021_14==16),"g1021_14"] <- 160
dfbmi[which(dfbmi$g1021_18==16),"g1021_18"] <- 160
dfbmi[which(dfbmi$g1021_11<=89),"g1021_11"] <- 100+dfbmi[which(dfbmi$g1021_11<=89),"g1021_11"]
dfbmi[which(dfbmi$g1021_14<=89),"g1021_14"] <- 100+dfbmi[which(dfbmi$g1021_14<=89),"g1021_14"]
dfbmi[which(dfbmi$g1021_18<=89),"g1021_18"] <- 100+dfbmi[which(dfbmi$g1021_18<=89),"g1021_18"]

dfsur <- read_csv("dfsur.csv")
dfbmi <- merge(dfbmi,dfsur[,c("id","sex")],by="id")
dfbmi <- as_tibble(dfbmi)
heightFromKnee <- function(dfbmi,knee,height){
  dfbmi[[height]] <- NA
  dfbmi[which(dfbmi$sex==1),height] <- 74.08+1.81*dfbmi[which(dfbmi$sex==1),knee]
  dfbmi[which(dfbmi$sex==0),height] <- 67.78+2.01*dfbmi[which(dfbmi$sex==0),knee]
  return(dfbmi)
}

dfbmi <- heightFromKnee(dfbmi,"g82","h98")
dfbmi <- heightFromKnee(dfbmi,"g102b_2","h02")

dfbmi$h00 <- rowMeans(dfbmi[,c("h98","h02")],na.rm=TRUE)
dfbmi$h05 <- rowMeans(dfbmi[,c("h02","g1021_8")],na.rm=TRUE)

sort(unique(dfbmi$g1021_11))

dfbmi$bmi_98 <- dfbmi$g12/((0.01*dfbmi$h98)^2)
dfbmi$bmi_0 <- dfbmi$g10_0/((0.01*dfbmi$h00)^2)
dfbmi$bmi_2 <- dfbmi$g101_2/((0.01*dfbmi$h02)^2)
dfbmi$bmi_5 <- dfbmi$g101_5/((0.01*dfbmi$h05)^2)
dfbmi$bmi_8 <- dfbmi$g101_8/((0.01*dfbmi$g1021_8)^2)
dfbmi$bmi_11 <- dfbmi$g101_11/((0.01*dfbmi$g1021_11)^2)
dfbmi$bmi_14 <- dfbmi$g101_14/((0.01*dfbmi$g1021_14)^2)
dfbmi$bmi_18 <- dfbmi$g101_18/((0.01*dfbmi$g1021_18)^2)
dfbmi2 <- dfbmi[,c("id","bmi_98","bmi_0","bmi_2","bmi_5","bmi_8","bmi_11","bmi_14","bmi_18")]
dfbmi2 <- melt(dfbmi2,id="id",variable.name = "year",value.name = "bmi")
dfbmi2$year <- recode(dfbmi2$year,"'bmi_98'=0;'bmi_0'=2;'bmi_2'=4;'bmi_5'=7;'bmi_8'=10;
                      'bmi_11'=13;'bmi_14'=16;'bmi_18'=20")
dfbmi2$year <- as.character(dfbmi2$year)
dfbmi2$year <- as.numeric(dfbmi2$year)
dfbmi2 <- mergeTwo(dfbmi2,dflongi)
summary(dfbmi2$bmi)
fwrite(dfbmi2,"dfbmi.csv")


######
library(tidyverse)
df <- read_csv("dfbmi.csv")
df$X <- NULL
dfl <- read_csv("dflongi.csv")
dfl$X <- NULL
dfll <- df[,c("id","year","bmi")]
df <- merge(dfl,dfll,by=intersect(colnames(dfl),colnames(dfll)),all=TRUE)

df$id <- as.character(df$id)
df <- as_tibble(df)

df <- arrange(df,id,year)

vf <- c("id","year","sex","age","residenc","edu","occ","marriage","ADL","emo","bmi","MSE","times","totalTime","outcome","outcomes")
dfl <- df[,vf]
write_csv(dfl,"dflongi2.csv")

dfbase <- dfl[which(dfl$times==0),]
q=dfbase$id[which(duplicated(dfbase$id))]
dfo <- dfbase[which(dfbase$id%in%q),]
length(unique(dfl$id))
df[which(df$id%in%q),]
write_csv(dfbase,"dfbase.csv")
  
dflongi <- read_csv("./dflongi2.csv")
dflongi$outcomes <- car::recode(dflongi$outcomes,"'illness'='Case';'health'='Control'")
dflongi$totalTime <- dflongi$totalTime/365
dflongi$times <- dflongi$times/365
dflongi$outcomes <- factor(dflongi$outcomes,levels = c("Control","Case"))
dflongi <- dflongi %>% filter(totalTime!=0) %>%filter(times <= totalTime)
dflongi <- dflongi %>% rename(SWB=emo,BMI=bmi,res=residenc,mar=marriage)
dflongi <- dflongi %>% 
  rename(age_y=age,edu_y=edu) %>%
  mutate(edu=if_else(edu_y==0,0,1),
         age=case_when(age_y<=70~1,age_y>70&age_y<=80~2,
                       age_y>80&age_y<=90~3,age_y>90~4))
dflongi %>% write_csv("dflongi_filtered.csv")

dfbase <- dflongi%>% filter(times==0)
dfbase %>% write_csv("dfbase_filtered.csv")

dftimes <- dflongi %>% group_by(id) %>%
  summarize(
    num = n(),
    enT = min(year),
    ouT = max(year),
    outcome = unique(outcome)
  )
dftimes %>% write_csv("dftimes.csv")

