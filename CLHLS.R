#######################################################################
#######################################################################
library(foreign)
library(stringr)
#######################################################################
#######################################################################
setwd("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS")
#######################################################################
#read data set from sav files
#######################################################################
dataset_1998_2018=read.spss("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/clhls_1998_2018_longitudinal_dataset_released_version1.sav")
write.csv(dataset_1998_2018,"dataset_1998_2018.csv")
dataset_2000_2018=read.spss("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/clhls_2000_2018_longitudinal_dataset_released_version1.sav")
write.csv(dataset_2000_2018,"dataset_2000_2018.csv")
dataset_2002_2018=read.spss("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/clhls_2002_2018_longitudinal_dataset_released_version1.sav")
write.csv(dataset_2002_2018,"dataset_2002_2018.csv")
dataset_2005_2018=read.spss("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/clhls_2005_2018_longitudinal_dataset_released_version1.sav")
write.csv(dataset_2005_2018,"dataset_2005_2018.csv")
dataset_2008_2018=read.spss("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/clhls_2008_2018_longitudinal_dataset_released_version1.sav")
write.csv(dataset_2008_2018,"dataset_2008_2018.csv")
dataset_2011_2018=read.spss("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/clhls_2011_2018_longitudinal_dataset_released_version1.sav")
write.csv(dataset_2011_2018,"dataset_2011_2018.csv")
dataset_2014_2018=read.spss("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/clhls_2014_2018_longitudinal_dataset_released_version1.sav")
write.csv(dataset_2014_2018,"dataset_2014_2018.csv")
#######################################################################
#######################################################################

#######################################################################
#######################################################################
dataset_1998_2018=read.csv("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_1998_2018.csv")
dataset_2000_2018=read.csv("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2000_2018.csv")
dataset_2002_2018=read.csv("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2002_2018.csv")
dataset_2005_2018=read.csv("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2005_2018.csv")
dataset_2008_2018=read.csv("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2008_2018.csv")
dataset_2011_2018=read.csv("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2011_2018.csv")
dataset_2014_2018=read.csv("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2014_2018.csv")
#######################################################################
#1998_2018
vid=c("id")
vdoi=c("year9899","month98","date98","month_0","day_0","month_2","day_2","month_5","day_5",
       "year_8","month_8","day_8","yearin_11","monthin_11","dayin_11","yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18")
vdod=c()
for (i in c(0,2,5,8,11,14,18))
{
  a=sprintf(c("d%svyear","d%svmonth","d%svday"),i)
  vdod=c(vdod,a)
}
vstatus=c("dth98_00","dth00_02","dth02_05","dth05_08","dth08_11","dth11_14","dth14_18")
vsex=c("a1")
vage=c("trueage","vage_0","vage_2","vage_5","vage_8","vage_11","trueage_14","trueage_18")
vresid=c("residenc","resid_0","resid_2","resid_5","resid_8","resic_11","residenc_14","residenc_18")
vcog=c()
for (i in c("","_0","_2","_5","_8","_11","_14","_18"))
{
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vedu=c("f1","f1_8","f1_14","f1_18")
vocc=c("f2","f2_8","f2_14","f2_18")
vmar=sprintf(c("f41%s"),c("","_0","_2","_5","_8","_11","_14","_18"))
vADL=c()
for (i in c("","_0","_2","_5","_8","_11","_14","_18"))
{
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vhabit=c()
for (i in c("_0","_2","_5","_8","_11","_14","_18"))
{
  a=sprintf(c("d71%s","d81%s","d91%s"),i)
  vhabit=c(vhabit,a)
}
vhabit=c("d61","d71","d81",vhabit)
vdis=c()
for (i in c("_2","_5","_8","_11","_14","_18"))
{
  a=sprintf(c("g101%s","g15a1%s","g15b1%s","g15c1%s"),i)
  vdis=c(vdis,a)
}
vdis=c("g12","g17a1","g17b1","g17c1","g10_0","g15a1_0","g15b1_0","g15c1_0",vdis)

length(which(str_detect(dataset_1998_2018[,1],"98$")==TRUE))  #9093 start from 1998 in 1998_2018 data set
write.csv(dataset_1998_2018[,c(vid,vdoi,vdod,vstatus,vsex,vage,vresid,vcog,vedu,vocc,vmar,vADL,vhabit,vdis)],"dataset_1998_2018_v1.csv")
#######################################################################
#2000_2018
vid=c("id")
vdoi=c("month00","day00","month_2","day_2","month_5","day_5","year_8","month_8","day_8","yearin_11","monthin_11",
       "dayin_11","yearin_14","monthin_14","dayin_14","yearin_18","monthin_18","dayin_18")
vdod=c()
for (i in c(2,5,8,11,14,18))
{
  a=sprintf(c("d%svyear","d%svmonth","d%svday"),i)
  vdod=c(vdod,a)
}
vstatus=c("dth00_02","dth02_05","dth05_08","dth08_11","dth11_14","dth14_18")
vsex=c("a1")
vage=c("trueage","vage_2","vage_5","vage_8","vage_11","trueage_14","trueage_18")
vresid=c("residenc","resid_2","resid_5","resid_8","resic_11","residenc_14","residenc_18")
vcog=c()
for (i in c("","_2","_5","_8","_11","_14","_18"))
{
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vedu=c("f1","f1_8","f1_14","f1_18")
vocc=c("f2","f2_8","f2_14","f2_18")
vmar=sprintf(c("f41%s"),c("","_2","_5","_8","_11","_14","_18"))
vADL=c()
for (i in c("","_2","_5","_8","_11","_14","_18"))
{
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vhabit=c()
for (i in c("","_2","_5","_8","_11","_14","_18"))
{
  a=sprintf(c("d71%s","d81%s","d91%s"),i)
  vhabit=c(vhabit,a)
}
vdis=c()
for (i in c("_2","_5","_8","_11","_14","_18"))
{
  a=sprintf(c("g101%s","g15a1%s","g15b1%s","g15c1%s"),i)
  vdis=c(vdis,a)
}
vdis=c("g10","g15a1","g15b1","g15c1",vdis)

a=which(str_detect(dataset_2000_2018[,1],"98$")==TRUE)  #4831 start from 1998 in 2000_2018 data set
b=which(str_detect(dataset_2000_2018[,1],"00$")==TRUE)  #6368 start from 2000 in 2000_2018 data set
length(intersect(dataset_1998_2018[,1],dataset_2000_2018[a,1])) #4831 already existed
write.csv(dataset_2000_2018[b,c(vid,vdoi,vdod,vstatus,vsex,vage,vresid,vcog,vedu,vocc,vmar,vADL,vhabit,vdis)],"dataset_2000_2018_v1.csv")
#######################################################################
#2002_2018
vid=c("id")
vdoi=c("month02","day02","month_5","day_5","year_8","month_8","day_8","yearin_11","monthin_11","dayin_11","yearin_14",
       "monthin_14","dayin_14","yearin_18","monthin_18","dayin_18")
vdod=c()
for (i in c(5,8,11,14,18))
{
  a=sprintf(c("d%svyear","d%svmonth","d%svday"),i)
  vdod=c(vdod,a)
}
vstatus=c("dth02_05","dth05_08","dth08_11","dth11_14","dth14_18")
vsex=c("a1")
vage=c("trueage","vage_5","vage_8","vage_11","trueage_14","trueage_18")
vresid=c("residenc","resid_5","resid_8","resic_11","residenc_14","residenc_18")
vcog=c()
for (i in c("","_5","_8","_11","_14","_18"))
{
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vedu=c("f1","f1_8","f1_14","f1_18")
vocc=c("f2","f2_8","f2_14","f2_18")
vmar=sprintf(c("f41%s"),c("","_5","_8","_11","_14","_18"))
vADL=c()
for (i in c("","_5","_8","_11","_14","_18"))
{
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vhabit=c()
for (i in c("","_5","_8","_11","_14","_18"))
{
  a=sprintf(c("d71%s","d81%s","d91%s"),i)
  vhabit=c(vhabit,a)
}
vdis=c()
for (i in c("","_5","_8","_11","_14","_18"))
{
  a=sprintf(c("g101%s","g15a1%s","g15b1%s","g15c1%s"),i)
  vdis=c(vdis,a)
}

a=which(str_detect(dataset_2002_2018[,1],"98$")==TRUE)  #2642 start from 1998 in 2002_2018 data set
b=which(str_detect(dataset_2002_2018[,1],"00$")==TRUE)  #3674 start from 2000 in 2002_2018 data set
c=which(str_detect(dataset_2002_2018[,1],"02$")==TRUE)  #9748 start from 2002 in 2002_2018 data set
length(intersect(dataset_1998_2018[,1],dataset_2002_2018[a,1]))  #2642 already existed
length(intersect(dataset_2000_2018[,1],dataset_2002_2018[b,1]))  #3673 already existed#1
write.csv(dataset_2002_2018[c,c(vid,vdoi,vdod,vstatus,vsex,vage,vresid,vcog,vedu,vocc,vmar,vADL,vhabit,vdis)],"dataset_2002_2018_v1.csv")
#######################################################################
#2005_2018
vid=c("id")
vdoi=c("monthin","dayin","year_8","month_8","day_8","yearin_11","monthin_11","dayin_11","yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18")
vdod=c()
for (i in c(8,11,14,18))
{
  a=sprintf(c("d%svyear","d%svmonth","d%svday"),i)
  vdod=c(vdod,a)
}
vstatus=c("dth05_08","dth08_11","dth11_14","dth14_18")
vsex=c("a1")
vage=c("trueage","vage_8","vage_11","trueage_14","trueage_18")
vresid=c("residenc","resid_8","resic_11","residenc_14","residenc_18")
vcog=c()
for (i in c("","_8","_11","_14","_18"))
{
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vedu=c("f1","f1_8","f1_14","f1_18")
vocc=c("f2","f2_8","f2_14","f2_18")
vmar=sprintf(c("f41%s"),c("","_8","_11","_14","_18"))
vADL=c()
for (i in c("","_8","_11","_14","_18"))
{
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vhabit=c()
for (i in c("","_8","_11","_14","_18"))
{
  a=sprintf(c("d71%s","d81%s","d91%s"),i)
  vhabit=c(vhabit,a)
}
vdis=c()
for (i in c("","_8","_11","_14","_18"))
{
  a=sprintf(c("g101%s","g15a1%s","g15b1%s","g15c1%s"),i)
  vdis=c(vdis,a)
}

a=which(str_detect(dataset_2005_2018[,1],"98$")==TRUE)  #1052 start from 1998 in 2005_2018 data set
b=which(str_detect(dataset_2005_2018[,1],"00$")==TRUE)  #1579 start from 2000 in 2005_2018 data set
c=which(str_detect(dataset_2005_2018[,1],"02$")==TRUE)  #5548 start from 2002 in 2005_2018 data set
d=which(str_detect(dataset_2005_2018[,1],"05$")==TRUE)  #7459 start from 2005 in 2005_2018 data set
length(intersect(dataset_1998_2018[,1],dataset_2005_2018[a,1]))  #1052 already existed
length(intersect(dataset_2000_2018[,1],dataset_2005_2018[b,1]))  #1577 already existed#2
length(intersect(dataset_2002_2018[,1],dataset_2005_2018[c,1]))  #5545 already existed#3
write.csv(dataset_2005_2018[d,c(vid,vdoi,vdod,vstatus,vsex,vage,vresid,vcog,vedu,vocc,vmar,vADL,vhabit,vdis)],"dataset_2005_2018_v1.csv")
#######################################################################
#2008_2018
vid=c("id")
vdoi=c("yearin","monthin","dayin","yearin_11","monthin_11","dayin_11","yearin_14","monthin_14","dayin_14",
       "yearin_18","monthin_18","dayin_18")
vdod=c()
for (i in c(11,14,18))
{
  a=sprintf(c("d%svyear","d%svmonth","d%svday"),i)
  vdod=c(vdod,a)
}
vstatus=c("dth08_11","dth11_14","dth14_18")
vsex=c("a1")
vage=c("trueage","vage_11","trueage_14","trueage_18")
vresid=c("residenc","resid_11","residenc_14","residenc_18")
vcog=c()
for (i in c("","_11","_14","_18"))
{
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vedu=c("f1","f1_14","f1_18")
vocc=c("f2","f2_14","f2_18")
vmar=sprintf(c("f41%s"),c("","_11","_14","_18"))
vADL=c()
for (i in c("","_11","_14","_18"))
{
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vhabit=c()
for (i in c("","_11","_14","_18"))
{
  a=sprintf(c("d71%s","d81%s","d91%s"),i)
  vhabit=c(vhabit,a)
}
vdis=c()
for (i in c("","_11","_14","_18"))
{
  a=sprintf(c("g101%s","g15a1%s","g15b1%s","g15c1%s"),i)
  vdis=c(vdis,a)
}

a=which(str_detect(dataset_2008_2018[,1],"98$")==TRUE)  #358 start from 1998 in 2008_2018 data set
b=which(str_detect(dataset_2008_2018[,1],"00$")==TRUE)  #593 start from 2000 in 2008_2018 data set
c=which(str_detect(dataset_2008_2018[,1],"02$")==TRUE)  #3242 start from 2002 in 2008_2018 data set
d=which(str_detect(dataset_2008_2018[,1],"05$")==TRUE)  #3282 start from 2005 in 2008_2018 data set
e=which(str_detect(dataset_2008_2018[,1],"08$")==TRUE)  #9165 start from 2008 in 2008_2018 data set
f=which(str_detect(dataset_2008_2018[,1],"09$")==TRUE)  #314 start from 2009 in 2008_2018 data set
length(intersect(dataset_1998_2018[,1],dataset_2008_2018[a,1]))  #358 already existed
length(intersect(dataset_2000_2018[,1],dataset_2008_2018[b,1]))  #592 already existed#1
length(intersect(dataset_2002_2018[,1],dataset_2008_2018[c,1]))  #3241 already existed#1
length(intersect(dataset_2005_2018[,1],dataset_2008_2018[d,1]))  #3281 already existed#1
write.csv(dataset_2008_2018[c(e,f),c(vid,vdoi,vdod,vstatus,vsex,vage,vresid,vcog,vedu,vocc,vmar,vADL,vhabit,vdis)],"dataset_2008_2018_v1.csv")
#######################################################################
#2011_2018
vid=c("id")
vdoi=c("yearin","monthin","dayin","yearin_14","monthin_14","dayin_14","yearin_18","monthin_18","dayin_18")
vdod=c()
for (i in c(14,18))
{
  a=sprintf(c("d%svyear","d%svmonth","d%svday"),i)
  vdod=c(vdod,a)
}
vstatus=c("dth11_14","dth14_18")
vsex=c("a1")
vage=c("trueage","trueage_14","trueage_18")
vresid=c("residenc","residenc_14","residenc_18")
vcog=c()
for (i in c("","_14","_18"))
{
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vedu=c("f1","f1_14","f1_18")
vocc=c("f2","f2_14","f2_18")
vmar=sprintf(c("f41%s"),c("","_14","_18"))
vADL=c()
for (i in c("","_14","_18"))
{
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vhabit=c()
for (i in c("","_14","_18"))
{
  a=sprintf(c("d71%s","d81%s","d91%s"),i)
  vhabit=c(vhabit,a)
}
vdis=c()
for (i in c("","_14","_18"))
{
  a=sprintf(c("g101%s","g15a1%s","g15b1%s","g15c1%s"),i)
  vdis=c(vdis,a)
}

a=which(str_detect(dataset_2011_2018[,1],"98$")==TRUE)  #128 start from 1998 in 2011_2018 data set
b=which(str_detect(dataset_2011_2018[,1],"00$")==TRUE)  #236 start from 2000 in 2011_2018 data set
c=which(str_detect(dataset_2011_2018[,1],"02$")==TRUE)  #2152 start from 2002 in 2011_2018 data set
d=which(str_detect(dataset_2011_2018[,1],"05$")==TRUE)  #1679 start from 2005 in 2011_2018 data set
e=which(str_detect(dataset_2011_2018[,1],"08$")==TRUE)  #4070 start from 2008 in 2011_2018 data set
f=which(str_detect(dataset_2011_2018[,1],"09$")==TRUE)  #160 start from 2009 in 2011_2018 data set
g=which(str_detect(dataset_2011_2018[,1],"12$")==TRUE)  #1340 start from 2012 in 2011_2018 data set
length(intersect(dataset_1998_2018[,1],dataset_2011_2018[a,1]))  #128 already existed
length(intersect(dataset_2000_2018[,1],dataset_2011_2018[b,1]))  #235 already existed#1
length(intersect(dataset_2002_2018[,1],dataset_2011_2018[c,1]))  #2151 already existed#1
length(intersect(dataset_2005_2018[,1],dataset_2011_2018[d,1]))  #1678 already existed#1
length(intersect(dataset_2008_2018[,1],dataset_2011_2018[e,1]))  #4070 already existed
length(intersect(dataset_2008_2018[,1],dataset_2011_2018[f,1]))  #160 already existed#1
write.csv(dataset_2011_2018[g,c(vid,vdoi,vdod,vstatus,vsex,vage,vresid,vcog,vedu,vocc,vmar,vADL,vhabit,vdis)],"dataset_2011_2018_v1.csv")
#######################################################################
#2014_2018
vid=c("id")
vdoi=c("yearin","monthin","dayin","yearin_18","monthin_18","dayin_18")
vdod=c()
for (i in c(18))
{
  a=sprintf(c("d%svyear","d%svmonth","d%svday"),i)
  vdod=c(vdod,a)
}
vstatus=c("dth14_18")
vsex=c("a1")
vage=c("trueage","trueage_18")
vresid=c("residenc","residenc_18")
vcog=c()
for (i in c("","_18"))
{
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vedu=c("f1","f1_18")
vocc=c("f2","f2_18")
vmar=sprintf(c("f41%s"),c("","_18"))
vADL=c()
for (i in c("","_18"))
{
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vhabit=c()
for (i in c("","_18"))
{
  a=sprintf(c("d71%s","d81%s","d91%s"),i)
  vhabit=c(vhabit,a)
}
vdis=c()
for (i in c("","_18"))
{
  a=sprintf(c("g101%s","g15a1%s","g15b1%s","g15c1%s"),i)
  vdis=c(vdis,a)
}

a=which(str_detect(dataset_2014_2018[,1],"98$")==TRUE)  #47 start from 1998 in 2014_2018 data set
b=which(str_detect(dataset_2014_2018[,1],"00$")==TRUE)  #96 start from 2000 in 2014_2018 data set
c=which(str_detect(dataset_2014_2018[,1],"02$")==TRUE)  #1539 start from 2002 in 2014_2018 data set
d=which(str_detect(dataset_2014_2018[,1],"05$")==TRUE)  #1111 start from 2005 in 2014_2018 data set
e=which(str_detect(dataset_2014_2018[,1],"08$")==TRUE)  #2339 start from 2008 in 2014_2018 data set
f=which(str_detect(dataset_2014_2018[,1],"09$")==TRUE)  #114 start from 2009 in 2014_2018 data set
g=which(str_detect(dataset_2014_2018[,1],"12$")==TRUE)  #821 start from 2012 in 2014_2018 data set
h=which(str_detect(dataset_2014_2018[,1],"14$")==TRUE)  #1125 start from 2012 in 2014_2018 data set
length(intersect(dataset_1998_2018[,1],dataset_2014_2018[a,1]))  #47 already existed
length(intersect(dataset_2000_2018[,1],dataset_2014_2018[b,1]))  #96 already existed#1
length(intersect(dataset_2002_2018[,1],dataset_2014_2018[c,1]))  #1539 already existed
length(intersect(dataset_2005_2018[,1],dataset_2014_2018[d,1]))  #1110 already existed#1
length(intersect(dataset_2008_2018[,1],dataset_2014_2018[e,1]))  #2339 already existed
length(intersect(dataset_2008_2018[,1],dataset_2014_2018[f,1]))  #114 already existed
length(intersect(dataset_2011_2018[,1],dataset_2014_2018[g,1]))  #821 already existed
write.csv(dataset_2014_2018[h,c(vid,vdoi,vdod,vstatus,vsex,vage,vresid,vcog,vedu,vocc,vmar,vADL,vhabit,vdis)],"dataset_2014_2018_v1.csv")




