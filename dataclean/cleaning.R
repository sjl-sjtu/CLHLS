setwd("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS")
Sys.setlocale("LC_ALL", "English")
dataset_1998_2018=read.csv("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_1998_2018.csv")
dataset_2000_2018=read.csv("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2000_2018.csv")
dataset_2002_2018=read.csv("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2002_2018.csv")
dataset_2005_2018=read.csv("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2005_2018.csv")
dataset_2008_2018=read.csv("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2008_2018.csv")
dataset_2011_2018=read.csv("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2011_2018.csv")
dataset_2014_2018=read.csv("D:/CLHLS/CLHLS_dataset_1998-2018_SPSS/dataset_2014_2018.csv")

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
p <- union(p1,p2)
p <- union(p,p3)
p <- union(p,p4)
p <- union(p,p5)
p <- union(p,p6)
p <- union(p,p7)
p <- sort(p)
q <- c("b11","b11_0","b11_11","b11_14","b11_18","b11_2","b11_5","b11_8","b12",
       "b12_0","b12_11","b12_14","b12_18","b12_2","b12_5","b12_8","b121","b121_11",
       "b121_14","b121_18","b121_2","b121_5","b121_8","b21","b21_0","b21_11","b21_14",
       "b21_18","b21_2","b21_5","b21_8","b210","b210_11","b210_14","b22","b22_0","b22_11",
       "b22_14","b22_18","b22_2","b22_5","b22_8","b23","b23_0","b23_11","b23_14","b23_18",
       "b23_2","b23_5","b23_8","b24","b24_0","b24_11","b24_14","b24_18","b24_2","b24_5",
       "b24_8","b25","b25_0","b25_11","b25_14","b25_18","b25_2","b25_5","b25_8","b26",
       "b26_0","b26_11","b26_14","b26_18","b26_2","b26_5","b26_8","b27","b27_0","b27_11",
       "b27_14","b27_18","b27_2","b27_5","b27_8","b28","b28_11","b28_14","b28_18","b29",
       "b29_11","b29_14","b31_18","b310a_18","b310b_18","b32_18","b33_18","b34_18","b35_18",
       "b36_18","b37_18","b38_18","b39_18","b41_18","b42_18","b43_18","b44_18","b45_18",
       "b46_18","b47_18","b48_18","b49_18","b49a_18")
q1 <- intersect(q,p1)
q2 <- intersect(q,p2)
q3 <- intersect(q,p3)
q4 <- intersect(q,p4)
q5 <- intersect(q,p5)
q6 <- intersect(q,p6)
q7 <- intersect(q,p7)
dff1 <- dataset_1998_2018[,c("id",q1)]
dff2 <- dataset_2000_2018[,c("id",q2)]
dff3 <- dataset_2002_2018[,c("id",q3)]
dff4 <- dataset_2005_2018[,c("id",q4)]
dff5 <- dataset_2008_2018[,c("id",q5)]
dff6 <- dataset_2011_2018[,c("id",q6)]
dff7 <- dataset_2014_2018[,c("id",q7)]

write.csv(dff1,"dff1.csv")
write.csv(dff2,"dff2.csv")
write.csv(dff3,"dff3.csv")
write.csv(dff4,"dff4.csv")
write.csv(dff5,"dff5.csv")
write.csv(dff6,"dff6.csv")
write.csv(dff7,"dff7.csv")


#excel中手动修改基线列名，去掉空格
dff1 <- read.csv("dff1.csv")
dff2 <- read.csv("dff2.csv")
dff3 <- read.csv("dff3.csv")
dff4 <- read.csv("dff4.csv")
dff5 <- read.csv("dff5.csv")
dff6 <- read.csv("dff6.csv")
dff7 <- read.csv("dff7.csv")

dff2 <- dff2[-which(dff2$id %in% dff1$id),]
dff3 <- dff3[-which(dff3$id %in% c(dff1$id,dff2$id)),]
dff4 <- dff4[-which(dff4$id %in% c(dff1$id,dff2$id,dff3$id)),]
dff5 <- dff5[-which(dff5$id %in% c(dff1$id,dff2$id,dff3$id,dff4$id)),]
dff6 <- dff6[-which(dff6$id %in% c(dff1$id,dff2$id,dff3$id,dff4$id,dff5$id)),]
dff7 <- dff7[-which(dff7$id %in% c(dff1$id,dff2$id,dff3$id,dff4$id,dff5$id,dff6$id)),]

dff <- mergeTwo(dff1,dff2)
dff <- mergeTwo(dff,dff3)
dff <- mergeTwo(dff,dff4)
dff <- mergeTwo(dff,dff5)
dff <- mergeTwo(dff,dff6)
dff <- mergeTwo(dff,dff7)
write.csv(dff,"dffeel.csv",row.names = FALSE)

qall <- colnames(dff)
sort(qall)
q98 <- c("b11","b12","b21","b22","b23","b24","b25","b26","b27")
q00 <- c("b11_0","b12_0","b21_0","b22_0","b23_0","b24_0","b25_0","b26_0","b27_0")
q02 <- c("b11_2","b12_2","b121_2","b21_2","b22_2","b23_2","b24_2","b25_2","b26_2","b27_2")
q05 <- c("b11_5","b12_5","b121_5","b21_5","b22_5","b23_5","b24_5","b25_5","b26_5","b27_5")
q08 <- c("b11_8","b12_8","b121_8","b21_8","b22_8","b23_8","b24_8","b25_8","b26_8","b27_8")
q11 <- c("b11_11","b12_11","b121_11","b21_11","b22_11","b23_11","b24_11","b25_11","b26_11",
         "b27_11","b28_11","b29_11","b210_11")
q14 <- c("b11_14","b12_14","b121_14","b21_14","b22_14","b23_14","b24_14","b25_14","b26_14",
         "b27_14","b28_14","b29_14","b210_14")
q18 <- c("b11_18","b12_18","b121_18","b21_18","b22_18","b23_18","b24_18","b25_18","b26_18",
         "b27_18","b28_18","b31_18","b310a_18","b310b_18","b32_18","b33_18","b34_18",
         "b35_18","b36_18","b37_18","b38_18","b39_18","b41_18","b42_18","b43_18","b44_18",
         "b45_18","b46_18","b47_18","b48_18","b49_18","b49a_18")


library(car)
#1998
dff$b11 <- recode(dff$b11,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b12 <- recode(dff$b12,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b21 <- recode(dff$b21,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b22 <- recode(dff$b22,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b23 <- recode(dff$b23,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b24 <- recode(dff$b24,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b25 <- recode(dff$b25,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b26 <- recode(dff$b26,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b27 <- recode(dff$b27,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')

#2000
dff$b11_0 <- recode(dff$b11_0,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b12_0 <- recode(dff$b12_0,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b21_0 <- recode(dff$b21_0,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b22_0 <- recode(dff$b22_0,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b23_0 <- recode(dff$b23_0,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b24_0 <- recode(dff$b24_0,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b25_0 <- recode(dff$b25_0,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b26_0 <- recode(dff$b26_0,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b27_0 <- recode(dff$b27_0,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')

#2002
dff$b11_2 <- recode(dff$b11_2,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b12_2 <- recode(dff$b12_2,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b121_2 <- recode(dff$b121_2,'"muchbetter"=4;"alittlebetter"=3;"nochange"=2;"alittlebetter"=1;"muchworse"=0;else=NA')
dff$b21_2 <- recode(dff$b21_2,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b22_2 <- recode(dff$b22_2,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b23_2 <- recode(dff$b23_2,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b24_2 <- recode(dff$b24_2,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b25_2 <- recode(dff$b25_2,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b26_2 <- recode(dff$b26_2,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b27_2 <- recode(dff$b27_2,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')

#2005
dff$b11_5 <- recode(dff$b11_5,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b12_5 <- recode(dff$b12_5,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b121_5 <- recode(dff$b121_5,'"muchbetter"=4;"alittlebetter"=3;"nochange"=2;"alittlebetter"=1;"muchworse"=0;else=NA')
dff$b21_5 <- recode(dff$b21_5,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b22_5 <- recode(dff$b22_5,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b23_5 <- recode(dff$b23_5,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b24_5 <- recode(dff$b24_5,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b25_5 <- recode(dff$b25_5,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b26_5 <- recode(dff$b26_5,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b27_5 <- recode(dff$b27_5,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')

#2008
dff$b11_8 <- recode(dff$b11_8,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b12_8 <- recode(dff$b12_8,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b121_8 <- recode(dff$b121_8,'"muchbetter"=4;"alittlebetter"=3;"nochange"=2;"alittlebetter"=1;"muchworse"=0;else=NA')
dff$b21_8 <- recode(dff$b21_8,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b22_8 <- recode(dff$b22_8,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b23_8 <- recode(dff$b23_8,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b24_8 <- recode(dff$b24_8,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b25_8 <- recode(dff$b25_8,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b26_8 <- recode(dff$b26_8,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b27_8 <- recode(dff$b27_8,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')

#2011
dff$b11_11 <- recode(dff$b11_11,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b12_11 <- recode(dff$b12_11,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b121_11 <- recode(dff$b121_11,'"muchbetter"=4;"alittlebetter"=3;"nochange"=2;"alittlebetter"=1;"muchworse"=0;else=NA')
dff$b21_11 <- recode(dff$b21_11,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b22_11 <- recode(dff$b22_11,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b23_11 <- recode(dff$b23_11,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b24_11 <- recode(dff$b24_11,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b25_11 <- recode(dff$b25_11,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b26_11 <- recode(dff$b26_11,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b27_11 <- recode(dff$b27_11,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b28_11 <- recode(dff$b28_11,'"yes"=0;"no"=1;else=NA')  #负面问题
dff$b29_11 <- recode(dff$b29_11,'"yes"=0;"no"=1;else=NA')  #负面问题
dff$b210_11 <- ifelse(dff$b28_11+dff$b29_11==2,"No",dff$b210_11)
dff$b210_11 <- recode(dff$b210_11,'"alldaylong"=0;"mostoftheday"=1;"abouthalfoftheday"=2;
                      "lessthanhalfoftheday"=3;"No"=4;else=NA')  #负面问题

#2014
dff$b11_14 <- recode(dff$b11_14,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b12_14 <- recode(dff$b12_14,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b121_14 <- recode(dff$b121_14,'"muchbetter"=4;"alittlebetter"=3;"nochange"=2;"alittlebetter"=1;"muchworse"=0;else=NA')
dff$b21_14 <- recode(dff$b21_14,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b22_14 <- recode(dff$b22_14,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b23_14 <- recode(dff$b23_14,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b24_14 <- recode(dff$b24_14,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b25_14 <- recode(dff$b25_14,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b26_14 <- recode(dff$b26_14,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b27_14 <- recode(dff$b27_14,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b28_14 <- recode(dff$b28_14,'"yes"=0;"no"=1;else=NA')  #负面问题
dff$b29_14 <- recode(dff$b29_14,'"yes"=0;"no"=1;else=NA')  #负面问题
dff$b210_14 <- ifelse(dff$b28_14+dff$b29_14==2,"No",dff$b210_14)
dff$b210_14 <- recode(dff$b210_14,'"alldaylong"=0;"mostoftheday"=1;"abouthalfoftheday"=2;
                      "lessthanhalfoftheday"=3;"No"=4;else=NA')  #负面问题

#2018
dff$b11_18 <- recode(dff$b11_18,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b12_18 <- recode(dff$b12_18,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b121_18 <- recode(dff$b121_18,'"muchbetter"=4;"alittlebetter"=3;"nochange"=2;"alittlebetter"=1;"muchworse"=0;else=NA')
dff$b21_18 <- recode(dff$b21_18,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b22_18 <- recode(dff$b22_18,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b23_18 <- recode(dff$b23_18,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b24_18 <- recode(dff$b24_18,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b25_18 <- recode(dff$b25_18,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b26_18 <- recode(dff$b26_18,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"never"=0;else=NA')
dff$b27_18 <- recode(dff$b27_18,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"never"=4;else=NA') #负面问题
dff$b28_18 <- recode(dff$b28_18,'"yes"=0;"no"=1;else=NA')  #负面问题
dff$b31_18 <- recode(dff$b31_18,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"rarelyornever"=4;else=NA') #负面问题
dff$b32_18 <- recode(dff$b32_18,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"rarelyornever"=4;else=NA') #负面问题
dff$b33_18 <- recode(dff$b33_18,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"rarelyornever"=4;else=NA') #负面问题
dff$b34_18 <- recode(dff$b34_18,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"rarelyornever"=4;else=NA') #负面问题
dff$b35_18 <- recode(dff$b35_18,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"rarelyornever"=0;else=NA')
dff$b36_18 <- recode(dff$b36_18,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"rarelyornever"=4;else=NA') #负面问题
dff$b37_18 <- recode(dff$b37_18,'"always"=4;"often"=3;"sometimes"=2;"seldom"=1;"rarelyornever"=0;else=NA')
dff$b38_18 <- recode(dff$b38_18,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"rarelyornever"=4;else=NA') #负面问题
dff$b39_18 <- recode(dff$b39_18,'"always"=0;"often"=1;"sometimes"=2;"seldom"=3;"rarelyornever"=4;else=NA') #负面问题
dff$b310a_18 <- recode(dff$b310a_18,'"verygood"=4;"good"=3;"soso"=2;"bad"=1;"verybad"=0;else=NA')
dff$b310b_18 <- as.numeric(dff$b310b_18)
dff$b41_18 <- recode(dff$b41_18,'"almosteveryday"=0;"morethanhalfofdays"=1;"forseveraldays"=2;"never"=3;else=NA') #负面问题
dff$b42_18 <- recode(dff$b42_18,'"almosteveryday"=0;"morethanhalfofdays"=1;"forseveraldays"=2;"never"=3;else=NA') #负面问题
dff$b43_18 <- recode(dff$b43_18,'"almosteveryday"=0;"morethanhalfofdays"=1;"forseveraldays"=2;"never"=3;else=NA') #负面问题
dff$b44_18 <- recode(dff$b44_18,'"almosteveryday"=0;"morethanhalfofdays"=1;"forseveraldays"=2;"never"=3;else=NA') #负面问题
dff$b45_18 <- recode(dff$b45_18,'"almosteveryday"=0;"morethanhalfofdays"=1;"forseveraldays"=2;"never"=3;else=NA') #负面问题
dff$b46_18 <- recode(dff$b46_18,'"almosteveryday"=0;"morethanhalfofdays"=1;"forseveraldays"=2;"never"=3;else=NA') #负面问题
dff$b47_18 <- recode(dff$b47_18,'"almosteveryday"=0;"morethanhalfofdays"=1;"forseveraldays"=2;"never"=3;else=NA') #负面问题

q18new <- c("b11_18","b12_18","b121_18","b21_18","b22_18","b23_18","b24_18","b25_18","b26_18",
         "b27_18","b28_18","b31_18","b310a_18","b32_18","b33_18","b34_18",
         "b35_18","b36_18","b37_18","b38_18","b39_18","b41_18","b42_18","b43_18","b44_18",
         "b45_18","b46_18","b47_18") #删掉了具体睡眠时间（不好处理???

dff$emo98 <- rowSums(dff[,q98])
dff$emo00 <- rowSums(dff[,q00])
dff$emo02 <- rowSums(dff[,q02])
dff$emo05 <- rowSums(dff[,q05])
dff$emo08 <- rowSums(dff[,q08])
dff$emo11 <- rowSums(dff[,q11])
dff$emo14 <- rowSums(dff[,q14])
dff$emo18 <- rowSums(dff[,q18new])
fullemo <- c(36,36,40,40,40,46,46,102)
dff$relaemo98 <- dff$emo98/fullemo[1]
dff$relaemo00 <- dff$emo00/fullemo[2]
dff$relaemo02 <- dff$emo02/fullemo[3]
dff$relaemo05 <- dff$emo05/fullemo[4]
dff$relaemo08 <- dff$emo08/fullemo[5]
dff$relaemo11 <- dff$emo11/fullemo[6]
dff$relaemo14 <- dff$emo14/fullemo[7]
dff$relaemo18 <- dff$emo18/fullemo[8]
vemo <- c("emo98","emo00","emo02","emo05","emo08","emo11","emo14","emo18")
vrelaemo <- c("relaemo98","relaemo00","relaemo02","relaemo05","relaemo08",
              "relaemo11","relaemo14","relaemo18")
dffemo <- dff[,c("id",vrelaemo)]

library(reshape2)
dffemo <- melt(dffemo,measure=vrelaemo,variable.name = "year",value.name = "emo")
dffemo$year <- recode(dffemo$year,'"relaemo98"=0;"relaemo00"=2;"relaemo02"=4;"relaemo05"=7;
                      "relaemo08"=10;"relaemo11"=13;"relaemo14"=16;"relaemo18"=20')
write.csv(dffemo,"dffemo.csv")

#######################################################################
#获取认知变化数据

vid=c("id")
#基线信息
vsex=c("a1")
vage=c("trueage")
vresid=c("residenc")
vedu=c("f1")
vocc=c("f2")
vmar=c("f41")

#1998-2018
vcog=c()  #客观认知水平
for (i in c("","_0","_2","_5","_8","_11","_14","_18")){
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vADL=c()  #主观评价
for (i in c("","_0","_2","_5","_8","_11","_14","_18")){
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vstatus=c("dth98_00","dth00_02","dth02_05","dth05_08","dth08_11","dth11_14","dth14_18")
vdoi=c("year9899","month98","date98","month_0","day_0","month_2","day_2","month_5","day_5",
       "year_8","month_8","day_8","yearin_11","monthin_11","dayin_11","yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18") #日期
df1an <- dataset_1998_2018[,c(vid,vsex,vage,vresid,vedu,vocc,vmar,vcog,vADL,vstatus,vdoi)]

#2000-2018
vcog=c()  #客观认知水平
for (i in c("","_2","_5","_8","_11","_14","_18")){
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vADL=c()  #主观评价
for (i in c("","_2","_5","_8","_11","_14","_18")){
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vstatus=c("dth00_02","dth02_05","dth05_08","dth08_11","dth11_14","dth14_18")
vdoi=c("month_0","day_0","month_2","day_2","month_5","day_5",
       "year_8","month_8","day_8","yearin_11","monthin_11","dayin_11","yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18") #日期
colnames(dataset_2000_2018)[which(colnames(dataset_2000_2018)%in% c("month00","day00"))] <- c("month_0","day_0")
df2an <- dataset_2000_2018[,c(vid,vsex,vage,vresid,vedu,vocc,vmar,vcog,vADL,vstatus,vdoi)]
L <- sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
               "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
               "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s","e1%s","e2%s",
               "e3%s","e4%s","e5%s","e6%s"),"")
Lchange <- sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
                     "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
                     "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s","e1%s","e2%s",
                     "e3%s","e4%s","e5%s","e6%s"),"_0")
colnames(df2an)[which(colnames(df2an) %in% L)] <- Lchange

#2002-2018
vcog=c()  #客观认知水平
for (i in c("","_5","_8","_11","_14","_18")){
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vADL=c()  #主观评价
for (i in c("","_5","_8","_11","_14","_18")){
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vstatus=c("dth02_05","dth05_08","dth08_11","dth11_14","dth14_18")
vdoi=c("month_2","day_2","month_5","day_5",
       "year_8","month_8","day_8","yearin_11","monthin_11","dayin_11","yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18") #日期
colnames(dataset_2002_2018)[which(colnames(dataset_2002_2018)%in% c("month02","day02"))] <- c("month_2","day_2")
df3an <- dataset_2002_2018[,c(vid,vsex,vage,vresid,vedu,vocc,vmar,vcog,vADL,vstatus,vdoi)]
Lchange<-sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
                   "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
                   "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s","e1%s","e2%s",
                   "e3%s","e4%s","e5%s","e6%s"),"_2")
colnames(df3an)[which(colnames(df3an) %in% L)] <- Lchange

#2005-2018
vcog=c()  #客观认知水平
for (i in c("","_8","_11","_14","_18")){
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vADL=c()  #主观评价
for (i in c("","_8","_11","_14","_18")){
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vstatus=c("dth05_08","dth08_11","dth11_14","dth14_18")
vdoi=c("month_5","day_5","year_8","month_8","day_8","yearin_11","monthin_11","dayin_11","yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18") #日期
colnames(dataset_2005_2018)[which(colnames(dataset_2005_2018)%in% c("monthin","dayin"))] <- c("month_5","day_5")
df4an <- dataset_2005_2018[,c(vid,vsex,vage,vresid,vedu,vocc,vmar,vcog,vADL,vstatus,vdoi)]
Lchange<-sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
                   "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
                   "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s","e1%s","e2%s",
                   "e3%s","e4%s","e5%s","e6%s"),"_5")
colnames(df4an)[which(colnames(df4an) %in% L)] <- Lchange

#2008-2018
vcog=c()  #客观认知水平
for (i in c("","_11","_14","_18")){
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vADL=c()  #主观评价
for (i in c("","_11","_14","_18")){
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vstatus=c("dth08_11","dth11_14","dth14_18")
vdoi=c("year_8","month_8","day_8","yearin_11","monthin_11","dayin_11","yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18") #日期
colnames(dataset_2008_2018)[which(colnames(dataset_2008_2018)%in% c("yearin","monthin","dayin"))] <- c("year_8","month_8","day_8")
df5an <- dataset_2008_2018[,c(vid,vsex,vage,vresid,vedu,vocc,vmar,vcog,vADL,vstatus,vdoi)]
Lchange<-sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
                   "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
                   "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s","e1%s","e2%s",
                   "e3%s","e4%s","e5%s","e6%s"),"_8")
colnames(df5an)[which(colnames(df5an) %in% L)] <- Lchange

#2011-2018
vcog=c()  #客观认知水平
for (i in c("","_14","_18")){
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vADL=c()  #主观评价
for (i in c("","_14","_18")){
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vstatus=c("dth11_14","dth14_18")
vdoi=c("yearin_11","monthin_11","dayin_11","yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18") #日期
colnames(dataset_2011_2018)[which(colnames(dataset_2011_2018)%in% c("yearin","monthin","dayin"))] <- c("yearin_11","monthin_11","dayin_11")
df6an <- dataset_2011_2018[,c(vid,vsex,vage,vresid,vedu,vocc,vmar,vcog,vADL,vstatus,vdoi)]
Lchange<-sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
                   "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
                   "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s","e1%s","e2%s",
                   "e3%s","e4%s","e5%s","e6%s"),"_11")
colnames(df6an)[which(colnames(df6an) %in% L)] <- Lchange

#2014-2018
vcog=c()  #客观认知水平
for (i in c("","_18")){
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i)
  vcog=c(vcog,a)
}
vADL=c()  #主观评价
for (i in c("","_18")){
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),i)
  vADL=c(vADL,a)
}
vstatus=c("dth14_18")
vdoi=c("yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18") #日期
colnames(dataset_2014_2018)[which(colnames(dataset_2014_2018)%in% c("yearin","monthin","dayin"))] <- c("yearin_14","monthin_14","dayin_14")
df7an <- dataset_2014_2018[,c(vid,vsex,vage,vresid,vedu,vocc,vmar,vcog,vADL,vstatus,vdoi)]
Lchange<-sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c16%s","c21a%s","c21b%s","c21c%s",
                   "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
                   "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s","e1%s","e2%s",
                   "e3%s","e4%s","e5%s","e6%s"),"_14")
colnames(df7an)[which(colnames(df7an) %in% L)] <- Lchange

write.csv(df1an,"df1an.csv")
write.csv(df2an,"df2an.csv")
write.csv(df3an,"df3an.csv")
write.csv(df4an,"df4an.csv")
write.csv(df5an,"df5an.csv")
write.csv(df6an,"df6an.csv")
write.csv(df7an,"df7an.csv")

df2an <- df2an[-which(df2an$id %in% df1an$id),]
df3an <- df3an[-which(df3an$id %in% c(df1an$id,df2an$id)),]
df4an <- df4an[-which(df4an$id %in% c(df1an$id,df2an$id,df3an$id)),]
df5an <- df5an[-which(df5an$id %in% c(df1an$id,df2an$id,df3an$id,df4an$id)),]
df6an <- df6an[-which(df6an$id %in% c(df1an$id,df2an$id,df3an$id,df4an$id,df5an$id)),]
df7an <- df7an[-which(df7an$id %in% c(df1an$id,df2an$id,df3an$id,df4an$id,df5an$id,df6an$id)),]

dfall <- mergeTwo(df1an,df2an)
dfall <- mergeTwo(dfall,df3an)
dfall <- mergeTwo(dfall,df4an)
dfall <- mergeTwo(dfall,df5an)
dfall <- mergeTwo(dfall,df6an)
dfall <- mergeTwo(dfall,df7an)
write.csv(dfall,"dfall.csv")
#手工去掉空格

dfall <- read.csv("dfall.csv")

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

vcog1=c()  #客观认知水平
for (i in c("","_0","_2","_5","_8","_11","_14","_18")){
  a=sprintf(c("c11%s","c12%s","c13%s","c14%s","c15%s","c21a%s","c21b%s","c21c%s",
              "c31a%s","c31b%s","c31c%s","c31d%s","c31e%s","c32%s","c41a%s","c41b%s",
              "c41c%s","c51a%s","c51b%s","c52%s","c53a%s","c53b%s","c53c%s"),i) #"c16%s"是列举题，暂时除???
  vcog1=c(vcog1,a)
}

for(i in vcog){
  dfall[,i] <- recode(dfall[,i],'"correct"=1;c("can\'tusepentodrawthefigure","notabletoanswer",
                      "notabletodo","notabletodothis(disabled)","unabletoanswer","unabletodo",
                      "wrong")=0;else=NA')
}

dfnew <- dfall
#处理列举题成???
a <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"c16")
b <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"c16trans")
for(i in 1:length(a)){
  dfnew[,a[i]] <- as.numeric(dfnew[,a[i]])
  dfnew[,b[i]] <- dfnew[,a[i]]
  dfnew[,b[i]][which(dfnew[,b[i]]>=7)] <- 7
}

#日常活动能力自评???"without assistance"???2，分三级
a <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"e1")
for(i in a){
  dfnew[,i] <- recode(dfnew[,i],"'morethanonepartassistance'=0;
                      'onepartassistance'=1;'withoutassistance'=2;else=NA")
}

a <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"e2")
for(i in a){
  dfnew[,i] <- recode(dfnew[,i],"c('assistanceingettingclothesandgettingdressed','morethanonepartassistance')=0;
                      c('needassistancefortryingshoes','onepartassistance')=1;
                      'withoutassistance'=2;else=NA")
}

a <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"e3")
for(i in a){
  dfnew[,i] <- recode(dfnew[,i],'c("don\'tusetoilet","morethanonepartassistance")=0;
                      c("assistanceincleaningorarrangingclothes","onepartassistance")=1;
                      "withoutassistance"=2;else=NA')
}

a <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"e4")
for(i in a){
  dfnew[,i] <- recode(dfnew[,i],'c("bedridden","morethanonepartassistance")=0;
                      c("withassistance","onepartassistance")=1;"withoutassistance"=2;else=NA')
}

a <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"e5")
for(i in a){
  dfnew[,i] <- recode(dfnew[,i],'c("incontinent","morethanonepartassistance")=0;
                      c("occasionalaccidents","onepartassistance")=1;"withoutassistance"=2;else=NA')
}

a <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"e6")
for(i in a){
  dfnew[,i] <- recode(dfnew[,i],'c("needfeeding","morethanonepartassistance")=0;
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
  dfmmse <- dfnew[,a]
  #dfmmse[is.na(dfmmse)] <- 0
  mmse <- c(unlist(apply(dfmmse,1,sum)))
  mmselist[i] <- list(mmse)
}
for(i in 1:length(vmse)){
  dfnew[,vmse[i]] <- mmselist[[i]]
}

#判断是否精神疾病
vmmse <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"mmse")
for(i in 1:length(vmmse)){
  dfnew[,vmmse[i]] <- ifelse(dfnew[,vmse[i]]<18,1,0)  #CI-1,non-0
}

#计算ADL自评
adllist <- list()
p <- c("","_0","_2","_5","_8","_11","_14","_18")
for (i in 1:length(p)){
  k <- p[i]
  a=sprintf(c("e1%s","e2%s","e3%s","e4%s","e5%s","e6%s"),k)
  dfadl <- dfnew[,a]
  #dfadl[is.na(dfadl)] <- 0
  adl <- c(unlist(apply(dfadl,1,sum)))
  adllist[i] <- list(adl)
}
vadl <- sprintf(c("%s","%s_0","%s_2","%s_5","%s_8","%s_11","%s_14","%s_18"),"adl")
for(i in 1:length(vadl)){
  dfnew[,vadl[i]] <- adllist[[i]]
}

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
dfifdie <- dfnew[,c(vid,vstatus)]
for(i in 1:nrow(dfifdie)){
  if(dfifdie$id[i] %in% idstart){
    dfifdie$dth98[i] <- 1
  }else{
    dfifdie$dth98[i] <- NA
  }
}
vstatus=c("dth98","dth98_00","dth00_02","dth02_05","dth05_08","dth08_11","dth11_14","dth14_18")
dfifdie <- melt(dfifdie,measure=vstatus,variable.name = "year",value.name = "status")
dfifdie$year <- recode(dfifdie$year,'"dth98"=0;"dth98_00"=2;"dth00_02"=4;"dth02_05"=7;"dth05_08"=10;
                       "dth08_11"=13;"dth11_14"=16;"dth14_18"=20')
dfifdie$year <- as.character(dfifdie$year)
dfifhea <- dfnew[,c(vid,vmmse)]
dfifhea <- melt(dfifhea,measure=vmmse,variable.name = "year",value.name = "mental")
dfifhea$year <- recode(dfifhea$year,'"mmse"=0;"mmse_0"=2;"mmse_2"=4;"mmse_5"=7;"mmse_8"=10;
                       "mmse_11"=13;"mmse_14"=16;"mmse_18"=20')
dfifhea$year <- as.character(dfifhea$year)
dfif <- merge(dfifdie,dfifhea,by=c("id","year"))
dfif$year <- as.numeric(dfif$year)
dfif <- dfif[-which(is.na(dfif$mental)),]
library(plyr)
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
et <- data.frame(id=et[1,],enterT=et[2,])
dfnew2 <- merge(dfnew,et,by="id")

it <- lapply(dfif2,illTime)
it <- unlist(it)
it <- matrix(it,nrow=2) #第一行id，第二行year
it <- data.frame(id=it[1,],illT=it[2,])

dfifhea <- dfnew2[,c(vid,vmmse,"enterT")]
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
ot <- data.frame(id=ot[1,],outT=ot[2,])

dfsur <- dfnew[c("id","sex","age","residenc","edu","occ","marriage")]
dfsur <- merge(dfsur,et,by="id")
dfsur <- merge(dfsur,it,by="id")
dfsur <- merge(dfsur,ot,by="id")

#具体时间
vdoi=c("year9899","month98","date98","month_0","day_0","month_2","day_2","month_5","day_5",
       "year_8","month_8","day_8","yearin_11","monthin_11","dayin_11","yearin_14","monthin_14",
       "dayin_14","yearin_18","monthin_18","dayin_18")
dfdate <- dfall[,c(vid,vdoi)]
dfdate$year_8 <- recode(dfdate$year_8,'c("-9","2009")=2009;"-7"=2007;c("-8","2008")=2008')
for(i in vdoi){
  dfdate[,i]<-replace(dfdate[,i],which(dfdate[,i]==99),NA)
  dfdate[,i]<- as.numeric(dfdate[,i])
}

dfdate2 <- data.frame(id=dfdate$id)
for(i in 1:nrow(dfdate)){
  if(!is.na(dfdate$year9899[i]) & !is.na(dfdate$month98[i]) & !is.na(dfdate$date98[i])){
    dfdate$time98[i] <- paste(dfdate$year9899[i],dfdate$month98[i],dfdate$date98[i],sep="-")
    dfdate$time98[i] <- as.Date(dfdate$time98[i])
    dfdate2$time98[i] <- as.numeric(dfdate$time98[i])
  }else{
    dfdate$time98[i] <- NA
    dfdate2$time98[i] <- NA
  }
}
for(i in 1:nrow(dfdate)){
  if(!is.na(dfdate$month_0[i]) & !is.na(dfdate$day_0[i])){
    dfdate$time00[i] <- paste("2000",dfdate$month_0[i],dfdate$day_0[i],sep="-")
    dfdate$time00[i] <- as.Date(dfdate$time00[i])
    dfdate2$time00[i] <- as.numeric(dfdate$time00[i])
  }else{
    dfdate$time00[i] <- NA
    dfdate2$time00[i] <- NA
  }
}
for(i in 1:nrow(dfdate)){
  if(!is.na(dfdate$month_2[i]) & !is.na(dfdate$day_2[i])){
    dfdate$time02[i] <- paste("2002",dfdate$month_2[i],dfdate$day_2[i],sep="-")
    dfdate$time02[i] <- as.Date(dfdate$time02[i])
    dfdate2$time02[i] <- as.numeric(dfdate$time02[i])
  }else{
    dfdate$time02[i] <- NA
    dfdate2$time02[i] <- NA
  }
}
#"2002-6-31"

for(i in 1:nrow(dfdate)){
  if(!is.na(dfdate$month_5[i]) & !is.na(dfdate$day_5[i])){
    dfdate$time05[i] <- paste("2005",dfdate$month_5[i],dfdate$day_5[i],sep="-")
    dfdate$time05[i] <- as.Date(dfdate$time05[i])
    dfdate2$time05[i] <- as.numeric(dfdate$time05[i])
  }else{
    dfdate$time05[i] <- NA
    dfdate2$time05[i] <- NA
  }
}
#"2005-6-31"

for(i in 1:nrow(dfdate)){
  if(!is.na(dfdate$year_8[i]) & !is.na(dfdate$month_8[i]) & !is.na(dfdate$day_8[i])){
    dfdate$time08[i] <- paste(dfdate$year_8[i],dfdate$month_8[i],dfdate$day_8[i],sep="-")
    dfdate$time08[i] <- as.Date(dfdate$time08[i])
    dfdate2$time08[i] <- as.numeric(dfdate$time08[i])
  }else{
    dfdate$time08[i] <- NA
    dfdate2$time08[i] <- NA
  }
}
#2008-11-31
for(i in 1:nrow(dfdate)){
  if(!is.na(dfdate$yearin_11[i]) & !is.na(dfdate$monthin_11[i]) & !is.na(dfdate$dayin_11[i])){
    dfdate$time11[i] <- paste(dfdate$yearin_11[i],dfdate$monthin_11[i],dfdate$dayin_11[i],sep="-")
    dfdate$time11[i] <- as.Date(dfdate$time11[i])
    dfdate2$time11[i] <- as.numeric(dfdate$time11[i])
  }else{
    dfdate$time11[i] <- NA
    dfdate2$time11[i] <- NA
  }
}
for(i in 1:nrow(dfdate)){
  if(!is.na(dfdate$yearin_14[i]) & !is.na(dfdate$monthin_14[i]) & !is.na(dfdate$dayin_14[i])){
    dfdate$time14[i] <- paste(dfdate$yearin_14[i],dfdate$monthin_14[i],dfdate$dayin_14[i],sep="-")
    dfdate$time14[i] <- as.Date(dfdate$time14[i])
    dfdate2$time14[i] <- as.numeric(dfdate$time14[i])
  }else{
    dfdate$time14[i] <- NA
    dfdate2$time14[i] <- NA
  }
}
for(i in 1:nrow(dfdate)){
  if(!is.na(dfdate$yearin_18[i]) & !is.na(dfdate$monthin_18[i]) & !is.na(dfdate$dayin_18[i])){
    dfdate$time18[i] <- paste(dfdate$yearin_18[i],dfdate$monthin_18[i],dfdate$dayin_18[i],sep="-")
    dfdate$time18[i] <- as.Date(dfdate$time18[i])
    dfdate2$time18[i] <- as.numeric(dfdate$time18[i])
  }else{
    dfdate$time18[i] <- NA
    dfdate2$time18[i] <- NA
  }
}

#入组时间

for(i in 1:nrow(dfdate2)){
  p <- dfdate2$id[i]
  if(p %in% id1998){
    dfdate2$baseTime[i] <- dfdate2$time98[i]
  }else if(p %in% id2000){
    dfdate2$baseTime[i] <- dfdate2$time00[i]
  }else if(p %in% id2002){
    dfdate2$baseTime[i] <- dfdate2$time02[i]
  }else if(p %in% id2005){
    dfdate2$baseTime[i] <- dfdate2$time05[i]
  }else if(p %in% id2008){
    dfdate2$baseTime[i] <- dfdate2$time08[i]
  }else if(p %in% id2011){
    dfdate2$baseTime[i] <- dfdate2$time11[i]
  }else if(p %in% id2014){
    dfdate2$baseTime[i] <- dfdate2$time14[i]
  }
}


#确诊时间
illid1998 <- dfsur$id[which(dfsur$illT==0)]
illid2000 <- dfsur$id[which(dfsur$illT==2)]
illid2002 <- dfsur$id[which(dfsur$illT==4)]
illid2005 <- dfsur$id[which(dfsur$illT==7)]
illid2008 <- dfsur$id[which(dfsur$illT==10)]
illid2011 <- dfsur$id[which(dfsur$illT==13)]
illid2014 <- dfsur$id[which(dfsur$illT==16)]
illid2018 <- dfsur$id[which(dfsur$illT==20)]
for(i in 1:nrow(dfdate2)){
  p <- dfdate2$id[i]
  if(p %in% illid1998){
    dfdate2$illTime[i] <- dfdate2$time98[i]
  }else if(p %in% illid2000){
    dfdate2$illTime[i] <- dfdate2$time00[i]
  }else if(p %in% illid2002){
    dfdate2$illTime[i] <- dfdate2$time02[i]
  }else if(p %in% illid2005){
    dfdate2$illTime[i] <- dfdate2$time05[i]
  }else if(p %in% illid2008){
    dfdate2$illTime[i] <- dfdate2$time08[i]
  }else if(p %in% illid2011){
    dfdate2$illTime[i] <- dfdate2$time11[i]
  }else if(p %in% illid2014){
    dfdate2$illTime[i] <- dfdate2$time14[i]
  }else if(p %in% illid2018){
    dfdate2$illTime[i] <- dfdate2$time18[i]
  }else{
    dfdate2$illTime[i] <- NA
  }
}

#出组时间
outid1998 <- dfsur$id[which(dfsur$outT==0)]
outid2000 <- dfsur$id[which(dfsur$outT==2)]
outid2002 <- dfsur$id[which(dfsur$outT==4)]
outid2005 <- dfsur$id[which(dfsur$outT==7)]
outid2008 <- dfsur$id[which(dfsur$outT==10)]
outid2011 <- dfsur$id[which(dfsur$outT==13)]
outid2014 <- dfsur$id[which(dfsur$outT==16)]
outid2018 <- dfsur$id[which(dfsur$outT==20)]
for(i in 1:nrow(dfdate2)){
  p <- dfdate2$id[i]
  if(p %in% outid1998){
    dfdate2$outTime[i] <- dfdate2$time98[i]
  }else if(p %in% outid2000){
    dfdate2$outTime[i] <- dfdate2$time00[i]
  }else if(p %in% outid2002){
    dfdate2$outTime[i] <- dfdate2$time02[i]
  }else if(p %in% outid2005){
    dfdate2$outTime[i] <- dfdate2$time05[i]
  }else if(p %in% outid2008){
    dfdate2$outTime[i] <- dfdate2$time08[i]
  }else if(p %in% outid2011){
    dfdate2$outTime[i] <- dfdate2$time11[i]
  }else if(p %in% outid2014){
    dfdate2$outTime[i] <- dfdate2$time14[i]
  }else if(p %in% outid2018){
    dfdate2$outTime[i] <- dfdate2$time18[i]
  }
}

dfsur <- merge(dfsur,dfdate2[,c("id","baseTime","illTime","outTime")],by="id")
for(i in 1:nrow(dfsur)){
  if(is.na(dfsur$illT[i])){
    dfsur$outcome[i] <- 0  #0表示结局未患???
    dfsur$totalT[i] <- dfsur$outT[i] - dfsur$enterT[i]
    if(!is.na(dfsur$outTime[i]) & !is.na(dfsur$baseTime[i])){
      dfsur$totalTime[i] <- dfsur$outTime[i]-dfsur$baseTime[i]
    }else{
      if(!is.na(dfsur$totalT[i])){
        dfsur$totalTime[i] <- dfsur$totalT[i]*365
      }else{
        dfsur$totalTime[i] <- NA
      }
    }
  }else{
    dfsur$outcome[i] <- 1
    dfsur$totalT[i] <- dfsur$illT[i] - dfsur$enterT[i]
    if(!is.na(dfsur$illTime[i]) & !is.na(dfsur$baseTime[i])){
      dfsur$totalTime[i] <- dfsur$illTime[i]-dfsur$baseTime[i]
    }else{
      if(!is.na(dfsur$totalT[i])){
        dfsur$totalTime[i] <- dfsur$totalT[i]*365
      }else{
        dfsur$totalTime[i] <- NA
      }
    }
  }
}

dfdate3 <- merge(dfdate2,dfsur[,c("id","enterT")],by="id")
dfdate3 <- melt(dfdate3,id=c("id","baseTime","illTime","outTime","enterT"),variable.name = "year",value.name = "time")
dfdate3$year <- recode(dfdate3$year,'"time98"=0;"time00"=2;
                       "time02"=4;"time05"=7;"time08"=10;"time11"=13;
                       "time14"=16;"time18"=20')
dfdate3$year <- as.character(dfdate3$year)
dfdate3$year <- as.numeric(dfdate3$year)
for(i in 1:nrow(dfdate3)){
  if(!is.na(dfdate3$baseTime[i]) & !is.na(dfdate3$time[i])){
    dfdate3$times[i] <- dfdate3$time[i]-dfdate3$baseTime[i]
  }else{
    dfdate3$times[i] <- NA
  }
}
write.csv(dfdate3,"dfdate3.csv")

dflongi <- dfnew[c("id","sex","age","residenc","edu","occ","marriage",vadl)]
dflongi <- merge(dflongi,dfsur[,c("id","outcome","totalTime")],by="id")

dflongi <- melt(dflongi,measure=vadl,variable.name = "year",value.name = "ADL")
dflongi$year <- recode(dflongi$year,'"adl"=0;"adl_0"=2;"adl_2"=4;"adl_5"=7;"adl_8"=10;"adl_11"=13;
                     "adl_14"=16;"adl_18"=20')
dflongi$year <- as.character(dflongi$year)
dflongi$year <- as.numeric(dflongi$year)

dflongi <- merge(dflongi,dfdate3[,c("id","baseTime","illTime","outTime","year","times")],by=c("id","year"))
dflongi <- merge(dflongi,dffemo,by=c("id","year"))

dfmse2 <- dfnew[,c(vid,vmse)]
dfmse2 <- melt(dfmse2,id="id",variable.name = "year",value.name = "MSE")
dfmse2$year <- recode(dfmse2$year,'"mse"=0;"mse_0"=2;"mse_2"=4;"mse_5"=7;"mse_8"=10;
                      "mse_11"=13;"mse_14"=16;"mse_18"=20')
dflongi <- merge(dflongi,dfmse2,by=c("id","year"))
dflongi$outcomes <- factor(dflongi$outcome, levels = 0:1,labels = c("health","illness"))
write.csv(dflongi,"dflongi.csv")
write.csv(dfsur,"dfsur.csv")

#确定跟踪次数
dftimes <- dfsur[,c("id","enterT","illT","outT")]
for(i in 1:nrow(dftimes)){
  if(dftimes$enterT[i]==0){
    dftimes$eN[i] <- 0
  }else if(dftimes$enterT[i]==2){
    dftimes$eN[i] <- 1
  }else if(dftimes$enterT[i]==4){
    dftimes$eN[i] <- 2
  }else if(dftimes$enterT[i]==7){
    dftimes$eN[i] <- 3
  }else if(dftimes$enterT[i]==10){
    dftimes$eN[i] <- 4
  }else if(dftimes$enterT[i]==13){
    dftimes$eN[i] <- 5
  }else if(dftimes$enterT[i]==16){
    dftimes$eN[i] <- 6
  }else{
    dftimes$eN[i] <- NA
  }
  
  if(is.na(dftimes$illT[i])){
    dftimes$iN[i] <- NA
  }else if(dftimes$illT[i]==0){
    dftimes$iN[i] <- 0
  }else if(dftimes$illT[i]==2){
    dftimes$iN[i] <- 1
  }else if(dftimes$illT[i]==4){
    dftimes$iN[i] <- 2
  }else if(dftimes$illT[i]==7){
    dftimes$iN[i] <- 3
  }else if(dftimes$illT[i]==10){
    dftimes$iN[i] <- 4
  }else if(dftimes$illT[i]==13){
    dftimes$iN[i] <- 5
  }else if(dftimes$illT[i]==16){
    dftimes$iN[i] <- 6
  }else if(dftimes$illT[i]==20){
    dftimes$iN[i] <- 7
  }else{
    dftimes$iN[i] <- NA
  }
  
  if(is.na(dftimes$outT[i])){
    dftimes$oN[i] <- NA
  }else if(dftimes$outT[i]==0){
    dftimes$oN[i] <- 0
  }else if(dftimes$outT[i]==2){
    dftimes$oN[i] <- 1
  }else if(dftimes$outT[i]==4){
    dftimes$oN[i] <- 2
  }else if(dftimes$outT[i]==7){
    dftimes$oN[i] <- 3
  }else if(dftimes$outT[i]==10){
    dftimes$oN[i] <- 4
  }else if(dftimes$outT[i]==13){
    dftimes$oN[i] <- 5
  }else if(dftimes$outT[i]==16){
    dftimes$oN[i] <- 6
  }else if(dftimes$outT[i]==20){
    dftimes$oN[i] <- 7
  }else{
    dftimes$oN[i] <- NA
  }
}
dftimes$count <- dftimes$oN-dftimes$eN+1
write.csv(dftimes,"dftimes.csv")

