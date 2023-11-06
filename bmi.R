vweight <- c("g12","g10_0","g101_2","g101_5","g101_8","g101_11","g101_14","g101_18")
vheight <- c("g1021_8","g1021_11","g1021_14","g1021_18")
vknee <- c("g82","g102b_2")
dfbmi1 <- dataset_1998_2018[,c("id",vweight,vheight,vknee)]

vweight <- c("g10","g101_2","g101_5","g101_8","g101_11","g101_14","g101_18")
vknee <- c("g102b_2")
dfbmi2 <- dataset_2000_2018[,c("id",vweight,vheight,vknee)]
colnames(dfbmi2)[which(colnames(dfbmi2) %in% c("g10"))] <- c("g10_0")

vweight <- c("g101","g101_5","g101_8","g101_11","g101_14","g101_18")
vknee <- c("g102b")
dfbmi3 <- dataset_2002_2018[,c("id",vweight,vheight,vknee)]
colnames(dfbmi3)[which(colnames(dfbmi3) %in% c("g101","g102b"))] <- c("g101_2","g102b_2")

vweight <- c("g101","g101_8","g101_11","g101_14","g101_18")
dfbmi4 <- dataset_2005_2018[,c("id",vweight,vheight)]
colnames(dfbmi4)[which(colnames(dfbmi4) %in% c("g101"))] <- c("g101_5")

vweight <- c("g101","g101_11","g101_14","g101_18")
vheight <- c("g1021","g1021_11","g1021_14","g1021_18")
dfbmi5 <- dataset_2008_2018[,c("id",vweight,vheight)]
colnames(dfbmi5)[which(colnames(dfbmi5) %in% c("g101","g1021"))] <- c("g101_8","g1021_8")

vweight <- c("g101","g101_14","g101_18")
vheight <- c("g1021","g1021_14","g1021_18")
dfbmi6 <- dataset_2011_2018[,c("id",vweight,vheight)]
colnames(dfbmi6)[which(colnames(dfbmi6) %in% c("g101","g1021"))] <- c("g101_11","g1021_11")

vweight <- c("g101","g101_18")
vheight <- c("g1021","g1021_18")
dfbmi7 <- dataset_2014_2018[,c("id",vweight,vheight)]
colnames(dfbmi7)[which(colnames(dfbmi7) %in% c("g101","g1021"))] <- c("g101_14","g1021_14")

library(car)
library(reshape2)

v <- c("g12","g10_0","g101_2","g101_5","g101_8","g101_11","g101_14",
       "g101_18","g1021_8","g1021_11","g1021_14","g1021_18","g82","g102b_2")
transNum <- function(df){
  colN <- intersect(colnames(df),v)
  df[,colN] <- lapply(df[,colN],as.numeric)
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

dfbmi <- mergeTwo(dfbmi1,dfbmi2)
dfbmi <- mergeTwo(dfbmi,dfbmi3)
dfbmi <- mergeTwo(dfbmi,dfbmi4)
dfbmi <- mergeTwo(dfbmi,dfbmi5)
dfbmi <- mergeTwo(dfbmi,dfbmi6)
dfbmi <- mergeTwo(dfbmi,dfbmi7)

q=dfbmi$id[which(duplicated(dfbmi$id))]
for(i in q){
  dfbmi[which(dfbmi$id==i),] <- colMeans(dfbmi[which(dfbmi$id==i),],na.rm=TRUE)
}
dfbmi[which(dfbmi$id==i),]
dfbmi[which(dfbmi$id%in%q),]

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

dfsur <- read.csv("dfsur.csv")
dfbmi <- merge(dfbmi,dfsur[,c("id","sex")],by="id")
heightFromKnee <- function(dfbmi,knee,height){
  dfbmi[,height] <- NA
  dfbmi[which(dfbmi$sex==1),height] <- 74.08+1.81*dfbmi[which(dfbmi$sex==1),knee]
  dfbmi[which(dfbmi$sex==0),height] <- 67.78+2.01*dfbmi[which(dfbmi$sex==0),knee]
  return(dfbmi)
}

dfbmi <- heightFromKnee(dfbmi,"g82","h98")
dfbmi <- heightFromKnee(dfbmi,"g102b_2","h02")

dfbmi$h00 <- rowMeans(dfbmi[,c("h98","h02")],na.rm=TRUE)
dfbmi$h05 <- rowMeans(dfbmi[,c("h02","g1021_8")],na.rm=TRUE)

sort(unique(dfbmi$g1021_11))



# dfbmi$g1021_8 <- recode(dfbmi$g1021_8,"0:20=NA")
# dfbmi$g1021_11 <- recode(dfbmi$g1021_11,"0:89=NA")
# dfbmi$g1021_14 <- recode(dfbmi$g1021_14,"0:89=NA")
# dfbmi$g1021_18 <- recode(dfbmi$g1021_14,"0:89=NA")
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
write.csv(dfbmi2,"dfbmi.csv")
setwd("E:/CLHLS")

vknee <- c("g82","g102b_2")
