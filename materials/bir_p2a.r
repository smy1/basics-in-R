#### Basics in R
##by MY Sia, NTNU (Taiwan)
##20.8.2024 (long count: 13.0.11.15.0, 13 Ajpu')

#### Part 2a.1 ####
setwd("C:/Users/smy/Desktop/Basics in R/materials")
library("readxl")
##first sheet
d1=read_excel("mock_data.xlsx", sheet="data")
str(d1)
d1=as.data.frame(d1)
##clean first sheet
d1$id=as.character(d1$id)
#d1$sex=as.factor(d1$sex)
#unique(d1$sex)
d1$sex=as.factor(tolower(d1$sex))
d1$grp=as.factor(d1$grp)
#d1$ma.edu=as.factor(d1$ma.edu)
#unique(d1$ma.edu)
d1$ma.edu=factor(d1$ma.edu, level=c("mid.sch", "grad.sch", "uni"))
d1$lang.his=factor(d1$lang.his, level=c("mono", "bi"))
d1$book=as.factor(d1$book)
d1[,c(10:14)]=apply(d1[,c(10:14)], MARGIN=2, FUN=as.numeric)
##about NA
any(is.na(d1$ch1.hbo))
#mean(d1$ch1.hbo)
mean(d1$ch1.hbo, na.rm=T)

##second sheet
d2=read_excel("mock_data.xlsx", sheet=2)
str(d2)
d2=as.data.frame(d2)
d2$id=as.character(d2$id)

##combine sheet 1 and 2
d.all=merge(d1, d2, by="id", all=T)
str(d.all) ##always check the data after merging!!
save.image("bir.RData")
#### END of Part 2a.1 ####

#### Part 2a.2 ####
##start with one data
#load("bir.RData")
ef=read.csv("./mock_ef/mock_ef_1.csv")
str(ef)
ef$id=as.character(ef$id)
ef$cond=factor(ef$cond, level=c("neut", "cong", "inco"))
ef[ef==""]=NA ##convert empty cells into NAs
##calculate scores
is.prac.corr=ifelse(ef$prac.resp==ef$prac.corr, 1, 0)
pass=ifelse(sum(is.prac.corr, na.rm=T)==3, "pass", "no")
ef$is.test.corr=ifelse(ef$test.resp==ef$test.corr, 1, 0)
test.score=aggregate(is.test.corr~cond, data=ef, FUN=sum, na.rm=T)
##extract data
#xx=data.frame(id=NA, pass=NA, neut=NA, cong=NA, inco=NA)
xx=c() ##create an empty holder
xx$id=ef$id[1]
xx$pass=pass
xx$neut=test.score$is.test.corr[1] ##test.score$is.test.corr[test.score$cond=="neut"]
xx$cong=test.score$is.test.corr[2] ##test.score$is.test.corr[test.score$cond=="cong"]
xx$inco=test.score$is.test.corr[3] ##test.score$is.test.corr[test.score$cond=="inco"]
##add data to an empty dataframe
d.ef=data.frame(matrix(NA, nrow=0, ncol=5))
d.ef=rbind(d.ef, xx)

##put everything in a loop
d.ef=data.frame(matrix(NA, nrow=0, ncol=5)) ##create an empty dataframe OUTSIDE the loop
eflist=list.files(path="./mock_ef/", pattern=".csv", ignore.case=T, all.files=T) ##specify file list
for (i in 1:length(eflist)){
    ef=read.csv(paste0(path="./mock_ef/", eflist[i])) ##use this instead of the file's name
    ef$id=as.character(ef$id)
    ef$cond=factor(ef$cond, level=c("neut", "cong", "inco"))
    ef[ef==""]=NA
    ##calculate scores
    is.prac.corr=ifelse(ef$prac.resp==ef$prac.corr, 1, 0)
    pass=ifelse(sum(is.prac.corr, na.rm=T)==3, "pass", "no")
    ef$is.test.corr=ifelse(ef$test.resp==ef$test.corr, 1, 0)
    test.score=aggregate(is.test.corr~cond, data=ef, FUN=sum, na.rm=T)
    ##extract data
    xx=c() ##create an empty holder
    xx$id=ef$id[1]
    xx$pass=pass
    xx$neut=test.score$is.test.corr[1]
    xx$cong=test.score$is.test.corr[2]
    xx$inco=test.score$is.test.corr[3]
    ##add data to an empty dataframe
    d.ef=rbind(d.ef, xx)
}

##combine all data together
xdata1=merge(d.all, d.ef, by="id")

xdata=merge(d.all, d.ef, by="id", all=T)
str(xdata) ##always check the data after merging!!
save.image("bir.RData")
#### END of Part 2a.2 ####
