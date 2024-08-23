#### Basics in R
##by MY Sia, NTNU (Taiwan)
##20.8.2024 (long count: 13.0.11.15.0, 13 Ajpu')

#### Part 1 ####
##getting started
#install.packages("ggplot2")
#library("ggplot")
getwd()
setwd("C:/Users/smy/Desktop") ##set working directory

##check workspace
ls()
rm(list=ls()) ##clear workspace

##simple calculations
10/3 ##10 divide by 3
10%/%3 ##integer of 10 divide by 3
10%%3 ##remainder of 10 divide by 3

##basic mathematical functions
a=-5.2363 ##this is the same as a <- -5.2363
abs(a)
sqrt(a)
round(a, 2) ##round the number stored as a to 2 decimal places

##more mathematical functions
b=1:10 ##create a vector with numbers 1 to 10
sum(b) ##sum up the numbers of the vector
sd.b=sd(b) ##store the standard deviation of b as a variable called "sd.b"

##storing variables
id=c(1, 2, 3, 4, 5, 6, 7, 8, 9)
str(id) ##check the structure of the variable
id=as.character(id) ##converts the numbers into characters
id[2] ##gives the second element of the vector id

##factors
sex=c("f", "f", "m", "f", "m", "m", "m", "f", "m")
str(sex)
sex=as.factor(sex)
str(sex)
lvl=factor(c("diff", "easy", "diff", "diff", "easy", "diff", "easy", "easy", "diff"),
           levels=c("easy", "diff"))
lvl2=as.factor(c("diff", "easy", "deff", "diff", "easy", "deff", "easy", "easy", "diff"))
##data frames
d=data.frame(id, sex, lvl) ##bind vectors into a data frame
d$score=c(30, 65, 51, 48, 73, 33, 80, 75, 60) ##create a new variable
str(d)
summary(d) ##get summary of data
table(d$sex, d$lvl)
d$id[d$score==80] ##which ID scored 80?
#### END of Part 1 ####
