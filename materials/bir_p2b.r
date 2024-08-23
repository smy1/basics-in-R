#### Basics in R
##by MY Sia, NTNU (Taiwan)
##20.8.2024 (long count: 13.0.11.15.0, 13 Ajpu')

#### Part 2b ####
setwd("C:/Users/smy/Desktop/Basics in R/materials")
load("bir.RData") ##we cleaned and compiled some data in part 2a
x.mono=subset(xdata, xdata$lang.his=="mono")
x.pass=subset(x.mono, x.mono$pass=="pass")

#### plot 1a: scatterplot
jpeg("plot1.jpg", width=500)
par(mar=c(3.4, 3.4, 0.5, 0.5), mgp=c(1.8, 0.5, 0), tcl=-0.3, las=1)
plot(x=x.pass$age, y=x.pass$read.dur, #ylim=c(0, 10),
     xlab="Age (year)", ylab="Reading duration (minutes)",
     pch=8, cex=1.3, lwd=2, col="blue",
     cex.axis=1.3, cex.lab=1.8)
##add line
age.read=lm(x.pass$read.dur~x.pass$age)
abline(age.read, lty=2, lwd=3)
dev.off()

#### plot 1b: separate group
x.int=subset(x.pass, x.pass$grp=="interest")
x.not=subset(x.pass, x.pass$grp=="not")
plot(x=x.pass$age, y=x.pass$read.dur, type="n", ##type n means no plot (i.e., empty)
     xlab="Age (year)", ylab="Reading duration (minutes)",
     cex.axis=1.3, cex.lab=1.8)
##add points of the two groups to the plot
points(x=x.int$age, y=x.int$read.dur, pch=4, lwd=2, col="blue")
points(x=x.not$age, y=x.not$read.dur, pch=4, lwd=2, col="red")
mtext(text="blue = interested", side=3, at=7.5, line=-1)
mtext(text="red = not interested", side=3, at=7.5, line=-2)
##add line
x1=lm(x.int$read.dur~x.int$age)
x2=lm(x.not$read.dur~x.not$age)
abline(x1, lty=2, lwd=3, col="blue")
abline(x2, lty=2, lwd=3, col="red")
##correlation
cor.test(x=x.int$age, y=x.int$read.dur)

##extra: correlation
cor.test(x=x.pass$age, y=x.pass$read.dur)
##alternative="greater" ##default is two-sided
##method="spearman" ##default is pearson
c=cor.test(x=x.pass$age, y=x.pass$read.dur)
names(c)
p.val=round(c$p.value, 3)
r.val=round(c$estimate, 3)

#### plot2: boxplot
par(mar=c(3.4, 3.4, 0.5, 0.5), mgp=c(1.8, 0.5, 0), tcl=-0.3, las=1)
plot(x.pass$grp, x.pass$read.dur,
     xlab="Group", ylab="Reading duration (minutes)",
     xaxt="n", ##suppress x-axis labels
     cex.axis=1.3, cex.lab=1.8, lwd=2, col="deepskyblue")
##add x-axis labels
mtext(text=c("interested", "not interested"), at=c(1, 2), side=1,
      line=0.4, cex=1.3)

#### plot 2b: brain activation
which(colnames(x.pass)=="ch1.hbo") ##get column number
x.pass$avg.hbo=apply(x.pass[, c(10:14)], MARGIN=1, FUN=mean, na.rm=T)
par(mar=c(3.4, 3.8, 0.5, 0.5), mgp=c(2.2, 0.4, 0), tcl=-0.3, las=1)
plot(x.pass$grp, x.pass$avg.hbo,
     xlab="Group", ylab="Brain activation (HbO)",
     xaxt="n", ##suppress x-axis labels
     cex.axis=1.3, cex.lab=1.8, lwd=2, col="deepskyblue")
##add x-axis labels
mtext(text=c("interested", "not interested"), at=c(1, 2), side=1,
      line=0.4, cex=1.3)

##both plots together
par(mfrow=c(1, 2), mar=c(3.4, 3.6, 2, 2), mgp=c(2, 0.5, 0), tcl=-0.3, las=1)
##plot reading duration
plot(x.pass$grp, x.pass$read.dur, main="Reading duration",
     xlab="Group", ylab="Reading duration (minutes)", xaxt="n",
     cex.axis=1.3, cex.lab=1.6, lwd=2, col="deepskyblue")
mtext(text=c("interested", "not interested"), at=c(1, 2), side=1,
      line=0.4, cex=1.3)
##plot brain activation
plot(x.pass$grp, x.pass$avg.hbo, main="Brain activation",
     xlab="Group", ylab="Brain activation (HbO)", xaxt="n",
     cex.axis=1.3, cex.lab=1.6, lwd=2, col="deepskyblue")
mtext(text=c("interested", "not interested"), at=c(1, 2), side=1,
      line=0.4, cex=1.3)

##extra: t-test
t.test(read.dur~grp, data=x.pass)
t.test(avg.hbo~grp, data=x.pass)

#### 21.8.2024 (long count: 13.0.11.15.1, 1 Imox) ####
#### plot 2c: for sasha to identify outliers
##aim: add values of outliers to the boxplot
##in order to run the following script, the subsets in "plot 1b" needs to be run first
##calculate values for "interest" group
a=summary(x.int$read.dur) ##get summary of variable "read.dur"
a.q1=a["1st Qu."] ##extract first quartile from the summary
a.q3=a["3rd Qu."]
##see https://www150.statcan.gc.ca/n1/edu/power-pouvoir/ch12/5214889-eng.htm for the calculation below
a.max=a.q3+1.5*(a.q3-a.q1) ##calculate the maximum value of the boxplot (anything more will be treated as an outlier)
a.min=a.q1-1.5*(a.q3-a.q1)
x.int$read.dur[x.int$read.dur<a.min] ##identify values that are less than the minimum
x.int$read.dur[x.int$read.dur>a.max] ##identify values that are more than the maximum
##calculate values for "not" group
b=summary(x.not$read.dur)
b.q1=b["1st Qu."]
b.q3=b["3rd Qu."]
b.max=b.q3+1.5*(b.q3-b.q1)
b.min=b.q1-1.5*(b.q3-b.q1)
x.int$read.dur[x.not$read.dur<b.min]
x.int$read.dur[x.not$read.dur>b.max]
##start plotting
par(mar=c(3.4, 3.4, 0.5, 0.5), mgp=c(1.8, 0.5, 0), tcl=-0.3, las=1)
plot(x.pass$grp, x.pass$read.dur,
     xlab="Group", ylab="Reading duration (minutes)",
     xaxt="n", ##suppress x-axis labels
     cex.axis=1.3, cex.lab=1.8, lwd=2, col="deepskyblue")
##add x-axis labels
mtext(text=c("interested", "not interested"), at=c(1, 2), side=1,
      line=0.4, cex=1.3)
##add values of outliers
mtext(text="1.1", at=1.1, side=1, line=-2)
mtext(text="5.7", at=2.1, side=3, line=-13)
##as an alternative to "mtext", we could probably use the function "text"
## and give the coordinates of the outliers as a location for the text

#### plot 1c: bubble plots for sasha
##explanation: bubble plots are like a combination of scatterplot and violinplot,
## where the area of the bubble depicts the number of observation for a particular score
## we usually use bubble plots for discontinuous variables, like a score of 0, 1, 2, & 3 only (no in betweens)
##in this example, we look at children's score in
## the congruent and incongruent conditions of the flanker task
## (see "extract data" under "put everything in a loop" in the script above for more context)
##start here:
##calculate sample size of unique scores for each condition
cong=data.frame(table(x.pass$cong))
inco=data.frame(table(x.pass$inco))
tab=rbind(cong, inco)
tab$cond=as.factor(rep(c("cong", "inco"), each=nrow(cong)))
tab$score=as.numeric(as.character(tab$Var1))
##start plotting
par(mar=c(3.4, 3.4, 0.5, 0.5), mgp=c(1.8, 0.5, 0), tcl=-0.3, las=1)
plot(x=as.numeric(tab$cond), y=tab$score,
     xlim=c(0.5, 2.5), xaxt="n",
     xlab="Conditions", ylab="Flanker score",
     pch=19, cex=sqrt(tab$Freq)*1.5, ##size of bubble depends on the frequency of the score
     col="skyblue1", cex.axis=1.3, cex.lab=1.8)
mtext(text=c("Congruent", "Incongruent"), at=c(1, 2), side=1, line=0.4, cex=1.3)
##comment: I used "as.numeric" for "tab$cond", otherwise, R will insist on showing a boxplot
##comment: the frequency of scores of the two conditions is quite similar,
## so I manipulated them below.
fake.tab=tab ##duplicate "tab"
fake.tab$Freq=c(3, 10, 40, 60, 19,
                24, 61, 38, 12, 9) ##change the frequencies
##start plotting the modified data
par(mar=c(3.4, 3.4, 0.5, 0.5), mgp=c(1.8, 0.5, 0), tcl=-0.3, las=1)
plot(x=as.numeric(fake.tab$cond), y=fake.tab$score,
     xlim=c(0.5, 2.5), xaxt="n",
     xlab="Conditions", ylab="Flanker score",
     pch=19, cex=sqrt(fake.tab$Freq)*1.5,
     col="khaki1", cex.axis=1.3, cex.lab=1.8)
mtext(text=c("Congruent", "Incongruent"), at=c(1, 2), side=1, line=0.4, cex=1.3)
#### END of Part extra ####
