#	Library
	library(plyr)
	library(BayesFactor)

#	read in data
	data.kp.1<- read.table("data.all.kp.2.txt", header=T, sep="\t", quote="?")
	data.m.1<- read.table("data.all.m.2.txt", header=T, sep="\t", quote="?")
	data.scores.1 <- read.table("bdi score.txt", header=T, sep="\t", quote="?")

#	create data frame for both key press and mouse exps
	data.all1<-rbind.fill(data.kp.1, data.m.1)

#	adding column to count when subject responds above on bisection
	data.all1$above<-ifelse(data.all1$midPointResponse=="above", 1, 0)
#	adding column to count when subject is correct in bisection
	data.all1$correct<-ifelse(data.all1$midPointCorrect=="correct", 1, 0)
#	adding column to calculate the distance the center number is from the true midpoint
	data.all1$midDist<- data.all1$presented-data.all1$midPoint
#	adding column for RT beep if true
	data.all1$rtTrue<-ifelse(data.all1$rtBeep=="true", 1, 0)
#	adding column to count if RT is over 3 seconds
	data.all1$over_3s<-ifelse(data.all1$midPointRT>3000, 1, 0)

#	removing subjects 16, 59, 75, 173, 184, 224 (because the computer froze)
	data.scores<-data.scores.1[data.scores.1$sn!=16 & data.scores.1$sn!=59 & data.scores.1$sn!=75 & data.scores.1$sn!=173 & data.scores.1$sn!=184 & data.scores.1$sn!=224 & data.scores.1$sn<234,]
	data.all1<-data.all1[data.all1$sn!=16 & data.all1$sn!=59 & data.all1$sn!=75 & data.all1$sn!=173 & data.all1$sn!=184 & data.all1$sn!=224 & data.all1$sn<999,]

#	remove practice trials
	data.all1<- data.all1[data.all1$pract!=0,]

#	merge bdi and gender scores with data
	data.all1<-merge(data.all1, data.scores, by="sn")


#	create data frame for info by sn
	data.sn<-data.scores

data1.sn <- ddply(data.all1, .(sn, scen1PosNeg), summarize, aboveM = mean(above), aboveSD = sd(above), aboveN = length(above))
data2.sn <- reshape(data1.sn, idvar = "sn", timevar = "scen1PosNeg", direction = "wide")
data2.sn$diffAbove <- data2.sn$aboveM.negative - data2.sn$aboveM.positive
data1.all1<-merge(data2.sn, data.scores.1, by="sn")

	data.sn$Prop_Above<-tapply(data.all1$above, data.all1$sn, mean)
#	adding column to calculate percent respond "above" for pos event on left and neg event on left
	data.sn$NegLeft_Prop_Above<-tapply(data.all1[data.all1$scen1PosNeg=="negative",]$above,data.all1[data.all1$scen1PosNeg=="negative",]$sn,mean)
	data.sn$PosLeft_Prop_Above<-tapply(data.all1[data.all1$scen1PosNeg=="positive",]$above,data.all1[data.all1$scen1PosNeg=="positive",]$sn,mean)
#	adding column to calculate difference between prop above when negative is on the right and negative event on left
	data.sn$above_diff<-data.sn$NegLeft_Prop_Above-data.sn$PosLeft_Prop_Above
#	adding column to calculate average RT for pos event on left and neg events on left
	data.sn$NegLeft_RT<-tapply(data.all1[data.all1$scen1PosNeg=="negative",]$midPointRT,data.all1[data.all1$scen1PosNeg=="negative",]$sn,mean)
	data.sn$PosLeft_RT<-tapply(data.all1[data.all1$scen1PosNeg=="positive",]$midPointRT,data.all1[data.all1$scen1PosNeg=="positive",]$sn,mean)
#	adding column to calculate difference between RT when pos event is on left and negative event on left
	data.sn$RT_diff<-data.sn$NegLeft_RT-data.sn$PosLeft_RT
#	adding column to calculate percent of times participant heard beep
	data.sn$RTbeep<-tapply(data.all1$rtTrue,data.all1$sn,mean)
#	adding column to calculate proportion of RT's over 3seconds
	data.sn$Prop_RT_Over3<-tapply(data.all1$over_3s, data.all1$sn, mean)
#	adding columns to calculate avg and SD for RT when the midpoint distance is 0
	data.sn$AvgRT_midDist_0<-tapply(data.all1[data.all1$midDist==0,]$midPointRT,data.all1[data.all1$midDist==0,]$sn,mean)
	data.sn$SD_RT_midDist_0<-tapply(data.all1[data.all1$midDist==0,]$midPointRT,data.all1[data.all1$midDist==0,]$sn,sd)
#	adding columns to calculate avg and SD for RT when the midpoint distance is 0
	data.sn$AvgRT_midDist_Not0<-tapply(data.all1[data.all1$midDist!=0,]$midPointRT,data.all1[data.all1$midDist!=0,]$sn,mean)
	data.sn$SD_RT_midDist_Not0<-tapply(data.all1[data.all1$midDist!=0,]$midPointRT,data.all1[data.all1$midDist!=0,]$sn,sd)

#	create dataframe with a few columns from data.sn
	data.sn2<-data.sn[c(1, 12:17)]
#	merge with data.all by sn
	data.all1<-merge(data.all1, data.sn2, by="sn")


#filter subjects
	t3000Prop <-.4
	RtThreshold <-5000

	sn.rm<-data.all1[data.all1$Prop_RT_Over3 < t3000Prop,]
	data.all<-sn.rm[sn.rm$midPointRT < RtThreshold,]

	snAll<-length(tapply(data.all1$sn,data.all1$sn,mean))
	snIncluded<-length(tapply(sn.rm$sn,sn.rm$sn,mean))
	snRemoved <-snAll - snIncluded

	RtRemoved <-length(sn.rm$midPointRT) - length(data.all$midPointRT)

###################

#	create one data frame for trials were the first event is positive and one for negative
	data.pos<-data.all[data.all$scen1PosNeg=="positive",]
	data.neg<-data.all[data.all$scen1PosNeg=="negative",]
#	create data frames for 0 midpoint distance
	data.pos.same<-data.pos[data.pos$midDist==0,]
	data.neg.same<-data.neg[data.neg$midDist==0,]
#	create data frames for all midpoint distances other than 0
	data.pos.dif<-data.pos[data.pos$midDist!=0,]
	data.neg.dif<-data.neg[data.neg$midDist!=0,]
#	calculate avg RT for each participant for 0 midpoint distance
	dt.pos<-ddply(data.pos.same, .(sn), summarize, PosLeft_RT_midDist_0=mean(midPointRT))
	dt.neg<-ddply(data.neg.same, .(sn), summarize, NegLeft_RT_midDist_0=mean(midPointRT))
#	create data frame for all 0 mid point distances
	data.same<-merge(dt.pos, dt.neg, by="sn", all=T)
#	creating column to calculate difference between neg on right and neg on left for 0 midpoint distance
	data.same$diff_RT_midDist_0<-data.same$PosLeft_RT_midDist_0-data.same$NegLeft_RT_midDist_0
#	calculate avg RT for each participant for all midpoint distances other than 0
	dt.pos.d<-ddply(data.pos.dif, .(sn), summarize, PosLeft_RT_midDist_Not0=mean(midPointRT))
	dt.neg.d<-ddply(data.neg.dif, .(sn), summarize, NegLeft_RT_midDist_Not0=mean(midPointRT))
#	create data frame for all midpoint distances other than 0
	data.dif<-merge(dt.pos.d, dt.neg.d, by="sn", all=T)
#	creating column to calculate difference between neg on right and neg on left for all midpoint distances other than 0
	data.dif$diff_RT_midDist_Not0<-data.dif$PosLeft_RT_midDist_Not0-data.dif$NegLeft_RT_midDist_Not0

#	merge data frames for same midpoint and different midpoint distances with dataframe for info by sn
	data.sn<-merge(data.sn, merge(data.same, data.dif, by="sn"), by="sn")

#	plot average above difference for each bdi score
	bdi.x<-tapply(data.sn$bdi,data.sn$bdi, mean)
	abv_diff<-tapply(data.sn$above_diff,data.sn$bdi, mean)
	xx.bdi2<-bdi.x^2
	plot(bdi.x, abv_diff, xlab= "BDI scores", ylab= "Above Response Difference")
	reg<-lm(abv_diff~xx.bdi2)
	lines(bdi.x, predict(reg))
	summary(reg)
	dev.copy(pdf,'avg above difference by bdi.pdf')
	dev.off()

	plot(bdi.x, abv_diff, xlab= "BDI scores", ylab= "Above Response Difference", pch=19)
	regL<-lm(abv_diff~bdi.x)
	lines(bdi.x, predict(regL))
	summary(regL)
	dev.copy(pdf,'avg above difference by bdiL.pdf')
	dev.off()

	df.tmp <- data.frame(abv_diff,bdi.x)
	regLBF<-lmBF(abv_diff~bdi.x, data = df.tmp)

#	mean and sd for affect value of first event when positive
	mean(data.pos$s1AffectValue)
	sd(data.pos$s1AffectValue)

#	mean and sd for Affect value of second event when negative
	mean(data.pos$s2AffectValue)
	sd(data.pos$s2AffectValue)

#	mean and sd for affect value of first event when negative
	mean(data.neg$s1AffectValue)
	sd(data.neg$s1AffectValue)

#	mean and sd for Affect value of second event when positive
	mean(data.neg$s2AffectValue)
	sd(data.neg$s2AffectValue)

#	mean and sd for distance between two numbers
	mean(data.all$distance)
	sd(data.all$distance)

#	mean and sd for RT to scenario
	mean(data.all$scenarioRT)
	sd(data.all$scenarioRT)

#	mean and sd for RT to midPoint
	mean(data.all$midPointRT)
	sd(data.all$midPointRT)

#	mean and sd for deadline
	mean(data.all$deadline)
	sd(data.all$deadline)


#	percentage respond "above" for each midpoint distance
	midBias.pos<-tapply(data.pos$above, data.pos$midDist, mean)
	midBias.neg<-tapply(data.neg$above, data.neg$midDist, mean)
	midBias.vals<-tapply(data.neg$midDist, data.neg$midDist, mean)


#	percentage of correct reponses for each midpoint distance
	correct.pos<-tapply(data.pos$correct, data.pos$midDist, mean)
	correct.neg<-tapply(data.neg$correct, data.neg$midDist, mean)
	correct.vals<-tapply(data.neg$midDist, data.neg$midDist, mean)

#	plot bias by midpoint distance for positive and negative on same graph

	plot(midBias.vals,midBias.pos,pch=19, col="black", xlab= "Midpoint Distance", ylab="Proportion of Above Responses", xlim=c(-4, 4), ylim=c(0.0, 1.0))
	points(midBias.vals,midBias.neg,col="black")
	legend(x=2, y=0.15, c("neg right", "neg left"), pch=c(19,1))
		Neg_Right_lm=lm(midBias.pos ~ midBias.vals)
		Neg_Left_lm=lm(midBias.neg ~ midBias.vals)
	abline(Neg_Right_lm)
	abline(Neg_Left_lm)
	abline(h=.5)
	dev.copy(pdf,'combined.bias.by.dist.pdf')
	dev.off()

	df.tmp <- data.frame(midBias.pos,midBias.neg,midBias.vals)
	Neg_Right_lmBF=lmBF(midBias.pos ~ midBias.vals, data=df.tmp)
	Neg_Left_lmBF=lmBF(midBias.neg ~ midBias.vals, data=df.tmp)



# testing difference in slope b/t neg left and neg right (midpoint bias)

nRight.se<-summary(Neg_Right_lm)$coef[[4]]
nLeft.se<-summary(Neg_Left_lm)$coef[[4]]
nRight.b<-summary(Neg_Right_lm)$coef[[2]]
nLeft.b<-summary(Neg_Left_lm)$coef[[2]]

b_RvL<-(nRight.b-nLeft.b)/sqrt(nRight.se^2+nLeft.se^2)
b_BF <- 1/exp(ttest.tstat(t=b_RvL, n1=length(midBias.vals), n2=length(midBias.vals), rscale = 0.707)[['bf']])


# testing difference in intercept b/t neg left and neg right (midpoint bias)
nRight.i<-summary(Neg_Right_lm)$coef[[1]]
nLeft.i<-summary(Neg_Left_lm)$coef[[1]]
nRight.ste<-summary(Neg_Right_lm)$coef[[3]]
nLeft.ste<-summary(Neg_Left_lm)$coef[[3]]

i_RvL<-(nRight.i-nLeft.i)/sqrt(nRight.ste^2+nLeft.ste^2)
i_BF <- exp(ttest.tstat(t=i_RvL, n1=length(midBias.vals), n2=length(midBias.vals), rscale = 0.707)[['bf']])

ib_DF <- 2*length(midBias.vals) - 4

## test relation between bdi and rt
rtBdi.dat <- ddply(data.all, .(sn), summarize, mRT = mean(midPointRT), mBdi = mean(bdi))
rtBdi.lm <- with (rtBdi.dat, lm(mRT~mBdi))
rtBdi.lm.BF <- lmBF(mRT~mBdi, data=rtBdi.dat)

sink("regression.txt")
print(t3000Prop)
print(RtThreshold)
print(snAll)
print(snIncluded)
print(snRemoved)
print(RtRemoved)

print(summary(reg))
print(summary(regL))
print(regLBF)
print("regL intercept BayesFactor")
print(1/exp(ttest.tstat(t=summary(regL)[["coefficients"]][, "t value"][[1]], n1=length(predict(regL)), rscale = 0.707)[['bf']]))
print("regL slope BayesFactor")
print(exp(ttest.tstat(t=summary(regL)[["coefficients"]][, "t value"][[2]], n1=length(predict(regL)), rscale = 0.707)[['bf']]))
print(summary(Neg_Right_lm))
print(Neg_Right_lmBF)
print(summary(Neg_Left_lm))
print(Neg_Left_lmBF)
print(b_RvL)
print(paste("DF = ", ib_DF))
print(b_BF)
print(i_RvL)
print(paste("DF = ", ib_DF))
print(i_BF)

print(summary(rtBdi.lm))
print(1/rtBdi.lm.BF)
sink(NULL)




#	average percent of responding above for both pos and negative events
	AvgAboveP<-mean(data.pos$above)
	AvgAboveN<-mean(data.neg$above)




fake.dt<- read.table("fake.dt.txt", header=T, sep="\t", quote="?")
plot(fake.dt$dist, fake.dt$prop,xlab= "Midpoint Distance", ylab="Proportion of Above Responses")
dev.copy(pdf,'no bias.pdf')
dev.off()
