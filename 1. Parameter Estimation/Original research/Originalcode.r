Geom = read.table(file.choose(), header=T, sep=",")
Geom = read.table("AnglesDataF98-new.csv", header=T, sep=",")
attach(Geom)

adj.count=Skill.count
adj.count[Condition=="Reason" & Qtype=="answer"] = Skill.count[Condition=="Reason"& Qtype=="answer"]*2-1
adj.count[Condition=="Reason" & Qtype=="reason"] = Skill.count[Condition=="Reason" & Qtype=="reason"]*2

new.data=data.frame(Student, Condition, Qtype, Success, Skill,   adj.count)
detach(Geom); attach(new.data)

log.model = glm(Success~Student+Skill+(adj.count-1):Skill:Condition-1, family=binomial(), data=new.data)

#####plots########
trial=1:141
error.rate.ans = rep(0,141)
for(i in 1:141){
	error.rate.ans[i] = 1- mean(Success[adj.count==i & Condition=="Answer"])
	}
error.rate.reas = rep(0,141)
for(i in 1:141){
	error.rate.reas[i] = 1- mean(Success[adj.count==i & Condition=="Reason"])
	}
plot(trial, error.rate.ans, type="l", main="Error Rate for all Students and all Knowledge Components", xlab="Opportunity Number", ylab="Error Rate"); points(trial, error.rate.ans, cex=0.8, pch=19)
points(trial, error.rate.reas, type="l", col="red"); points(trial, error.rate.reas, cex=0.8, pch=19, col="red")
legend(0,1,c("Answer Only", "Explanation"), col=c("black", "red"), lty=c(1,1), pch=c(19,19))


###Errors before Hints 
errors.ans=rep(0,141)
for(i in 1:141){
	errors.ans[i] = mean(Geom$N.Errors.BEFORE.1st.hint[ adj.count==i & Condition=="Answer"])
	}

errors.rea=rep(0,141)
for(i in 1:141){
	errors.rea[i] = mean(Geom$N.Errors.BEFORE.1st.hint[ adj.count==i & Condition=="Reason"])
	}
##making a plot
plot(trial, errors.ans, type="l", main="Mean Number of Errors Before First Hint", xlab="Opportunity Number", ylab="Number of Errors", ylim=c(0,7)); points(trial, errors.ans, cex=0.8, pch=19)
points(trial, errors.rea, type="l", col="red"); points(trial, errors.rea, cex=0.8, pch=19, col="red")
legend(0,7,c("Answer Only", "Explanation"), col=c("black", "red"), lty=c(1,1), pch=c(19,19))

err.rea.ans=rep(0,141)
for(i in 1:141){
	err.rea.ans[i] = mean(Geom$N.Errors.BEFORE.1st.hint[ adj.count==i & Condition=="Reason" & Qtype=="answer"])
	}
err.rea.rea=rep(0,141)	
for(i in 1:141){
	err.rea.rea[i] = mean(Geom$N.Errors.BEFORE.1st.hint[ adj.count==i & Condition=="Reason" & Qtype=="reason"])
	}
##making a plot
plot(trial, errors.ans, type="l", main="Mean Number of Errors Before First Hint", xlab="Opportunity Number", ylab="Number of Errors", ylim=c(0,7)); points(trial, errors.ans, cex=0.8, pch=19)
points(trial[err.rea.ans!="NaN"], err.rea.ans[err.rea.ans!="NaN"], type="l", col="red"); points(trial, err.rea.ans, cex=0.8, pch=19, col="red")
points(trial[err.rea.rea!="NaN"], err.rea.rea[err.rea.rea!="NaN"], type="l", col="blue"); points(trial, err.rea.rea, cex=0.8, pch=19, col="blue")
legend(0,7,c("Answer Only", "Explanation - Reason Components", "Explanation-Answer Components"), col=c("black", "blue", "red"), lty=c(1,1,1), pch=c(19,19,19))




################## Answer Only Questions #################
ans.only = Geom[Geom$Qtype=="answer",]
attach(ans.only)

#trial=1:141
#suc.rate.ans=rep(0,141)
#for(i in 1:141){
#	suc.rate.ans[i]=mean(Success[Skill.count==i & Condition=="Answer"])
#	}

#suc.rate.rea=rep(0,141)
#for(i in 1:141){
#	suc.rate.rea[i]=mean(Success[Skill.count==i & Condition=="Reason"])
#	}

#R.err.rate=1-suc.rate.rea
#A.err.rate=1-suc.rate.ans

#plot(trial,R.err.rate, xlab="Trial number", ylab="error rate", type="l", main="Average Error Rate for all Knowledge Components")
#points(trial, A.err.rate, type="l", col="red")
#detach(ans.only)

#reas.only = Geom[Geom$Qtype=="reason",]
#attach(reas.only)
#trial=1:64
#suc.rate.reas.o=rep(0,64)
#for(i in 1:64){
#	suc.rate.reas.o[i]=mean(Success[Skill.count==i])
#	}
	
#err.rate.R.only = 1-suc.rate.reas.o
#plot(trial,err.rate.R.only, type="l")
#points(1:141, R.err.rate, type="l", col="red")
#points(1:141, A.err.rate, type="l", col="purple")





####fit model for errors before hints
#out=lm(log(N.Errors.BEFORE.1st.hint+1)~Student+Skill+(Skill.count-1):Skill:Condition-1)  ##I think this is the good one.

#out.2 = lm(log(N.Errors.BEFORE.1st.hint+1)~Student+Skill+(Skill.count-1))

#out.3=lm(log(N.Errors.BEFORE.1st.hint+1)~Student+Skill:(Skill.count-1))

#E=log(N.Errors.BEFORE.1st.hint+1)
#pred=predict(out)
#res=residuals(out)
#plot(pred,res)


##############LOGISTIC REGRESSION#####################
#stuff=glm(Success~Student+Skill+(Skill.count-1):Skill:Condition-1, family=binomial())

#beta=coef(stuff)
#slope.reas=beta[86:108]
#slope.ans=beta[63:85]

#############Adjust Skill.count########################
temp=Skill.count
adj.count=Skill.count
adj.count[Condition=="Reason"]=temp[Condition=="Reason"]*2-1
adj.ans.only=data.frame(Student, Condition, Qtype, Success, Skill,   adj.count)
detach(ans.only); #attach(adj.ans.only)

log.model.basic = glm(Success~Student+Skill+(adj.count-1):Skill -1, family=binomial(), data=adj.ans.only)
aic=summary(log.model.basic)$aic
n=length(coef(log.model.basic))
log.like=(aic-2*n)/-2

log.model = glm(Success~Student+Skill+(adj.count-1):Skill:Condition-1, family=binomial(), data=adj.ans.only)

log.model.del = glm(Success~Student+Skill+(adj.count-1):Skill + (adj.count-1):Condition-1, family=binomial(), data=adj.ans.only)

###################Final Adjusted Data set #################
#Reason only condition counts reason steps as practicing components
#excluding one component because of low data

final.data=data.frame(adj.ans.only[adj.ans.only$Skill!="(TRIANGLE-REMOTE-INTERIOR-ANGLE ANSWER)",])
attach(final.data)
log.model.del2 = glm(Success~Student+Skill+(adj.count-1):Skill + (adj.count-1):Condition-1, family=binomial(), data=final.data)

log.model = glm(Success~Student+Skill+(adj.count-1):Skill:Condition-1, family=binomial(), data=final.data)
beta=coef(log.model)
aic=summary(log.model)$aic
n=length(coef(log.model))
log.like=(aic-2*n)/-2

log.model.stu = glm(Success~Student+Skill+(adj.count-1):Student-1, family=binomial(), data=final.data)
gamma=coef(log.model.stu)[62:101]

stu.ans=unique(Student[Condition=="Answer"])
stu.reas=unique(Student[Condition=="Reason"])

gamma.ans=gamma[c(1,3,4,6,8,11,12,14,16,19,20,22,23,24,28,31,32,35,36,38,40)]
gamma.reas=gamma[c(2,5,7,9,10,,13,15,17,18,21,25,26,27,29,30,34,37,39)]
t.test(gamma.reas, gamma.ans)


alpha=coef(log.model.stu)[1:40]
alpha.ans=alpha[c(1,3,4,6,8,11,12,14,16,19,20,22,23,24,28,31,32,35,36,38,40)]
alpha.reas=alpha[c(2,5,7,9,10,,13,15,17,18,21,25,26,27,29,30,34,37,39)]
t.test(alpha.reas, alpha.ans)