datafilename <- 'D:/Ruonan/Projects in the lab/Ellen Ambig Avers/Data/all subjects 12052016.csv'
mydata <- read.csv(datafilename,na.strings = "NA",header = TRUE) #save it as a data frame

#converting these variables to factors since R recognizes them as being numeric by default
# gender: 1 male, 0 female
# phase: 0 pre-intervention, 1 post-intervention
mydata<-within(mydata, {cond<-factor(cond) 
gender<-factor(gender) 
subj<-factor(subj)
phase<-factor(phase)})


# compute the change in ambiguity choices
mydata$ambigChg<-NA
mydata$ambigChg[which(mydata$phase==0)] <- mydata$ambig[which(mydata$phase==1)]-
  mydata$ambig[which(mydata$phase==0)]
mydata$ambigChg[which(mydata$phase==1)] <- mydata$ambig[which(mydata$phase==1)]-
  mydata$ambig[which(mydata$phase==0)]

mydata$riskChg<-NA
mydata$riskChg[which(mydata$phase==0)] <- mydata$risk[which(mydata$phase==1)]-
  mydata$risk[which(mydata$phase==0)]
mydata$riskChg[which(mydata$phase==1)] <- mydata$risk[which(mydata$phase==1)]-
  mydata$risk[which(mydata$phase==0)]

# dummy coding conditions
mydata$cond1[mydata$cond==0]<-1
mydata$cond1[mydata$cond==1]<-0
mydata$cond1[mydata$cond==2]<-0

mydata$cond2[mydata$cond==0]<-0
mydata$cond2[mydata$cond==1]<-0
mydata$cond2[mydata$cond==2]<-1


####install 
install.packages("ez") #I use the ez package because this makes it easy to use TypeIII SS, which you'll eventually need to use wih unbalanced data
install.packages("lsr") #for calculating partial eta squared
install.packages("psych")
library(ez)
library(lsr)
library(ggplot2)
library(psych)

?ezANOVA #examine it

#manipulation check
mscore<-ezANOVA(mydata[which(mydata$phase==0 & is.na(mydata$ep_score)!=TRUE),],ep_score,subj,between=cond,type=3, detailed=T,return_aov=TRUE)
mscore
posthocscore<-TukeyHSD(mscore,cond,conf.level=0.95)

contrasts(mydata$cond)
contrasts(mydata$cond)<-matrix(c(1,0,0,0,0,1),nrow=3,ncol=2)
contrasts(mydata$cond)
mscoreCtr<-lm(ep_score~cond1+cond2,mydata[which(mydata$phase==0),])
summary(mscoreCtr)

# standardized beta:
mscoreCtr<-lm(scale(ep_score)~scale(cond1)+scale(cond2),mydata[which(mydata$phase==0),])
summary(mscoreCtr)

mscore1<-ezANOVA(mydata[which(mydata$phase==0 & mydata$cond!=0 & is.na(mydata$ep_score)!=TRUE),],ep_score,subj,between=cond,type=3, detailed=T)
mscore1 # comparisons only include AN and NC

describeBy(mydata$ep_score[mydata$phase ==0], mydata$cond[mydata$phase ==0])

# Mixed ANOVA, within-phase, between-conditions
mrisk <- ezANOVA(mydata, risk, subj,within=.(phase),between=cond, type = 3, detailed=T)
mrisk

contrasts(mydata$cond)
mriskCtr <- lm(riskChg~cond,mydata[which(mydata$phase==0),])
summary(mriskCtr)

# Mixed ANOVA on ambig att, within-phase, between-conditions
mambig<- ezANOVA(mydata, ambig, subj,within=phase,between=cond, type = 3, detailed=T)
mambig

describeBy(mydata$ambig[mydata$cond ==2], mydata$phase[mydata$cond ==2])
describeBy(mydata$ambig, mydata$phase)

  #break down into simple effect
cambig<-ezANOVA(mydata[mydata$cond ==2,], ambig, subj,within=phase, type = 3, detailed=T)
cambig

# pre-intervention ambiguity attitude
mambig<-ezANOVA(mydata[mydata$phase==0,], ambig, subj,between=cond, type = 3, detailed=T)
mambig

contrasts(mydata$cond)
m3 <- lm(ambigChg~cond1+cond2,mydata[which(mydata$phase==0),])
summary(m3)

m3 <- lm(scale(ambigChg)~scale(cond1)+scale(cond2),mydata[which(mydata$phase==0),])
summary(m3)

describeBy(mydata$ambigChg[mydata$phase ==0], mydata$cond[mydata$phase ==0])

# regression plot
ggplot(mydata[which(mydata$phase==0),], aes(x=ambig, y=ambigChg))+
  geom_point(shape=1)+    # Use hollow circles
  geom_smooth(method=lm)  # Add linear regression line  (by default includes 95% confidence region)

ggplot(mydata[(mydata$phase==0 & mydata$cond!=0),], aes(x=ambig, y=ambigChg))+
  geom_point(shape=1)+    # Use hollow circles
  geom_smooth(method=lm)  # Add linear regression line  (by default includes 95% confidence region)

# correlation
# ambig choices change, pre-intervention ambig choices, for AC and NC only
cor.test(mydata$ambig[which(mydata$phase==0& mydata$cond!=0)],mydata$ambigChg[which(mydata$phase==0& mydata$cond!=0)]) 

summary(lm_ambigChg<-lm(scale(ambigChg)~scale(ambig),mydata[which(mydata$phase==0 & mydata$cond!=0),])) 


plot(mydata$ambig[which(mydata$phase==0& mydata$cond!=0)],lm_ambigChg$residuals)
plot(mydata$ambig[which(mydata$phase==0& mydata$cond!=0)],lm_ambigChg$fitted.values)

mydata$asinter= mydata$ambig*mydata$ep_score #interaction term

summary(lm_ambigChgFull <-lm(ambigChg~ambig+ep_score + asinter,mydata[which(mydata$phase==0 & mydata$cond!=0),]))
plot(mydata$ambig[which(mydata$phase==0& mydata$cond!=0)],lm_ambigChgFull$residuals)


# ambig choices change, pre-intervention ambig att model based, for AC and NC only
# subj 2506 beta=-2.4x10^7, subj2563 betw=15.024
cor.test(mydata$beta[which(mydata$phase==0& mydata$cond!=0)],mydata$ambigChg[which(mydata$phase==0& mydata$cond!=0)]) 

summary(lm_ambigChg<-lm(ambigChg~beta,mydata[which(mydata$phase==0 & mydata$cond!=0),]))  # beta non-significant
summary(lm_ambigChg<-lm(ambigChg~beta + ep_score,mydata[which(mydata$phase==0 & mydata$cond!=0),]))  # beta significant after controlling ep_score


plot(mydata$beta[which(mydata$phase==0& mydata$cond!=0)],lm_ambigChg$residuals)
plot(mydata$beta[which(mydata$phase==0& mydata$cond!=0)],lm_ambigChg$fitted.values)

# risk choices change, pre-intervention ambig/risk, for AC and NC only
summary(lm_riskChg<-lm(ambigChg~ambig,mydata[which(mydata$phase==0 & mydata$cond!=0),])) 
summary(lm_riskChg<-lm(ambigChg~risk,mydata[which(mydata$phase==0 & mydata$cond!=0),]))

cor.test(mydata$risk[which(mydata$phase==0& mydata$cond!=0)],mydata$ambig[which(mydata$phase==0& mydata$cond!=0)])
cor.test(mydata$alpha[which(mydata$phase==0& mydata$cond!=0)],mydata$beta[which(mydata$phase==0& mydata$cond!=0)])