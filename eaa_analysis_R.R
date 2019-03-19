##############install packages########
# install.packages("ggplot2")
library("ggplot2")

# installed.packages("car")
library('car')

# install.packages('psych')
library(psych)

# install.packages("ez")
library(ez)

# multcomp package
# install.packages("multcomp")
library(multcomp)

# install.packages('nlme')
library(nlme)

# install.packages('lme4')
library(lme4)

# install.packages('agricolae')
library(agricolae)

# install.packages("cocor")
library(cocor)


##### Load data ####
path = "D:/Ruonan/Projects in the lab/Ellen Ambig Avers/Data"
setwd(path)

# eaatb = read.csv("D:/Ruonan/Projects in the lab/Ellen Ambig Avers/Data/feedback_analysis_all_09052018.csv", header = TRUE)
# eaatb = read.csv("feedback_analysis_all_09052018.csv", header = TRUE)

load("feedback_all_03132019.rda")
eaatb = all

# factorize
eaatb$id <- as.factor(eaatb$id)
eaatb$is_post <- as.factor(eaatb$is_post)
eaatb$cond <- as.factor(eaatb$cond)

eaatb <- eaatb[eaatb$is_excluded == 0,]
eaatb <- eaatb[eaatb$is_excluded == 0 & eaatb$is_excluded_mb == 0, ]

eaatbpre = eaatb[eaatb$is_post == 0,]
eaatbpost = eaatb[eaatb$is_post == 1,]

# only include AC NC participants
eaatbpre = eaatb[eaatb$is_post == 0 & eaatb$cond != 0,]
eaatbpost = eaatb[eaatb$is_post == 1 & eaatb$cond != 0,]


############Local function#################################################
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
################# descriptive statistics ####

# Pre intervention by group
describeBy(eaatbpre, group = eaatbpre$cond)
describeBy(eaatbpre$ep_score, group = eaatbpre$cond)
describe(eaatbpre)
t.test(eaatbpre$a,mu=1)
t.test(eaatbpre$a_r50, mu=0)
t.test(eaatbpre$r,mu=0.75)

t.test(eaatbpre$beta_t,mu=0)

# Post intervention by group
describeBy(eaatbpost, group = eaatbpost$cond)
describe(eaatbpost)

t.test(eaatbpost$a[eaatbpost$cond==1],mu=1)
t.test(eaatbpost$a[eaatbpost$cond==2],mu=1)

t.test(eaatbpost$r,mu=0.75)

test0 <- t.test(eaatbpost$a_increase[eaatbpost$cond==0],mu=0)
test1 <- t.test(eaatbpost$a_increase[eaatbpost$cond==1],mu=0)
test2 <- t.test(eaatbpost$a_increase[eaatbpost$cond==2],mu=0)
p = c(test0$p.value, test1$p.value, test2$p.value)

p.adjust(p, 
         method = c("bonferroni"),
         n = length(p))


test0 <- t.test(eaatbpost$r_increase[eaatbpost$cond==0],mu=0)
test1 <- t.test(eaatbpost$r_increase[eaatbpost$cond==1],mu=0)
test2 <- t.test(eaatbpost$r_increase[eaatbpost$cond==2],mu=0)
p = c(test0$p.value, test1$p.value, test2$p.value)

p.adjust(p, 
         method = c("holm"),
         n = length(p))

############# test difference between ambiguity-risk correlation for ACNC (Fisher's z), between pre and post #####
preACNC = eaatb[(eaatb$cond == 1 | eaatb$cond == 2) & eaatb$is_post==0 ,]
postACNC = eaatb[(eaatb$cond == 1 | eaatb$cond == 2) & eaatb$is_post==1 ,]
postAC = eaatb[eaatb$cond == 1 & eaatb$is_post==1,]
postNC = eaatb[eaatb$cond == 2 & eaatb$is_post==1,]

cocordata <- data.frame("pre_a_r50"=preACNC$a_r50, "pre_r"=preACNC$r,
                        "post_a_r50"=postACNC$a_r50, "post_r"=postACNC$r)

cocor(~pre_a_r50 + pre_r | post_a_r50 + post_r, cocordata,
      alternative ="two.sided", test="all",
      alpha = 0.05, return.htest = FALSE)

ggplot(postACNC, aes(x=a_r50_increase, y=r_increase))+geom_point()
cor.test(postACNC$a_r50_increase, postACNC$r_increase, method = c("pearson", "kendall", "spearman"))

ggplot(postAC, aes(x=ep_score, y=a_r50_increase))+geom_point()
cor.test(postAC$ep_score, postAC$a_r50_increase, method = c("pearson", "kendall", "spearman"))

ggplot(postNC, aes(x=ep_score, y=a_r50_increase))+geom_point()
cor.test(postNC$ep_score, postNC$a_r50_increase, method = c("pearson", "kendall", "spearman"))

# Mann-Whitney test, reference: http://rcompanion.org/handbook/F_04.html ####
aACNC = eaatb[(eaatb$cond == 1 | eaatb$cond == 2) & eaatb$is_post==1 & eaatb$is_excluded==0,]
wil = wilcox.test(a_r50~cond, data = aACNC, paired=FALSE)
wil$statistic
wil$p.value
qnorm(wil$p.value)

wilcox.test(a_increase~cond, data = aACNC)

# post
aAC = eaatb[eaatb$cond == 1 & eaatb$is_post==1 & eaatb$is_excluded==0,]
wil = wilcox.test(aAC$a, mu = 1, alternative = "two.sided")
wil$p.value
qnorm(wil$p.value)

# pre all
wil = wilcox.test(eaatb$a[eaatb$is_post==0 & eaatb$is_excluded==0],
            mu = 1, alternative = "two.sided")
wil$statistic
wil$p.value
qnorm(wil$p.value)

# pre all model based ambiguity
wil = wilcox.test(eaatb$a_r50[eaatb$is_post==0 & eaatb$is_excluded==0],
                  mu = 0, alternative = "two.sided")
wil$statistic
wil$p.value
qnorm(wil$p.value)

# NC
aNC = eaatb[eaatb$cond == 2 & eaatb$is_post==1 & eaatb$is_excluded==0,]
wil = wilcox.test(aNC$a, mu = 1, alternative = "two.sided")
wil$p.value
qnorm(wil$p.value)


# correlation ####

# correlation between model based and model free att
ggplot(eaatbpre, aes(x=r, y=alpha_t))+geom_point()
ggplot(eaatbpre, aes(x=r50, y=r))+geom_point()
ggplot(eaatbpre, aes(x=r50, y=alpha_t))+geom_point()
ggplot(eaatbpre, aes(x=a_r50, y=beta_t))+geom_point()
ggplot(eaatbpost, aes(x=r, y=alpha_t))+geom_point()
ggplot(eaatbpost, aes(x=a, y=beta_t))+geom_point()

ggplot(eaatbpre, aes(x=r, y=a))+geom_point()

ggplot(eaatbpre, aes(x=r, y=a_r50))+geom_point()
ggplot(eaatbpre, aes(x=a_r50, y=r))+geom_point()
cor.test(eaatbpre$r, eaatbpre$a_r50, method = c("pearson", "kendall", "spearman"))

ggplot(eaatbpre, aes(x=alpha_t, y=a_r50))+geom_point()
cor.test(eaatbpre$alpha_t, eaatbpre$a_r50, method = c("pearson", "kendall", "spearman"))

ggplot(eaatbpre[eaatbpre$cond!=0, ], aes(x=a_r50_increase, y= r_increase))+geom_point()
cor.test(eaatbpre$r_increase[eaatbpre$cond!=0], eaatbpre$a_r50_increase[eaatbpre$cond!=0], method = c("pearson", "kendall", "spearman"))

ggplot(eaatbpre, aes(x=alpha_t, y=beta_t))+geom_point()
ggplot(eaatbpost, aes(x=r, y=a))+geom_point()

ggplot(eaatbpost, aes(x=r, y=a_r50))+geom_point()
ggplot(eaatbpost, aes(x=a_r50, y=r))+geom_point()
cor.test(eaatbpost$r, eaatbpost$a_r50, method = c("pearson", "kendall", "spearman"))

ggplot(eaatbpost, aes(x=alpha_t, y=beta_t))+geom_point()

qplot(eaatbpre$r50, geom="histogram") 
qplot(eaatbpre$a_r50, geom="histogram")

qplot(eaatbpost$r50, geom="histogram") 
qplot(eaatbpost$a_r50, geom="histogram")

######### anova of group on ambiguity aversion and risk aversion #####
################# repeated measure anova using ez package

# ezanova, repeated meausre, unbalanced design
# because design is unbalanced, need to consider type of SSm no consensus, but most statistian suggest type III (default in SPSS)

# There is NO consensus on which type of SS should be used for unbalanced designs, but
# most statisticians generally recommend type III, which is the default in most software
# packages such as SAS, SPSS, JMP, Minitab, Stata, Statista, Systat, and Unistat while R,
# S-Plus, Genstat, and Mathematica use type I. However, Langsrud (2003) argues that Type
# II is preferable considering the power of types II and III. (http://www.utstat.utoronto.ca/reid/sta442f/2009/typeSS.pdf)

# ambig choice
ac_anova = ezANOVA(data=eaatb, 
        dv = a,
        wid = .(id),
        within = .(is_post),
        between = .(cond),
        type = 3,
        detailed = TRUE,
        return_aov = TRUE
        )
ac_anova

# model free ambig att
ac_anova = ezANOVA(data=eaatb, 
                   dv = a_r50,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# model free ambig att
ac_anova = ezANOVA(data=eaatb, 
                   dv = a50_a24,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

ac_anova = ezANOVA(data=eaatb, 
                   dv = a74_a24,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# model free ambig att (adiff)
ac_anova = ezANOVA(data=eaatb, 
                   dv = adiff,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# model based ambig att
ac_anova = ezANOVA(data=eaatb, 
                   dv = beta_t,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# summary(glht(ac_anova$aov, linfct = mcp(cond = "Tukey"), alternative = c("two.sided") )) # not working
pairwise.t.test(eaatb$a, interaction(eaatb$cond, eaatb$is_post),
                paired=F, p.adjust.method="bonferroni") 

# model free risk
rc_anova = ezANOVA(data=eaatb, 
                   dv = r,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
rc_anova

# model based risk
rc_anova = ezANOVA(data=eaatb, 
                   dv = beta_t,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
rc_anova

# plot model free a-r50
tbplot <- data_summary(eaatb,varname="a_r50",groupnames=c("cond","is_post"))

ggplot(data=tbplot,aes(x=cond, y=a_r50, fill=is_post)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=a_r50-sd, ymax=a_r50+sd), width=0.2, position=position_dodge(0.9))


# plot model free adiff
tbplot <- data_summary(eaatb,varname="adiff",groupnames=c("cond","is_post"))

ggplot(data=tbplot,aes(x=cond, y=adiff, fill=is_post)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=adiff-sd, ymax=adiff+sd), width=0.2, position=position_dodge(0.9))


# plot model based risk
tbplot <- data_summary(eaatb,varname="alpha_t",groupnames=c("cond","is_post"))

ggplot(data=tbplot,aes(x=cond, y=alpha_t, fill=is_post)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=alpha_t-sd, ymax=alpha_t+sd), width=0.2, position=position_dodge(0.9))



ac_anova = ezANOVA(data=eaatbpre, 
                   dv = a,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

ac_anova = ezANOVA(data=eaatbpre, 
                   dv = a50_a24,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova


ac_anova = ezANOVA(data=eaatbpre, 
                   dv = r,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

ac_anova = ezANOVA(data=eaatbpre, 
                   dv = a_increase,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)

ac_anova = ezANOVA(data=eaatbpre, 
                   dv = a_r50_increase,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)

ac_anova = ezANOVA(data=eaatbpre, 
                   dv = a_r50,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)

ac_anova = ezANOVA(data=eaatbpre, 
                   dv = adiff_increase,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)

ac_anova = ezANOVA(data=eaatbpre, 
                   dv = r_increase,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)

ac_anova = ezANOVA(data=eaatbpre, 
                   dv = alpha_t_increase,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)

ep_anova = ezANOVA(data=eaatbpre[!is.nan(eaatbpre$ep_score),], 
                   dv = ep_score,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ep_anova
TukeyHSD(ep_anova$aov)

kruskal.test(ep_score ~ cond, data=eaatbpre[!is.nan(eaatbpre$ep_score),]) 

# try post hoc test with repeated measure ANOVA, by using generalized linear model


a1 <- aov(a ~ cond + is_post + cond*is_post +  Error(id/is_post), data = eaatb)
summary(a1)

TukeyHSD(a1)

lme_ambig = lm(a ~ is_post + cond + is_post*cond + (1|id) , data = eaatb) 
residues = resid(lme_ambig)
plot(eaatb$cond,resid)

lme_ambig = lmer(a ~ is_post + cond + is_post*cond + (1|id) , data = eaatb)

lme_ambig = lmer(a ~ is_post*cond + (1|id) + (1|is_post:id), data = eaatb) # does not work
lme_ambig = lmer(a ~ is_post*cond + (1|id) + (is_post|id), data = eaatb) # does not work
lme_ambig = lmer(a ~ is_post*cond + (1|id), data = eaatb)


lme_ambig = lme(a ~ is_post + cond + is_post*cond, 
                random=list(id=pdBlocked(list(~1, pdIdent(~is_post-1)))),
                method="ML", data = eaatb)

lme_ambig = lme(a ~ is_post + cond + is_post*cond, 
                random= ~ 1|id + 1|id/is_post,
                method="ML", data = eaatb)

resi = residuals(lme_ambig)
plot(eaatb$cond, resi)

lme_ambig = lme(a_increase ~  cond , 
                random= ~ 1|id,
                method="ML", data = eaatbpre)

Anova(lme_ambig,type=3,test.statistic="F")
Anova(lme_ambig,test.statistic="F")
multicompare <- glht(lme_ambig, linfct=mcp(cond  ="Tukey"))
summary(multicompare)



# model based
ac_anova = ezANOVA(data=eaatb, 
                   dv = beta_t,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

ac_anova = ezANOVA(data=eaatb, 
                   dv = beta_t_increase,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

rc_anova = ezANOVA(data=eaatb, 
                   dv = r,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
rc_anova

rc_anova = ezANOVA(data=eaatb, 
                   dv = alpha_t,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
rc_anova

rc_anova = ezANOVA(data=eaatb, 
                   dv = alpha_t_increase,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
rc_anova


# posthoc TukeyHSD
TukeyHSD(ac_anova$aov,"cond",data=eaatb)

#####  ANOVA by lme ####
a1 <- aov(a_increase~cond, data = eaatb[eaatb$is_post == 1,])
summary(a1)
summary(glht(a1, linfct = mcp(cond = "Tukey"), alternative = c("two.sided") ))
TukeyHSD(a1)

mean(eaatb$a_increase[eaatb$cond == 1])-mean(eaatb$a_increase[eaatb$cond == 2])

r1 <- aov(r_increase~cond, data = eaatb[eaatb$is_post == 1,])
summary(r1)
TukeyHSD(r1)

ep <- aov(ep_score~cond, data = eaatbpre)
summary(ep)
Anova(ep, type=3)
TukeyHSD(ep)

lme_ep = lmer(ep_score ~ cond + (1) , data = eaatbpre)

eaatbep = eaatbpre[is.nan(eaatbpre$ep_score) == FALSE,]
sum(is.nan(eaatbpre$ep_score) == TRUE)
ep_anova = ezANOVA(data=eaatbep, 
                   dv = ep_score,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ep_anova

#################### correlation between 





################ ANOVA ######## 
# exclude extreme model fitting values
# eaatb2 already excludes them
eaatb2 = read.csv("D:/Ruonan/Projects in the lab/Ellen Ambig Avers/Data/feedback_analysis_excludeLargeAtt_08012018.csv", header = TRUE)
eaatb2 <- eaatb2[eaatb2$is_excluded == 0 & eaatb2$is_excluded_mb == 0,]

# factorize
eaatb2$id <- as.factor(eaatb2$id)
eaatb2$is_post <- as.factor(eaatb2$is_post)
eaatb2$cond <- as.factor(eaatb2$cond)

# anova of ambiguity att
# eaatb2beta = eaatb2[is.na(eaatb2$beta_t_increase) == 0,]
beta_anova = ezANOVA(data=eaatb2, 
                     dv = beta_t,
                     wid = .(id),
                     within = .(is_post),
                     between = .(cond),
                     type = 3,
                     detailed = TRUE,
                     return_aov = TRUE
)
beta_anova


alpha_anova = ezANOVA(data=eaatb2, 
                      dv = alpha_t,
                      wid = .(id),
                      within = .(is_post),
                      between = .(cond),
                      type = 3,
                      detailed = TRUE,
                      return_aov = TRUE
)
alpha_anova

##### Post hoc Tukey on reduction of aversion
beta1 <- aov(beta_t_increase~cond, data = eaatb2beta)
summary(beta1)
TukeyHSD(beta1)


###### descriptive stat by group, t test
eaatb2pre = eaatb2[eaatb2$is_post == 0,]
describeBy(eaatb2pre, group = eaatb2pre$cond)
describe(eaatb2pre)
t.test(eaatb2betapre$beta_t,mu=0)

describeBy(eaatb2pre, group = eaatb2pre$cond)
describe(eaatb2alphapre)

eaatb2post = eaatb2[eaatb2$is_post == 1,]
describeBy(eaatb2post, group = eaatb2post$cond)
describe(eaatb2post)

t.test(eaatb2betapost$beta_t[eaatb2betapost$cond==1],mu=0)
t.test(eaatb2betapost$beta_t[eaatb2betapost$cond==2],mu=0)

eaatb2alphapost = eaatb2alpha[eaatb2alpha$is_post == 1,]
describeBy(eaatb2alphapost, group = eaatb2alphapost$cond)
describe(eaatb2alphapost)
##################################################################
