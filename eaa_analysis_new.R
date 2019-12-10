##### Install packages ######
# install.packages("ggplot2")
library("ggplot2")

# installed.packages("car")
library('car')

# install.packages("emmeans")
library(emmeans)

# install.packages('psych')
library(psych)

# install.packages("ez")
library(ez)

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

# for sign test
if(!require(psych)){install.packages("psych")}
if(!require(BSDA)){install.packages("BSDA")}
if(!require(DescTools)){install.packages("DescTools")}

##### Local function ######

data_summary <- function(data, varname, groupnames){
  # Function to calculate the mean and the standard error
  # for each group
  #+++++++++++++++++++++++++
  # data : a data frame
  # varname : the name of a column containing the variable
  #to be summariezed
  # groupnames : vector of column names to be used as
  # grouping variables
  
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



data_meanstd <- function(x) {
  # Function to produce summary statistics (mean and +/- sd)
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

data_meanse <- function(x) {
  # Function to produce summary statistics (mean and +/- sd)
  m <- mean(x)
  ymin <- m-sd(x)/sqrt(length(x))
  ymax <- m+sd(x)/sqrt(length(x))
  return(c(y=m,ymin=ymin,ymax=ymax))
}

##### Load and select data ####

path = "D:/Ruonan/Projects in the lab/Ellen Ambig Avers/Data"
setwd(path)

load("feedback_all_1111022019.rda")

write.csv(eaatb,"preprocessed_data_v2.csv")

# rename
# eaatb = all1
View(eaatb)
# Subject list to exclude
exclude_riskOnly = c(1271, 1518, 1690, 1860, 2503, 2506, 2507, 2522, 2527, 2528, 2531,
                     2563, 2570, 2575, 1547, 1689, 1858, 2526, 2534, 2544, 2574, 2579)

exclude_riskAmbig = c(1518, 1860, 2503, 2506, 2507, 2522, 2527, 2531, 2563, 2570, 1271,
                      1547, 1689, 1690, 1696, 1858, 2526, 2528, 2529, 2544, 2571, 2574,
                      2573, 1519, 1559, 1921, 2501, 2543, 2549, 2567)

# select constrained
eaatb <- eaatb[eaatb$is_excluded == 0 & eaatb$is_constrained == 1,]

# select unconstrained
eaatb <- eaatb[eaatb$is_excluded == 0 & eaatb$is_constrained == 0,]

# exclude subjects with bad fittings
eaatb <- eaatb[!is.element(eaatb$id, exclude_riskOnly),]
eaatb <- eaatb[!is.element(eaatb$id, exclude_riskAmbig),]

eaatbpre = eaatb[eaatb$is_post == 0,]
eaatbpost = eaatb[eaatb$is_post == 1,]

# Find subjects with extreme values
id_out_alpha_pre = eaatbpre[eaatbpre$alpha_risk < 0.1070 | eaatbpre$alpha_risk > 2.0987,]$id
id_out_alpha_post = eaatbpost[eaatbpost$alpha_risk < 0.1070 | eaatbpost$alpha_risk > 2.0987,]$id
id_out_alpha = unique(c(id_out_alpha_pre, id_out_alpha_post))
sum(is.element(eaatb$id[eaatb$cond == 2], id_out_alpha))

id_out_alpha_pre = eaatbpre[eaatbpre$alpha < 0.1070 | eaatbpre$alpha > 2.0987,]$id
id_out_alpha_post = eaatbpost[eaatbpost$alpha < 0.1070 | eaatbpost$alpha > 2.0987,]$id
id_out_alpha = unique(c(id_out_alpha_pre, id_out_alpha_post))
sum(is.element(eaatb$id[eaatb$cond == 0], id_out_alpha))

id_out_beta_pre = eaatbpre[eaatbpre$beta < 0.0897 | eaatbpre$beta > 4.1475,]$id
id_out_beta_post = eaatbpost[eaatbpost$beta < 0.0897 | eaatbpost$beta > 4.1475,]$id
id_out_beta = unique(c(id_out_beta_pre, id_out_beta_post))
sum(is.element(eaatb$id[eaatb$cond == 2], id_out_beta))

id_out = unique(c(id_out_alpha, id_out_beta))
sum(is.element(eaatb$id[eaatb$cond == 0], id_out))



##### Descriptive statistics #####
# Pre intervention by group
describeBy(eaatbpre, group = eaatbpre$cond)
des <- describeBy(eaatbpre$a_r50, group = eaatbpre$cond)
print(des, digits = 4)

des <- describeBy(eaatbpre$a, group = eaatbpre$cond)
print(des, digits = 4)

des <- describeBy(eaatbpre$a_increase, group = eaatbpre$cond)
print(des, digits = 4)

des <- describeBy(eaatbpre$a50_a24, group = eaatbpre$cond)
print(des, digits = 4)


des <- describeBy(eaatbpre$a74_a24, group = eaatbpre$cond)
print(des, digits = 4)

des <- describeBy(eaatbpre$a_5in_r50, group = eaatbpre$cond)
print(des, digits = 4)

des <- describeBy(eaatbpre$a_5in_r50_increase, group = eaatbpre$cond)
print(des, digits = 4)

describeBy(eaatbpre$ep_score, group = eaatbpre$cond)
eaatb$gender <- as.factor(eaatb$gender)
describeBy(eaatbpre$gender, group = eaatbpre$cond)
describeBy(eaatbpre$age, group = eaatbpre$cond)

describe(eaatbpre)
des <- describe(eaatbpre$a_r50)
print(des, digits=4)

t.test(eaatbpre$a,mu=1)
t.test(eaatbpre$a_r50, mu=0)
t.test(eaatbpre$a_5in_r50, mu=0)

t.test(eaatbpre$r,mu=0.75)

t.test(eaatbpre$beta_t,mu=0)

# Post intervention by group
describeBy(eaatbpost, group = eaatbpost$cond)
describe(eaatbpost)

des <- describeBy(eaatbpost$a50_a24, group = eaatbpost$cond)
print(des, digits = 4)

des <- describeBy(eaatbpost$a, group = eaatbpost$cond)
print(des, digits = 4)

des <- describeBy(eaatbpost$a74_a24, group = eaatbpost$cond)
print(des, digits = 4)

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


##### Investigate risky choices #####
 
# violin plot
ggplot(eaatb, aes(x = cond, y = r, fill = is_post)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=TRUE) + # not trimed
  # geom_dotplot(binaxis='y', binwidth = 0.035, stackdir='center',position=position_dodge(0.9)) + # add dots
  scale_fill_manual(values = c("gray55", "white")) +
  # stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red", position = position_dodge(0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge((0.9))) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(limits=c(-.15, 1.15), breaks = c(0.0, .25, 0.5, .75, 1.0)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 16, color = "black")) +
  ggtitle("Risky trials choice proportion") + xlab("") + ylab("Choice Proportion") +
  theme(axis.title.y=element_text(size = 12)) +
  theme(axis.title = element_text(size = 14))


ggplot(eaatb, aes(x = cond, y = r50, fill = is_post)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=TRUE) + # not trimed
  # geom_dotplot(binaxis='y', binwidth = 0.035, stackdir='center',position=position_dodge(0.9)) + # add dots
  scale_fill_manual(values = c("gray55", "white")) +
  # stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red", position = position_dodge(0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge((0.9))) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  # scale_y_continuous(limits=c(-.15, 1.15), breaks = c(0.0, .25, 0.5, .75, 1.0)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 16, color = "black")) +
  ggtitle("Risky trials choice proportion") + xlab("") + ylab("Choice Proportion") +
  theme(axis.title.y=element_text(size = 12)) +
  theme(axis.title = element_text(size = 14))

# increase violin plot
ggplot(eaatb[eaatb$is_post == 1,], aes(x = cond, y = r_increase)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=TRUE, fill = "grey75") + # not trimed
  # stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red") +
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  # scale_y_continuous(limits=c(-0.5, 0.75), breaks = c(-.5, -.25, 0.0, .25, 0.5, .75)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black")) +
  ggtitle("Risk choice proportion") + xlab("") + ylab("Choice Proportion") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))

# test
ac_anova = ezANOVA(data=eaatb, 
                   dv = r,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# pre-intervention
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = r,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)

# post-intervention increase
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

# mann-whitney tests

wil <- wilcox.test(x = eaatb$r_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                   y = eaatb$r_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                   paired = FALSE, 
                   conf.level = 0.95)

# wil <- wilcox.test(r_increase~cond, 
#                    data = eaatb[eaatb$is_post==1 & eaatb$cond != 2,], 
#                    paired=FALSE, 
#                    conf.level = 0.95)

wil$statistic
wil$p.value
qnorm(wil$p.value)

# sign test
library(BSDA) # basic statistics and data analysis

sign1<- SIGN.test(x = eaatb$r[eaatb$is_post == 1 & eaatb$cond == 1],
                  y = eaatb$r[eaatb$is_post == 0 & eaatb$cond == 1],
                  alternative = "two.sided",
                  conf.level = 0.95)

sign2 <- SIGN.test(x = eaatb$r[eaatb$is_post == 1 & eaatb$cond == 2],
                   y = eaatb$r[eaatb$is_post == 0 & eaatb$cond == 2],
                   alternative = "two.sided",
                   conf.level = 0.95)

sign3 <- SIGN.test(x = eaatb$r[eaatb$is_post == 1 & eaatb$cond == 0],
                   y = eaatb$r[eaatb$is_post == 0 & eaatb$cond == 0],
                   alternative = "two.sided",
                   conf.level = 0.95)

p.adjust(c(sign1$p.value, sign2$p.value, sign3$p.value),
         method = "bonferroni")

# mann-whitney test
wil1 <- wilcox.test(x = eaatb$r_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                    y = eaatb$r_increase[eaatb$cond == 0 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)

wil2 <- wilcox.test(x = eaatb$r_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                    y = eaatb$r_increase[eaatb$cond == 0 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)

wil3 <- wilcox.test(x = eaatb$r_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                    y = eaatb$r_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)

p.adjust(c(wil1$p.value, wil2$p.value, wil3$p.value),
         method = "bonferroni")

library(DescTools)

SignTest(x = eaatb$r[eaatb$is_post == 0],
         y = eaatb$a[eaatb$is_post == 1])

# histogram
ggplot(eaatb[eaatb$cond == 0,], aes(x = r, fill = is_post)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("gray 100", "gray20"))

ggplot(eaatb[eaatb$cond == 1,], aes(x = r, fill = is_post)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("gray 100", "gray20"))

ggplot(eaatb[eaatb$cond == 2,], aes(x = r, fill = is_post)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("gray 100", "gray20"))

##### Investigate ambiguous choices #####

# test
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

# pre-intervention
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = a,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)

# post-intervention increase
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

# sign test
library(BSDA) # basic statistics and data analysis

sign1<- SIGN.test(x = eaatb$a[eaatb$is_post == 1 & eaatb$cond == 1],
                  y = eaatb$a[eaatb$is_post == 0 & eaatb$cond == 1],
                  alternative = "two.sided",
                  conf.level = 0.95)

sign2 <- SIGN.test(x = eaatb$a[eaatb$is_post == 1 & eaatb$cond == 2],
                   y = eaatb$a[eaatb$is_post == 0 & eaatb$cond == 2],
                   alternative = "two.sided",
                   conf.level = 0.95)

sign3 <- SIGN.test(x = eaatb$a[eaatb$is_post == 1 & eaatb$cond == 0],
                   y = eaatb$a[eaatb$is_post == 0 & eaatb$cond == 0],
                   alternative = "two.sided",
                   conf.level = 0.95)

p.adjust(c(sign1$p.value, sign2$p.value, sign3$p.value),
         method = "bonferroni")

# mann-whitney test
wil1 <- wilcox.test(x = eaatb$a_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                    y = eaatb$a_increase[eaatb$cond == 0 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)

wil2 <- wilcox.test(x = eaatb$a_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                    y = eaatb$a_increase[eaatb$cond == 0 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)

wil3 <- wilcox.test(x = eaatb$a_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                    y = eaatb$a_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)

p.adjust(c(wil1$p.value, wil2$p.value, wil3$p.value),
         method = "bonferroni")

wil1$statistic
wil1$p.value
qnorm(wil1$p.value)



# histogram
transp = 0.7

ggplot(eaatb[eaatb$cond == 0,], aes(x = a, fill = is_post)) +
  geom_histogram(bins = 30, color = "black", alpha = transp, position = "identity", breaks = seq(0,1,1/25)) +
  # geom_density(alpha = transp, color =  "black", size = 1) +
  scale_fill_grey(start = 0.20, end = 1) +
  scale_x_continuous(limits = c(0.0, 1.0), breaks = seq(0,1.0,0.2)) +
  scale_y_continuous(limits = c(0,18 ), breaks = seq(0,18,2)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black"))

ggplot(eaatb[eaatb$cond == 1,], aes(x = a, fill = is_post)) +
  # scale_fill_manual(values = c("white", "gray20")) +
  geom_histogram(bins = 30, color = "black", alpha = transp, position = "identity",breaks = seq(0,1,1/25)) +
  # geom_density(alpha = transp, color = "black", size = 1) +
  scale_fill_grey(start = 0.20, end = 1) +
  scale_x_continuous(limits = c(0.0, 1.0), breaks = seq(0,1.0,0.2)) +
  scale_y_continuous(limits = c(0, 18), breaks = seq(0,18,2)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black"))
  

ggplot(eaatb[eaatb$cond == 2,], aes(x = a, fill = is_post)) +
  # scale_fill_manual(values = c("white", "gray20")) +
  geom_histogram(bins = 30, color = "black", alpha = transp, position = "identity",breaks = seq(0,1,1/25)) +
  #geom_density(alpha = transp, color = "black", size = 1) +
  scale_fill_grey(start = 0.2, end = 1) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1.0,0.2)) +
  scale_y_continuous(limits = c(0, 18), breaks = seq(0,18,2)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black"))

##### Investigate constrained alpha #####

# bar plot
data_sum = data_summary(eaatb, varname = "alpha_t", groupnames = c("is_post", "cond"))
data_sum = data_summary(eaatb[!is.element(eaatb$id, exclude_riskAmbig),], varname = "alpha_t", groupnames = c("is_post", "cond"))

ggplot(data_sum, aes(x = cond, y = alpha_t, fill = is_post)) + 
  geom_bar(stat = "identity", position = position_dodge(), color = "black", size = 0.6) +
  scale_fill_manual(values = c("white", "gray80")) +
  geom_errorbar(aes(ymin = alpha_t-sd, ymax = alpha_t+sd), width = 0.1, size = 1, position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(limits = c(-0.6,0.0)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black"))

# increase barplot
data_sum = data_summary(eaatb[eaatb$is_post == 1,], varname = "alpha_t_increase", groupnames = c("cond"))
ggplot(data_sum, aes(x = cond, y = alpha_t_increase)) + 
  geom_bar(stat = "identity", color = "black", fill = "gray75", size = 0.6, position = position_dodge()) +
  geom_errorbar(aes(ymin = alpha_t_increase-sd, ymax = alpha_t_increase+sd), width = 0.1, size = 1, position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(limits = c(-0.05,0.25), breaks = c(-0.05, 0.00, 0.05, 0.10, 0.15, 0.20, 0.25)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black"))

# violin plot
ggplot(eaatb, aes(x = cond, y = alpha_t, fill = is_post)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=FALSE, size=1) + # not trimed
  scale_fill_manual(values = c("gray 90", "gray55")) +
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red", position = position_dodge(0.9)) +
  # geom_boxplot(width = 0.1) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(limits=c(-2, 2), breaks = c(-2.0, -1.0, 0.0, 1.0, 2.0)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 16, color = "black")) +
  ggtitle("Constrained Risk Attitude") + xlab("") + ylab("Transformed Alpha") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))

# increase violin plot
ggplot(eaatb[eaatb$is_post == 1,], aes(x = cond, y = alpha_t_increase)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=FALSE, size=1, fill = "white") + # not trimed
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red") +
  # geom_boxplot(width = 0.1) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  # scale_y_continuous(breaks = c(-2.0, -1.0, 0.0, 1.0, 2.0, 3.0, 4.0)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 14, color = "black")) +
  ggtitle("Constrained Risk Attitude") + xlab("") + ylab("Transformed Alpha") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))


# Test
# two-way
ac_anova = ezANOVA(data=eaatb, 
                   dv = alpha_t,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# pre-intervention
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = alpha_t,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)

# post-intervention increase
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

##### Investigate constrained beta ####
# bar plot
data_sum = data_summary(eaatb, varname = "beta_t", groupnames = c("is_post", "cond"))
data_sum = data_summary(eaatb[!is.element(eaatb$id, exclude_riskAmbig),], varname = "beta_t", groupnames = c("is_post", "cond"))

ggplot(data_sum, aes(x = cond, y = beta_t, fill = is_post)) + 
  geom_bar(stat = "identity", position = position_dodge(), color = "black", size = 0.6) +
  scale_fill_manual(values = c("gray90", "gray55")) +
  geom_errorbar(aes(ymin = beta_t-sd, ymax = beta_t+sd), width = 0.1, size = 1, position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  #scale_y_continuous(limits = c(-1.2,0.0), breaks = c(-1.2, -1.0, -0.8, -0.6, -0.4, -0.2, 0.0)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black"))

# increase barplot
data_sum = data_summary(eaatb[eaatb$is_post == 1,], varname = "beta_t_increase", groupnames = c("cond"))
ggplot(data_sum, aes(x = cond, y = beta_t_increase)) + 
  geom_bar(stat = "identity", color = "black", fill = "gray75", size = 0.6, position = position_dodge()) +
  geom_errorbar(aes(ymin = beta_t_increase-sd, ymax = beta_t_increase+sd), width = 0.1, size = 1, position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  # scale_y_continuous(limits = c(-0.05,0.25), breaks = c(-0.05, 0.00, 0.05, 0.10, 0.15, 0.20, 0.25)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black"))

# violin plot
ggplot(eaatb, aes(x = cond, y = beta_t, fill = is_post)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=FALSE, size=1) + # not trimed
  scale_fill_manual(values = c("gray90", "gray55")) +
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red", position = position_dodge(0.9)) +
  # geom_boxplot(width = 0.1) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(breaks = c(-4.0, -3.0, -2.0, -1.0, -0.0)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 16, color = "black")) +
  ggtitle("Beta") + xlab("") + ylab("Ambiguity Attitude") +
  theme(axis.title.y=element_text(size = 18)) +
  theme(axis.title = element_text(size = 12))

# increase violin plot
ggplot(eaatb[eaatb$is_post == 1 & eaatb$beta_t_increase < 3,], aes(x = cond, y = beta_t_increase)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=FALSE, size=1, fill = "white") + # not trimed
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red") +
  # geom_boxplot(width = 0.1) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(breaks = c(-2.0, -1.0, 0.0, 1.0, 2.0, 3.0, 4.0)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 14, color = "black")) +
  ggtitle("Beta") + xlab("") + ylab("Ambiguity Attitude Increase") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))

# Test
# two-way
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

# pre-intervention
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = beta_t,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)

# post-intervention increase
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = beta_t_increase,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)


##### Investigate constrained gamma #####

# bar graph
data_sum = data_summary(eaatb, varname = "gamma", groupnames = c("is_post", "cond"))
ggplot(data_sum, aes(x = cond, y = gamma, fill = is_post)) + 
  geom_bar(stat = "identity", position = position_dodge(), color = "black", size = 0.6) +
  scale_fill_manual(values = c("white", "gray80")) +
  geom_errorbar(aes(ymin = gamma-sd, ymax = gamma+sd), width = 0.1, size = 1, position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  #scale_y_continuous(limits = c(-1.2,0.0), breaks = c(-1.2, -1.0, -0.8, -0.6, -0.4, -0.2, 0.0)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black"))

# violin plot
ggplot(eaatb, aes(x = cond, y = gamma, fill = is_post)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=FALSE, size=1) + # not trimed
  scale_fill_manual(values = c("gray90", "gray55")) +
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red", position = position_dodge(0.9)) +
  # geom_boxplot(width = 0.1) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  #scale_y_continuous(breaks = c(-4.0, -3.0, -2.0, -1.0, -0.0)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 16, color = "black")) +
  ggtitle("Constrained Gamma") + xlab("") + ylab("") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))

# increase violin plot
ggplot(eaatb[eaatb$is_post == 1,], aes(x = cond, y = gamma_increase)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=FALSE, size=1, fill = "white") + # not trimed
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red") +
  # geom_boxplot(width = 0.1) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  #scale_y_continuous(breaks = c(-2.0, -1.0, 0.0, 1.0, 2.0, 3.0, 4.0)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 14, color = "black")) +
  ggtitle("Constrained Gamma") + xlab("") + ylab("") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))

# Test
# two-way
ac_anova = ezANOVA(data=eaatb, 
                   dv = gamma,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# pre-intervention
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = gamma,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)

# post-intervention increase
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = gamma_increase,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)

##### Investigate ep score #####
# bar graph
data_sum = data_summary(eaatbpost,
                        varname = "ep_score",
                        groupnames = c("cond"))

ggplot(data_sum, aes(x = cond, y = ep_score)) + 
  geom_bar(stat = "identity", color = "black", fill = "gray75", size = 0.6, position = position_dodge()) +
  geom_errorbar(aes(ymin = ep_score-sd, ymax = ep_score+sd), width = 0.1, size = 1, position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(limits = c(0,2.0)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black")) +
  ggtitle("Understanding the Ellsberg Paradox") + xlab("") + ylab("Score") +
  theme(axis.title.y=element_text(size = 18)) +
  theme(axis.title=element_text(size=12))

# violin plot
ggplot(eaatb[eaatb$is_post == 0,], aes(x = cond, y = ep_score)) + 
  geom_violin(size=1) + # trimed
  # geom_violin(trim=FALSE, size=1) + # not trimed
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red") +
  # geom_boxplot(width = 0.1) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(breaks = c(0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 16, color = "black")) +
  ggtitle("Understanding the Ellsberg Paradox") + xlab("") + ylab("Score") +
  theme(axis.title.y=element_text(size = 18)) +
  theme(axis.title = element_text(size = 12))

# group comparison ep score
ac_anova = ezANOVA(data=eaatbpre[!is.nan(eaatbpre$ep_score),], 
                   dv = ep_score,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)

##### Investigate constrained alpha, for risk only fitting #####

# violin plot
ggplot(eaatb, aes(x = cond, y = alpha_risk_t, fill = is_post)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=FALSE, size=1) + # not trimed
  scale_fill_manual(values = c("gray 90", "gray55")) +
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red", position = position_dodge(0.9)) +
  # geom_boxplot(width = 0.1) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  # scale_y_continuous(limits=c(-2, 2), breaks = c(-2.0, -1.0, 0.0, 1.0, 2.0)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 16, color = "black")) +
  ggtitle("Constrained Risk Attitude risk only fitting") + xlab("") + ylab("Transformed Alpha") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))

# increase violin plot
ggplot(eaatb[eaatb$is_post == 1 & eaatb$alpha_risk_t_increase<1,], aes(x = cond, y = alpha_risk_t_increase)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=FALSE, size=1, fill = "white") + # not trimed
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red") +
  # geom_boxplot(width = 0.1) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  # scale_y_continuous(breaks = c(-2.0, -1.0, 0.0, 1.0, 2.0, 3.0, 4.0)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 14, color = "black")) +
  ggtitle("Constrained Risk Attitude Risk Only Fitting") + xlab("") + ylab("Transformed Alpha") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))


# Test
# two-way
ac_anova = ezANOVA(data=eaatb, 
                   dv = alpha_risk_t,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# pre-intervention
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = alpha_risk_t,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)

# post-intervention increase
ac_anova = ezANOVA(data=eaatbpre[eaatbpre$alpha_risk_t_increase<1,], 
                   dv = alpha_risk_t_increase,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)


##### Investigate constrained ambiguous choice, for risk only fitting #####

# violin plot
ggplot(eaatb, aes(x = cond, y = a_r50, fill = is_post)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=TRUE) + # not trimed
  # geom_dotplot(binaxis='y', binwidth = 0.035, stackdir='center',position=position_dodge(0.9)) + # add dots
  scale_fill_manual(values = c("gray55", "white")) +
  # stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red", position = position_dodge(0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(limits=c(-1, 0.5), breaks = c(-1.0, -0.75, -0.50,-0.25, 0.0, 0.25, 0.5)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 16, color = "black")) +
  ggtitle("Model based ambiguity attitude in choice proportion") + xlab("") + ylab("Choice Proportion") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))

# increase violin plot
ggplot(eaatb[eaatb$is_post == 1,], aes(x = cond, y = a_r50_increase)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=TRUE, fill = "grey75") + # not trimed
  # stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red") +
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(limits=c(-0.5, 1), breaks = c(-.5, -.25, 0.0, .25, 0.5, .75, 1)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 14, color = "black")) +
  ggtitle("Model based ambiguity attitude in choice proportion") + xlab("") + ylab("Choice Proportion") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))


# Test
# two-way
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

# pre-intervention
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

# post-intervention increase
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

# sign test
library(BSDA) # basic statistics and data analysis

sign1<- SIGN.test(x = eaatb$a_r50[eaatb$is_post == 1 & eaatb$cond == 1],
                  y = eaatb$a_r50[eaatb$is_post == 0 & eaatb$cond == 1],
                  alternative = "two.sided",
                  conf.level = 0.95)

sign2 <- SIGN.test(x = eaatb$a_r50[eaatb$is_post == 1 & eaatb$cond == 2],
                   y = eaatb$a_r50[eaatb$is_post == 0 & eaatb$cond == 2],
                   alternative = "two.sided",
                   conf.level = 0.95)

sign3 <- SIGN.test(x = eaatb$a_r50[eaatb$is_post == 1 & eaatb$cond == 0],
                   y = eaatb$a_r50[eaatb$is_post == 0 & eaatb$cond == 0],
                   alternative = "two.sided",
                   conf.level = 0.95)

p.adjust(c(sign1$p.value, sign2$p.value, sign3$p.value),
         method = "bonferroni")

# mann-whitney test
wil1 <- wilcox.test(x = eaatb$a_r50_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                    y = eaatb$a_r50_increase[eaatb$cond == 0 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)

wil2 <- wilcox.test(x = eaatb$a_r50_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                    y = eaatb$a_r50_increase[eaatb$cond == 0 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)

wil3 <- wilcox.test(x = eaatb$a_r50_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                    y = eaatb$a_r50_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)

p.adjust(c(wil1$p.value, wil2$p.value, wil3$p.value),
         method = "bonferroni")

wil1$statistic
wil1$p.value
qnorm(wil1$p.value)

#Fixed effect probit analysis
model1 <- glm(a ~ cond + is_post,
              family = binomial(link = "probit"),
              data = eaatb)

summary(model1)

##### Investigate difference between choice proportion #####

# violin plot, ambiguity 50 and 24 difference 
ggplot(eaatb, aes(x = cond, y = a50_a24, fill = is_post)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=TRUE) + # not trimed
  # geom_dotplot(binaxis='y', binwidth = 0.02, stackdir='center',position=position_dodge(0.9)) + # add dots
  scale_fill_manual(values = c("gray55", "white")) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) + # add box plot
  # stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red", position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(limits=c(-1.0, 0.25), breaks = c(-1.0, -.75,-0.5, -.25, 0.0, 0.25)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black")) +
  ggtitle("Difference between choice proportion ambiguity 50 and 24") + xlab("") + ylab("Choice Proportion") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))

# test
# ez anova
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


# violin plot, ambiguity 74 and 24 difference 
ggplot(eaatb, aes(x = cond, y = a74_a24, fill = is_post)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=TRUE) + # not trimed
  # geom_dotplot(binaxis='y', binwidth = 0.03, stackdir='center',position=position_dodge(0.9)) + # add dots
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) + # add box plot
  scale_fill_manual(values = c("gray55", "white")) +
  #stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red", position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(limits=c(-1.1, 0.3), breaks = c(-1.0, -.75,-0.5, -.25, 0.0, 0.25)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black")) +
  ggtitle("Difference between choice proportion ambiguity 74 and 24") + xlab("") + ylab("Choice Proportion") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))

# test
# ez anova
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

anova_boot = ezBoot(data = eaatb,
                    dv = a74_a24,
                    wid = .(id),
                    within = .(is_post),
                    between = .(cond),
                    resample_within = FALSE,
                    iterations = 1e3,
                    lmer = FALSE
                    )

##### Risk and ambiguity attitudes correlation #####

# all subjects, pre intervention
ggplot(eaatbpre, aes(x=alpha_t, y=beta_t)) + 
  geom_point() +
  theme_classic()

ggplot(eaatbpre[eaatbpre$beta_t>-1.5, ], aes(x=alpha_t, y=beta_t)) + 
  geom_point() +
  theme_classic()

cor.test(eaatbpre$alpha_t,
         eaatbpre$beta_t,
         method = c("spearman"))
cor.test(eaatbpre$alpha_t[eaatbpre$beta_t>-1.5], 
         eaatbpre$beta_t[eaatbpre$beta_t>-1.5],
         method = c("spearman"))

#  AC and NC together, pre intervention

ggplot(eaatbpre[!eaatbpre$cond==0, ], aes(x=alpha_t, y=beta_t)) + 
  geom_point() +
  theme_classic()

ggplot(eaatbpre, aes(x=a_r50, y=r)) + 
  geom_point() +
  theme_classic()

ggplot(eaatbpre[!eaatbpre$cond==0, ], aes(x=a_r50, y=r)) + 
  geom_point(size=2) +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color = "black") +
  scale_x_continuous(limits=c(-.8, 0.4), breaks = c( -.8, -.6,-0.4, -.2, 0.0, 0.2, .4)) +
  scale_y_continuous(limits=c(0, 1.0), breaks = c(0, .2, .4, .6, .8, 1.0)) +
  theme_classic()+
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black")) +
  ggtitle("Pre-intervention") + xlab("Ambiguity Attitude") + ylab("Risk Attitude") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))


ggplot(eaatbpre[!eaatbpre$cond==0 & eaatbpre$beta_t>-4, ], aes(x=alpha_t, y=beta_t)) + 
  geom_point() +
  theme_classic()

cor.test(eaatbpre$alpha_t[!eaatbpost$cond==0], eaatbpre$beta_t[!eaatbpost$cond==0],
         method = c("spearman"))
cor.test(eaatbpre$a_r50[!eaatbpost$cond==0], eaatbpre$r[!eaatbpost$cond==0],
         method = c("pearson"))
cor.test(eaatbpre$a_r50, eaatbpre$r,
         method = c("pearson"))
cor.test(eaatbpre$alpha_t[!eaatbpre$cond==0 & eaatbpre$beta_t>-4], 
         eaatbpre$beta_t[!eaatbpre$cond==0 & eaatbpre$beta_t>-4],
         method = c("spearman"))

# AC and NC , post intervention
ggplot(eaatbpost[!eaatbpost$cond==0, ], aes(x=alpha_t, y=beta_t)) + 
  geom_point() +
  theme_classic()

ggplot(eaatbpost[!eaatbpost$cond==0, ], aes(x=a_r50, y=r)) + 
  geom_point(size=2) +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color = "black") +
  scale_x_continuous(limits=c(-0.8, 0.4), breaks = c(-.8, -.6,-0.4, -.2, 0.0, 0.2, .4)) +
  scale_y_continuous(limits=c(0, 1.0), breaks = c(0, .2, .4, .6, .8, 1.0)) +
  theme_classic()+
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black")) +
  ggtitle("Post-intervention") + xlab("Ambiguity Attitude") + ylab("Risk Attitude") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title.x=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))

ggplot(eaatbpost[!eaatbpost$cond==0 & eaatbpost$beta_t>-4, ], aes(x=alpha_t, y=beta_t)) + 
  geom_point() +
  theme_classic()

cor.test(eaatbpost$alpha_t[!eaatbpost$cond==0], eaatbpost$beta_t[!eaatbpost$cond==0], 
         method = c("spearman"))

cor.test(eaatbpost$a_r50[!eaatbpost$cond==0], eaatbpost$r[!eaatbpost$cond==0], 
         method = c("pearson"))

cor.test(eaatbpost$alpha_t[!eaatbpost$cond==0 & eaatbpost$beta_t>-4],
         eaatbpost$beta_t[!eaatbpost$cond==0 & eaatbpost$beta_t>-4], 
         method = c("spearman"))

# comparing pre and post intervention risk and ambiguity correlation
preACNC = eaatbpre[!eaatb$cond==0,]
postACNC = eaatbpost[!eaatb$cond==0,]
postAC = eaatb[!eaatb$cond==0,]
postNC = eaatb[!eaatb$cond==0,]

cocordata <- data.frame("pre_beta_t"=preACNC$beta_t, "pre_alpha_t"=preACNC$alpha_t,
                        "post_beta_t"=postACNC$beta_t, "post_alpha_t"=postACNC$alpha_t)

cocordata <- data.frame("pre_a_r50"=preACNC$a_r50, "pre_r"=preACNC$r,
                        "post_a_r50"=postACNC$a_r50, "post_r"=postACNC$r)

cocordata$pre_beta_t[cocordata$pre_beta_t < -4] <- NaN
cocordata$pre_alpha_t[cocordata$pre_beta_t < -4] <- NaN

cocordata$post_beta_t[cocordata$post_beta_t < -4] <- NaN
cocordata$post_alpha_t[cocordata$post_beta_t < -4] <- NaN

cocor(~pre_beta_t + pre_alpha_t | post_beta_t + post_alpha_t, cocordata,
      alternative ="two.sided", test="all",
      alpha = 0.05, return.htest = FALSE)

cocor(~pre_a_r50 + pre_r | post_a_r50 + post_r, cocordata,
      alternative ="two.sided", test="all",
      alpha = 0.05, return.htest = FALSE)

# correlation between attitudes change
ggplot(eaatbpost[!eaatbpost$cond==0, ], aes(x=alpha_t_increase, y=beta_t_increase)) + 
  geom_point() +
  theme_classic()

ggplot(eaatbpost[!eaatbpost$cond==0 & eaatbpost$beta_t_increase<4, ], aes(x=alpha_t_increase, y=beta_t_increase)) + 
  geom_point() +
  theme_classic()

ggplot(eaatbpost[!eaatbpost$cond==0, ], aes(x=a_r50_increase, y=r_increase)) + 
  geom_point(size=2) +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color = "black") +
  scale_x_continuous(limits=c(-.4, 1.0), breaks = c(-0.4, -.2, 0.0, 0.2, .4, .6, .8, 1.0)) +
  scale_y_continuous(limits=c(-.4, 0.8), breaks = c(-.4, -.2, 0, .2, .4, .6, .8)) +
  theme_classic()+
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black")) +
  ggtitle("Correlation of change") + xlab("Increase in Ambiguity Attitude") + ylab("Increase in Risk Attitude") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title.x=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))

cor.test(eaatbpost$alpha_t_increase[!eaatbpost$cond==0],
         eaatbpost$beta_t_increase[!eaatbpost$cond==0], 
         method = c("spearman"))

cor.test(eaatbpost$alpha_t_increase[!eaatbpost$cond==0 & eaatbpost$beta_t_increase<4],
         eaatbpost$beta_t_increase[!eaatbpost$cond==0 & eaatbpost$beta_t_increase<4], 
         method = c("spearman"))

cor.test(eaatbpost$a_r50_increase[!eaatbpost$cond==0],
         eaatbpost$r_increase[!eaatbpost$cond==0], 
         method = c("pearson"))

# correlation between pre-intervention attitude and change in attitude
ggplot(eaatbpre[!eaatbpre$cond==0, ], aes(x=a_r50, y=a_r50_increase)) + 
  geom_point(size=2) +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color = "black") +
  # scale_x_continuous(limits=c(-.4, 1.0), breaks = c(-0.4, -.2, 0.0, 0.2, .4, .6, .8, 1.0)) +
  # scale_y_continuous(limits=c(-.4, 0.8), breaks = c(-.4, -.2, 0, .2, .4, .6, .8)) +
  theme_classic()+
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black")) +
  ggtitle("Correlation") + xlab("Pre-intervention ambiguity attitude") + ylab("Change in ambiguity attitude") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title.x=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))

cor.test(eaatbpre$a_r50[!eaatbpre$cond==0],
         eaatbpre$a_r50_increase[!eaatbpre$cond==0], 
         method = c("pearson"))

ggplot(eaatbpre[!eaatbpre$cond==0, ], aes(x=r, y=r_increase)) + 
  geom_point(size=2) +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color = "black") +
  # scale_x_continuous(limits=c(-.4, 1.0), breaks = c(-0.4, -.2, 0.0, 0.2, .4, .6, .8, 1.0)) +
  # scale_y_continuous(limits=c(-.4, 0.8), breaks = c(-.4, -.2, 0, .2, .4, .6, .8)) +
  theme_classic()+
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black")) +
  ggtitle("Correlation") + xlab("Pre-intervention risk attitude") + ylab("Change in risk attitude") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title.x=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))

cor.test(eaatbpre$r[!eaatbpre$cond==0],
         eaatbpre$r_increase[!eaatbpre$cond==0], 
         method = c("pearson"))

##### EP and change correlation #####
ggplot(eaatbpost[eaatbpost$cond==1, ], aes(x=ep_score, y=a_r50_increase)) + 
  geom_point(size=2) +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color = "black") +
  # scale_x_continuous(limits=c(-1.0, 0.2), breaks = c(-1, -.8, -.6,-0.4, -.2, 0.0, 0.2)) +
  scale_y_continuous(limits=c(-0.2, 0.6), breaks = c(-0.2, 0, .2, .4, .6)) +
  theme_classic()+
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black")) +
  ggtitle("Active Calculation") + xlab("Understanding") + ylab("Increase in Ambiguity Attitude") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title.x=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))


cor.test(eaatbpost$ep_score[eaatbpost$cond==1],
         eaatbpost$a_r50_increase[eaatbpost$cond==1],
         method = c("spearman"))

ggplot(eaatbpost[eaatbpost$cond==2, ], aes(x=ep_score, y=a_r50_increase)) + 
  geom_point(size=2) +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color = "black") +
  # scale_x_continuous(limits=c(-1.0, 0.2), breaks = c(-1, -.8, -.6,-0.4, -.2, 0.0, 0.2)) +
  scale_y_continuous(limits=c(-0.3, 0.6), breaks = c(-0.2, 0, .2, .4, .6)) +
  theme_classic()+
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black")) +
  ggtitle("Non-active Calculation") + xlab("Understanding") + ylab("Increase in Ambiguity Attitude") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title.x=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))


cor.test(eaatbpost$ep_score[eaatbpost$cond==2],
         eaatbpost$a_r50_increase[eaatbpost$cond==2],
         method = c("spearman"))

ggplot(eaatbpre[!eaatbpre$cond==0, ], aes(x=ep_score, y=beta_t)) + 
  geom_point() +
  theme_classic()

cor.test(eaatbpre$alpha_t[!eaatbpost$cond==0], eaatbpre$beta_t[!eaatbpost$cond==0],
         method = c("spearman"))


cor.test(eaatbpre$ep_score[eaatbpost$cond==1],
         eaatbpre$beta_t_increase[eaatbpost$cond==1],
         method = c("spearman"))

ggplot(eaatbpre[eaatbpre$cond==2, ], aes(x=ep_score, y=beta_t_increase)) + 
  geom_point() +
  theme_classic()

ggplot(eaatbpre[eaatbpre$cond==2 & eaatbpre$beta_t_increase<4, ], aes(x=ep_score, y=beta_t_increase)) + 
  geom_point() +
  theme_classic()

cor.test(eaatbpre$ep_score[eaatbppre$cond==2],
         eaatbpre$beta_t_increase[eaatbpre$cond==2],
         method = c("spearman"))

cor.test(eaatbpre$ep_score[eaatbpre$cond==2 & eaatbpre$beta_t_increase<4],
         eaatbpre$beta_t_increase[eaatbpre$cond==2 & eaatbpre$beta_t_increase<4],
         method = c("spearman"))

##### ANOVA by linear model and multiple comparison #####

# construct model
model1 <- lme(a50_a24 ~ cond*is_post, random = ~is_post|id, na.action=na.omit, data = eaatb)
model1 <- lme(a74_a24 ~ cond*is_post, random = ~is_post|id, na.action=na.omit, data = eaatb)
model1 <- lme(beta_t ~ cond*is_post, random = ~is_post|id, na.action=na.omit, data = eaatb)
model1 <- lme(alpha_t ~ cond*is_post, random = ~is_post|id, na.action=na.omit, data = eaatb)
# model1 <- lmer(a50_a24 ~ cond*is_post + (1|id), na.action=na.omit, data = eaatb )
# model1 <- lmer(a50_a24 ~ cond*is_post + (1|id), na.action=na.omit, data = eaatb )

model1 <- lme(a_r50 ~ cond*is_post, random = ~is_post|id, na.action=na.omit, data = eaatb)
model1 <- lme(r ~ cond*is_post, random = ~is_post|id, na.action=na.omit, data = eaatb)
model1 <- lme(alpha_risk_t ~ cond*is_post, random = ~is_post|id, na.action=na.omit, data = eaatb)

anova(model1)

model2 <- lme(ep_score ~ cond, random = ~1|id, na.action=na.omit, data = eaatbpost)
model2 <- lme(a_r50_increase ~ cond, random = ~1|id, na.action=na.omit, data = eaatbpost)
model2 <- lme(a_5in_r50_increase ~ cond, random = ~1|id, na.action=na.omit, data = eaatbpost)
model2 <- lme(r_increase ~ cond, random = ~1|id, na.action=na.omit, data = eaatbpost)
model2 <- lme(a_increase ~ cond, random = ~1|id, na.action=na.omit, data = eaatbpost)

model2 <- lme(a_r50 ~ cond, random = ~1|id, na.action=na.omit, data = eaatbpre)
model2 <- lme(r ~ cond, random = ~1|id, na.action=na.omit, data = eaatbpre)


anova(model2)




# interpreting interaction, require emmeans package
# visualize the interaction
emmip(model1, cond ~ is_post)

# post-hoc comparison of means, require packages emmeans
model1.emm <- emmeans(model1,  ~ cond:is_post)
model2.emm <- emmeans(model2,  ~ cond)

emmeans(model1, pairwise ~ cond|is_post)

contrast(model1.emm, method="pairwise", adjust = "bonferroni")
contrast(model1.emm, method="pairwise")

ic_st <- contrast(model1.emm, interaction=c("consec", "consec"), adjust = "fdr")
ic_st <- contrast(model1.emm, interaction=c("consec", "consec"))
ic_st
coef(ic_st) # see the constrast

# interaction contrast - contrast of contrast, ref: https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html
ic_st <- contrast(model1.emm, interaction=TRUE)
coef(ic_st)
test(ic_st, joint = TRUE)

# simple contrast
contrast(model1.emm, simple= "cond")
pairs(model1.emm, simple = "cond", adjust="bonferroni")
pairs(model1.emm, simple = "cond")
pairs(model1.emm, simple = "is_post", adjust="fdr")
pairs(model1.emm, simple = "each")
contrast(model1.emm, simple = "each", adjust = 'fdr')
pairs(model1.emm, simple = "each", adjust="fdr")
contrast(model2.emm, simple = "each")
pairs(model2.emm, simple = "each", adjust = "fdr")

summary(glht(model1, emm(pairwise ~ cond:is_post)))

model2 <- lme(a50_a24 ~ cond*is_post, random = ~1|id, na.action=na.omit, data = eaatb)

# simple anova
summary(aov(a50_a24 ~ cond*is_post + Error(id), data = eaatb))

# return p value by t stats
pt(-abs(-4.546),df=116)

# Anova(model1, type=c("III"))
# Anova(model1, test.statistics=c("F"))
# Anova(model1)

anova(model1)
anova(model2)

summary(model1)

# multiple comparison
eaatb$inter <- interaction(eaatb$cond, eaatb$is_post, drop=T)
glht(model1, linfct = mcp(cond="Tukey", is_post = "Tukey")) # all-pair comparisons
glht(model1, linfct = mcp(inter = "Tukey")) # all-pair comparisons

contrast <- rbind("Cond1 - Cond0" = c(-1, 1 ,0),
                  "Cond2 - Cond0" = c(-1, 0 ,1),
                  "Cond3 - Cond2" = c(0, -1, 1))

glht(model1, linfct = mcp(cond=contrast))
##### Model fitting quality comparing groups ######

# violin plot
ggplot(eaatb,aes(x=cond, y=r2, fill=is_post)) + 
  geom_violin(trim=FALSE, size = 1) +
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red", position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  ggtitle("Constrained fitting Pseudo R2")

# bar plot
data_sum = data_summary(eaatb, "r2", c("is_post", "cond"))

ggplot(data_sum,aes(x=cond, y=r2, fill=is_post)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=r2-sd, ymax=r2+sd), width=0.2, position=position_dodge(0.9)) +
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  ggtitle("Constrained fitting Pseudo R2")

##### Investigate reaction time #####

print(describeBy(eaatbpre$rt_a_5out, group = eaatbpre$cond), digits = 4)
print(describe(eaatbpre$rt_a_5out), digits = 4)
print(describeBy(eaatbpost$rt_a_5out, group = eaatbpost$cond), digits = 4)
print(describe(eaatbpre$rt_r_5out), digits = 4)


print(describeBy(eaatbpre$rt_r_5out, group = eaatbpre$cond), digits = 4)
print(describe(eaatbpost$rt_a_5out), digits = 4)
print(describeBy(eaatbpost$rt_r_5out, group = eaatbpost$cond), digits = 4)
print(describe(eaatbpost$rt_r_5out), digits = 4)

# violin plot, risky trials
ggplot(eaatb, aes(x = cond, y = rt_r_5out, fill = is_post)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=TRUE) + # not trimed
  # geom_dotplot(binaxis='y', binwidth = 0.035, stackdir='center',position=position_dodge(0.9)) + # add dots
  scale_fill_manual(values = c("gray55", "white")) +
  # stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red", position = position_dodge(0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge((0.9))) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  # scale_y_continuous(limits = c(500,4000), breaks = seq(1000,4000,1000)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 12, color = "black")) +
  ggtitle("Risky trials reaction time") + xlab("") + ylab("Reaction time (ms)") +
  theme(axis.title.y=element_text(size = 12)) +
  theme(axis.title = element_text(size = 14))

# violin plot, ambiguous trials
ggplot(eaatb, aes(x = cond, y = rt_a_5out, fill = is_post)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=TRUE) + # not trimed
  # geom_dotplot(binaxis='y', binwidth = 0.035, stackdir='center',position=position_dodge(0.9)) + # add dots
  scale_fill_manual(values = c("gray55", "white")) +
  # stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red", position = position_dodge(0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge((0.9))) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(limits = c(300,4000), breaks = seq(1000,4000,1000)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 16, color = "black")) +
  ggtitle("Ambiguous trials reaction time") + xlab("") + ylab("Reaction time (ms)") +
  theme(axis.title.y=element_text(size = 12)) +
  theme(axis.title = element_text(size = 14))


# test
ac_anova = ezANOVA(data=eaatb,
                   dv = rt_r_5out,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

ac_anova = ezANOVA(data=eaatb,
                   dv = rt_a_5out,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# histogram, risky trials
ggplot(eaatb[eaatb$cond == 0,], aes(x = rt_r_5out, fill = is_post)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("gray 100", "gray20"))

ggplot(eaatb[eaatb$cond == 1,], aes(x = rt_r_5out, fill = is_post)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("gray 100", "gray20"))

ggplot(eaatb[eaatb$cond == 2,], aes(x = rt_r_5out, fill = is_post)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("gray 100", "gray20"))

# histogram, ambiguous trials
ggplot(eaatb[eaatb$cond == 0,], aes(x = rt_a_5out, fill = is_post)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("gray 100", "gray20"))

ggplot(eaatb[eaatb$cond == 1,], aes(x = rt_a_5out, fill = is_post)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("gray 100", "gray20"))

ggplot(eaatb[eaatb$cond == 2,], aes(x = rt_a_5out, fill = is_post)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("gray 100", "gray20"))

# compare risky and ambiguous trials

# reorganize data table
# colnames(eaatb)
risk <- eaatb[eaatb$is_post == 0, c(1:4, 66)]
amb <- eaatb[eaatb$is_post == 0, c(1:4, 67)]
# colnames(risk) 
# colnames(amb)
colnames(risk) <- c("id", "is_post", "cond", "is_excluded", "rt_5out")
risk$is_risk <- 1
colnames(amb) <- c("id", "is_post", "cond", "is_excluded", "rt_5out")
amb$is_risk <- 0

rt_pre <- rbind(risk, amb)
# View(rt_pre)

risk <- eaatb[eaatb$is_post == 1, c(1:4, 66)]
amb <- eaatb[eaatb$is_post == 1, c(1:4, 67)]
# colnames(risk) 
# colnames(amb)
colnames(risk) <- c("id", "is_post", "cond", "is_excluded", "rt_5out")
risk$is_risk <- 1
colnames(amb) <- c("id", "is_post", "cond", "is_excluded", "rt_5out")
amb$is_risk <- 0

rt_post <- rbind(risk, amb)
# View(rt_post)

rt <- rbind(rt_pre, rt_post)
rt_pre$is_risk <- as.factor(rt_pre$is_risk)
rt_post$is_risk <- as.factor(rt_post$is_risk)

rt$is_risk <- as.factor(rt$is_risk)
# View(rt)

# violin plot, comparing risky and ambiguous trials, pre intervention
ggplot(rt_pre, aes(x = cond, y = rt_5out, fill = is_risk)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=TRUE) + # not trimed
  # geom_dotplot(binaxis='y', binwidth = 0.035, stackdir='center',position=position_dodge(0.9)) + # add dots
  scale_fill_manual(values = c("gray55", "white")) +
  # stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red", position = position_dodge(0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge((0.9))) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  # scale_y_continuous(limits = c(500,4000), breaks = seq(1000,4000,1000)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 16, color = "black")) +
  ggtitle("Pre-intervention Reaction time") + xlab("") + ylab("Reaction time (ms)") +
  theme(axis.title.y=element_text(size = 12)) +
  theme(axis.title = element_text(size = 14))

# violin plot, comparing risky and ambiguous trials, post intervention
ggplot(rt_post, aes(x = cond, y = rt_5out, fill = is_risk)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=TRUE) + # not trimed
  # geom_dotplot(binaxis='y', binwidth = 0.035, stackdir='center',position=position_dodge(0.9)) + # add dots
  scale_fill_manual(values = c("gray55", "white")) +
  # stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red", position = position_dodge(0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge((0.9))) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  # scale_y_continuous(limits = c(500,4000), breaks = seq(1000,4000,1000)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 16, color = "black")) +
  ggtitle("Post-intervention Reaction time") + xlab("") + ylab("Reaction time (ms)") +
  theme(axis.title.y=element_text(size = 12)) +
  theme(axis.title = element_text(size = 14))

# test
ac_anova = ezANOVA(data=rt_pre,
                   dv = rt_5out,
                   wid = .(id),
                   within = .(is_risk),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova


ac_anova = ezANOVA(data=rt_post,
                   dv = rt_5out,
                   wid = .(id),
                   within = .(is_risk),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

model1 <- lme(rt_5out ~ cond*is_risk, random = ~is_risk|id, na.action=na.omit, data = rt[rt$is_post == 1, ])
model1.emm <- emmeans(model1,  ~ cond:is_risk)
pairs(model1.emm, simple = "each")
contrast(model1.emm, simple = "each", adjust = 'fdr')


##### Investigate constrained ambiguous choice, calculated with $5 ambiguous trials included, for risk only fitting #####


# violin plot
ggplot(eaatb, aes(x = cond, y = a_5in_r50, fill = is_post)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=TRUE) + # not trimed
  # geom_dotplot(binaxis='y', binwidth = 0.035, stackdir='center',position=position_dodge(0.9)) + # add dots
  scale_fill_manual(values = c("gray55", "white")) +
  # stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red", position = position_dodge(0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(limits=c(-1, 0.5), breaks = c(-1.0, -0.75, -0.50,-0.25, 0.0, 0.25, 0.5)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 16, color = "black")) +
  ggtitle("Model based ambiguity attitude in choice proportion") + xlab("") + ylab("Choice Proportion") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))

# increase violin plot
ggplot(eaatb[eaatb$is_post == 1,], aes(x = cond, y = a_5in_r50_increase)) + 
  # geom_violin(size=1) + # trimed
  geom_violin(trim=TRUE, fill = "grey75") + # not trimed
  # stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red") +
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(limits=c(-0.5, 1), breaks = c(-.5, -.25, 0.0, .25, 0.5, .75, 1)) +
  theme_classic() +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.ticks = element_line(size = 1, color = "black")) +
  theme(axis.text = element_text(size = 14, color = "black")) +
  ggtitle("Model based ambiguity attitude in choice proportion") + xlab("") + ylab("Choice Proportion") +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title = element_text(size = 16))


# Test
# two-way
ac_anova = ezANOVA(data=eaatb, 
                   dv = a_5in_r50,
                   wid = .(id),
                   within = .(is_post),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# pre-intervention
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = a_5in_r50,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)

# post-intervention increase
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = a_5in_r50_increase,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova
TukeyHSD(ac_anova$aov)

# sign tests
sign1<- SIGN.test(x = eaatb$a_5in_r50[eaatb$is_post == 1 & eaatb$cond == 1],
                  y = eaatb$a_5in_r50[eaatb$is_post == 0 & eaatb$cond == 1],
                  alternative = "two.sided",
                  conf.level = 0.95)

sign2 <- SIGN.test(x = eaatb$a_5in_r50[eaatb$is_post == 1 & eaatb$cond == 2],
                   y = eaatb$a_5in_r50[eaatb$is_post == 0 & eaatb$cond == 2],
                   alternative = "two.sided",
                   conf.level = 0.95)

sign3 <- SIGN.test(x = eaatb$a_5in_r50[eaatb$is_post == 1 & eaatb$cond == 0],
                   y = eaatb$a_5in_r50[eaatb$is_post == 0 & eaatb$cond == 0],
                   alternative = "two.sided",
                   conf.level = 0.95)

p.adjust(c(sign1$p.value, sign2$p.value, sign3$p.value),
         method = "bonferroni")

# mann-whitney test
wil1 <- wilcox.test(x = eaatb$a_5in_r50_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                    y = eaatb$a_5in_r50_increase[eaatb$cond == 0 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)

wil2 <- wilcox.test(x = eaatb$a_5in_r50_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                    y = eaatb$a_5in_r50_increase[eaatb$cond == 0 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)

wil3 <- wilcox.test(x = eaatb$a_5in_r50_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                    y = eaatb$a_5in_r50_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)

p.adjust(c(wil1$p.value, wil2$p.value, wil3$p.value),
         method = "bonferroni")
