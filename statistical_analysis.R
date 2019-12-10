##### Install packages ######
# install.packages("ggplot2")
library("ggplot2")

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

# install.packages("phia")
library(phia)

# for sign test
if(!require(psych)){install.packages("psych")}
if(!require(BSDA)){install.packages("BSDA")}
if(!require(DescTools)){install.packages("DescTools")}
library(BSDA) # basic statistics and data analysis

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

path = "D:/Data" # Need to change to data directory
setwd(path)

eaatb <- read.csv("preprocessed_data.csv", header = TRUE)

# exclude and select constrained 
eaatb <- eaatb[eaatb$is_excluded == 0 & eaatb$is_constrained == 1,]

eaatbpre = eaatb[eaatb$is_post == 0,]
eaatbpost = eaatb[eaatb$is_post == 1,]

##### t tests of pre-intervention choice#####
# Pre-intervention ambiguous choice
t.test(eaatbpre$a,mu=1)
# Pre-intervention risk-corrected ambiguous choice
t.test(eaatbpre$a_r50, mu=0)
# Pre-intervention risk-corrected ambiguous choice ($5 trials in)
t.test(eaatbpre$a_5in_r50, mu=0)

# Pre-intervention risky choice
t.test(eaatbpre$r,mu=0.75)

##### Mann-Whitney test of post-intervention chocie ####
# post-intervention ambiguous choice, AC 
aAC = eaatb[eaatb$cond == 1 & eaatb$is_post==1 & eaatb$is_excluded==0,]
wil = wilcox.test(aAC$a, mu = 1, alternative = "two.sided")
wil$p.value
qnorm(wil$p.value)


# post-intervention ambiguous choice, NC
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


# increase, violin plot
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

# 2-way ANOVA test
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

# pre-intervention 1-way anova
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = r,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# increase, 1-way anova
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = r_increase,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# sign test compare pre- and post- within each group
# AC
sign1<- SIGN.test(x = eaatb$r[eaatb$is_post == 1 & eaatb$cond == 1],
                  y = eaatb$r[eaatb$is_post == 0 & eaatb$cond == 1],
                  alternative = "two.sided",
                  conf.level = 0.95)
# NC
sign2 <- SIGN.test(x = eaatb$r[eaatb$is_post == 1 & eaatb$cond == 2],
                   y = eaatb$r[eaatb$is_post == 0 & eaatb$cond == 2],
                   alternative = "two.sided",
                   conf.level = 0.95)
# Control
sign3 <- SIGN.test(x = eaatb$r[eaatb$is_post == 1 & eaatb$cond == 0],
                   y = eaatb$r[eaatb$is_post == 0 & eaatb$cond == 0],
                   alternative = "two.sided",
                   conf.level = 0.95)

# bonferroni multiple comparison correction
p.adjust(c(sign1$p.value, sign2$p.value, sign3$p.value),
         method = "bonferroni")

# mann-whitney test pair-wise comparison between groups
# AC
wil1 <- wilcox.test(x = eaatb$r_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                    y = eaatb$r_increase[eaatb$cond == 0 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)
# NC
wil2 <- wilcox.test(x = eaatb$r_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                    y = eaatb$r_increase[eaatb$cond == 0 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)
# Control
wil3 <- wilcox.test(x = eaatb$r_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                    y = eaatb$r_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)

# bonferroni multiple comparison correction
p.adjust(c(wil1$p.value, wil2$p.value, wil3$p.value),
         method = "bonferroni")


# histogram 
# control
ggplot(eaatb[eaatb$cond == 0,], aes(x = r, fill = is_post)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("gray 100", "gray20"))
# AC
ggplot(eaatb[eaatb$cond == 1,], aes(x = r, fill = is_post)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("gray 100", "gray20"))
# NC
ggplot(eaatb[eaatb$cond == 2,], aes(x = r, fill = is_post)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("gray 100", "gray20"))

##### Investigate ambiguous choices #####

# 2-way anova
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

# pre-intervention 1-way anova
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = a,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# increase, 1-way anova
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = a_increase,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# sign test compare pre- and post- within each group
# AC
sign1<- SIGN.test(x = eaatb$a[eaatb$is_post == 1 & eaatb$cond == 1],
                  y = eaatb$a[eaatb$is_post == 0 & eaatb$cond == 1],
                  alternative = "two.sided",
                  conf.level = 0.95)
# NC
sign2 <- SIGN.test(x = eaatb$a[eaatb$is_post == 1 & eaatb$cond == 2],
                   y = eaatb$a[eaatb$is_post == 0 & eaatb$cond == 2],
                   alternative = "two.sided",
                   conf.level = 0.95)
# Control
sign3 <- SIGN.test(x = eaatb$a[eaatb$is_post == 1 & eaatb$cond == 0],
                   y = eaatb$a[eaatb$is_post == 0 & eaatb$cond == 0],
                   alternative = "two.sided",
                   conf.level = 0.95)

# multiple comparison correction
p.adjust(c(sign1$p.value, sign2$p.value, sign3$p.value),
         method = "bonferroni")

# mann-whitney test pair-wise comparison between groups
# AC
wil1 <- wilcox.test(x = eaatb$a_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                    y = eaatb$a_increase[eaatb$cond == 0 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)
# NC
wil2 <- wilcox.test(x = eaatb$a_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                    y = eaatb$a_increase[eaatb$cond == 0 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)
# control
wil3 <- wilcox.test(x = eaatb$a_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                    y = eaatb$a_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)
# multiple comparison correction
p.adjust(c(wil1$p.value, wil2$p.value, wil3$p.value),
         method = "bonferroni")


# histogram
transp = 0.7
# control
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

# AC
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
  
# NC
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

##### Investigate Ellsberg paradox understanding score #####

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

# group comparison 1-way anova
ac_anova = ezANOVA(data=eaatbpre[!is.nan(eaatbpre$ep_score),], 
                   dv = ep_score,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

##### Investigate ambiguity attitude (model-based risk-corrected choice proportion of ambiguous lottery) #####

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

# increase, violin plot
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

# two-way anova
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

# pre-intervention, 1-way anova
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = a_r50,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# increase, 1-way anova
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = a_r50_increase,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# sign test compare pre- and post- within each group
# AC
sign1<- SIGN.test(x = eaatb$a_r50[eaatb$is_post == 1 & eaatb$cond == 1],
                  y = eaatb$a_r50[eaatb$is_post == 0 & eaatb$cond == 1],
                  alternative = "two.sided",
                  conf.level = 0.95)
# NC
sign2 <- SIGN.test(x = eaatb$a_r50[eaatb$is_post == 1 & eaatb$cond == 2],
                   y = eaatb$a_r50[eaatb$is_post == 0 & eaatb$cond == 2],
                   alternative = "two.sided",
                   conf.level = 0.95)
# control
sign3 <- SIGN.test(x = eaatb$a_r50[eaatb$is_post == 1 & eaatb$cond == 0],
                   y = eaatb$a_r50[eaatb$is_post == 0 & eaatb$cond == 0],
                   alternative = "two.sided",
                   conf.level = 0.95)
# multiple comparison correction
p.adjust(c(sign1$p.value, sign2$p.value, sign3$p.value),
         method = "bonferroni")

# mann-whitney test pair-wise comparison between groups
# AC
wil1 <- wilcox.test(x = eaatb$a_r50_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                    y = eaatb$a_r50_increase[eaatb$cond == 0 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)
# NC
wil2 <- wilcox.test(x = eaatb$a_r50_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                    y = eaatb$a_r50_increase[eaatb$cond == 0 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)
# control
wil3 <- wilcox.test(x = eaatb$a_r50_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                    y = eaatb$a_r50_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)
# multiple comparison correction
p.adjust(c(wil1$p.value, wil2$p.value, wil3$p.value),
         method = "bonferroni")

##### Investigate ambiguity attitude (model-free difference between choice proportion) #####

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

# 2-way anova
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

# 2-way anova
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


##### Risk and ambiguity attitudes correlation #####

# all subjects, pre intervention
ggplot(eaatbpre, aes(x=a_r50, y=r)) + 
  geom_point() +
  theme_classic()

cor.test(eaatbpre$a_r50, eaatbpre$r,
         method = c("pearson"))

#  AC and NC together, pre intervention
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

cor.test(eaatbpre$a_r50[!eaatbpost$cond==0], eaatbpre$r[!eaatbpost$cond==0],
         method = c("pearson"))


# AC and NC , post intervention
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


cor.test(eaatbpost$a_r50[!eaatbpost$cond==0], eaatbpost$r[!eaatbpost$cond==0], 
         method = c("pearson"))


# comparing pre and post intervention risk and ambiguity correlation, AC and NC
preACNC = eaatbpre[!eaatb$cond==0,]
postACNC = eaatbpost[!eaatb$cond==0,]
postAC = eaatb[!eaatb$cond==0,]
postNC = eaatb[!eaatb$cond==0,]

cocordata <- data.frame("pre_a_r50"=preACNC$a_r50, "pre_r"=preACNC$r,
                        "post_a_r50"=postACNC$a_r50, "post_r"=postACNC$r)


cocor(~pre_a_r50 + pre_r | post_a_r50 + post_r, cocordata,
      alternative ="two.sided", test="all",
      alpha = 0.05, return.htest = FALSE)

# correlation between attitudes change, AC and NC
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

cor.test(eaatbpost$a_r50_increase[!eaatbpost$cond==0],
         eaatbpost$r_increase[!eaatbpost$cond==0], 
         method = c("pearson"))

# correlation between pre-intervention attitude and change in attitude, AC and NC
# ambiguity attitude 
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

# risky attitude
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

##### Understanding Ellesberg paradox and change of ambiguity attitude correlation #####
# AC, 
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

# NC
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

cor.test(eaatbpre$ep_score[eaatbpost$cond==1],
         eaatbpre$beta_t_increase[eaatbpost$cond==1],
         method = c("spearman"))


##### ANOVA by linear model and post-hoc multiple comparison correction #####

# construct model, 2-way anova
# model-free ambiguity attitude 
model1 <- lme(a50_a24 ~ cond*is_post, random = ~is_post|id, na.action=na.omit, data = eaatb)
model1 <- lme(a74_a24 ~ cond*is_post, random = ~is_post|id, na.action=na.omit, data = eaatb)
# mode-based ambiguity atttiude
model1 <- lme(a_r50 ~ cond*is_post, random = ~is_post|id, na.action=na.omit, data = eaatb)
# risky choice
model1 <- lme(r ~ cond*is_post, random = ~is_post|id, na.action=na.omit, data = eaatb)

# construct model, 1-way anova
# understanding Ellsberg paradox
model2 <- lme(ep_score ~ cond, random = ~1|id, na.action=na.omit, data = eaatbpost)
# model-based ambiguity attitude change
model2 <- lme(a_r50_increase ~ cond, random = ~1|id, na.action=na.omit, data = eaatbpost)
# model-based ambiguity attitude change (includeing $5)
model2 <- lme(a_5in_r50_increase ~ cond, random = ~1|id, na.action=na.omit, data = eaatbpost)
# risky choice change
model2 <- lme(r_increase ~ cond, random = ~1|id, na.action=na.omit, data = eaatbpost)
# ambiguous choice change
model2 <- lme(a_increase ~ cond, random = ~1|id, na.action=na.omit, data = eaatbpost)
# pre-intervention model-based ambiguity attitude
model2 <- lme(a_r50 ~ cond, random = ~1|id, na.action=na.omit, data = eaatbpre)
# pre-intervention risky choice
model2 <- lme(r ~ cond, random = ~1|id, na.action=na.omit, data = eaatbpre)

# post-hoc comparison of means, require packages emmeans
model1.emm <- emmeans(model1,  ~ cond:is_post)
model2.emm <- emmeans(model2,  ~ cond)

# pair-wise contrast
contrast(model1.emm, simple = "each", adjust = 'fdr')
pairs(model1.emm, simple = "each", adjust="fdr")
contrast(model2.emm, simple = "each")
pairs(model2.emm, simple = "each", adjust = "fdr")


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

##### Investigate ambiguity attitude (model-based risk-corrected choice proportion of ambiguous lottery), calculated with $5 ambiguous trials included #####

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

# increase, violin plot
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


# two-way anova
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

# pre-intervention, 1-way anova
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

# increase, 1-way anova
ac_anova = ezANOVA(data=eaatbpre, 
                   dv = a_5in_r50_increase,
                   wid = .(id),
                   between = .(cond),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
ac_anova

# sign tests comparing pre- post- within each group
# AC
sign1<- SIGN.test(x = eaatb$a_5in_r50[eaatb$is_post == 1 & eaatb$cond == 1],
                  y = eaatb$a_5in_r50[eaatb$is_post == 0 & eaatb$cond == 1],
                  alternative = "two.sided",
                  conf.level = 0.95)
# NC
sign2 <- SIGN.test(x = eaatb$a_5in_r50[eaatb$is_post == 1 & eaatb$cond == 2],
                   y = eaatb$a_5in_r50[eaatb$is_post == 0 & eaatb$cond == 2],
                   alternative = "two.sided",
                   conf.level = 0.95)
# control
sign3 <- SIGN.test(x = eaatb$a_5in_r50[eaatb$is_post == 1 & eaatb$cond == 0],
                   y = eaatb$a_5in_r50[eaatb$is_post == 0 & eaatb$cond == 0],
                   alternative = "two.sided",
                   conf.level = 0.95)
# multiple comparison correction
p.adjust(c(sign1$p.value, sign2$p.value, sign3$p.value),
         method = "bonferroni")

# mann-whitney test, pair-wise comparison of change between groups
# AC
wil1 <- wilcox.test(x = eaatb$a_5in_r50_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                    y = eaatb$a_5in_r50_increase[eaatb$cond == 0 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)
# NC
wil2 <- wilcox.test(x = eaatb$a_5in_r50_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                    y = eaatb$a_5in_r50_increase[eaatb$cond == 0 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)
# control
wil3 <- wilcox.test(x = eaatb$a_5in_r50_increase[eaatb$cond == 1 & eaatb$is_post == 1],
                    y = eaatb$a_5in_r50_increase[eaatb$cond == 2 & eaatb$is_post == 1],
                    paired = FALSE, 
                    conf.level = 0.95)
# multiple comparison
p.adjust(c(wil1$p.value, wil2$p.value, wil3$p.value),
         method = "bonferroni")

##### Logistic regression GLM ####
# load choice data
data <- read.csv('trial_choice.csv')
group <- read.csv('subject_info.csv')

# organize
data$id <- as.factor(data$id)
data$is_post <- as.factor(data$is_post)
group$id <- as.factor(group$id)
group$cond <- as.factor(group$cond)
group$is_excluded <- as.factor(group$is_excluded)
group$gender <- as.factor(group$gender)
group$age_scale <- scale(group$age)
group$ep_score_scale[!is.na(group$ep_score)] <- scale(group$ep_score[!is.na(group$ep_score)])

choice_data <- merge(data, group, by = intersect(colnames(data), colnames(group)))

choice_data$is_risk[choice_data$ambig == 0] <- 1
choice_data$is_risk[choice_data$ambig != 0] <- 0

choice_data$is_risk <- as.factor(choice_data$is_risk)

colnames(choice_data)

# z-score
prob_unique <- unique(choice_data$prob[choice_data$is_risk == 1])
ambig_unique <- unique(choice_data$ambig[choice_data$is_risk == 0])
val_unique <- unique(choice_data$val)

prob_unique_scale <- scale(prob_unique)
ambig_unique_scale <- scale(ambig_unique)
val_unique_scale <- scale(val_unique)

# create scaled columns
choice_data$val_scale <- choice_data$val
choice_data$prob_scale <- choice_data$prob
choice_data$ambig_scale <- choice_data$ambig

# replace with scaled values
for (i in val_unique) {
  choice_data$val_scale[choice_data$val == i] <- val_unique_scale[val_unique == i]
}

for (j in prob_unique) {
  choice_data$prob_scale[choice_data$prob == j] <- prob_unique_scale[prob_unique == j]
}

for (k in ambig_unique) {
  choice_data$ambig_scale[choice_data$ambig == k] <- ambig_unique_scale[ambig_unique == k]
}

# GLM, risky trials, excluding $5
logit_model <- glmer(choice ~ is_post + cond + is_post*cond + prob_scale + val_scale + ep_score_scale + ep_score_scale*cond + age_scale + gender + (1 + is_post|id),
                     data = choice_data[choice_data$is_excluded == 0 & !choice_data$choice == 2 & choice_data$is_risk == 1 & !is.na(choice_data$ep_score) & choice_data$val != 5, ],
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa")
)

print(summary(logit_model), digits = 8)
testInteractions(logit_model, pairwise=c("is_post","cond"))

# GLM, ambiguous trials, excluding $5
logit_model_amb <- glmer(choice ~ is_post + cond + is_post*cond + ambig_scale + val_scale + ep_score_scale + ep_score_scale*cond + age_scale + gender + (1 + is_post|id),
                         data = choice_data[choice_data$is_excluded == 0 & !choice_data$choice == 2 & choice_data$is_risk == 0 & !is.na(choice_data$ep_score) & choice_data$val != 5, ],
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa")
) 

print(summary(logit_model_amb), digits = 6)
print(testInteractions(logit_model_amb, pairwise=c("is_post","cond")), digit = 6)
