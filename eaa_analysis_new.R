##### Install packages ######
# install.packages("ggplot2")
library("ggplot2")

# installed.packages("car")
library('car')

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

##### Load and select data ####

path = "D:/Ruonan/Projects in the lab/Ellen Ambig Avers/Data"
setwd(path)

load("feedback_all_03142019.rda")

# rename
eaatb = all

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

# Subjects with extreme values
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


##### Investigate risky choices #####
 
ggplot(eaatb[eaatb$cond == 0,], aes(x = r, fill = is_post)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("gray 100", "gray20"))

ggplot(eaatb[eaatb$cond == 1,], aes(x = r, fill = is_post)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("gray 100", "gray20"))

ggplot(eaatb[eaatb$cond == 2,], aes(x = r, fill = is_post)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("gray 100", "gray20"))

##### Investigate ambigous choices #####

ggplot(eaatb[eaatb$cond == 0,], aes(x = a, fill = is_post)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7, position = "identity") +
  scale_fill_grey(start = 0.3, end = 1) +
  scale_x_continuous(limits = c(-0.2, 1.2), breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_y_continuous(limits = c(0, 12)) +
  theme_classic()

ggplot(eaatb[eaatb$cond == 1,], aes(x = a, fill = is_post, color = is_post)) +
  # scale_fill_manual(values = c("white", "gray20")) +
  scale_fill_grey(start = 0.3, end = 1) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7, position = "identity") +
  scale_x_continuous(limits = c(-0.2, 1.2), breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_y_continuous(limits = c(0, 12)) +
  theme_classic()
  

ggplot(eaatb[eaatb$cond == 2,], aes(x = a, fill = is_post)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7, position = "identity") +
  scale_fill_grey(start = 0.3, end = 1) +
  scale_x_continuous(limits = c(-0.2, 1.2), breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_y_continuous(limits = c(0, 12)) +
  theme_classic()

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
data_sum = data_summary(eaatb[eaatb$is_post == 0,],
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
  theme(axis.text = element_text(size = 12, color = "black"))

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
  geom_violin(trim=FALSE, size=1) + # not trimed
  geom_dotplot(binaxis='y', binwidth = 0.035, stackdir='center',position=position_dodge(0.9)) + # add dots
  scale_fill_manual(values = c("gray 90", "gray55")) +
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red", position = position_dodge(0.9)) +
  # geom_boxplot(width = 0.1) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(limits=c(-1.0, 0.5), breaks = c(-1.0,-0.5, 0.0, 0.5)) +
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
  geom_violin(trim=FALSE, size=1, fill = "white") + # not trimed
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red") +
  # geom_boxplot(width = 0.1) + # add box plot
  scale_x_discrete(limits = c("1", "2", "0"), labels = c("1"="AC", "2" ="NC", "0" = "Control")) +
  scale_y_continuous(limits=c(-0.5, 1.0), breaks = c(-.5, 0.0, 0.5, 1.0)) +
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


##### Risk and ambiguity attitudes correlation #####
ggplot(eaatbpre[!eaatbpre$cond==0, ], aes(x=alpha_t, y=beta_t)) + 
  geom_point() +
  theme_classic()

cor.test(eaatbpre$alpha_t[!eaatbpost$cond==0], eaatbpre$beta_t[!eaatbpost$cond==0],
         method = c("pearson", "kendall", "spearman"))

ggplot(eaatbpost[!eaatbpost$cond==0, ], aes(x=alpha_t, y=beta_t)) + 
  geom_point() +
  theme_classic()

cor.test(eaatbpost$alpha_t[!eaatbpost$cond==0], eaatbpost$beta_t[!eaatbpost$cond==0], 
         method = c("pearson", "kendall", "spearman"))


ggplot(eaatbpost[!eaatbpost$cond==0, ], aes(x=alpha_t_increase, y=beta_t_increase)) + 
  geom_point() +
  theme_classic()

cor.test(eaatbpost$alpha_t_increase[!eaatbpost$cond==0], eaatbpost$beta_t_increase[!eaatbpost$cond==0], 
         method = c("pearson", "kendall", "spearman"))
