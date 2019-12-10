##### Packages ####
library("ggplot2")
install.packages("xlsx")
library(xlsx)

##### Load data ####
path = "D:/Ruonan/Projects in the lab/Ellen Ambig Avers/Data"
setwd(path)

nonpar_file = file.path(path, "feedback_analysis_nonPar_03112019.csv")
par_risk_file = file.path(path, "feedback_All_Par_RiskOnly_03112019.csv")
parcon_risk_file = file.path(path, "feedback_All_ParCon_RiskOnly_03112019.csv")
par_file = file.path(path, "feedback_All_Par_03112019.csv")
parcon_file = file.path(path, "feedback_All_ParCon_03112019.csv")
choiceprob_file = file.path(path, "feedback_All_modeled_choiceProb_uncon_03142019.csv")
choiceprob_con_file = file.path(path, "feedback_All_modeled_choiceProb_con_03142019.csv")

nonpar = read.csv(nonpar_file, header = TRUE)
par_risk = read.csv(par_risk_file, header = TRUE)
parcon_risk = read.csv(parcon_risk_file, header = TRUE)
par = read.csv(par_file, header = TRUE)
parcon = read.csv(parcon_file, header = TRUE)
choiceprob = read.csv(choiceprob_file, header = TRUE)
choiceprob_con = read.csv(choiceprob_con_file, header = TRUE)

#change the column name of fitted parameters using risky trials only,
# to differentiate from fittings from both risk and ambiguous trials 
colnames(par_risk) <- c("id", "is_post", "gamma_risk", "alpha_risk", 
                        "LL_risk", "LL0_risk", "r2_risk", "AIC_risk", "BIC_risk")

colnames(parcon_risk) <- c("id", "is_post", "gamma_risk", "alpha_risk", 
                           "LL_risk", "LL0_risk", "r2_risk", "AIC_risk", "BIC_risk")

# add a column identifying constrained fitting
# fit both risk and ambiguity
par$is_constrained = 0
parcon$is_constrained = 1
par_risk$is_constrained = 0
parcon_risk$is_constrained = 1
choiceprob$is_constrained = 0
choiceprob_con$is_constrained = 1

# combine constrained and unconstrained fitting
uncon_con = rbind(par, parcon)
uncon_con_risk = rbind(par_risk, parcon_risk)
uncon_con_choice = rbind(choiceprob, choiceprob_con)

# combine all model fitting results
all_par = merge(uncon_con, uncon_con_risk, by = intersect(names(uncon_con_risk), names(uncon_con)))
all_par_choice = merge(all_par, uncon_con_choice, by = intersect(names(all_par), names(uncon_con_choice)))
all = merge(nonpar, all_par_choice, by = intersect(names(nonpar), names(all_par_choice)))

# factorize
all$is_excluded = as.factor(all$is_excluded)
all$is_post = as.factor(all$is_post)
all$cond = as.factor(all$cond)
all$is_constrained = as.factor(all$is_constrained)
all$id = as.factor(all$id)

# calculate the transformed alpha and beta
all$alpha_t = all$alpha - 1
all$beta_t = - all$beta
all$alpha_risk_t = all$alpha_risk - 1

# calculate ambiguity trials choice prob based on the modeled risk choice prob
all$r50 = rowMeans(cbind(all$r50_9.5, all$r50_18, all$r50_34, all$r50_65))
all$a_r50 = all$a - all$r50

# sort data frame, phase, constrained, or id
all <- all[
  with(all, order(is_post, is_constrained, id)),
  ]

# check sorting
# phase
ggplot(all, aes(x = 1:nrow(all), y = is_post)) +
  geom_point()

# constrained or not 
ggplot(all, aes(x = 1:nrow(all), y = is_constrained)) +
  geom_point()

# subject ID
plot(all$id[all$is_post == 0], all$id[all$is_post == 1])
plot(all$id[all$is_post == 0 & all$is_constrained == 0], all$id[all$is_post == 1 & all$is_constrained == 0])
plot(all$id[all$is_post == 0 & all$is_constrained == 1], all$id[all$is_post == 1 & all$is_constrained == 1])

# calculate the increase
alpha_t_increase = all$alpha_t[all$is_post == 1] - all$alpha_t[all$is_post == 0]
alpha_risk_t_increase = all$alpha_risk_t[all$is_post == 1] - all$alpha_risk_t[all$is_post == 0]
beta_t_increase = all$beta_t[all$is_post == 1] - all$beta_t[all$is_post == 0]
gamma_increase = all$gamma[all$is_post == 1] - all$gamma[all$is_post == 0]
gamma_risk_increase = all$gamma_risk[all$is_post == 1] - all$gamma_risk[all$is_post == 0]
a_r50_increase = all$a_r50[all$is_post == 1] - all$a_r50[all$is_post == 0]
# save them to data frame
all$alpha_t_increase = c(alpha_t_increase, alpha_t_increase)
all$alpha_risk_t_increase = c(alpha_risk_t_increase, alpha_risk_t_increase)
all$beta_t_increase = c(beta_t_increase, beta_t_increase)
all$gamma_increase = c(gamma_increase, gamma_increase)
all$gamma_risk_increase = c(gamma_risk_increase, gamma_risk_increase)
all$a_r50_increase = c(a_r50_increase, a_r50_increase)

# check increase match between pre and post intervention
pre = all$a_r50_increase[all$is_post == 0]
post = all$a_r50_increase[all$is_post == 1]
plot(pre, post)

# save data frame
save(all, file = "feedback_all_03232019.rda")

write.csv(all, file = "check_table.csv")


# check 
eaatb = all
# select constrained
eaatb <- eaatb[eaatb$is_excluded == 0 & eaatb$is_constrained == 1,]
# select unconstrained
eaatb <- eaatb[eaatb$is_excluded == 0 & eaatb$is_constrained == 0,]

eaatbpre = eaatb[eaatb$is_post == 0,]
eaatbpost = eaatb[eaatb$is_post == 1,]

plot(eaatbpre$a_r50_increase, eaatbpost$a_r50_increase)
plot(eaatbpre$r_increase, eaatbpost$r_increase)
idmatch = data.frame("preid"=eaatbpre$id, "postid"=eaatbpost$id)
View(idmatch)

plot(eaatbpre$a_r50_increase, eaatbpost$a_r50_increase)


# combine additional stats: rt, choice with $5 trials included
path = "D:/Ruonan/Projects in the lab/Ellen Ambig Avers/Data"
setwd(path)

load("feedback_all_03232019.rda")


View(all)
rt_5in <- read.csv("feedback_AllSubj_rtByUncert_5in_11012019.csv", header = TRUE)
colnames(rt_5in)
rt_5in$rt_r_5in <- rowMeans(cbind(rt_5in$rt_r38_5in, rt_5in$rt_r25_5in, rt_5in$rt_r13_5in))
rt_5in$rt_a_5in <- rowMeans(cbind(rt_5in$rt_a24_5in, rt_5in$rt_a50_5in, rt_5in$rt_a74_5in))

rt_5out <- read.csv("feedback_AllSubj_rtByUncert_5out_11112019.csv", header = TRUE)
View(rt_5out)
colnames(rt_5out)
rt_5out$rt_r_5out <- rowMeans(cbind(rt_5out$rt_r38_5out, rt_5out$rt_r25_5out, rt_5out$rt_r13_5out))
rt_5out$rt_a_5out <- rowMeans(cbind(rt_5out$rt_a24_5out, rt_5out$rt_a50_5out, rt_5out$rt_a74_5out))

nonpar_5in <- read.csv("feedback_AllSubj_nonparByUncert_5in_11012019.csv", header = TRUE)
View(nonpar_5in)
colnames(nonpar_5in)
nonpar_5in$r_5in <- rowMeans(cbind(nonpar_5in$r38_5in, nonpar_5in$r25_5in, nonpar_5in$r13_5in))
nonpar_5in$a_5in <- rowMeans(cbind(nonpar_5in$a24_5in, nonpar_5in$a50_5in, nonpar_5in$a74_5in))

# data1 <- merge(rt_5in, rt_5out, by = c("id", "is_post"))
data1 <- rt_5out
View(data1)

data2 <- merge(data1, nonpar_5in, by = c("id", "is_post"))
View(data2)

data2$id <- as.factor(data2$id)
data2$is_post <- as.factor(data2$is_post)

all1 <- merge(all, data2, by = c("id", "is_post"))
View(all1)

save(all1, file = "feedback_all_11112019.rda")

eaatb <- all1
# calculate a_5in_r50
eaatb$a_5in_r50 <- eaatb$a_5in - eaatb$r50
eaatb <- eaatb[
  with(eaatb, order(is_post, is_constrained, id)),
  ]

a_5in_r50_increase <- eaatb$a_5in_r50[eaatb$is_post == 1] - eaatb$a_5in_r50[eaatb$is_post == 0]
length(a_5in_r50_increase)

eaatb$a_5in_r50_increase = c(a_5in_r50_increase, a_5in_r50_increase)

plot(as.numeric(eaatb$id[eaatb$is_post == 0]), as.numeric(eaatb$id[eaatb$is_post == 1]))

save(eaatb, file = "feedback_all_1111022019.rda")

