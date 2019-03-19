##### Packages ####
library("ggplot2")

##### Local function ####
data_summary <- function(data, varname, groupnames){
  # Function to calculate the mean and the standard deviation
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

##### Load data ####
path = "D:/Ruonan/Projects in the lab/Ellen Ambig Avers/Data"
setwd(path)

load("feedback_all_03142019.rda")

##### Histogram and scatter of pre and post intervention ####
data2plot = all[all$is_post == 0 & all$is_excluded == 0, ]
data2plot = all[all$is_post == 1 & all$is_excluded == 0, ]
data2plot = all[all$is_excluded == 0 & !is.element(all$id, id_out_alpha), ]
data2plot = all[all$is_excluded == 0 & is.element(all$id, id_out_alpha), ]
data2plot = all[all$is_excluded == 0, ]
data2plot = all[all$is_excluded == 0 & all$id != 2506 & all$id != 1518, ]
data2plot = all[all$is_excluded == 0 & all$cond == 0, ]
data2plot = all[all$is_excluded == 0 & all$cond == 1, ]
data2plot = all[all$is_excluded == 0 & all$cond == 2, ]
data2plot = all[all$is_excluded == 0 & all$cond != 0, ]

# subjects fitting out of range
id_out_alpha_pre = data2plot[data2plot$alpha < 0.1070 | data2plot$alpha > 2.0987,]$id
id_out_alpha_post= data2plot[data2plot$alpha < 0.1070 | data2plot$alpha > 2.0987,]$id
id_out_alpha = unique(c(id_out_alpha_pre, id_out_alpha_post))
id_out_alpha = data2plot[data2plot$alpha > 2.0987,]$id

id_extreme_gamma = data2plot[data2plot$gamma > 10000,]$id
id_extreme_gamma = data2plot[data2plot$gamma < -100000,]$id

# histograms
ggplot(data2plot, aes(x = alpha, color = is_post)) +  
  geom_histogram(binwidth = 0.1,  fill = "white", alpha = 0.2, position = "identity") +
  ggtitle("Alpha")

ggplot(data2plot, aes(x = beta, color = is_post)) +  
  geom_histogram(binwidth = 0.1,  fill = "white", alpha = 0.2, position = "identity") +
  ggtitle("Beta")

ggplot(data2plot, aes(x = gamma, color = is_post)) +  
  geom_histogram(bins = 200, fill = "white", alpha = 0.2, position = "identity") +
  ggtitle("Gamma")

ggplot(data2plot, aes(x = alpha_con, color = is_post)) +  
  geom_histogram(binwidth = 0.02,  fill = "white", alpha = 0.2, position = "identity") +
  ggtitle("Alpha constrained")

ggplot(data2plot, aes(x = beta_con, color = is_post)) +  
  geom_histogram(binwidth = 0.02,  fill = "white", alpha = 0.2, position = "identity") +
  ggtitle("Beta constrained")

ggplot(data2plot, aes(x = gamma_con, color = is_post)) +  
  geom_histogram(binwidth = 0.1,  fill = "white", alpha = 0.2, position = "identity") +
  ggtitle("Gamma constrained")

# scatter plots
ggplot(data2plot, aes(x = alpha, y = alpha_con)) +  
  geom_point(aes(color = is_post)) +
  ggtitle("Constrained and unconstrained alpha")

ggplot(data2plot, aes(x = gamma, y = gamma_con)) +  
  geom_point(aes(color = is_post)) +
  ggtitle("Constrained and unconstrained gamma")

ggplot(data2plot, aes(x = r2, y = r2_con)) +  
  geom_point(aes(color = is_post)) +
  ggtitle("Constrained and unconstrained Pseudo R2")

##### Histogram and scatter comparing constrained and unconstrained ####
data2plot = all[all$is_post == 0 & all$is_excluded == 0, ]
data2plot = all[all$is_post == 1 & all$is_excluded == 0, ]
data2plot = all[all$is_excluded == 0, ]
data2plot = all[all$is_excluded == 0 & all$cond == 0, ]
data2plot = all[all$is_excluded == 0 & all$cond == 1, ]
data2plot = all[all$is_excluded == 0 & all$cond == 2, ]
data2plot = all[all$is_excluded == 0 & all$cond != 0, ]

data2plot = all[all$is_excluded == 0 & !is.element(all$id, id_out_alpha), ]
data2plot = all[all$is_excluded == 0 & is.element(all$id, id_out_alpha), ]

data2plot = all[all$is_excluded == 0 & !is.element(all$id, id_out), ]
data2plot = all[all$is_excluded == 0 & is.element(all$id, id_out), ]

# histograms
ggplot(data2plot, aes(x = alpha, color = is_constrained)) +  
  geom_histogram(binwidth = 0.05,  fill = "white", alpha = 0.2, position = "identity") +
  ggtitle("Alpha")

ggplot(data2plot, aes(x = alpha, color = is_constrained)) +  
  geom_histogram(binwidth = 0.05,  fill = "white", alpha = 0.2, position = "identity") +
  ggtitle("Alpha")

ggplot(data2plot, aes(x = beta, color = is_constrained)) +  
  geom_histogram(binwidth = 0.1,  fill = "white", alpha = 0.2, position = "identity") +
  ggtitle("Beta")

ggplot(data2plot, aes(x = gamma, color = is_constrained)) +  
  geom_histogram(binwidth = 0.1,  fill = "white", alpha = 0.2, position = "identity") +
  ggtitle("Gamma")

# scatter plots compareing constrained and unconstrained
# reorganize data frame first
data = all[all$is_excluded == 0 ,]
uncon_con = data.frame("id" = data$id[data$is_constrained == 0],
                       "cond" = data$cond[data$is_constrained == 0],
                       "is_post" = data$is_post[data$is_constrained == 0],
                       "alpha_con" = data$alpha[data$is_constrained == 1],
                       "beta_con" = data$beta[data$is_constrained == 1],
                       "gamma_con" = data$gamma[data$is_constrained == 1],
                       "r2_con" = data$r2[data$is_constrained == 1],
                       "alpha" = data$alpha[data$is_constrained == 0],
                       "beta" = data$beta[data$is_constrained == 0],
                       "gamma" = data$gamma[data$is_constrained == 0],
                       "r2" = data$r2[data$is_constrained == 0])

data2plot = uncon_con[is.element(uncon_con$id, id_out),]
data2plot = uncon_con[!is.element(uncon_con$id, id_out),]

# scatter
ggplot(data2plot, aes(x = r2_con, y = r2)) + 
  geom_point(aes(color = is_post)) +
  ggtitle("Constrained and unconstrained Pseudo R2")

ggplot(data2plot, aes(x = gamma_con, y = gamma)) + 
  geom_point(aes(color = is_post)) +
  ggtitle("Constrained and unconstrained Gamma")

ggplot(data2plot, aes(x = alpha_con, y = alpha)) + 
  geom_point(aes(color = is_post)) +
  ggtitle("Constrained and unconstrained Alpha")

ggplot(data2plot, aes(x = beta_con, y = beta)) + 
  geom_point(aes(color = is_post)) +
  ggtitle("Constrained and unconstrained Beta")

# bar graphs
data_sum = data_summary(data2plot, varname = "r2", groupnames = c("is_post", "is_constrained"))

ggplot(data=data_sum,aes(x=is_post, y=r2, fill=is_constrained)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=r2-sd, ymax=r2+sd), width=0.2, position=position_dodge(0.9)) +
  ggtitle("Constrained and unconstrained Pseudo R2")


##### Model fitting quality ####
data2plot = all[all$is_excluded == 0 & all$is_constrained == 1, ]

# inconsistency and R2
ggplot(data2plot, aes(x = r2, y = r5)) +  
  geom_point(aes(color = is_post)) +
  ggtitle("R squared and error")

ggplot(data2plot, aes(x = r2, y = inconsist)) +  
  geom_point(aes(color = is_post)) +
  ggtitle("R squared and inconsistency")

ggplot(data2plot, aes(x = r2, y = inconsist)) +  
  geom_point(aes(color = cond, shape = is_post)) +
  ggtitle("R square and inconsistency")

# identify subjects whose unconstrained fits were bad
data_uncon = all[all$is_excluded == 0 & all$is_constrained == 0 & all$is_post == 0,]
data_uncon = all[all$is_excluded == 0 & all$is_constrained == 0 & all$is_post == 1,]

# alpha out of range
id_out_alpha_pre = data_uncon[data_uncon$alpha < 0.1070 | data_uncon$alpha > 2.0987,]$id
id_out_alpha_post = data_uncon[data_uncon$alpha < 0.1070 | data_uncon$alpha > 2.0987,]$id
id_out_alpha = unique(c(id_out_alpha_pre, id_out_alpha_post))

id_out_beta_pre = data_uncon[data_uncon$beta < 0.0897 | data_uncon$beta > 4.1475,]$id
id_out_beta_post = data_uncon[data_uncon$beta < 0.0897 | data_uncon$beta > 4.1475,]$id
id_out_beta = unique(c(id_out_beta_pre, id_out_beta_post))

id_out = unique(c(id_out_alpha, id_out_beta))

##### Error ####