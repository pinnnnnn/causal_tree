#install.packages("devtools")
library(devtools) 
install_github("susanathey/causalTree")
library(causalTree)
library(dplyr)
library(ggplot2)
setwd("G:/causal_tree/simulation_study3_large_sample_norm")

#read in source data
source_data <- read.csv('G:/causal_tree/simulation_study3_large_sample_norm/large_norm.csv', header = TRUE, sep = ',')

#train test split
smp_size <- floor(0.625 * nrow(source_data))
set.seed(123)
train_ind <- sample(seq_len(nrow(source_data)), size = smp_size)

cf_plot <- function(test_set){
  p <- ggplot(test_set, aes(x=x1, y=pred)) +
    geom_point(shape=1) +
    geom_smooth(method=lm) + 
    geom_line(data = test_set, aes(x = x1, y = true_effect), color = 'red') +
    ylim(c(-1.5,1.5)) + 
    xlim(c(-6,6))  
  
  return(p)
}


#Design 1, k = 2
d1_data_k2 <- source_data %>% 
                select(x1, x2, treatment, error) %>% 
                mutate(y = (x1/2+x2) + (1/2)*(2*treatment -1)*(x1/2) + error,
                       true_effect = x1/2)
d1_train_k2 <- d1_data_k2[train_ind, ]
d1_test_k2 <- d1_data_k2[-train_ind, ]

cf_d1_k2 <- causalForest(as.formula(paste("y~","x1+x2")),
                           data=d1_train_k2, treatment=d1_train_k2$treatment,
                           split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                           bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                           split.alpha = 0.5, cv.alpha = 0.5,
                           sample.size.total = 5000, sample.size.train.frac = .5,
                           mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                           ncolx=2, ncov_samp=1)

cfpredtest <- predict(cf_d1_k2, newdata=d1_test_k2, type="vector")
d1_test_k2$pred <- cfpredtest

d1k2 <- cf_plot(d1_test_k2)
ggsave (paste0("result\\d1k2.png"), device = "png")

#Design 1, k = 3
d1_data_k3 <- source_data %>% 
  select(x1, x2, x3, treatment, error) %>% 
  mutate(y = (x1/2+x2+x3) + (1/2)*(2*treatment -1)*(x1/2) + error,
         true_effect = x1/2)
d1_train_k3 <- d1_data_k3[train_ind, ]
d1_test_k3 <- d1_data_k3[-train_ind, ]

cf_d1_k3 <- causalForest(as.formula(paste("y~","x1+x2+x3")),
                         data=d1_train_k3, treatment=d1_train_k3$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 5000, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=3, ncov_samp=2)

cfpredtest <- predict(cf_d1_k3, newdata=d1_test_k3, type="vector")
d1_test_k3$pred <- cfpredtest

d1k3 <- cf_plot(d1_test_k3)
ggsave (paste0("result\\d1k3.png"), device = "png")

#Design 1, k = 5
d1_data_k5 <- source_data %>% 
  select(x1, x2, x3, x4, x5, treatment, error) %>% 
  mutate(y = (x1/2+x2+x3+x4+x5) + (1/2)*(2*treatment -1)*(x1/2) + error,
         true_effect = x1/2)
d1_train_k5 <- d1_data_k5[train_ind, ]
d1_test_k5 <- d1_data_k5[-train_ind, ]

cf_d1_k5 <- causalForest(as.formula(paste("y~","x1+x2+x3+x4+x5+x6+x7+x8+x9+x10")),
                          data=d1_train_k5, treatment=d1_train_k5$treatment,
                          split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                          bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                          split.alpha = 0.5, cv.alpha = 0.5,
                          sample.size.total = 5000, sample.size.train.frac = .5,
                          mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                          ncolx=5, ncov_samp=3)

cfpredtest <- predict(cf_d1_k5, newdata=d1_test_k5, type="vector")
d1_test_k5$pred <- cfpredtest

cf_plot(d1_test_k5)


#Design 1, k = 10
d1_data_k10 <- source_data %>% 
  select(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, treatment, error) %>% 
  mutate(y = (x1/2+x2+x3+x4+x5+x6+x7+x8+x9+x10) + (1/2)*(2*treatment -1)*(x1/2) + error,
         true_effect = x1/2)
d1_train_k10 <- d1_data_k10[train_ind, ]
d1_test_k10 <- d1_data_k10[-train_ind, ]

cf_d1_k10 <- causalForest(as.formula(paste("y~","x1+x2+x3+x4+x5+x6+x7+x8+x9+x10")),
                         data=d1_train_k10, treatment=d1_train_k10$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 5000, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=10, ncov_samp=6)

cfpredtest <- predict(cf_d1_k10, newdata=d1_test_k10, type="vector")
d1_test_k10$pred <- cfpredtest

cf_plot(d1_test_k10)



############# Design 2


cf_plot <- function(test_set){
  ggplot(test_set, aes(x=x1, y=pred)) +
    geom_point(shape=1) +
    geom_smooth(method=loess) + 
    geom_smooth(data = test_set, aes(x = x1, y = true_effect), color = 'red') +
    ylim(c(-30,30)) #+ 
    #xlim(c(-6,6))  
}

#Design 2, k = 2
d1_data_k2 <- source_data %>% 
  select(x1, x2, treatment, error) %>% 
  mutate(y = (x1/2+x2) + (1/2)*(2*treatment -1)*(x1^3) + error,
         true_effect = x1^3)
d1_train_k2 <- d1_data_k2[train_ind, ]
d1_test_k2 <- d1_data_k2[-train_ind, ]

cf_d1_k2 <- causalForest(as.formula(paste("y~","x1+x2")),
                         data=d1_train_k2, treatment=d1_train_k2$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 5000, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=2, ncov_samp=1)

cfpredtest <- predict(cf_d1_k2, newdata=d1_test_k2, type="vector")
d1_test_k2$pred <- cfpredtest

cf_plot(d1_test_k2)

#Design 2, k = 3
d1_data_k3 <- source_data %>% 
  select(x1, x2, x3, treatment, error) %>% 
  mutate(y = (x1/2+x2+x3) + (1/2)*(2*treatment -1)*(x1^3) + error,
         true_effect = x1^3)
d1_train_k3 <- d1_data_k3[train_ind, ]
d1_test_k3 <- d1_data_k3[-train_ind, ]

cf_d1_k3 <- causalForest(as.formula(paste("y~","x1+x2+x3")),
                         data=d1_train_k3, treatment=d1_train_k3$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 5000, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=3, ncov_samp=2)

cfpredtest <- predict(cf_d1_k3, newdata=d1_test_k3, type="vector")
d1_test_k3$pred <- cfpredtest

cf_plot(d1_test_k3)


#Design 2, k = 5
d1_data_k5 <- source_data %>% 
  select(x1, x2, x3, x4, x5, treatment, error) %>% 
  mutate(y = (x1/2+x2+x3+x4+x5) + (1/2)*(2*treatment -1)*(x1^3) + error,
         true_effect = x1^3)
d1_train_k5 <- d1_data_k5[train_ind, ]
d1_test_k5 <- d1_data_k5[-train_ind, ]

cf_d1_k5 <- causalForest(as.formula(paste("y~","x1+x2+x3+x4+x5")),
                          data=d1_train_k5, treatment=d1_train_k5$treatment,
                          split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                          bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                          split.alpha = 0.5, cv.alpha = 0.5,
                          sample.size.total = 5000, sample.size.train.frac = .5,
                          mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                          ncolx=5, ncov_samp=3)

cfpredtest <- predict(cf_d1_k5, newdata=d1_test_k5, type="vector")
d1_test_k5$pred <- cfpredtest

cf_plot(d1_test_k5)

#Design 2, k = 10
d1_data_k10 <- source_data %>% 
  select(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, treatment, error) %>% 
  mutate(y = (x1/2+x2+x3+x4+x5+x6+x7+x8+x9+x10) + (1/2)*(2*treatment -1)*(x1^3) + error,
         true_effect = x1^3)
d1_train_k10 <- d1_data_k10[train_ind, ]
d1_test_k10 <- d1_data_k10[-train_ind, ]

cf_d1_k10 <- causalForest(as.formula(paste("y~","x1+x2+x3+x4+x5+x6+x7+x8+x9+x10")),
                          data=d1_train_k10, treatment=d1_train_k10$treatment,
                          split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                          bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                          split.alpha = 0.5, cv.alpha = 0.5,
                          sample.size.total = 5000, sample.size.train.frac = .5,
                          mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                          ncolx=10, ncov_samp=6)

cfpredtest <- predict(cf_d1_k10, newdata=d1_test_k10, type="vector")
d1_test_k10$pred <- cfpredtest

cf_plot(d1_test_k10)


########### design 3
cf_plot <- function(test_set){
  ggplot(test_set, aes(x=x1, y=pred)) +
    geom_point(shape=1) +
    geom_smooth(method=loess) + 
    geom_smooth(data = test_set, aes(x = x1, y = true_effect), color = 'red') +
    ylim(c(0,3)) #+ 
  #xlim(c(-6,6))  
}

#Design 3, k = 2
d1_data_k2 <- source_data %>% 
  select(x1, x2, treatment, error) %>% 
  mutate(x1_pos = ifelse(x1 > 0, x1, 0)) %>% 
  mutate(y = (x1/2+x2) + (1/2)*(2*treatment -1)*x1_pos + error,
         true_effect = x1_pos)
d1_train_k2 <- d1_data_k2[train_ind, ]
d1_test_k2 <- d1_data_k2[-train_ind, ]

cf_d1_k2 <- causalForest(as.formula(paste("y~","x1+x2")),
                         data=d1_train_k2, treatment=d1_train_k2$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 5000, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=2, ncov_samp=1)

cfpredtest <- predict(cf_d1_k2, newdata=d1_test_k2, type="vector")
d1_test_k2$pred <- cfpredtest

cf_plot(d1_test_k2)

#Design 3, k = 3
d1_data_k3 <- source_data %>% 
  select(x1, x2, x3, treatment, error) %>% 
  mutate(x1_pos = ifelse(x1 > 0, x1, 0)) %>% 
  mutate(y = (x1/2+x2+x3) + (1/2)*(2*treatment -1)*(x1_pos) + error,
         true_effect = x1_pos)
d1_train_k3 <- d1_data_k3[train_ind, ]
d1_test_k3 <- d1_data_k3[-train_ind, ]

cf_d1_k3 <- causalForest(as.formula(paste("y~","x1+x2+x3")),
                         data=d1_train_k3, treatment=d1_train_k3$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 5000, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=3, ncov_samp=2)

cfpredtest <- predict(cf_d1_k3, newdata=d1_test_k3, type="vector")
d1_test_k3$pred <- cfpredtest

cf_plot(d1_test_k3)

#Design 3, k = 5
d1_data_k5 <- source_data %>% 
  select(x1, x2, x3, x4, x5, treatment, error) %>% 
  mutate(x1_pos = ifelse(x1 > 0, x1, 0)) %>%
  mutate(y = (x1/2+x2+x3+x4+x5) + (1/2)*(2*treatment -1)*(x1_pos) + error,
         true_effect = x1_pos)
d1_train_k5 <- d1_data_k5[train_ind, ]
d1_test_k5 <- d1_data_k5[-train_ind, ]

cf_d1_k5 <- causalForest(as.formula(paste("y~","x1+x2+x3+x4+x5")),
                         data=d1_train_k5, treatment=d1_train_k5$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 5000, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=5, ncov_samp=3)

cfpredtest <- predict(cf_d1_k5, newdata=d1_test_k5, type="vector")
d1_test_k5$pred <- cfpredtest

cf_plot(d1_test_k5)

#Design 3, k = 10
d1_data_k10 <- source_data %>% 
  select(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, treatment, error) %>% 
  mutate(x1_pos = ifelse(x1 > 0, x1, 0)) %>%
  mutate(y = (x1/2+x2+x3+x4+x5+x6+x7+x8+x9+x10) + (1/2)*(2*treatment -1)*(x1_pos) + error,
         true_effect = x1_pos)
d1_train_k10 <- d1_data_k10[train_ind, ]
d1_test_k10 <- d1_data_k10[-train_ind, ]

cf_d1_k10 <- causalForest(as.formula(paste("y~","x1+x2+x3+x4+x5+x6+x7+x8+x9+x10")),
                          data=d1_train_k10, treatment=d1_train_k10$treatment,
                          split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                          bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                          split.alpha = 0.5, cv.alpha = 0.5,
                          sample.size.total = 5000, sample.size.train.frac = .5,
                          mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                          ncolx=10, ncov_samp=6)

cfpredtest <- predict(cf_d1_k10, newdata=d1_test_k10, type="vector")
d1_test_k10$pred <- cfpredtest

cf_plot(d1_test_k10)


########### design 4
cf_plot <- function(test_set){
  ggplot(test_set, aes(x=x1, y=pred)) +
    geom_point(shape=1) +
    geom_smooth(method=loess) + 
    geom_smooth(data = test_set, aes(x = x1, y = true_effect), color = 'red') +
    ylim(c(-4,10)) #+ 
  #xlim(c(-6,6))  
}

#Design 4, k = 2
d1_data_k2 <- source_data %>% 
  select(x1, x2, treatment, error) %>% 
  mutate(x1_new = case_when(x1 < -1 ~ x1,
                            x1 >= -1 & x1 <= 1 ~ 0,
                            x1 > 1 ~ x1^2)) %>% 
  mutate(y = (x1/2+x2) + (1/2)*(2*treatment -1)*x1_new + error,
         true_effect = x1_new)

d1_train_k2 <- d1_data_k2[train_ind, ]
d1_test_k2 <- d1_data_k2[-train_ind, ]

cf_d1_k2 <- causalForest(as.formula(paste("y~","x1+x2")),
                         data=d1_train_k2, treatment=d1_train_k2$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 5000, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=2, ncov_samp=1)

cfpredtest <- predict(cf_d1_k2, newdata=d1_test_k2, type="vector")
d1_test_k2$pred <- cfpredtest

cf_plot(d1_test_k2)

#Design 4, k = 3
d1_data_k3 <- source_data %>% 
  select(x1, x2, x3, treatment, error) %>% 
  mutate(x1_new = case_when(x1 < -1 ~ x1,
                            x1 >= -1 & x1 <= 1 ~ 0,
                            x1 > 1 ~ x1^2)) %>% 
  mutate(y = (x1/2+x2+x3) + (1/2)*(2*treatment -1)*(x1_new) + error,
         true_effect = x1_new)
d1_train_k3 <- d1_data_k3[train_ind, ]
d1_test_k3 <- d1_data_k3[-train_ind, ]

cf_d1_k3 <- causalForest(as.formula(paste("y~","x1+x2+x3")),
                         data=d1_train_k3, treatment=d1_train_k3$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 5000, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=3, ncov_samp=2)

cfpredtest <- predict(cf_d1_k3, newdata=d1_test_k3, type="vector")
d1_test_k3$pred <- cfpredtest

cf_plot(d1_test_k3)

#Design 4, k = 5
d1_data_k5 <- source_data %>% 
  select(x1, x2, x3, x4, x5, treatment, error) %>% 
  mutate(x1_new = case_when(x1 < -1 ~ x1,
                            x1 >= -1 & x1 <= 1 ~ 0,
                            x1 > 1 ~ x1^2)) %>% 
  mutate(y = (x1/2+x2+x3+x4+x5) + (1/2)*(2*treatment -1)*(x1_new) + error,
         true_effect = x1_new)
d1_train_k5 <- d1_data_k5[train_ind, ]
d1_test_k5 <- d1_data_k5[-train_ind, ]

cf_d1_k5 <- causalForest(as.formula(paste("y~","x1+x2+x3+x4+x5")),
                         data=d1_train_k5, treatment=d1_train_k5$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 5000, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=5, ncov_samp=3)

cfpredtest <- predict(cf_d1_k5, newdata=d1_test_k5, type="vector")
d1_test_k5$pred <- cfpredtest

cf_plot(d1_test_k5)

#Design 4, k = 10
d1_data_k10 <- source_data %>% 
  select(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, treatment, error) %>% 
  mutate(x1_new = case_when(x1 < -1 ~ x1,
                            x1 >= -1 & x1 <= 1 ~ 0,
                            x1 > 1 ~ x1^2)) %>% 
  mutate(y = (x1/2+x2+x3+x4+x5+x6+x7+x8+x9+x10) + (1/2)*(2*treatment -1)*(x1_new) + error,
         true_effect = x1_new)
d1_train_k10 <- d1_data_k10[train_ind, ]
d1_test_k10 <- d1_data_k10[-train_ind, ]

cf_d1_k10 <- causalForest(as.formula(paste("y~","x1+x2+x3+x4+x5+x6+x7+x8+x9+x10")),
                          data=d1_train_k10, treatment=d1_train_k10$treatment,
                          split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                          bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                          split.alpha = 0.5, cv.alpha = 0.5,
                          sample.size.total = 5000, sample.size.train.frac = .5,
                          mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                          ncolx=10, ncov_samp=6)

cfpredtest <- predict(cf_d1_k10, newdata=d1_test_k10, type="vector")
d1_test_k10$pred <- cfpredtest

cf_plot(d1_test_k10)

########### design 4
cf_plot <- function(test_set){
  ggplot(test_set, aes(x=x1, y=pred)) +
    geom_point(shape=1) +
    geom_smooth(method=loess) + 
    geom_smooth(data = test_set, aes(x = x1, y = true_effect), color = 'red') +
    ylim(c(-1,2)) #+ 
  #xlim(c(-6,6))  
}

#Design 5, k = 2
d1_data_k2 <- source_data %>% 
  select(x1, x2, treatment, error) %>% 
  mutate(x1_new = x1/2 + x2^2) %>% 
  mutate(y = (x1/2+x2) + (1/2)*(2*treatment -1)*x1_new + error,
         true_effect = x1_new)

d1_train_k2 <- d1_data_k2[train_ind, ]
d1_test_k2 <- d1_data_k2[-train_ind, ]

cf_d1_k2 <- causalForest(as.formula(paste("y~","x1+x2")),
                         data=d1_train_k2, treatment=d1_train_k2$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 5000, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=2, ncov_samp=1)

cfpredtest <- predict(cf_d1_k2, newdata=d1_test_k2, type="vector")
d1_test_k2$pred <- cfpredtest

cf_plot(d1_test_k2)

#Design 5, k = 3
d1_data_k3 <- source_data %>% 
  select(x1, x2, x3, treatment, error) %>% 
  mutate(x1_new = x1/2 + x2^2) %>% 
  mutate(y = (x1/2+x2+x3) + (1/2)*(2*treatment -1)*(x1_new) + error,
         true_effect = x1_new)
d1_train_k3 <- d1_data_k3[train_ind, ]
d1_test_k3 <- d1_data_k3[-train_ind, ]

cf_d1_k3 <- causalForest(as.formula(paste("y~","x1+x2+x3")),
                         data=d1_train_k3, treatment=d1_train_k3$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 5000, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=3, ncov_samp=2)

cfpredtest <- predict(cf_d1_k3, newdata=d1_test_k3, type="vector")
d1_test_k3$pred <- cfpredtest

cf_plot(d1_test_k3)

#Design 5, k = 5
d1_data_k5 <- source_data %>% 
  select(x1, x2, x3, x4, x5, treatment, error) %>% 
  mutate(x1_new = x1/2 + x2^2) %>% 
  mutate(y = (x1/2+x2+x3+x4+x5) + (1/2)*(2*treatment -1)*(x1_new) + error,
         true_effect = x1_new)
d1_train_k5 <- d1_data_k5[train_ind, ]
d1_test_k5 <- d1_data_k5[-train_ind, ]

cf_d1_k5 <- causalForest(as.formula(paste("y~","x1+x2+x3+x4+x5")),
                         data=d1_train_k5, treatment=d1_train_k5$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 5000, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=5, ncov_samp=3)

cfpredtest <- predict(cf_d1_k5, newdata=d1_test_k5, type="vector")
d1_test_k5$pred <- cfpredtest

cf_plot(d1_test_k5)

#Design 5, k = 10
d1_data_k10 <- source_data %>% 
  select(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, treatment, error) %>% 
  mutate(x1_new = x1/2 + x2^2) %>% 
  mutate(y = (x1/2+x2+x3+x4+x5+x6+x7+x8+x9+x10) + (1/2)*(2*treatment -1)*(x1_new) + error,
         true_effect = x1_new)
d1_train_k10 <- d1_data_k10[train_ind, ]
d1_test_k10 <- d1_data_k10[-train_ind, ]

cf_d1_k10 <- causalForest(as.formula(paste("y~","x1+x2+x3+x4+x5+x6+x7+x8+x9+x10")),
                          data=d1_train_k10, treatment=d1_train_k10$treatment,
                          split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                          bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                          split.alpha = 0.5, cv.alpha = 0.5,
                          sample.size.total = 5000, sample.size.train.frac = .5,
                          mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                          ncolx=10, ncov_samp=6)

cfpredtest <- predict(cf_d1_k10, newdata=d1_test_k10, type="vector")
d1_test_k10$pred <- cfpredtest

cf_plot(d1_test_k10)