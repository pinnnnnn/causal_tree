#install.packages("devtools")
library(devtools) 
#install_github("susanathey/causalTree")
library(causalTree)
library(dplyr)
setwd("G:/causal_tree")
# rand d1
rand_df <-  read.csv('fake_data_x2_rand.csv', header = TRUE, sep = ',')
rf_test_set_rand_d1 <- read.csv('test_set_for_r\\rf_test_set_rand_d1.csv', header = TRUE, sep = ',')
cf_rand_d1 <- causalForest(as.formula(paste("design1_y~","x1+x2+x3+x4+x5+x6+x7+x8+x9+x10")),
                           data=rand_df, treatment=rand_df$treatment,
                           split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                           bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                           split.alpha = 0.5, cv.alpha = 0.5,
                           sample.size.total = 750, sample.size.train.frac = .5,
                           mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                           ncolx=10, ncov_samp=6)

cfpredtest1 <- predict(cf_rand_d1, newdata=rf_test_set_rand_d1, type="vector")
rf_test_set_rand_d1$pred <- cfpredtest1
rf_test_set_rand_d1 <- rf_test_set_rand_d1 %>% 
  mutate(x1_pos = ifelse(x1 > 0, x1, 0),
         x2_pos = ifelse(x2 > 0, x2, 0),
         true = x1_pos + x2_pos)


ggplot(rf_test_set_rand_d1, aes(x=x1, y=pred)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_line(data = rf_test_set_rand_d1, aes(x = x1, y = true), color = 'red') +
  ylim(c(-1,1)) + xlim(c(-6,6))

# rand d2
rf_test_set_rand_d2 <- read.csv('test_set_for_r\\rf_test_set_rand_d2.csv', header = TRUE, sep = ',')
cf_rand_d2 <- causalForest(as.formula(paste("design2_y~","x1+x2+x3+x4+x5+x6+x7+x8+x9+x10")),
                           data=rand_df, treatment=rand_df$treatment,
                           split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                           bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                           split.alpha = 0.5, cv.alpha = 0.5,
                           sample.size.total = 750, sample.size.train.frac = .5,
                           mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                           ncolx=10, ncov_samp=6)

cfpredtest2 <- predict(cf_rand_d2, newdata=rf_test_set_rand_d2, type="vector")
rf_test_set_rand_d2$pred <- cfpredtest2
rf_test_set_rand_d2 <- rf_test_set_rand_d2 %>% 
  mutate(x1_pos = ifelse(x1 > 0, x1, 0),
         x2_pos = ifelse(x2 > 0.5, x2, 0),
         true = x1_pos + x2_pos)


ggplot(rf_test_set_rand_d2, aes(x=x1, y=pred)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_smooth(data = rf_test_set_rand_d2, aes(x = x1, y = true), color = 'red') +
  ylim(c(0,2)) + xlim(c(-6,6))


# rand d3
rf_test_set_rand_d3 <- read.csv('test_set_for_r\\rf_test_set_rand_d3.csv', header = TRUE, sep = ',')
cf_rand_d3 <- causalForest(as.formula(paste("design3_y~","x1+x2+x3+x4+x5+x6+x7+x8+x9+x10")),
                           data=rand_df, treatment=rand_df$treatment,
                           split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                           bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                           split.alpha = 0.5, cv.alpha = 0.5,
                           sample.size.total = 750, sample.size.train.frac = .5,
                           mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                           ncolx=10, ncov_samp=6)

cfpredtest3 <- predict(cf_rand_d3, newdata=rf_test_set_rand_d3, type="vector")
rf_test_set_rand_d3$pred <- cfpredtest3
rf_test_set_rand_d3 <- rf_test_set_rand_d3 %>% mutate(true = x1/2 + x2^2)
#rf_test_set_rand_d3$true <- (rf_test_set_rand_d3$x1)/2 + (rf_test_set_rand_d3$x2)^2
head(rf_test_set_rand_d3)

ggplot(rf_test_set_rand_d3, aes(x=x1, y=pred)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_smooth(data = rf_test_set_rand_d3, aes(x = x1, y = true), color = 'red') +
  ylim(c(-1,1)) + xlim(c(-6,6))


df_25 <-  read.csv('fake_data_x2_25.csv', header = TRUE, sep = ',')
# 0.25 d1
rf_test_set_25_d1 <- read.csv('test_set_for_r\\rf_test_set_25_d1.csv', header = TRUE, sep = ',')
cf_25_d1 <- causalForest(as.formula(paste("design1_y~","x1+x2+x3+x4+x5+x6+x7+x8+x9+x10")),
                           data=df_25, treatment=df_25$treatment,
                           split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                           bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                           split.alpha = 0.5, cv.alpha = 0.5,
                           sample.size.total = 750, sample.size.train.frac = .5,
                           mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                           ncolx=10, ncov_samp=6)

cfpredtest1 <- predict(cf_25_d1, newdata=rf_test_set_25_d1, type="vector")
rf_test_set_25_d1$pred <- cfpredtest1
rf_test_set_25_d1 <- rf_test_set_25_d1 %>% 
  mutate(x1_pos = ifelse(x1 > 0, x1, 0),
         x2_pos = ifelse(x2 > 0, x2, 0),
         true = x1_pos + x2_pos)


ggplot(rf_test_set_25_d1, aes(x=x1, y=pred)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_line(data = rf_test_set_25_d1, aes(x = x1, y = true), color = 'red') +
  ylim(c(-1,1)) + xlim(c(-6,6))

# 0.25 d2
rf_test_set_25_d2 <- read.csv('test_set_for_r\\rf_test_set_25_d2.csv', header = TRUE, sep = ',')
cf_25_d2 <- causalForest(as.formula(paste("design2_y~","x1+x2+x3+x4+x5+x6+x7+x8+x9+x10")),
                         data=df_25, treatment=df_25$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 750, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=10, ncov_samp=6)

cfpredtest2 <- predict(cf_25_d2, newdata=rf_test_set_25_d2, type="vector")
rf_test_set_25_d2$pred <- cfpredtest2
rf_test_set_25_d2 <- rf_test_set_25_d2 %>% 
  mutate(x1_pos = ifelse(x1 > 0, x1, 0),
         x2_pos = ifelse(x2 > 0.5, x2, 0),
         true = x1_pos + x2_pos)


ggplot(rf_test_set_25_d2, aes(x=x1, y=pred)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_line(data = rf_test_set_25_d2, aes(x = x1, y = true), color = 'red') +
  ylim(c(0,2)) + xlim(c(-6,6))

# 0.25 d3
rf_test_set_25_d3 <- read.csv('test_set_for_r\\rf_test_set_25_d3.csv', header = TRUE, sep = ',')
cf_25_d3 <- causalForest(as.formula(paste("design3_y~","x1+x2+x3+x4+x5+x6+x7+x8+x9+x10")),
                         data=df_25, treatment=df_25$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 750, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=10, ncov_samp=6)

cfpredtest3 <- predict(cf_25_d3, newdata=rf_test_set_25_d3, type="vector")
rf_test_set_25_d3$pred <- cfpredtest3
rf_test_set_25_d3$true <- (rf_test_set_25_d3$x1)/2 + (rf_test_set_25_d3$x2)^2
head(rf_test_set_25_d3)

ggplot(rf_test_set_25_d3, aes(x=x1, y=pred)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_line(data = rf_test_set_25_d3, aes(x = x1, y = true), color = 'red') +
  ylim(c(-1,1)) + xlim(c(-6,6))





df_75 <-  read.csv('fake_data_x2_75.csv', header = TRUE, sep = ',')

# 0.75 d1
rf_test_set_75_d1 <- read.csv('test_set_for_r\\rf_test_set_75_d1.csv', header = TRUE, sep = ',')
cf_75_d1 <- causalForest(as.formula(paste("design1_y~","x1+x2+x3+x4+x5+x6+x7+x8+x9+x10")),
                         data=df_75, treatment=df_75$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 750, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=10, ncov_samp=6)

cfpredtest1 <- predict(cf_75_d1, newdata=rf_test_set_75_d1, type="vector")
rf_test_set_75_d1$pred <- cfpredtest1
rf_test_set_75_d1 <- rf_test_set_75_d1 %>% 
  mutate(x1_pos = ifelse(x1 > 0, x1, 0),
         x2_pos = ifelse(x2 > 0, x2, 0),
         true = x1_pos + x2_pos)


ggplot(rf_test_set_75_d1, aes(x=x1, y=pred)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_line(data = rf_test_set_75_d1, aes(x = x1, y = true), color = 'red') +
  ylim(c(-1,1)) + xlim(c(-6,6))

# 0.75 d2
rf_test_set_75_d2 <- read.csv('test_set_for_r\\rf_test_set_75_d2.csv', header = TRUE, sep = ',')
cf_75_d2 <- causalForest(as.formula(paste("design2_y~","x1+x2+x3+x4+x5+x6+x7+x8+x9+x10")),
                         data=df_75, treatment=df_75$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 750, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=10, ncov_samp=6)

cfpredtest2 <- predict(cf_75_d2, newdata=rf_test_set_75_d2, type="vector")
rf_test_set_75_d2$pred <- cfpredtest2
rf_test_set_75_d2 <- rf_test_set_75_d2 %>% 
                       mutate(x1_pos = ifelse(x1 > 0, x1,0),
                              x2_pos = ifelse(x2 > 0.5, x2,0),
                              true = x1_pos + x2_pos)


ggplot(rf_test_set_75_d2, aes(x=x1, y=pred)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_line(data = rf_test_set_75_d2, aes(x = x1, y = true), color = 'red') +
  ylim(c(0.5,2.5)) + xlim(c(-6,6))

# 0.75 d3
rf_test_set_75_d3 <- read.csv('test_set_for_r\\rf_test_set_75_d3.csv', header = TRUE, sep = ',')
cf_75_d3 <- causalForest(as.formula(paste("design3_y~","x1+x2+x3+x4+x5+x6+x7+x8+x9+x10")),
                         data=df_75, treatment=df_75$treatment,
                         split.Rule="CT", split.Honest=T, split.Bucket=F, bucketNum = 5,
                         bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                         split.alpha = 0.5, cv.alpha = 0.5,
                         sample.size.total = 750, sample.size.train.frac = .5,
                         mtry = ceiling(ncol(dataTrain)/3), nodesize = 25, num.trees= 100, 
                         ncolx=10, ncov_samp=6)

cfpredtest3 <- predict(cf_75_d3, newdata=rf_test_set_75_d3, type="vector")
rf_test_set_75_d3$pred <- cfpredtest3
rf_test_set_75_d3$true <- (rf_test_set_75_d3$x1)/2 + (rf_test_set_75_d3$x2)^2
head(rf_test_set_75_d3)


ggplot(rf_test_set_75_d3, aes(x=x1, y=pred)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_line(data = rf_test_set_75_d3, aes(x = x1, y = true), color = 'red') +
  ylim(c(-0.5,1)) + xlim(c(-6,6))





## plot result from python

#d1
pred_df_rand_d1 <- read.csv('predtf\\pred_df_rand_d1.csv', header = TRUE, sep = ',')
ggplot(pred_df_rand_d1, aes(x=x1, y=rf_pred_causal_effect)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_line(data = pred_df_rand_d1, aes(x = x1, y = true_causal_eff), color = 'red') +
  ylim(c(-1,1)) + xlim(c(-6,6))

pred_df_25_d1 <- read.csv('predtf\\pred_df_25_d1.csv', header = TRUE, sep = ',')
ggplot(pred_df_25_d1, aes(x=x1, y=rf_pred_causal_effect)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_line(data = pred_df_25_d1, aes(x = x1, y = true_causal_eff), color = 'red') +
  ylim(c(-1,1)) + xlim(c(-6,6))

pred_df_75_d1 <- read.csv('predtf\\pred_df_75_d1.csv', header = TRUE, sep = ',')
ggplot(pred_df_75_d1, aes(x=x1, y=rf_pred_causal_effect)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_line(data = pred_df_75_d1, aes(x = x1, y = true_causal_eff), color = 'red') +
  ylim(c(-1,1)) + xlim(c(-6,6))
#d2
pred_df_rand_d2 <- read.csv('predtf\\pred_df_rand_d2.csv', header = TRUE, sep = ',')
ggplot(pred_df_rand_d2, aes(x=x1, y=rf_pred_causal_effect)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_smooth(data = pred_df_rand_d2, aes(x = x1, y = true_causal_eff), color = 'red') +
  ylim(c(0,2)) + xlim(c(-6,6))

pred_df_25_d2 <- read.csv('predtf\\pred_df_25_d2.csv', header = TRUE, sep = ',')
ggplot(pred_df_25_d2, aes(x=x1, y=rf_pred_causal_effect)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_smooth(data = pred_df_25_d2, aes(x = x1, y = true_causal_eff), color = 'red') +
  ylim(c(0.5,3)) + xlim(c(-6,6))

pred_df_75_d2 <- read.csv('predtf\\pred_df_75_d2.csv', header = TRUE, sep = ',')
ggplot(pred_df_75_d2, aes(x=x1, y=rf_pred_causal_effect)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_smooth(data = pred_df_75_d2, aes(x = x1, y = true_causal_eff), color = 'red') +
  ylim(c(0.5,3)) + xlim(c(-6,6))

#d3
pred_df_rand_d3 <- read.csv('predtf\\pred_df_rand_d3.csv', header = TRUE, sep = ',')
ggplot(pred_df_rand_d3, aes(x=x1, y=rf_pred_causal_effect)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_smooth(data = pred_df_rand_d3, aes(x = x1, y = true_causal_eff), color = 'red') +
  ylim(c(-1,1)) + xlim(c(-6,6))

pred_df_25_d3 <- read.csv('predtf\\pred_df_25_d3.csv', header = TRUE, sep = ',')
ggplot(pred_df_25_d3, aes(x=x1, y=rf_pred_causal_effect)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_line(data = pred_df_25_d3, aes(x = x1, y = true_causal_eff), color = 'red') +
  ylim(c(-1,1)) + xlim(c(-6,6))

pred_df_75_d3 <- read.csv('predtf\\pred_df_75_d3.csv', header = TRUE, sep = ',')
ggplot(pred_df_75_d3, aes(x=x1, y=rf_pred_causal_effect)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_line(data = pred_df_75_d3, aes(x = x1, y = true_causal_eff), color = 'red') +
  ylim(c(-0.5,1)) + xlim(c(-6,6))