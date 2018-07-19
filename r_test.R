setwd("G:/classroom_analytics/causal_tree")
library(causalTree)


#test df
test_df <-  read.csv('test_df.csv', header = TRUE, sep = ',')
tree_1 <- causalTree(y ~ x1+x2, data = test_df, 
                     treatment = test_df$trt, split.Rule = "CT", 
                     cv.option = "CT", split.Honest = T, split.Bucket = F, xval = 5, cp = 0, 
                     minsize = 20, propensity = 0.5)
opcp <- tree_1$cptable[,1][which.min(tree_1$cptable[,4])]

opfit <- prune(tree_1, opcp)
rpart.plot(opfit)


#fake data validation
fake_data <-  read.csv('fake_data.csv', header = TRUE, sep = ',')
tree_1 <- causalTree(design1_y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, data = fake_data, 
                     treatment = fake_data$treatment, split.Rule = "CT", 
                     cv.option = "CT", split.Honest = T, split.Bucket = F, xval = 5, cp = 0, 
                     minsize = 20, propensity = 0.5)
opcp <- tree_1$cptable[,1][which.min(tree_1$cptable[,4])]

opfit <- prune(tree_1, opcp)
rpart.plot(opfit)

#fake data validation(large size)
fake_data <-  read.csv('fake_data_large.csv', header = TRUE, sep = ',')
tree_1 <- causalTree(design1_y ~ x1+x2, data = fake_data, 
                     treatment = fake_data$treatment, split.Rule = "CT", 
                     cv.option = "CT", split.Honest = T, split.Bucket = F, xval = 5, cp = 0, 
                     minsize = 25, propensity = 0.5)
opcp <- tree_1$cptable[,1][which.min(tree_1$cptable[,4])]

opfit <- prune(tree_1, opcp)
rpart.plot(opfit)