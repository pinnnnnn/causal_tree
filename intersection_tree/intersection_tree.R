library(dplyr)
library(ggplot2)
#install.packages("grf")
library(grf)
library(randomForest)

#read in data
source_data <- read.csv('G:/causal_tree/simulation/simulation_study5_large_sample_unif/large_unif.csv',header = TRUE, sep = ',')
source_data <- source_data %>% mutate(y = (x1/2+x2) + (1/2)*(2*treatment -1)*(x1/2 +x3) +  error, true_effect = x1/2 + x3)

#train test split
smp_size <- floor(0.625 * nrow(source_data))
set.seed(123)
train_ind <- sample(seq_len(nrow(source_data)), size = smp_size)
train_set <- source_data[train_ind, ]
test_set <- source_data[-train_ind, ]

#prepare data for forest training
X_train <- as.matrix(train_set %>% select(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
Y <- as.matrix(train_set %>% select(y))
W <- as.matrix(train_set %>% select(treatment))
X_test <- as.matrix(test_set %>% select(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))

### Train the tree
num_tree <- 50
# train random forest
rf <- causal_forest(X_train, Y, W, num.trees = num_tree, min.node.size = 25)

#make prediction on test set
#test_set$pred_effect <- predict(rf, X_test)$prediction

#2000 trees in total
tree <- get_tree(rf, 1)


# a function to get the path of a tree with the last item being the number of samples in a tree 
get_path <- function(tree, i = 1, current_path = c(), all_path = list()){
  if(tree[3]$nodes[[i]]$'is_leaf' == TRUE){
    num_samples <- length(tree[3]$nodes[[i]]$'samples')
    current_path <- append(current_path, num_samples)
    all_path[[length(all_path) + 1]] <- current_path
    
  }
  
  else{
    new_current_path <- current_path
    new_current_path <- append(new_current_path, tree[3]$nodes[[i]]$'split_variable')
    all_path <- get_path(tree, tree[3]$nodes[[i]]$'left_child', new_current_path, all_path)
    all_path <- get_path(tree, tree[3]$nodes[[i]]$'right_child', new_current_path, all_path)
  }
  
  return(all_path)
}

temp <- get_path(tree)
