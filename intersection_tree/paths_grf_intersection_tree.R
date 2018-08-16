library(dplyr)
library(ggplot2)
#install.packages("grf")
library(grf)
library(randomForest)
library(FSInteract)

#############################################################################################
## In this part, the random forest model is generated with fake data, you can also use your 
## own data to get your randome forest model.

n = 2000; p = 10
X = matrix(rnorm(n*p), n, p)
X.test = matrix(0, 101, p)
X.test[,1] = seq(-2, 2, length.out = 101)

# Train a causal forest.
W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0))
Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)
n_tree = 15
rf = causal_forest(X, Y, W, num.trees = n_tree, min.node.size = 25)

#############################################################################################

### get path
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
#############################################################################################

### from tree to df

#initiate an empty df
df <- data.frame(matrix(NA, nrow=0, ncol=p))

for(j in 1:n_tree){
  tree <- get_tree(rf, j)
  tree_path <- get_path(tree)
  
  for(i in 1:length(tree_path)){
    # extract each path
    branch <- tree_path[[i]]
    # get path and number of samples in that path
    path <- branch[-length(branch)]
    n_sample <- branch[length(branch)]
    
    # create a list of 0's
    row <- c(rep(0,p))
    # update the column that is in the path with 1
    row[c(path)] <- 1
    for(i in 1:n_sample){
      df <- rbind(df, row)
    }
  }
}

colnames(df) <- c('x1','x2','x3','x4','x5','x6','x7','x8','x9','x10')
#############################################################################################
# compute intersection (cast the data frame to matrix)
out1 <- RIT(as.matrix(df))
out1
