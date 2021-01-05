# bsnsing
Learn a Classification Tree using Boolean Sensing

Other tree-building methods, such as ctree and rpart available in R, utilize only a single variable in each split, which limits the expressiveness and in some cases the predictive accuracy of the tree model. 

In bsnsing, an optimization problem is solved at each node to identify the best combination of features used to split the node. Currently, supported MIP solvers include cplex, gurobi, lpSolve and a greedy heuristic.  

To use the CPLEX solver, licensed CPLEX software and the R package cplexAPI must be installed. 
To use the Gurobi solver, licensed Gurobi software and the R package gurobi must be installed. 
The lpSolve package should be automatically installed along with bsnsing. If not, install it by install.packages('lpSolve').


## Install bsnsing from Github
install.packages('devtools')

library(devtools)

install_github("profyliu/bsnsing")

library(bsnsing)

## Usage Examples
data("GlaucomaMVF", package = "ipred") 

n <- nrow(GlaucomaMVF)

set.seed(2018)

train_index <- sample(1:n, round(0.5 * n))

test_index <- setdiff(1:n, train_index)

### Out-of-the-box run
bs <- bsnsing(Class ~ ., data = GlaucomaMVF, subset = train_index)

summary(bs)

### Predict and display confusion matrix
pred <- predict(bs, GlaucomaMVF[test_index, ], type = 'class')

table(pred, actual = GlaucomaMVF[test_index, 'Class'])

### Customize parameters
bs <- bsnsing(Class ~ ., data = GlaucomaMVF, subset = train_index, stop.prob = 0.99)

summary(bs)

table(pred = predict(bs, GlaucomaMVF[test_index, ], type = 'class'), actual = GlaucomaMVF[test_index, 'Class'])

### Multi-class classification
n <- nrow(iris)

set.seed(2018)

train_index <- sample(1:n, round(0.5 * n))

test_index <- setdiff(1:n, train_index)

bs <- bsnsing(Species ~ ., data = iris, subset = train_index)

summary(bs[[1]])  # display the first tree

summary(bs[[3]])  # display the third tree

table(pred = predict(bs, iris[test_index, ], type = 'class'), actual = iris[test_index, 'Species']) # Confusion matrix on the test set

### Visualize the bsnsing tree
Use the plot function. 
