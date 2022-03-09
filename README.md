# bsnsing
Learn a Classification Tree using Boolean Sensing

Other tree-building methods, such as C50, tree, ctree and rpart available in R, utilize only a single variable in each split, which limits the expressiveness and in some cases the predictive accuracy of the tree model. 

In bsnsing, an optimization problem is solved at each node to identify the best combination of features by which to split the node. Currently, supported MIP solvers include cplex, gurobi, lpSolve, an implicit enumeration (ENUM) algorithm and a greedy heuristic.  

The bsnsing package does not depend on them to work out-of-the-box, but

To use the CPLEX solver, licensed CPLEX software and the R package cplexAPI must be installed. 

To use the Gurobi solver, licensed Gurobi software and the R package gurobi must be installed. 

The lpSolve package should be automatically installed along with bsnsing. If not, install it by install.packages('lpSolve').


## Install bsnsing from Github
Windows users need to install Rtools (https://cran.r-project.org/bin/windows/Rtools/) before installing the devtools package. 

install.packages('devtools')

library(devtools)

install_github("profyliu/bsnsing")

library(bsnsing)

## Install bsnsing from source
Download this repository, and in R run:

install.packages(path_to_file, repos = NULL, type="source")

## Build the bslearn.so / bslearn.dylib / bslearn.dll
On windows: 

C:\rtools40\mingw64\bin\gcc -I"C:\Program Files\R\R-4.1.2\include" -fpic -c "C:\...\bsnsing\C\bslearn.c"

C:\rtools40\mingw64\bin\gcc -I"C:\Program Files\R\R-4.1.2\include" -shared bslearn.o -o bslearn.dll

On Mac:

Go to the bsnsing/C folder, modify include path in the Makefile and run

make

to generate the bslearn.dylib file.
On Linux:

Follow a similar procedure as on Mac, to generate the bslearn.so file.

Finally, copy & paste the dynamic library file (bslearn.dll on Windows, bslearn.dylib on Mac or bslearn.so on Linux) to the working directory of R in which bsnsing is run.

Note that the dynamic library file is not required to run bsnsing, but having it can boost bsnsing's speed by up to two orders of magnitude. 


## Usage Examples

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
bs <- bsnsing(Class ~ ., data = GlaucomaMVF, subset = train_index, opt.model = 'error', opt.solver = 'gurobi')

### To learn more about control parameters

?bscontrol

### To display the current and default parameter values

bscontrol()

### Multi-class classification
n <- nrow(iris)

set.seed(2018)

train_index <- sample(1:n, round(0.5 * n))

test_index <- setdiff(1:n, train_index)

bs <- bsnsing(Species ~ ., data = iris, subset = train_index)

summary(bs[[1]])  # display the first tree

summary(bs[[2]])  # display the second tree

summary(bs[[3]])  # display the third tree

table(pred = predict(bs, iris[test_index, ], type = 'class'), actual = iris[test_index, 'Species']) # Confusion matrix on the test set

### Visualize the bsnsing tree
Use the plot function to generate a PDF plot as well as the latex code. For example,

plot(bs)

or

plot(bs, file = 'a.pdf')

