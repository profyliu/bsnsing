#' bsnsing: A package for decision tree learning with Boolean sensing
#'
#' The bsnsing package provides functions for building a decision tree classifier and making predictions. It solves the two-class and multi-class classification problems under the supervised learning paradigm. While building a decision tree, \code{bsnsing} uses a Boolean rule involving multiple variables to split a node. Each split rule is identified by solving an optimization model that minimizes misclassification and complexity. Compared to other decision tree learners such as \code{\link[rpart]{rpart}} and \code{\link[party]{party}} that split a node based on a single variable, \code{\link{bsnsing}} can provide a shorter and more interpretable tree while scoring a high predictive accuracy.
#'
#' @section  Learn functions:
#' The learn functions include \code{\link{bsnsing}}, \code{\link{bsnsing.formula}} and \code{\link{bsnsing.default}}.
#' @section Predict functions:
#' The predict functions include: \code{\link{predict.bsnsing}} and \code{\link{predict.mbsnsing}}.
#' @section Auxilliary functions:
#' Here is a list of internal functions of the package that are open for end users.
#' \code{\link{summary.bsnsing}}
#' \code{\link{summary.mbsnsing}}
#' \code{\link{binarize}},
#' \code{\link{binarize.numeric}},
#' \code{\link{binarize.factor}},
#' \code{\link{binarize.y}},
#' \code{\link{bslearn}},
#' \code{\link{bscontrol}}
#'
#' @section Future work:
#' Visualization functions. Existing tree plotting functions such as \code{\link[rpart]{plot.rpart}} and \code{\link[rpart.plot]{rpart.plot}} restrict the tree structure to have single-variable splits, thus it is not straightforward to adopt these functions for plotting a \code{\link{bsnsing}} object. A dedicated viualization tool should be developed.
#'
#' Default parameter tuning for out-of-the-box performance. The default parameters in \code{\link{bscontrol}} are currently set quite arbitrarily. Experiments will be performed on a large collection of data sets in order to pinpoint the parameter combination that work well (in terms of training speed, predictive accuracy and interpretability, etc.) under most use cases.
#'
#' Weighting positive and negative cases in imbalanced training sets. If false positive and false negative are given the same weight in a highly imbalanced data set, a null split might be produced in as early as the root node, resulting in a trivial classification, i.e., claiming all cases to fall in the majority class. A weighting scheme should be implemented to ameliorate this situation.
#'
#' @author Yanchao Liu
#'
#' @docType package
#' @name bsnsing-package
NULL



#' Create Binary Variables by the Classification Target
#'
#' Create a set of variables (columns) with binary values for each column in the input data. For a variable with values of 0 and 1, the column is retained and no new column is created. For a numeric variable, the function \code{binarize.numeric} is called. For a factor column, the function \code{binarize.factor} is called.
#'
#' @param x a data frame or matrix to be binarized.
#' @param y a vector with two unique values (0 and 1). It is the response variable that guides the optimal discretization of variables in \code{x}.
#' @param target the level of y (0 or 1) which indicates the boolean rule target
#' @param control a list or a \code{bscontrol()} object. The list should contain the following three attributes: \emph{nseg.numeric}, a positive integer indicating the maximum number of segments used in discretizing a numeric variable, \emph{nseg.factor}, a positive integer indicating the maximum number of levels allowed for a factor variable, and \emph{bin.size}, a positive integer indicating the minimum number of observations to fall in a segment.
#' @return a data frame containing binary variables, or a character string describing the rule that perfectly split the target.
#' @examples
#' # Load and prepare data
#' data(Auto, package = 'ISLR')
#' x <- Auto[, c('mpg', 'cylinders', 'displacement')]
#' x$cylinders <- as.factor(x$cylinders)
#' y <- ifelse(Auto$origin == 1, 1, 0)
#' # binarize x by y = 1
#' bx1 <- binarize(x, y, target = 1)
#' head(bx1)
#' # binarize x by y = 0
#' bx0 <- binarize(x, y, target = 0)
#' head(bx0)
#' @export
#'
binarize <- function(x, y, target = stop("'target' (0 or 1) must be provided"), control = bscontrol()) {
  # parse the control parameters
  nseg.numeric <- control$nseg.numeric
  nseg.factor <- control$nseg.factor
  num2factor <- control$num2factor
  bin.size <- control$bin.size

  if (length(unique(y)) == 1) stop("There is only one unqiue value in y")
  if (length(unique(y)) > 2) stop("There are more than two unique values in y")

  # classify columns by type
  x <- as.data.frame(x)

  # remove the (Intercept) column if exists
  x[,'(Intercept)'] <- NULL
  x.col.names <- colnames(x)
  numeric.col.index <- seq(ncol(x))[sapply(x, class) == 'numeric']
  factor.col.index <- seq(ncol(x))[sapply(x, class) == 'factor' |
                                     (sapply(x, class) == 'numeric' & sapply(x, function(x) length(unique(x)) <= num2factor))]
  binary.col.index <- seq(ncol(x))[apply(x, 2, function(x) all(x %in% 0:1))]
  numeric.col.index <- setdiff(numeric.col.index, binary.col.index)
  factor.col.index <- setdiff(factor.col.index, binary.col.index)
  retain.col.index <- setdiff(seq(ncol(x)), sort(union(numeric.col.index, factor.col.index)))

  bx <- data.frame(placeholder = y)
  if (length(numeric.col.index) > 0){
    # discretize numeric columns
    for (j in numeric.col.index) {
      nb <- binarize.numeric(x[,j], x.col.names[j], y, target = target, segments = nseg.numeric, bin.size = bin.size)
      if (is.data.frame(nb)){
        bx <- cbind(bx, nb)
      } else {
        return(nb)
      }
    }
  }
  if (length(factor.col.index) > 0){
    # create binary dummies for factor columns
    for (j in factor.col.index) {
      nb <- binarize.factor(x[,j], x.col.names[j], y, segments = nseg.factor, bin.size = bin.size)
      bx <- cbind(bx, nb)
    }
  }
  if (length(binary.col.index) > 0){
    # create dummies for binary columns
    for (j in binary.col.index) {
      # if (grepl('[<=>]', x.col.names[j])) {
      #   # if the binary column name is already a logical expression, i.e., containing character =, > or <, then use it directly
      #   nb <- data.frame(placeholder = y)
      #   nb[paste0(x.col.names[j])] <- x[,j]
      #   nb['placeholder'] <- NULL
      #   bx <- cbind(bx, nb)
      # } else {
      nb <- data.frame(placeholder = y)
      for (level in 0:1) {
        nb[paste0(x.col.names[j], '==', level)] <- ifelse(x[,j] == level, 1, 0)
      }
      nb['placeholder'] <- NULL
      bx <- cbind(bx, nb)
      # }
    }
  }
  bx['placeholder'] <- NULL
  return(bx)
}


#' Create Binary Features based on a Numeric Vector
#'
#' Discretize a continuous variable \code{x} by splitting its range at a sequence of cutpoints. The cutpoints are determined so as to effectively split the binary target \code{y}. This function is used internally by \code{\link{binarize}}.
#'
#' @param x a numeric vector.
#' @param name a character string, the variable name of \code{x}.
#' @param y a numeric or integer vector of the same length as \code{x}, consisting of two unique values: 0 and 1.
#' @param target a scalar, valued 0 or 1, indicating the target level of \code{y}.
#' @param segments a positive integer, any value below 3 is set to 3. It is the maximum number of segments the range of \code{x} is divided into.
#' @param bin.size a positive integer. It is the minimum number of observations required to fall into each bin.
#'
#' @return a data frame with binary (0 and 1) entries, or a character string describing the rule that perfectly splits \code{y}. If a data frame is returned, the column names are indicative of the conditions used to form the corresponding columns.
#'
binarize.numeric <- function(x, name, y, target = stop("Must provide a target, 0 or 1"), segments = 10, bin.size = 5) {
  if (length(x) != length(y)) stop("length(x) and length(y) do not match")
  n <- length(x)

  if (segments < 3) {
    segments <- 3
    print("The parameter segments is coerced to 3.")
  }
  segments = round(segments)

  min.1 <- min(x[y == 1])
  min.0 <- min(x[y == 0])
  max.1 <- max(x[y == 1])
  max.0 <- max(x[y == 0])
  if ((min.0 < min.1 & max.0 < min.1) | (min.1 < min.0 & max.1 < min.0)) {
    # return the perfect partition rule
    if (min.0 < min.1 & max.0 < min.1) {
      perfect.rule <- paste(name, ">=", (max.0 + min.1)/2)
    } else {
      perfect.rule <- paste(name, "<=", (max.1 + min.0)/2)
    }
    return(perfect.rule)
    #stop(paste("Response can be perfectly classified by", name, "."))
  }

  if(target != 0 & target != 1) stop("Invalid 'target' argument. Must be 0 or 1")

  ox <- x[order(x)]
  oy <- y[order(x)]
  # remove all unuseful points in the two ends
  maxmin <- max(min.1, min.0)
  minmax <- min(max.1, max.0)
  ss <- ox[ox < maxmin]
  lo <- ifelse(length(ss) > 0, max(ss), maxmin)
  ss <- ox[ox > minmax]
  up <- ifelse(length(ss) > 0, min(ss), minmax)
  yy <- oy[ox >= lo & ox <= up]
  xx <- ox[ox >= lo & ox <= up]
  nn <- length(xx)
  if (nn < 2) stop("This should not happen.")
  cutpoints.gt <- c()
  for (i in 2:nn) {
    if (yy[i-1] != target & yy[i] == target & xx[i-1] != xx[i]) {
      cp <- (xx[i-1] + xx[i])/2
      cutpoints.gt <- c(cutpoints.gt, cp)
    }
  }
  cutpoints.lt <- c()
  for (i in (nn-1):1) {
    if (yy[i+1] != target & yy[i] == target & xx[i+1] != xx[i]) {
      cp <- (xx[i+1] + xx[i])/2
      cutpoints.lt <- c(cp, cutpoints.lt)
    }
  }

  # if there are more cutpoints than allowed, remove ones that yield fewest data points
  ngt <- length(cutpoints.gt)
  if(ngt > segments) {
    # for gt cutpoints, always keep the last (greatest) cutpoints
    # cnt.pts <- rep(0, ngt)
    # cnt.pts[ngt] <- n  # assign a big count value for the last cutpoint
    # cnt.pts[1] <- length(ox[ox < cutpoints.gt[1]])
    # for (i in 2:(ngt-1)) {
    #   cnt.pts[i] <- length(xx[xx > cutpoints.gt[i-1] & xx < cutpoints.gt[i]])
    # }
    # removecp <- (cutpoints.gt[order(cnt.pts)])[c(1:(ngt-segments))]
    # cutpoints.gt <- cutpoints.gt[!cutpoints.gt %in% removecp]
    bsize <- round(length(ox[ox < cutpoints.gt[ngt]])/segments)
    keep.cp <- c()
    for (i in 1:(ngt-1)) {
      if (length(keep.cp) == 0) {
        cumcnt <- length(ox[ox < cutpoints.gt[i]])
      } else {
        cumcnt <- length(ox[ox > keep.cp[length(keep.cp)] & ox < cutpoints.gt[i]])
      }
      if (cumcnt >= bsize) keep.cp <- c(keep.cp, cutpoints.gt[i])
      if (length(keep.cp) >= (segments - 1)) break
    }
    keep.cp <- c(keep.cp, cutpoints.gt[ngt])  # always keep the last cutpoints for gt
    cutpoints.gt <- cutpoints.gt[cutpoints.gt %in% keep.cp]
  }

  nlt <- length(cutpoints.lt)
  if(nlt > segments) {
    # for lt cutpoints, always keep the first (smallest) cutpoints
    # cnt.pts <- rep(0, nlt)
    # cnt.pts[1] <- n  # assign a big count value for the first cutpoint
    # cnt.pts[nlt] <- length(ox[ox > cutpoints.lt[nlt]])
    # for (i in 2:(nlt-1)) {
    #   cnt.pts[i] <- length(xx[xx > cutpoints.lt[i] & xx < cutpoints.lt[i+1]])
    # }
    # removecp <- (cutpoints.lt[order(cnt.pts)])[c(1:(nlt-segments))]
    # cutpoints.lt <- cutpoints.lt[!cutpoints.lt %in% removecp]
    bsize <- round(length(ox[ox > cutpoints.lt[1]])/segments)
    keep.cp <- c()
    for (i in nlt:2) {
      if (length(keep.cp) == 0) {
        cumcnt <- length(ox[ox > cutpoints.lt[i]])
      } else {
        cumcnt <- length(ox[ox < keep.cp[1] & ox > cutpoints.lt[i]])
      }
      if (cumcnt >= bsize) keep.cp <- c(cutpoints.lt[i], keep.cp)
      if (length(keep.cp) >= (segments - 1)) break
    }
    keep.cp <- c(cutpoints.lt[1], keep.cp)  # always keep the first cutpoints for lt
    cutpoints.lt <- cutpoints.lt[cutpoints.lt %in% keep.cp]
  }
  # cat("gt: ")
  # print(cutpoints.gt)
  # cat("\n lt: ")
  # print(cutpoints.lt)

  ngt <- length(cutpoints.gt)
  nlt <- length(cutpoints.lt)
  bmat <- data.frame(matrix(0L, nrow = n, ncol =  (ngt + nlt)))
  sgns <- c(rep(">", ngt), rep("<", nlt))
  cps <- c(cutpoints.gt, cutpoints.lt)
  if ((ngt + nlt) > 0) {
    colnames(bmat) <- paste(name, sgns, cps)
    if (ngt > 0) {
      for (j in 1:ngt) {
        bmat[,j] <- ifelse(x > cps[j], 1L, 0L)
      }
    }
    if (nlt > 0) {
      for (j in (ngt + 1):(ngt + nlt)) {
        bmat[,j] <- ifelse(x < cps[j], 1L, 0L)
      }
    }
  }
  return(bmat)
}

#' Create Binary Features based on a Factor Vector
#'
#' Create binary dummy variables based on a factor variable. This function is used internally by \code{\link{binarize}}.
#'
#' @inheritParams binarize.numeric
#' @param segments a positive integer indicating the maximum number of levels allowed in the factor variable.
#' @return a data frame with binary (0 and 1) entries. The column names are indicative of the conditions used to form the corresponding columns.
#'
binarize.factor <- function(x, name, y, segments = 10, bin.size = 5) {
  nl = length(unique(x))
  bx <- data.frame(placeholder = y)
  if (nl <= segments) {
    # check for bin size
    tab <- table(x, y)
    nobs <- rowSums(table(x,y))
    small.levels <- names(nobs)[nobs < bin.size]
    if (length(small.levels) == 0) {
      # all levels will be used
      for (level in unique(x)) {
        bx[paste0(name, "=='", level, "'")] <- ifelse(x == level, 1, 0)
      }
    } else {
      # combine all small level into one group. if there is only one small level, accept it as is
      for (level in setdiff(unique(x), small.levels)) {
        bx[paste0(name, "=='", level, "'")] <- ifelse(x == level, 1, 0)
      }
      bx[paste0(name, "%in%c(", paste0(paste0("'", small.levels, "'"), collapse = ','), ")")] <- ifelse(x %in% small.levels, 1, 0)
    }
  } else {
    stop(paste("Factor", name, "has more levels than allowed segments. Level collapsing is yet to be implemented."))
  }
  bx['placeholder'] <- NULL
  return(bx)
}

#' Find the Optimal Boolean Rule for Binary Classification
#'
#' The function solves a mixed integer program (MIP) or a linear program (LP) to minimize the sum of two terms: the number of misclassifications (false positives and false negatives), and \code{lambda} times the L1 norm of the solution vector. The solution vector is of length \code{ncol(bx)}. In an LP, its elements are between 0 and 1; in a MIP, its elements are binary. The L1 norm of the solution vector is constrained to be less than or equal to \code{max.rules}. The optimal rule serves as the split condition in the classification tree built by \code{\link{bsnsing}}.
#'
#' @param bx a data frame with binary (0 and 1) entries.
#' @param y an integer vector with binary entries.
#' @param control an object of class \code{bscontrol()}, specifying the algorithmic parameters. The list should contain the following attributes: \emph{lambda}, the penalty factor on the L1 norm of the solution, \emph{max.rules}, the maximum L1 norm of the solution, \emph{epsilon}, a small positive number serving as the threshold for numerical zero, \emph{bigM}, a positive integer serving as the big M in a MIP formulation, \emph{opt.model}, a character string in {\code{'mip', 'lp', 'hybrid'}} indicating the type of optimization model to solve, \emph{opt.solver}, a character string in {\code{'cplex', 'lpSolve'}} indicating the optimization solver to use.
#'
#' @return a list containing the splitting solution.
#' @examples
#' data(Auto, package = 'ISLR')
#' x <- Auto[, c('mpg', 'cylinders', 'displacement')]
#' y <- ifelse(Auto$origin == 1, 1, 0)
#' # binarize x by y = 1
#' bx <- binarize(x, y, target = 1)
#' # learn the optimal Boolean rule
#' bssol <- bslearn(bx, y)
#' cat(paste("Optimal rule:" , bssol$rules, "\n"))
#' bssol$confusion.matrix
#' @export

bslearn <- function(bx, y, control = bscontrol()) {

  n <- dim(bx)[1]
  p <- dim(bx)[2]
  if (n != length(y)) stop("Dimensions of bx and y do not match.")

  # parse control parameters
  verbose <- control$verbose
  lambda <- control$lambda
  max.rules <- control$max.rules
  epsilon <- control$LP.epsilon
  mip <- ifelse(control$opt.model %in% c('mip', 'MIP'), T, F)
  bigM <- ifelse(mip, min(max.rules, control$bigM), 1L)
  if (max.rules > p) max.rules <- p

  # Decide integer variables
  if (mip) {
    #int.vec = 1:p
    int.vec = c(1:p, p + which(y == 0))  # requiring w and slacks corresponding to y == 0 to be integer
    #int.vec = p + which(y == 0)
  } else {
    int.vec = c()
  }

  if (control$opt.solver == 'lpSolve') {
    # Use lpSolve
    n1 <- length(y[y == 1])
    n0 <- length(y[y == 0])
    index1 <- (1:n)[y == 1]
    index0 <- (1:n)[y == 0]
    const.mat1 <- cbind(bx, setNames(as.data.frame(diag(ifelse(y == 1, 1L, -bigM))), paste0('s', 1:n)))
    var.names <- colnames(const.mat1)
    const.mat2 <- setNames(as.data.frame(cbind(diag(rep(1L,p)), matrix(0L, nrow = p, ncol = n))), var.names)
    const.mat3 <- setNames(as.data.frame(cbind(matrix(1L, nrow = 1, ncol = p), matrix(0L, nrow = 1, ncol = n))), var.names)
    const.mat <- rbind(const.mat1,
                       const.mat2,
                       const.mat3)
    const.rhs <- c(y, rep(1, p), max.rules)
    const.dir <- c(ifelse(y == 1, ">=", "<="), rep("<=", p), "<=")
    objective.in <- c(rep(lambda, p), rep(1, n))

    if (verbose) cat(paste("Running lpSolve ... nrow:", nrow(const.mat), "ncol:", ncol(const.mat), "integer:", length(int.vec), "..."))
    lptime <- system.time(
      sol <- lpSolve::lp(direction = "min", objective.in = objective.in, const.mat = const.mat,
                         const.dir = const.dir, const.rhs = const.rhs, int.vec = int.vec)
    )
    if (verbose) cat(paste("Elapsed: ", sprintf("%1.5f", lptime['elapsed']), "s ... "))

    LPsol <- list()
    LPsol$status <- sol$status
    LPsol$w <- setNames((sol$solution)[1:p], var.names[1:p])
    LPsol$slack <- (sol$solution)[(p+1):(p+n)]
    LPsol$objval <- sol$objval
    LPsol$fractional <- sum(LPsol$w < 1 - epsilon & LPsol$w > epsilon)
  } else if(control$opt.solver == 'cplex') {
    # Use cplex
    cplex.env <- cplexAPI::openEnvCPLEX()
    cplex.prob <- cplexAPI::initProbCPLEX(cplex.env)
    cplexAPI::chgProbNameCPLEX(cplex.env, cplex.prob, "bsnsing")
    cplex.nc <- p + n
    cplex.nr <- n + 1
    cplex.nz <- sum(bx) + n
    cplex.obj <- c(rep(lambda, p), rep(1, n))
    cplex.rhs <- c(y, max.rules)
    cplex.sense <- c(ifelse(y == 1, "G", "L"), "L")
    cplex.lb <- rep(0, cplex.nc)
    cplex.ub <- c(rep(1, p), rep(cplexAPI::CPX_INFBOUND, n))
    cplex.beg <- rep(0, p)
    cplex.cnt <- rep(0, p)
    cplex.ind <- c()
    cplex.val <- c(rep(1, sum(bx) + p), ifelse(y == 1, 1L, -bigM))
    for (j in 1:p) {
      cplex.cnt[j] <- sum(bx[,j]) + 1  # number of nonzeros in the column plus 1 (the max.rules constraint coefficient)
      if (j == 1) {
        cplex.beg[j] <- 0
      } else {
        cplex.beg[j] <- cplex.beg[j-1] + cplex.cnt[j-1]
      }
      cplex.ind <- c(cplex.ind, which(bx[,j] != 0) - 1, n)
    }
    for (i in 1:n) {
      cplex.cnt[p + i] <- 1
      if (i == 1) {
        cplex.beg[p + i] <- cplex.beg[p] + cplex.cnt[p]
      } else {
        cplex.beg[p + i] <- cplex.beg[p + i - 1] + cplex.cnt[p + i - 1]
      }
      cplex.ind <- c(cplex.ind, i - 1)
    }

    cplex.ctype <- rep("C", p + n)
    cplex.ctype[int.vec] <- "I"

    cplexAPI::copyLpCPLEX(env = cplex.env, lp = cplex.prob, nCols = cplex.nc, nRows = cplex.nr,
                          lpdir = cplexAPI::CPX_MIN,
                          objf = cplex.obj, rhs = cplex.rhs, sense = cplex.sense,
                          matbeg = cplex.beg, matcnt = cplex.cnt, matind = cplex.ind, matval = cplex.val,
                          lb = cplex.lb, ub = cplex.ub)
    cplexAPI::copyColTypeCPLEX(cplex.env, cplex.prob, cplex.ctype)

    if (verbose) cat(paste("Running cplex ... nrow:", cplex.nr, "ncol:", cplex.nc, "integer:", length(int.vec), "..."))
    lptime <- system.time(
      {
        cplexAPI::mipoptCPLEX(cplex.env, cplex.prob)
        cplex.solution <- cplexAPI::solutionCPLEX(cplex.env, cplex.prob)
        cplexAPI::delProbCPLEX(cplex.env, cplex.prob)
        cplexAPI::closeEnvCPLEX(cplex.env)
      }
    )
    if (verbose) cat(paste("Elapsed: ", sprintf("%1.5f", lptime['elapsed']), "s ... "))

    LPsol <- list()
    LPsol$status <- cplex.solution$lpstat
    LPsol$w <- setNames((cplex.solution$x)[1:p], colnames(bx))
    LPsol$slack <- (cplex.solution$x)[(p+1):(p+n)]
    LPsol$objval <- cplex.solution$objval
    LPsol$fractional <- sum(LPsol$w < 1 - epsilon & LPsol$w > epsilon)
  } else if(control$opt.solver == 'gurobi'){
    # Gurobi
    n1 <- length(y[y == 1])
    n0 <- length(y[y == 0])
    index1 <- (1:n)[y == 1]
    index0 <- (1:n)[y == 0]
    const.mat1 <- cbind(bx, setNames(as.data.frame(diag(ifelse(y == 1, 1L, -bigM))), paste0('s', 1:n)))
    var.names <- colnames(const.mat1)
    const.mat2 <- setNames(as.data.frame(cbind(diag(rep(1L,p)), matrix(0L, nrow = p, ncol = n))), var.names)
    const.mat3 <- setNames(as.data.frame(cbind(matrix(1L, nrow = 1, ncol = p), matrix(0L, nrow = 1, ncol = n))), var.names)
    grbmod <- list()
    const.mat <- rbind(const.mat1,
                       const.mat2,
                       const.mat3)
    grbmod$A <- as.matrix(const.mat, nrow = nrow(const.mat), ncol = ncol(const.mat), byrow = T)
    grbmod$rhs <- c(y, rep(1, p), max.rules)
    grbmod$sense <- c(ifelse(y == 1, ">", "<"), rep("<", p), "<")
    grbmod$grbmodsense <- 'min'
    grbmod$obj <- c(rep(lambda, p), rep(1, n))
    grbmod$vtype <- rep("C", p + n)
    grbmod$vtype[int.vec] <- "B"
    grbparams <- list(OutputFlag=0)
    if (verbose) cat(paste("Running GUROBI ... nrow:", nrow(const.mat), "ncol:", ncol(const.mat), "integer:", length(int.vec), "..."))
    grbtime <- system.time(
      grbsol <- gurobi(grbmod, grbparams)
    )
    if (verbose) cat(paste("Elapsed: ", sprintf("%1.5f", grbtime['elapsed']), "s ... "))

    LPsol <- list()
    LPsol$status <- grbsol$status
    LPsol$w <- setNames((grbsol$x)[1:p], var.names[1:p])
    LPsol$slack <- (grbsol$x)[(p+1):(p+n)]
    LPsol$objval <- grbsol$objval
    LPsol$fractional <- sum(LPsol$w < 1 - epsilon & LPsol$w > epsilon)
  } else if(control$opt.solver == 'greedy'){
    selected_cols <- c()
    subset.rows <- 1:length(y)
    subset.cols <- 1:ncol(bx)
    TP <- rep(0, length(subset.cols))
    FP <- rep(0, length(subset.cols))
    while (TRUE){
      TP[subset.cols] <- 0
      FP[subset.cols] <- 0
      for(j in subset.cols){
        for(i in subset.rows){
          if (bx[i,j] == 1){
            if (y[i] == 1){
              TP[j] <- TP[j] + 1
            } else {
              FP[j] <- FP[j] + 1
            }
          }
        }
      }
      best_net <- -1
      best_j <- 0
      for(j in subset.cols){
        this_net <- TP[j] - FP[j]
        if (this_net > best_net){
          best_j <- j
          best_net <- this_net
        }
      }
      if (best_net < max(lambda, 0)){
        break
      } else {
        selected_cols <- c(selected_cols, best_j)
        subset.cols <- setdiff(subset.cols, best_j)
        subset.rows <- which(bx[,best_j] == 0)
      }
    }
    n.rules <- length(selected_cols)
    rules <- paste(names(bx)[selected_cols], collapse = ' | ')
    rowsum_selected_cols <- rowSums(cbind(rep(0, length(y)), bx[,selected_cols]))
    fitted.values <- integer(length(y))
    fitted.values[rowsum_selected_cols > 0] <- 1
    confusion.matrix <- table(fitted.values, y)
    LPsol <- list()
  }

  if(control$opt.solver %in% c('lpSolve','cplex','gurobi')){
    if (verbose) cat(paste("fractional: ", LPsol$fractional, "\n"))
    fitted.values <- ifelse(LPsol$slack <= epsilon, y, (1 - y))
    confusion.matrix <- table(fitted.values, y)
    n.rules <- sum(LPsol$w > epsilon)
    rules <- paste(names(LPsol$w)[LPsol$w > epsilon], collapse = ' | ')
  }

  bsol <- list(LPsol = LPsol, fitted.values = fitted.values, confusion.matrix = confusion.matrix,
               n.rules = n.rules, rules = rules)

  # debug
  # print(rules)
  # print(LPsol$objval)
  # print(LPsol$w)

  return(bsol)
}

#' Learn a Classification Tree using Boolean Sensing
#'
#' Depending on the arguments provided, either \code{\link[bsnsing]{bsnsing.default}} or \code{\link[bsnsing]{bsnsing.formula}} will be called.
#' @param x a data frame or a \code{\link[stats]{formula}} object.
#' @param ... arguments passed on to \code{\link{bsnsing.default}} or \code{\link{bsnsing.formula}}.
#' @return an object of class \code{bsnsing} for a two-class problem or an object of class \code{mbsnsing} for a multi-class problem.
#'
#' @examples
#' # Use the formula format
#' bs <- bsnsing(Species ~ ., data = iris)
#' summary(bs)
#' summary(bs[[1]])  # display the tree for the first class
#' summary(bs[[2]])  # display the tree for the second class
#' summary(bs[[3]])  # display the tree for the third class
#' predict(bs, type = 'class')  # the fitted class membership
#' predict(bs, type = 'prob')  # the fitted probabilities
#'
#' # Use the (x, y) format, y must have two levels
#' y <- ifelse(iris$Species == 'setosa', 1, 0)
#' x <- iris[, c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width')]
#' bs <- bsnsing(x, y, verbose = T)
#' summary(bs)
#'
#' @export
bsnsing <- function(x, ...) UseMethod("bsnsing")

#' A class that contains multi-class classification model built by bsnsing
#'
#' @export
mbsnsing <- setClass('mbsnsing')


#' Learn a Classification Tree with Boolean Sensing
#'
#' This is the default method for \code{bsnsing} and handles binary classification only. \code{\link{bsnsing.formula}}, which calls \code{bsnsing.default} as the basic tree builder, can handle multiclass classification problems. Missing values in numeric variables are imputed as the median of the non-missing ones, and missing values in factor variables are treated as a separate level named 'NA'.
#'
#' @param x a data frame containing independent variables. Columns can be of numeric, integer, factor and logical types. The column names must be proper identifiers (e.g., must start with a letter, cannot contain special characters and spaces, etc.).
#' @param y a vector of the response variable. The response variable can be of an integer, numeric, logical or factor type, but must have only two unique values. Typical coding of a binary response variable is 0 (for negative case) and 1 (for positive cases).
#' @param control an object of class \code{\link{bscontrol}}.
#' @return an object of class \code{bsnsing}.
#' @examples
#' y <- ifelse(iris$Species == 'setosa', 1, 0)
#' x <- iris[, c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width')]
#' bs <- bsnsing(x, y, verbose = T)
#' summary(bs)
#' @export
#'
bsnsing.default <- function(x, y, controls = bscontrol(), ...) {
  # Make sure no invalid argument exisits and all valid arguments are captured
  extraArgs <- list(...)
  if (length(extraArgs)) {
    controlargs <- names(formals(bscontrol)) # legal arg names
    indx <- match(names(extraArgs), controlargs, nomatch = 0L)
    if (any(indx == 0L))
      stop(gettextf("Argument '%s' not matched",
                    names(extraArgs)[indx == 0L]),
           domain = NA)
  }
  # create the list of controls
  control <- bscontrol(...)
  if (!missing(controls)) control[names(control)] <- controls

  verbose <- control$verbose

  x <- as.data.frame(x)

  # remove the (Intercept) column if exists
  x[,'(Intercept)'] <- NULL

  # Make sure x has appropriate column names
  x.col.names <- colnames(x)
  for (i in 1:length(x.col.names)) {
    if(grepl('[<=>^*-+:/[:space:]]', x.col.names[i])) stop(paste("The column", i, "of x, i.e.,", trimws(x.col.names[i]), ", does not have an acceptable name."))
  }

  # Impute NA in x
  numeric.na.col.index <- seq(ncol(x))[sapply(x, class) == 'numeric' & sapply(x, anyNA) == T]
  factor.na.col.index <- seq(ncol(x))[sapply(x, class) == 'factor' & sapply(x, anyNA) == T]
  if (length(numeric.na.col.index) > 0) {
    # replace NA with column median
    for (j in numeric.na.col.index) {
      naVal <- median(x[,j], na.rm = T)
      x[is.na(x[, j]), j] <- naVal
    }
  }
  if (length(factor.na.col.index) > 0) {
    # treat NA as a level
    for (j in factor.na.col.index) {
      naVal <- 'NA'
      x[is.na(x[, j]), j] <-naVal
    }
  }

  # assign the big-M value
  # if (control$opt.model %in% c('mip', 'MIP')) {
  #   control$bigM <- ncol(x)
  # } else {
  #   control$bigM <- 1L
  # }
  control$bigM <- ncol(x)

  if (control$opt.solver %in% c('cplex','Cplex','CPLEX')) {
    if(is.element('cplexAPI', installed.packages()[,1])) control$opt.solver <- 'cplex'
    else {
      control$opt.solver <- 'lpSolve'
      warning("The cplexAPI is not installed. The opt.solver is set to 'lpSolve' instead.")
    }
  }

  if(verbose) {
    # print out all control values
    print.bscontrol(control)
  }

  # convert y to binary if possible
  ylist <- binarize.y(y, verbose)
  y <- ylist$y
  y.coding.scheme <- ylist$coding.scheme
  ycode <- ylist$ycode

  # initialize bookkeeping variables
  nobs <- nrow(x)
  seq.no <- 0  # global unique node number
  fval <- c()  # fitted values
  fprob <- c()  # fitted probability
  n.frac.splits <- 0  # number of splits by fractional LP solution

  node.number <- seq.no  # root node
  node.parent <- (-1)
  node.lchild <- (-1)
  node.rchild <- (-1)
  node.obs <- 1:nrow(x)  # for the root node
  node.nobs <- length(node.obs)
  node.y <- y[node.obs]
  node.n1 <- length(node.y[node.y == 1])
  node.n0 <- length(node.y[node.y == 0])
  node.class <- ifelse(node.n1 >= node.n0, 1L, 0L)
  node.split.rule <- ""
  node.split.target <- node.class  # split target is the majority class in the node
  node.prop <- node.nobs/nobs
  node.prob <- node.n1/node.nobs
  # node.gini <- (node.n1/node.nobs)^2 + (node.n0/node.nobs)^2
  # node.entropy <- ifelse(node.n1*node.n0 == 0, 0, -((node.n1/node.nobs)*log2(node.n1/node.nobs) + (node.n0/node.nobs)*log2(node.n0/node.nobs)))

  node.info <- list(node.number = node.number, node.parent = node.parent, node.lchild = node.lchild, node.rchild = node.rchild,
                    node.class = node.class,
                    node.prop = node.prop, node.nobs = node.nobs, node.n1 = node.n1, node.n0 = node.n0, node.prob = node.prob,
                    node.obs = node.obs, node.split.rule = node.split.rule,
                    node.split.target = node.split.target)
  tree.nodes <- list()

  todo.nodes <- list(node.info)

  iter.count = 0
  while(length(todo.nodes)){
    iter.count <- iter.count + 1
    this <- todo.nodes[[1]]
    todo.nodes[[1]] <- NULL
    if(verbose) cat(paste("Iter:", iter.count, "exploring node", this$node.number, "\n"))

    this.x <- x[this$node.obs,]
    this.y <- y[this$node.obs]
    bx <- binarize(this.x, this.y, target = this$node.split.target, control = control)
    this.rule <- ""
    nfrac <- 0  # number of fractional solutions from LP

    # 'case' is the flag for different cases:
    # 1. perfect split from binarize;
    # 2. no split from LP;
    # 3. null split from LP;
    # 4. valid split from LP
    # 5. binarize returns an empty data.frame
    case <- 0
    if(!is.data.frame(bx)) {
      this.rule <- bx
      case <- 1
    } else {
      if (ncol(bx) == 0) {
        # unable to binarize x, binarize returned an empty data.frame
        this.rule <- ""
        case <- 5
        if (verbose) cat("Case 5: no meaningful binarization. \n")
      } else {
        if (this$node.split.targe == 1L) {
          bsol <- bslearn(bx, this.y, control = control)
        } else {
          bsol <- bslearn(bx, 1 - this.y, control = control)
        }
        if (bsol$n.rules == 0) {
          this.rule <- ""
          case <- 2
        } else {

          left.obs <- with(this.x, this$node.obs[which(eval(parse(text = bsol$rules)))])
          right.obs <- setdiff(this$node.obs, left.obs)
          left.nobs <- length(left.obs)
          right.nobs <- length(right.obs)
          if (left.nobs == 0 | right.nobs == 0) {
            # cat("Case 3 bsol$rules = ")
            # cat(bsol$rules)
            # cat(" LPsol$status = ")
            # cat((bsol$LPsol)$status)
            # cat(" LPsol$objval = ")
            # cat((bsol$LPsol)$objval)
            # cat(" LPsol$fractional = ")
            # cat((bsol$LPsol)$fractional)
            # cat(" LPsol$w = \n")
            # print((bsol$LPsol)$w)
            # cat("\n")
            # cat(" LPsol$slack = \n")
            # print((bsol$LPsol)$slack)
            if (verbose) cat("Case 3 null split produced. \n")

            if(control$opt.model == 'hybrid') {
              if (verbose) cat("Switching temporarily to MIP and trying again. \n")
              control$opt.model <- 'mip'
              if (this$node.split.targe == 1L) {
                bsol <- bslearn(bx, this.y, control = control)
              } else {
                bsol <- bslearn(bx, 1 - this.y, control = control)
              }
              control$opt.model <- 'hybrid'
              if (bsol$n.rules == 0) {
                this.rule <- ""
                case <- 2
                if (verbose) cat("MIP produced an empty rule.\n")
              } else {

                left.obs <- with(this.x, this$node.obs[which(eval(parse(text = bsol$rules)))])
                right.obs <- setdiff(this$node.obs, left.obs)
                left.nobs <- length(left.obs)
                right.nobs <- length(right.obs)
                if (left.nobs == 0 | right.nobs == 0) {
                  this.rule <- ""
                  case <- 3
                  if (verbose) cat("Case 3 null split persists under MIP. \n")
                } else {
                  this.rule <- bsol$rules
                  nfrac <- bsol$LPsol['fractional']
                  if(nfrac > 0) n.frac.splits <- n.frac.splits + 1
                  case <- 4
                  if (verbose) cat("Case 3 condition removed. A valid split is produced by MIP. \n")
                }
              }
            }
            # else if (control$opt.model == 'mip') {
            #   if (verbose) cat("Flip split target and try MIP again. \n")
            #   this$node.split.target <- 1 - this$node.split.target
            #   bx <- binarize(this.x, this.y, target = this$node.split.target, control = control)
            #   if(!is.data.frame(bx)) {
            #     this.rule <- bx
            #     case <- 1
            #   } else {
            #     if (this$node.split.targe == 1L) {
            #       bsol <- bslearn(bx, this.y, control = control)
            #     } else {
            #       bsol <- bslearn(bx, 1 - this.y, control = control)
            #     }
            #     if (bsol$n.rules == 0) {
            #       this.rule <- ""
            #       case <- 2
            #       if (verbose) cat("An empty rule is produced after target flip.\n")
            #     } else {
            #
            #       left.obs <- with(this.x, this$node.obs[which(eval(parse(text = bsol$rules)))])
            #       right.obs <- setdiff(this$node.obs, left.obs)
            #       left.nobs <- length(left.obs)
            #       right.nobs <- length(right.obs)
            #       if (left.nobs == 0 | right.nobs == 0) {
            #         this.rule <- ""
            #         case <- 3
            #         if (verbose) cat("Case 3 null split persists after target flip. \n")
            #       } else {
            #         this.rule <- bsol$rules
            #         nfrac <- bsol$LPsol['fractional']
            #         if(nfrac > 0) n.frac.splits <- n.frac.splits + 1
            #         case <- 4
            #         if (verbose) cat("Case 3 condition removed after target flip. A valid split is produced. \n")
            #       }
            #     }
            #   }
            #
            # }
            else {
              this.rule <- ""
              case <- 3
              if (verbose) cat("Terminate as leaf node.\n")
            }
          } else {
            this.rule <- bsol$rules
            nfrac <- bsol$LPsol['fractional']
            if(nfrac > 0) n.frac.splits <- n.frac.splits + 1
            case <- 4
          }
        }
      }
    }

    if (case == 1) {
      left.obs <- with(this.x, this$node.obs[which(eval(parse(text = this.rule)))])
      right.obs <- setdiff(this$node.obs, left.obs)
      left.nobs <- length(left.obs)
      right.nobs <- length(right.obs)

      this$node.split.rule <- this.rule
      if(verbose) cat(paste("Rule by binarize.numeric:", this.rule, "\n"))
      seq.no <- seq.no + 1
      left.number <- seq.no
      seq.no <- seq.no + 1
      right.number <- seq.no
      left.parent <- this$node.number
      right.parent <- this$node.number
      this$node.lchild <- left.number
      this$node.rchild <- right.number

      left.y <- y[left.obs]
      left.n1 <- length(left.y[left.y == 1])
      left.n0 <- length(left.y[left.y == 0])
      left.class <- ifelse(left.n1 >= left.n0, 1L, 0L)
      left.split.rule <- ""
      left.split.target <- left.class
      left.prop <- left.nobs/nobs
      left.prob <- left.n1/left.nobs
      # left.gini <- (left.n1/left.nobs)^2 + (left.n0/left.nobs)^2
      # left.entropy <- ifelse(left.n1*left.n0 == 0, 0, -((left.n1/left.nobs)*log2(left.n1/left.nobs) + (left.n0/left.nobs)*log2(left.n0/left.nobs)))


      right.y <- y[right.obs]
      right.n1 <- length(right.y[right.y == 1])
      right.n0 <- length(right.y[right.y == 0])
      right.class <- ifelse(right.n1 >= right.n0, 1L, 0L)
      right.split.rule <- ""
      right.split.target <- right.class
      right.prop <- right.nobs/nobs
      right.prob <- right.n1/right.nobs
      # right.gini <- (right.n1/right.nobs)^2 + (right.n0/right.nobs)^2
      # right.entropy <- ifelse(right.n1*right.n0 == 0, 0, -((right.n1/right.nobs)*log2(right.n1/right.nobs) + (right.n0/right.nobs)*log2(right.n0/right.nobs)))

      left <- list(node.number = left.number, node.parent = left.parent, node.lchild = -1, node.rchild = -1, node.class = left.class,
                   node.prop = left.prop, node.nobs = left.nobs, node.n1 = left.n1, node.n0 = left.n0, node.prob = left.prob,
                   node.obs = left.obs, node.split.rule = left.split.rule,
                   node.split.target = left.split.target)
      right <- list(node.number = right.number, node.parent = right.parent, node.lchild = -1, node.rchild = -1,
                    node.class = right.class,
                    node.prop = right.prop, node.nobs = right.nobs, node.n1 = right.n1, node.n0 = right.n0, node.prob = right.prob,
                    node.obs = right.obs, node.split.rule = right.split.rule,
                    node.split.target = right.split.target)
      tree.nodes[[length(tree.nodes) + 1]] <- this
      tree.nodes[[length(tree.nodes) + 1]] <- left
      tree.nodes[[length(tree.nodes) + 1]] <- right

      fval <- c(fval, setNames(rep(left.class, left.nobs), left.obs))
      fval <- c(fval, setNames(rep(right.class, right.nobs), right.obs))
      fprob <- c(fprob, setNames(rep(left.prob, left.nobs), left.obs))
      fprob <- c(fprob, setNames(rep(right.prob, right.nobs), right.obs))

      if(verbose) {
        cat(paste("-- Node", this$node.number, "is perfectly split by rule:", this$node.split.rule, "\n"))
        cat(paste("----> Left (rule = true) leaf: Node", left.number, ", parent =", left.parent, "nobs =", left.nobs, "prop =", sprintf("%1.4f", left.prop), "class =", left.class, "n1 =", left.n1, "n0 =", left.n0, "prob =", sprintf("%1.4f", left.prob), "\n"))
        cat(paste("----> Right (rule = false) leaf: Node", right.number, ", parent =", right.parent, "nobs =", right.nobs, "prop =", sprintf("%1.4f", right.prop), "class =", right.class, "n1 =", right.n1, "n0 =", right.n0, "prob =", sprintf("%1.4f", right.prob), "\n"))
      }
    } else if (case == 2) {
      this$node.split.rule <- this.rule
      tree.nodes[[length(tree.nodes) + 1]] <- this
      fval <- c(fval, setNames(rep(this$node.class, this$node.nobs), this$node.obs))
      fprob <- c(fprob, setNames(rep(this$node.prob, this$node.nobs), this$node.obs))
      if(verbose) cat(paste("-- Node", this$node.number, "becomes a leaf node. No split on it. \n"))
    } else if (case == 3) {
      this$node.split.rule <- this.rule
      tree.nodes[[length(tree.nodes) + 1]] <- this
      fval <- c(fval, setNames(rep(this$node.class, this$node.nobs), this$node.obs))
      fprob <- c(fprob, setNames(rep(this$node.prob, this$node.nobs), this$node.obs))
      if(verbose) cat(paste("-- Node", this$node.number, "becomes a leaf node. A null split is disgarded. \n"))
    } else if (case == 4) {
      left.obs <- with(this.x, this$node.obs[which(eval(parse(text = this.rule)))])
      right.obs <- setdiff(this$node.obs, left.obs)
      left.nobs <- length(left.obs)
      right.nobs <- length(right.obs)
      this$node.split.rule <- this.rule
      if(verbose) cat(paste("Rule by LP:", this.rule, "\n"))
      seq.no <- seq.no + 1
      left.number <- seq.no
      seq.no <- seq.no + 1
      right.number <- seq.no
      left.parent <- this$node.number
      right.parent <- this$node.number
      this$node.lchild <- left.number
      this$node.rchild <- right.number

      left.y <- y[left.obs]
      left.n1 <- length(left.y[left.y == 1])
      left.n0 <- length(left.y[left.y == 0])
      left.class <- ifelse(this$node.split.targe == 1L, 1L, 0L)
      left.split.rule <- ""
      left.split.target <- ifelse(left.n1 >= left.n0, 1L, 0L)
      left.probability <- ifelse(left.n1 >= left.n0, left.n1/(left.n1 + left.n0), left.n0/(left.n1 + left.n0))
      left.prop <- left.nobs/nobs
      left.prob <- left.n1/left.nobs
      # left.gini <- (left.n1/left.nobs)^2 + (left.n0/left.nobs)^2
      # left.entropy <- ifelse(left.n1*left.n0 == 0, 0, -((left.n1/left.nobs)*log2(left.n1/left.nobs) + (left.n0/left.nobs)*log2(left.n0/left.nobs)))


      right.y <- y[right.obs]
      right.n1 <- length(right.y[right.y == 1])
      right.n0 <- length(right.y[right.y == 0])
      right.class <- ifelse(this$node.split.targe == 1L, 0L, 1L)
      right.split.rule <- ""
      right.split.target <- ifelse(right.n1 >= right.n0, 1L, 0L)
      right.probability <- ifelse(right.n1 >= right.n0, right.n1/(right.n1 + right.n0), right.n0/(right.n1 + right.n0))
      right.prop <- right.nobs/nobs
      right.prob <- right.n1/right.nobs
      # right.gini <- (right.n1/right.nobs)^2 + (right.n0/right.nobs)^2
      # right.entropy <- ifelse(right.n1*right.n0 == 0, 0, -((right.n1/right.nobs)*log2(right.n1/right.nobs) + (right.n0/right.nobs)*log2(right.n0/right.nobs)))


      left <- list(node.number = left.number, node.parent = left.parent, node.lchild = -1, node.rchild = -1, node.class = left.class,
                   node.prop = left.prop, node.nobs = left.nobs, node.n1 = left.n1, node.n0 = left.n0, node.prob = left.prob,
                   node.obs = left.obs, node.split.rule = left.split.rule,
                   node.split.target = left.split.target)
      right <- list(node.number = right.number, node.parent = right.parent, node.lchild = -1, node.rchild = -1,
                    node.class = right.class,
                    node.prop = right.prop, node.nobs = right.nobs, node.n1 = right.n1, node.n0 = right.n0, node.prob = right.prob,
                    node.obs = right.obs, node.split.rule = right.split.rule,
                    node.split.target = right.split.target)

      tree.nodes[[length(tree.nodes) + 1]] <- this
      if(verbose) cat(paste("-- Node",this$node.number,"is split by rule:", this$node.split.rule, ", nfrac =", nfrac, "\n"))

      if (left.nobs > control$node.size & left.n1 != 0 & left.n0 != 0 & left.probability < control$stop.prob) {
        todo.nodes[[length(todo.nodes) + 1]] <- left
        if(verbose) cat(paste("----> Left (rule = true) child: Node", left.number, ", parent =", left.parent, "nobs =", left.nobs,
                              "prop =", sprintf("%1.4f", left.prop), "class =", left.class, "n1 =", left.n1, "n0 =", left.n0, "prob =", sprintf("%1.4f", left.prob), "\n"))
      } else {
        tree.nodes[[length(tree.nodes) + 1]] <- left
        fval <- c(fval, setNames(rep(left.class, left.nobs), left.obs))
        fprob <- c(fprob, setNames(rep(left.prob, left.nobs), left.obs))
        if(verbose) {
          cat(paste("----> Left (rule = true) leaf: Node", left.number, ", parent =",
                    left.parent, "nobs =", left.nobs, "prop =", sprintf("%1.4f", left.prop), "class =", left.class, "n1 =",
                    left.n1, "n0 =", left.n0, "prob =", sprintf("%1.4f", left.prob), "\n"))
          if(left.nobs <= control$node.size) {
            cat("* Reason for leaf: minimum node size reached. (node.size)\n")
          }
          if(left.probability >= control$stop.prob) {
            cat("* Reason for leaf: desired node probability reached. (stop.prob)\n")
          }
        }
      }

      if (right.nobs > control$node.size & right.n1 != 0 & right.n0 != 0 & right.probability < control$stop.prob) {
        todo.nodes[[length(todo.nodes) + 1]] <- right
        if(verbose) cat(paste("----> Right (rule = false) child: Node", right.number, ", parent =", right.parent, "nobs =", right.nobs,
                              "prop =", sprintf("%1.4f", right.prop), "class =", right.class, "n1 =", right.n1, "n0 =", right.n0, "prob =", sprintf("%1.4f", right.prob), "\n"))
      } else {
        tree.nodes[[length(tree.nodes) + 1]] <- right
        fval <- c(fval, setNames(rep(right.class, right.nobs), right.obs))
        fprob <- c(fprob, setNames(rep(right.prob, right.nobs), right.obs))
        if(verbose) {
          cat(paste("----> Right (rule = false) leaf: Node", right.number, ", parent =",
                    right.parent, "nobs =", right.nobs, "prop =", sprintf("%1.4f", right.prop), "class =", right.class, "n1 =",
                    right.n1, "n0 =", right.n0, "prob =", sprintf("%1.4f", right.prob), "\n"))
          if(right.nobs <= control$node.size) {
            cat("* Reason for leaf: minimum node size reached. (node.size)\n")
          }
          if(right.probability >= control$stop.prob) {
            cat("* Reason for leaf: desired node probability reached. (stop.prob)\n")
          }
        }
      }
    } else if (case == 5) {
      this$node.split.rule <- this.rule
      tree.nodes[[length(tree.nodes) + 1]] <- this
      fval <- c(fval, setNames(rep(this$node.class, this$node.nobs), this$node.obs))
      fprob <- c(fval, setNames(rep(this$node.prob, this$node.nobs), this$node.obs))
      if(verbose) cat(paste("-- Node", this$node.number, "becomes a leaf node. No split on it. \n"))
    } else {
      stop("Something went wrong.")
    }
  }

  if (verbose) cat("An optimal tree is built.\n")

  fitted.values <- fval[order(as.numeric(names(fval)))]
  fitted.prob <- fprob[order(as.numeric(names(fprob)))]
  confusion.matrix <- table(fitted.values, y, dnn = c("fitted", "actual"))
  # if(verbose) {
  #   cat("Confusion matrix: \n")
  #   print(confusion.matrix)
  # }
  class(tree.nodes) <- 'treelist'
  res = list(tree.nodes = tree.nodes,
             fitted.values = fitted.values,
             fitted.prob = fitted.prob,
             confusion.matrix = confusion.matrix,
             n.frac.splits = n.frac.splits,
             y.coding.scheme = y.coding.scheme,
             ycode = ycode,
             call = match.call())

  class(res) <- "bsnsing"
  return(res)
}


#' Learn a Classification Tree using Boolean Sensing
#'
#' The program builds a binary classification tree for each unique value in the response variable. Each tree classifies a target value against all the other values (internally coded as 'all.other') in the response variable. If the response variable is a numeric type, the number of unique values must not exceed 30. There is no programmatic restriction on the number of unique levels for a factor response.
#'
#' @param formula an object of class "\code{\link[stats]{formula}}": a symbolic description of the model to be fitted.
#' @param data an optional data frame, list or environment (or object coercible by \code{\link{as.data.frame}} to a data frame) containing the variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically the environment from which \code{bsnsing.formula} is called.
#' @param subset an optional vector specifying a subset (in terms of index numbers, not actual data) of observations to be used in the fitting process.
#' @param na.action a function which indicates what should happen when the data contain NAs. If \code{na.pass} is used, \code{bsnsing} will still apply an internal NA treatment logic, as follows: missing values in numeric variables will be replaced by the median of the non-missing values of the variable; missing values in factor variables will be treated as a spearate level named 'NA'.
#' @param ... additional arguments to be passed to the low level fitting functions, e.g., elements in the \code{\link{bscontrol}} object.
#' @return an object of \code{\link{bsnsing}} for a two-class problem or an object of \code{\link{mbsnsing}} for a multi-class problem.
#' @examples
#' # Multi-class classification
#' bs <- bsnsing(Species ~ ., data = iris)
#' summary(bs)
#' summary(bs[[1]])  # display the tree for the first class
#' summary(bs[[2]])  # display the tree for the second class
#' summary(bs[[3]])  # display the tree for the third class
#'
#' # Two-class classification
#' data(Auto, package = 'ISLR')
#' Auto$origin <- ifelse(Auto$origin == 1, 'USA', 'NonUSA')
#' bs <- bsnsing(origin ~ mpg + displacement + horsepower + weight, data = Auto, verbose = T)
#' summary(bs)
#' @export
#'
bsnsing.formula <- function(formula, data, subset, na.action = na.pass, ...) {
  Call <- match.call()
  indx <- match(c("formula", "data", "subset"), names(Call), nomatch = 0L)
  if (indx[1] == 0L) stop("a 'formula' argument is required")
  temp <- Call[c(1L, indx)]      # only keep the arguments we wanted
  temp$na.action <- na.action    # This one has a default
  temp[[1L]] <- quote(stats::model.frame) # change the function called
  mf <- eval.parent(temp)
  Terms <- attr(mf, "terms")

  # remove factor variables with only 1 unique value
  mfv <- mf[, attr(Terms, "term.labels")]
  factorcol <- sapply(mfv, function(x) is.factor(x))
  factormfv <- mfv[, factorcol, drop = F]
  collevels <- sapply(factormfv, function(x) length(levels(x)))
  removecol <- names(collevels[collevels == 1])
  if(length(removecol) > 0) stop(paste("Factor variable", removecol, "has only one unique level. Remove this variable and try again."))
  x <- model.matrix(Terms, data = mf)
  y <- model.response(mf)

  # Decide if y is binary or multiclass
  nclass <- length(unique(y))
  if(nclass <= 2) {
    contrasts <- contrasts(as.factor(y))
    res <- bsnsing.default(x, y, ...)
    res$terms <- Terms
    res$contrasts <- contrasts
    res$call <- match.call()
    return(res)
  } else if (nclass <= 30 | is.factor(y)) {
    multires <- list()
    ylevels <- sort(unique(y))
    for (i in 1:nclass) {
      yi <- as.factor(ifelse(y == ylevels[i], as.character(ylevels[i]), 'all.other'))  # assuming there is no level called "all.other"
      contrasts <- contrasts(yi)
      res <- bsnsing.default(x, yi, ...)
      res$terms <- Terms
      res$contrasts <- contrasts
      res$call <- match.call()
      multires[[i]] <- res
    }
    multires$call <- match.call()
    multires$nclass <- nclass
    multires$ylevels <- ylevels
    multires$yprior <- prop.table(table(y))[ylevels]
    class(multires) <- 'mbsnsing'
    return(multires)
  } else {
    cat("There are more than 30 unique values in the response variable. Bsnsing is for classification. If you insist running bsnsing on the data, change the response variable type to factor and try again. \n")
  }
}


#' Define Parameters for the \code{\link{bsnsing}} Fit
#' @param lambda the penalty multiplier of the L1 norm of the solution vector.
#' @param bin.size the minimum number of observations required in a binarization bucket.
#' @param max.rules the upper bound of the L1 norm of the solution vector.
#' @param nseg.numeric the maximum number of segments the range of a numeric variable is divided into for each inequality direction.
#' @param nseg.factor the maximum number of unique levels allowed in a factor variable.
#' @param num2factor an equality binarization rule will be created for each unique value of a numeric variable (in addition to the inequality binarization attempt), if the number of unique values of the numeric variable is less than \code{num2factor}.
#' @param LP.epsilon any element in the LP solution \code{w} smaller than \code{LP.epsilon} will be taken as zero.
#' @param node.size if the number of training cases falling into a tree node is less than \code{node.size}, the node will become a leaf and no further split will be attempted on it.
#' @param stop.prob if the proportion of the majority class in a tree node is greater than \code{stop.prob}, the node will become a leaf and no further split will be attempted on it.
#' @param opt.solver a character string in the set {'cplex', 'gurobi', 'lpSolve', 'greedy'} indicating the optimization solver to be used in the program. The choice of 'cplex' requires the package \code{\link[cplexAPI]{cplexAPI}}, 'gurobi' requires the package \code{\link[gurobi]{gurobi}}, and 'lpSolve' requires the package \code{\link[lpSolve]{lpSolve}}. The default is 'cplex'.
#' @param opt.model a character string in the set {'mip', 'hybrid', 'lp'} indicating the optimization model to solve in the program. The default is 'mip'. The choice of 'lp' is faster but may sacrifice the classification accuracy.
#' @param bigM a positive integer representing the big M value used in the MIP formulation. The default is 1.
#' @param verbose a logical value (TRUE or FALSE) indicating whether the solution details are to be printed on the screen.
#' @return An object of class \code{\link{bscontrol}}.
#' @examples
#' bscontrol()  # display the default parameters
#' bsc <- bscontrol(stop.prob = 0.8, nseg.numeric = 10, verbose = T)
#' bsc
#' @export
#'

bscontrol <- function(lambda = 1L, bin.size = 5, max.rules = Inf,
                            nseg.numeric = 10, nseg.factor = 20, num2factor = 5,
                            LP.epsilon = 1e-8,
                            node.size = 20, stop.prob = 0.9,
                            opt.solver = c('cplex', 'lpSolve','gurobi','greedy'),
                            opt.model = c('mip', 'hybrid', 'lp'), bigM = 1,
                            verbose = F) {
  if (lambda < 0L) {
    warning("The value of 'lambda' supplied is < 0; the value 1 was used instead")
    lambda <- 1L
  }
  if (bin.size < 1L) {
    warning("The value of 'bin.size' supplied is < 1; the value 1 was used instead")
    bin.size <- 1L
  }
  if (max.rules < 1L) {
    warning("The value of 'max.rules' supplied is < 1; the value 1 was used instead")
    max.rules <- 1L
  }
  if (nseg.numeric < 3L) {
    warning("The value of 'nseg.numeric' supplied is < 3; the value 3 was used instead")
    nseg.numeric <- 3L
  }
  if (nseg.factor < 3L) {
    warning("The value of 'nseg.factor' supplied is < 3; the value 3 was used instead")
    nseg.factor <- 3L
  }
  if (num2factor > 2*nseg.numeric) {
    warning("The value of 'num2factor' is too big; it is set to 2*nseg.numeric")
    num2factor <- 2*nseg.numeric
  }
  if (stop.prob < 0 | stop.prob > 1) {
    warning("The value of 'stop.prob' supplied is not in [0, 1]; the value 0.97 was used instead")
    stop.prob <- 0.95
  }
  if (node.size < 1L) {
    warning("The value of 'node.size' supplied is < 1; the value 3 was used instead")
    node.size <- 1L
  }

  control <- list(lambda = lambda, bin.size = bin.size, max.rules = max.rules, nseg.numeric = nseg.numeric,
       nseg.factor = nseg.factor, num2factor = num2factor, LP.epsilon = LP.epsilon, node.size = node.size, stop.prob = stop.prob,
       opt.solver = match.arg(opt.solver),
       opt.model = match.arg(opt.model), bigM = bigM, verbose = verbose)
  class(control) <- "bscontrol"
  return(control)
}

#' Print the Object of Class \code{\link{bscontrol}}
#' @param control an object of class \code{\link{bscontrol}}.
#' @export
print.bscontrol <- function(control = bscontrol()) {
  # Make sure no invalid argument exisits and all valid arguments are captured
  controlargs <- names(formals(bscontrol)) # legal arg names
  controls <- control
  default.controls <- bscontrol()

  cat("--------------------------------\n")
  cat("Control Parameters: [name = value (default)] \n")
  for(arg in controlargs) {
    cat(paste("\t", arg, "=", controls[[arg]], "(", default.controls[[arg]], ")\n"))
  }
  cat("--------------------------------\n")
}

#' Print the Object of Class \code{\link{bsnsing}}
#'
#' @param object an object of class \code{\link{bsnsing}}.
#' @export
print.bsnsing <- function(object, print.call = T, ...){
  if (print.call) {
    cat("Call: ")
    print(object$call)
  }
  cat(object$y.coding.scheme)
  # print.bscontrol(object$control)
  cat("\nConfusion matrix:\n")
  print(object$confusion.matrix)
  xname <- deparse(substitute(object))
  cat(paste0("Use the summary() function to access details of the tree.", "\n"))
}

#' Print the Object of Class \code{\link{mbsnsing}}
#'
#' @param object an object of class \code{\link{mbsnsing}}.
#' @export
print.mbsnsing <- function(object, ...){
  cat("Call: ")
  print(object$call)
  cat(paste0(object$nclass, " trees, one for each class.", "\n"))
  for (i in 1:object$nclass) {
    cat(paste0("Tree ", i, ":", "\n"))
    print(object[[i]], print.call = F)
  }
}

#' Summarize mbsnsing Model Fits
#'
#' @param object an object of class \code{\link{mbsnsing}}.
#' @return a list of detailed information in the given \code{object}.
#' @export
summary.mbsnsing <- function(object = stop("no 'object' arg"), ...) {
  smries <- list()
  # for (i in 1:object$nclass) {
  #   smries[[i]] <- summary.bsnsing(object[[i]])
  # }
  smries$nclass <- object$nclass
  smries$ylevels <- object$ylevels
  smries$yprior <- object$yprior
  smries$call <- match.call()
  smries$argname <- deparse(substitute(object))
  class(smries) <- "summary.mbsnsing"
  return(smries)
}

#' Print the summary of \code{\link{mbsnsing}} model fits
#'
#' @param object an object of class \code{\link{summary.mbsnsing}}.
#' @export
print.summary.mbsnsing <- function(object, ...) {
  xname <- object$argname
  cat(paste0("There are ", object$nclass, " trees in ", xname, ", one for each class in [", paste(as.character(object$ylevels), collapse = ', '), "].", "\n"))
  cat(paste0("Use the command summary(", xname, "[[1]]) to access details of the 1st tree, for instance.", "\n"))
}

#' Summarize the bsnsing Model Fits
#'
#' @param object an object of class \code{\link{bsnsing}}.
#' @return a list of detailed information in the given \code{object}.
#' @export
summary.bsnsing <- function(object = stop("no 'object' arg"), ...) {
  # Calculate the confusion matrix and accuracy for the fit
  # The number of leaf nodes

  tree.nodes <- (object$tree.nodes)[order(sapply((object$tree.nodes),'[[',1))]
  if ((tree.nodes[[length(tree.nodes)]])$node.number != length(tree.nodes) - 1) stop("Something messed up in the summary function.")

  current.i <- 1  ## current list element
  current.node.number <- current.i - 1  ## current node number
  depth <- 0L  ## distance to the root node
  nsplits <- 0  # number of splits in the tree
  done <- rep(0, length(tree.nodes))  ## 0: not visited, 1: self done, 2: left child done, 3: all done
  nodes <- data.frame(node.number = integer(),
                      depth = integer(),
                      predicted.class = integer(),
                      prob.1 = numeric(),
                      proportion = numeric(),
                      n1 = integer(),
                      n0 = integer(),
                      parent.node = integer(),
                      # gini = numeric(),
                      # entropy = numeric(),
                      rule = character(),
                      if.rule.then = integer(),
                      is.leaf = logical(),
                      stringsAsFactors=FALSE)
  while (TRUE) {
    if (current.i <= 0) break
    this.node <- tree.nodes[[current.i]]
    if (done[current.i] == 3) {
      if (this.node$node.parent == -1) break
      else {
        current.i <- this.node$node.parent + 1
        depth <- depth - 1
        next
      }
    } else if (done[current.i] == 2) {
      done[current.i] <- done[current.i] + 1
      current.i <- this.node$node.rchild + 1
      depth <- depth + 1
      next
    } else if (done[current.i] == 1) {
      done[current.i] <- done[current.i] + 1
      current.i <- this.node$node.lchild + 1
      depth <- depth + 1
      nsplits <- nsplits + 1
      next
    } else {
      this.record <- data.frame(node.number = this.node$node.number,
                                depth = depth,
                                predicted.class = this.node$node.class,
                                prob.1 = ifelse(this.node$node.n1 == 0, 0, ifelse(this.node$node.n0 == 0, 1, this.node$node.n1/(this.node$node.n1 + this.node$node.n0))),
                                proportion = this.node$node.prop,
                                n1 = this.node$node.n1,
                                n0 = this.node$node.n0,
                                parent.node = this.node$node.parent,
                                # gini = this.node$node.gini,
                                # entropy = this.node$node.entropy,
                                rule = this.node$node.split.rule,
                                if.rule.then = this.node$node.split.target,
                                is.leaf = ifelse(this.node$node.lchild == -1 & this.node$node.rchild == -1, T, F))
      nodes <- rbind(nodes, this.record)
      done[current.i] <- 1
    }
    if (this.node$node.lchild == -1 & this.node$node.rchild == -1) {
      done[current.i] <- 3
      current.i <- this.node$node.parent + 1
      depth <- depth - 1
      next
    }
  }

  nleaves <- sum(nodes$is.leaf)  # number of leaves in the tree
  accuracy <- (object$confusion.matrix[1,1] + ifelse(sum(dim(object$confusion.matrix)) == 4, object$confusion.matrix[2,2], 0))/sum(object$confusion.matrix)
  tree.summary <- list(nleaves = nleaves,
                       nsplits = nsplits,
                       n.frac.splits = object$n.frac.splits,
                       confusion.matrix = object$confusion.matrix,
                       accuracy = accuracy,
                       y.coding.scheme = object$y.coding.scheme,
                       nodes = nodes, call = match.call())
  class(tree.summary) <- "summary.bsnsing"
  return(tree.summary)
}

#' Print the Summary of \code{\link{bsnsing}} Model Fits
#'
#' @param object an object of class \code{\link{summary.bsnsing}}.
#' @export
print.summary.bsnsing <- function(object, print.call = T, ...) {
  nodes <- object$nodes
  if (print.call) {
    cat("Call: ")
    print(object$call)
  }
  cat(object$y.coding.scheme)
  cat("\nDecision Tree:\n")
  cat("-------------\n")
  for(i in 1:nrow(nodes)) {
    line.info <- paste0(paste(rep(" ", 6*nodes[i, 'depth']), collapse = ""), ifelse(nodes[i, 'is.leaf'], "*", ""), "Node ", nodes[i, 'node.number'], ":", " class = ", nodes[i, 'predicted.class'], " w/ prob ", sprintf("%1.4f", ifelse(nodes[i, 'predicted.class'] == 1, nodes[i, 'prob.1'], 1 - nodes[i, 'prob.1'])), ", prop ", sprintf("%1.4f", nodes[i, 'proportion']),
                        # ", gini ", sprintf("%1.4f", nodes[i, 'gini']), ", entropy ", sprintf("%1.4f", nodes[i, 'entropy']),
                        ", n1 ", nodes[i, 'n1'], ", n0 ", nodes[i, 'n0'], "\n")
    cat(line.info)
    if (nodes[i, 'rule'] != "") {
      split.info <- paste0(paste(rep(" ", 6*nodes[i, 'depth']), collapse = ""), "Split by (", nodes[i, 'rule'], ")\n")
      cat(split.info)
    }
  }
  cat("-------------\n")
  cat(paste0("The tree has ", object$nsplits, " splits (", object$n.frac.splits, " fractional), ", object$nleaves, " leaf nodes. \n"))
  cat("Confusion matrix:\n")
  print(object$confusion.matrix)
  cat(paste0("Accuracy: ", sprintf("%1.4f", object$accuracy), "\n"))
}


#' Recode a Variable with Two Unique Values into an 0/1 Vector
#'
#' @param y a vector, must contain two unique values.
#' @param verbose a logical value, TRUE or FALSE, indicating whehter details are to be printed on the screen.
#' @return a list with three elements: \code{y}, a vector of the same length as \code{y}, whose entries are coded to 0 and 1, \code{coding.scheme}, a character string describing the map from the original coding to 0/1 coding, and \code{ycode}, a character vector containing the original level names of \code{y}.
#' @examples
#' y <- factor(c('good', 'bad', 'good', 'good', 'bad'))
#' (yb <- binarize.y(y))
#' y <- c(T, F, F, F, T)
#' (yb <- binarize.y(y))
#' y <- c(1, 2, 2, 1, 2)
#' (yb <- binarize.y(y))
#' @export
#'
binarize.y <- function(y, verbose = F) {
  if (!is.numeric(y) & !is.factor(y) & !is.integer(y) & !is.logical(y) & !is.character(y)) stop("Response variable y must be a numeric, integer, logical, factor or character typed.")
  coding.scheme <- character()
  ycode <- c()
  if (is.character(y)) y <- as.factor(y)
  if (is.numeric(y)) {
    if (length(unique(y)) == 2) {
      ymax <- max(y)
      ymin <- min(y)
      y[y == ymax] <- 1L
      y[y == ymin] <- 0L
      coding.scheme <- paste("Response coding:", ymax, "==> 1  ", ymin, "==> 0")
      ycode <- c(ymin, ymax)
    } else {
      stop("Response must have two unique values.")
    }
  } else if (is.factor(y)) {
    ylevels <- as.character(sort(unique(y), decreasing = T))
    if (length(ylevels) == 2 & !"all.other" %in% ylevels) {
      y <- ifelse(y == ylevels[1], 1L, 0L)
      coding.scheme <- paste("Response coding:",  ylevels[1], "==> 1  ",  ylevels[2], "==> 0")
      ycode <- c(ylevels[2], ylevels[1])
    } else if (length(ylevels) == 2 & "all.other" %in% ylevels) {
      y <- ifelse(y == "all.other", 0L, 1L)
      coding.scheme <- paste("Response coding:",  setdiff(ylevels, "all.other") , "==> 1  ", "all.other ==> 0")
      ycode <- c("all.other", setdiff(ylevels, "all.other"))
    } else {
      stop("Response must have two unique levels.")
    }
  } else if (is.integer(y)) {
    if (length(unique(y)) == 2) {
      ymax <- max(y)
      ymin <- min(y)
      y[y == ymax] <- 1L
      y[y == ymin] <- 0L
      coding.scheme <- paste("Response coding:", ymax, "==> 1  ", ymin, "==> 0")
      ycode <- c(ymin, ymax)
    } else {
      stop("Response must have two unique values.")
    }
  } else if (is.logical(y)) {
    if (length(unique(y)) == 2) {
      y[y == T] <- 1L
      y[y == F] <- 0L
      coding.scheme <- paste("Response coding: True ==> 1   False ==> 0")
      ycode <- c(FALSE, TRUE)
    } else {
      stop("Response must have two unique levels.")
    }
  } else stop("Response variable must be a numeric, integer, logical or factor.")
  if (verbose) cat(paste(coding.scheme, "\n"))
  ylist <- list(y = y, coding.scheme = coding.scheme, ycode = ycode)
  return(ylist)
}


#' Make Predictions with a Fitted \code{\link{bsnsing}} Model
#'
#' Implements the generic \code{predict} function to make predictions on new data using a trained \code{\link{bsnsing}} model.
#' @param object a \code{\link{bsnsing}} model object.
#' @param newdata a optional data frame in which to look for variables for prediction. If omitted, the fitted class or probability will be returned.
#' @param type a character string indicating the type of prediction. \emph{'prob'} predicts the probability of being a positive case (i.e., y = 1), and \emph{'class'} predicts the class membership.
#' @return a vector containing the predicted values.
#' @examples
#' # Load data
#' data('GlaucomaMVF', package = 'ipred')
#' n <- nrow(GlaucomaMVF)
#' train_index = sample(1:n, round(0.5*n))
#' test_index = setdiff(1:n, train_index)
#' # Fit a model using training set
#' bs <- bsnsing(Class ~ ., data = GlaucomaMVF, subset = train_index)
#' # Make predictions on the test set
#' pred <- predict(bs, GlaucomaMVF[test_index, ], type = 'class')
#' # Display the confusion matrix
#' table(pred, actual = GlaucomaMVF[test_index, 'Class'])
#' @export
#'
predict.bsnsing <- function(object, newdata = NULL, type = c("prob", "class")) {
  if (!inherits(object, "bsnsing")) stop("Not a legitimate \"bsnsing\" object")
  type <- match.arg(type)
  ycode <- object$ycode

  if (is.null(newdata)) {
    if (type == 'prob') {
      return(object$fitted.prob)
    } else if (type == 'class') {
      pred <- ifelse(object$fitted.values == 0, ycode[1], ycode[2])
      return(pred)
    } else stop("unrecognized 'type' arg")
  } else {
    if (is.null(object$terms)) {
      # Model is built by bsnsing.default
      newdata <- ifelse(is.data.frame(newdata), newdata, as.data.frame(newdata))
      # ycode <- object$ycode
    } else {
      # Model is built by bsnsing.formula
      if (is.null(attr(newdata, "terms"))) {
        Terms <- delete.response(object$terms)
        mf <- model.frame(Terms, newdata, xlev = attr(object, "xlevels"), na.action = na.pass)
        newdata <- as.data.frame(model.matrix(Terms, mf))
      }
      # ycode <- rownames(object$contrasts)  # this line may be unnecessary
    }
    tree.nodes <- (object$tree.nodes)[order(sapply((object$tree.nodes),'[[',1))]
    if ((tree.nodes[[length(tree.nodes)]])$node.number != length(tree.nodes) - 1) stop("Something messed up in the predict function.")
    current.i <- 1  ## current list element
    current.node.number <- current.i - 1  ## current node number
    tree.nodes[[current.i]]$node.obs <- 1:nrow(newdata)  ## assign node.obs for the root node
    this.node <- tree.nodes[[current.i]]
    if (type == 'prob') {
      pred = setNames(rep(this.node$node.prob, nrow(newdata)), 1:nrow(newdata))
      #pred = setNames(rep(ifelse(this.node$node.n1 == 0, 0, ifelse(this.node$node.n0 == 0, 1, this.node$node.n1/(this.node$node.n1 + this.node$node.n0))), nrow(newdata)), 1:nrow(newdata))
    } else if (type == 'class') {
      pred = setNames(rep(ycode[this.node$node.class + 1], nrow(newdata)), 1:nrow(newdata))
    } else stop("The 'type' argument is not recognized")

    done <- rep(0, length(tree.nodes))  ## 0: not visited, 1: self done, 2: left child done, 3: all done
    while(TRUE) {
      if (current.i <= 0) break
      this.node <- tree.nodes[[current.i]]
      if (done[current.i] == 3) {
        if (this.node$node.parent == -1) break
        else {
          current.i <- this.node$node.parent + 1
          next
        }
      } else if (done[current.i] == 2) {
        done[current.i] <- done[current.i] + 1
        current.i <- this.node$node.rchild + 1
        next
      } else if (done[current.i] == 1) {
        done[current.i] <- done[current.i] + 1
        current.i <- this.node$node.lchild + 1
        next
      } else {
        if (type == 'prob') {
          pred[this.node$node.obs] = this.node$node.prob #ifelse(this.node$node.n1 == 0, 0, ifelse(this.node$node.n0 == 0, 1, this.node$node.n1/(this.node$node.n1 + this.node$node.n0)))
        } else if (type == 'class') {
          pred[this.node$node.obs] = ycode[this.node$node.class + 1]
        } else stop("The 'type' argument is not recognized")

        if (this.node$node.lchild == -1 & this.node$node.rchild == -1) {
          done[current.i] <- 3
          current.i <- this.node$node.parent + 1
          next
        } else {
          this.x <- newdata[this.node$node.obs,]
          left.obs <- with(this.x, this.node$node.obs[which(eval(parse(text = this.node$node.split.rule)))])
          right.obs <- setdiff(this.node$node.obs, left.obs)
          tree.nodes[[this.node$node.lchild + 1]]$node.obs <- left.obs
          tree.nodes[[this.node$node.rchild + 1]]$node.obs <- right.obs
          done[current.i] <- 1
        }
      }
    }
    return(pred)
  }
}

#' Make Predictions with a Fitted \code{\link{bsnsing}} Model
#'
#' @param object an object of class \code{\link{mbsnsing}}.
#' @param an optional data frame in which to look for variables for prediction. If omitted, the fitted class or probability will be returned.
#' @param type a character string indicating the type of prediction. \emph{'prob'} predicts the probability of being in each class, and \emph{'class'} predicts the class membership.
#' @return a data frame containing the predicted values.
#' @examples
#' n <- nrow(iris)
#' train_index <- sample(1:n, round(0.5*n))
#' test_index <- setdiff(1:n, train_index)
#' # Fit a model on the training set
#' bs <- bsnsing(Species ~ ., data = iris, subset = train_index)
#' # Make predictions on the test set
#' pred <- predict(bs, iris[test_index, ], type = 'class')
#' # Display the confusion matrix
#' table(pred, actual = iris[test_index, 'Species'])
#' # Predict the probabilities
#' predprob <- predict(bs, iris[test_index, ], type = 'prob')
#' head(predprob)
#' @export
#'
predict.mbsnsing <- function(object, newdata = NULL, type = c("prob", "class")) {
  if (!inherits(object, "mbsnsing")) stop("Not a legitimate \"mbsnsing\" object")
  type <- match.arg(type)
  nclass <- object$nclass
  ylevels <- object$ylevels
  yprior <- object$yprior
  if (is.null(newdata)) {
    mpred <- matrix(rep(NA, length(object[[1]]$fitted.values)), ncol = 1, dimnames = list(names(object[[1]]$fitted.values), c("class")))
  } else {
    mpred <- matrix(rep(NA, nrow(newdata)), ncol = 1, dimnames = list(rownames(newdata), c("class")))
  }
  for (i in 1:nclass) {
    pred <- predict.bsnsing(object[[i]], newdata, type = "prob")
    pred <- as.matrix(pred, ncol = 1)
    colnames(pred) <- as.character(ylevels[i])
    mpred <- cbind(mpred, pred)
  }

  # weight by class prior probability
  for (level in ylevels) {
    mpred[,level] <- mpred[,level] * yprior[level]
  }
  # normalize the class probabilities
  rsum <- base::rowSums(mpred, na.rm = T)
  for (level in ylevels) {
    mpred[,level] <- ifelse(rsum != 0, mpred[,level]/rsum, 0)
  }
  if (type == 'prob') {
    return(mpred[, as.character(ylevels)])
  } else if (type == 'class') {
    mpred[,'class'] <- as.character(ylevels[apply(mpred[, as.character(ylevels)], 1, which.max)])
    return(mpred[, 'class'])
  } else stop("unrecognized 'type' arg")
}
