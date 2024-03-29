% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bsnsing.R
\name{bsnsing.default}
\alias{bsnsing.default}
\title{Learn a Classification Tree with Boolean Sensing}
\usage{
\method{bsnsing}{default}(x, y, controls = bscontrol(), ...)
}
\arguments{
\item{x}{a data frame containing independent variables. Columns can be of numeric, integer, factor and logical types. The column names must be proper identifiers (e.g., must start with a letter, cannot contain special characters and spaces, etc.).}

\item{y}{a vector of the response variable. The response variable can be of an integer, numeric, logical or factor type, but must have only two unique values. Typical coding of a binary response variable is 0 (for negative case) and 1 (for positive cases).}

\item{controls}{an object of class \code{\link{bscontrol}}.}

\item{...}{further argument to be passed to bsnsing.default.}
}
\value{
an object of class \code{bsnsing}.
}
\description{
This is the default method for \code{bsnsing} and handles binary classification only. \code{\link{bsnsing.formula}}, which calls \code{bsnsing.default} as the basic tree builder, can handle multiclass classification problems. Missing values in numeric variables are imputed as the median of the non-missing ones, and missing values in factor variables are treated as a separate level named 'NA'.
}
\examples{
\dontrun{
y <- ifelse(iris$Species == 'setosa', 1L, 0L)
x <- iris[, c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width')]
bs <- bsnsing(x, y, verbose = TRUE)
summary(bs)
}
}
