#' @title Multiple Correlation
#' @author Abirami S
#' @description  Computes Mutliple Correlation Coefficient between one variable and a set of variables
#' @param dda Data
#' @param ld Dependent Variable
#' @param rd vector of independent variables
#' @param rawdata a boolean variable taking F if the input is a correlation matrix T if it is data matrix
#' @return Returns the value of Multiple Correlation between dependent and independent variables
#' @export
#' @import MASS,matrixcalc
#'
#' @examples
#'
#'## Example 1:
#'mcr(iris[,-5],1,c(2,3,4))  ## Returns multiple correlation between Sepal.Length
#'                           ## and the other variables
#'
#'## Example 2
#' mu<-c(10,12,13,14)
#' sig<-matrix(0,4,4)
#' diag(sig)<-c(2,1,1,3)
#' da<-MASS::mvrnorm(25,mu,sig)
#' mcr(da, 2,c(1,3,4))       ## Returns Multiple correlation when the data matrix
#'                           ## simulated from a quadrivariate normal distribution
#'                           ## is given as input
#'
#'## Example 3
#' da<-var(iris[,-5])
#' mcr(da,3,c(1,2,4),FALSE) ## Returns multiple correlation between Petal.Width
#'                          ## and the other variables when the correlation matrix
#'                          ## is given as input
#'
mcr<-function (dda, ld, rd, rawdata=T)
{   if (rawdata==T) a <- (stats::var(dda))
  if (rawdata==F)   { a<-dda
                      if (!matrixcalc::is.positive.definite(a)) stop("Input matrix not positive definite")
                     }

  s11 <- a[ld, ld]
  s1tr <- a[ld, rd]
  s22 <- a[rd, rd]
  s1tr <- as.matrix(s1tr)
  as.vector(sqrt((t(s1tr) %*% solve(s22) %*% (s1tr))/s11))
}

