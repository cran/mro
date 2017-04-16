#' @title Multiple Correlation Test of Significance
#' @author Abirami S
#' @description  Tests the significance of mutliple correlation coefficient
#' @param x Data Matrix or Variance Covariance or Correlation matrix
#' @param ld Label of dependent Variable
#' @param rd Vector of labels of independent variables
#' @return a htest class object
#' @export
#' @import MASS,matrixcalc
#' @examples
#' ## Example
#' library(MASS)
#' mu<-c(10,12,13,14)
#' sig<-matrix(0,4,4)
#' diag(sig)<-c(2,1,1,2)
#' da<-mvrnorm(25,mu,sig)
#' mcr.test(da,1,c(2:4))
mcr.test<-function(x,ld,rd)
{mc<-mcr(x,ld,rd)
N<-nrow(x)
p<-ncol(x)
T<-(mc*mc*(N-p))/((1-mc*mc)*(p-1))
rt<-list(MC=mc,Statistic.Value=T,p.value=1-stats::pf(T,p-1,N-p),df1=p,df2=N-p)
ht2<-structure(
  list(
    statistic=c(Statistic=rt$Statistic.Value),
    p.value=rt$p.value,
    parameter=c(df1=round(rt$df1),df2=round(rt$df2),sample.MC=rt$MC),
    alternative="true population multiple correlation greater than 0",
    method="Testing Multiple Correlation Coefficient is Zero",
    data.name=deparse(substitute(x))
    ## null.value= "greater than 0"
  ),
  .Names=c("statistic","p.value", "parameter","alternative","method","data.name"),
  class="htest"
)
ht2
}
