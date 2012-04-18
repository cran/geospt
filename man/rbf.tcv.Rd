\name{rbf.tcv}
\alias{rbf.tcv}

\title{
rbf cross validation, leave-one-out
}
\description{
Cross validation functions for gaussian (GAU), exponential (EXP), trigonometric (TRI),
thin plate spline (TPS), inverse multiquadratic (IM), and multiquadratic
(M) radial basis functions (\emph{rbf})
}
\usage{
rbf.tcv(sigma, z, coordinates, n.neigh, func)
}

\arguments{
  \item{sigma}{
value of sigma, optimal sigma advisable
}
  \item{z}{
vector of observations associated with the dependent variable, also
called regionalized variable
}
  \item{coordinates}{
spatial coordinates \emph{x} and \emph{y}
}
  \item{n.neigh}{
number of nearest observations that should be used for a \emph{rbf}
prediction, where nearest is defined in terms of the spatial locations
}
  \item{func}{
radial basis function model type, e.g. "GAU", "EXP", "TRI", "TPS",
"MI" and "M", are currently available
}
}
\details{
Leave-one-out cross validation (LOOCV) visits a data point, predicts
the value at that location by leaving out the observed value, and
proceeds with the next data point. The observed value is left out
because rbf would otherwise predict the value itself.
}
\value{
data frame contain the data coordinates, prediction columns, observed
values, residuals, the prediction variance, zscore (residual divided
by standard error) which left with NA's, and the fold column which
is associated to cross-validation count. Prediction columns and residuals
are obtained from cross-validation data points
}
\seealso{
\link{rbf}
}
\examples{
data(preci)
attach(preci)
rbf.tcv(sigma=0.1460814, z=prec, coordinates=preci[,2:3], n.neigh=9,
    func="TPS")
}

\keyword{ spatial }
