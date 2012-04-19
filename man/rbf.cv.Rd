\name{rbf.cv}
\alias{rbf.cv}

\title{
generates a RMSPE value, result of cross validation leave-one-out
}
\description{
Generate the RMSPE value, which is given by the radial basis function
with parameter eta
}
\usage{
rbf.cv(eta, z, coordinates, n.neigh, func)
}

\arguments{
\item{eta}{the optimal smoothing parameter, we recommend using the parameter found by minimizing the root-mean-square prediction errors using cross-validation}
\item{z}{vector of observations associated with the dependent variable, also called regionalized variable}
\item{coordinates}{spatial coordinates \emph{x} and \emph{y}}
\item{n.neigh}{number of nearest observations that should be used for a \emph{rbf} prediction, where nearest is defined in terms of the spatial locations}
\item{func}{radial basis function model type, e.g. "GAU", "EXPON", "TRI", "TPS", "CRS", "ST", "IM" and "M", are currently available}
}
\value{
returns the RMSPE value
}
\seealso{
\code{\link{rbf}}
}
\examples{
data(preci)
rbf.cv(eta=0.1460814, z=preci$prec, coordinates=preci[,2:3],
	n.neigh=9, func="TPS")
}

\keyword{ spatial }
