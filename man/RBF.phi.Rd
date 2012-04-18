\name{RBF.phi}
\alias{RBF.phi}

\title{
RBF.phi
}
\description{
generate the value associated with radial basis functions; gaussian
(GAU), exponential (EXPON), trigonometric (TRI), thin plate spline (TPS), inverse multiquadratic (IM), and multiquadratic (M)
}
\usage{
RBF.phi(distance, sigma, func)
}

\arguments{
\item{distance}{corresponds to the Euclidean distance between two points in space}
\item{sigma}{The optimal smoothing parameter is found by minimizing the root-mean-square prediction errors using cross-validation}
\item{func}{radial basis function model type, e.g. "GAU", "EXPON", "TRI", "TPS", "IM" and "M", are currently available}
}
\value{
value obtained from the radial basis function generated with a distance,
a sigma smoothing parameter, and a function "GAU", "EXPON", "TRI", "TPS", "IM" or "M"
}
\examples{
data(preci) 
d1 <- dist(rbind(preci[1,],preci[2,])) 
RBF.phi(distance=d1, sigma=0.5, func="TPS")
}

\keyword{ spatial }
