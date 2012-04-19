\name{graph.rbf}
\alias{graph.rbf}

\title{
graph that describes the behavior of the optimized parameter eta, associated with radial basis function
}
\description{
Function for plotting the RMSPE in several values of smoothing parameter
eta with the same dataset. A curve is plotted to the points, and
then from the curve, the optimal eta that provides the smallest
RMSPE is determined by \code{optimize} function from the \code{stats}
package.
}
\usage{
graph.rbf(z, coordinates, newdata, n.neigh, func, np, dmax, n.eta,
P.T)
}

\arguments{
\item{z}{vector of observations associated with the dependent variable, also called regionalized variable}
\item{coordinates}{spatial coordinates \emph{x} and \emph{y}}
\item{newdata}{ data frame or \emph{x} and \emph{y} locations, where you want to generate new predictions. In the case of a single point, the object must be
of class matrix}
\item{n.neigh}{number of nearest observations that should be used for a \emph{rbf} prediction, where nearest is defined in terms of the spatial locations}
\item{func}{function to be optimized. Radial basis function model type, e.g. gaussian "GAU", exponential "EXPON", trigonometric "TRI", thin plate spline 
"TPS", completely regularized spline "CRS", spline with tension "ST", inverse multiquadratic "IM", and multiquadratic "M", are currently available}
\item{np}{number of points, where the radial basis function is calculated}
\item{dmax}{maximum range value which assess the \code{optimize} function, to be searched for the eta minimum}
\item{n.eta}{factor X axis length, number of times of the smoothing parameter eta}
\item{P.T}{ logical. Print Table (P.T), T=True or F=False}
}
\value{
returns a graph that describes the behavior of the optimized parameter eta, and a table of values associated with the graph including optimal smoothing 
parameter eta
}
\references{
Johnston K, Ver J, Krivoruchko K, Lucas N. 2001. \emph{Using ArcGIS Geostatistical Analysis ESRI}.
}
\examples{
data(preci)
graph.rbf(z=preci$prec, coordinates=preci[,2:3],n.neigh=9, func="TPS", np=40, 
	dmax=0.3, n.eta=6, P.T=TRUE)
}

\keyword{ spatial }
