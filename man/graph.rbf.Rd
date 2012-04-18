\name{graph.rbf}
\alias{graph.rbf}

\title{
graph that describes the behavior of the optimized parameter sigma, associated with radial basis function
}
\description{
Function for plotting the RMSPE in several values of smoothing parameter
sigma with the same dataset. A curve is plotted to the points, and
then from the curve, the optimal sigma that provides the smallest
RMSPE is determined by the \code{optimize} function from the \code{stats}
package.
}
\usage{
graph.rbf(z, coordinates, newdata, n.neigh, func, np, dmax, n.sigma,
P.T)
}

\arguments{
\item{z}{vector of observations associated with the dependent variable, also called regionalized variable}
\item{coordinates}{spatial coordinates \emph{x} and \emph{y}}
\item{newdata}{ data frame or \emph{x} and \emph{y} locations, where you want to generate new predictions. In the case of a single point, the object must be
of class matrix}
\item{n.neigh}{number of nearest observations that should be used for a \emph{rbf} prediction, where nearest is defined in terms of the spatial locations}
\item{func}{function to be optimized. Radial basis function model type, e.g. "GAU", "EXP", "TRI", "TPS", "IM" and "M", are currently available}
\item{np}{number of points, where the radial basis function is calculated}
\item{dmax}{maximum range value which assess the \code{optimize} function, to be searched for the sigma minimum}
\item{n.sigma}{factor X axis length, number of times of the smoothing parameter sigma}
\item{P.T}{ logical. Print Table (P.T), T=True or F=False}
}
\value{
returns a graph that describes the behavior of the optimized parameter sigma
}
\references{
Johnston, K., Ver, J., Krivoruchko, K., Lucas, N., 2001. \emph{Using ArcGIS Geostatistical Analysis ESRI}.
}
\examples{
data(preci)
graph.rbf(z=preci$prec, coordinates=preci[,2:3],n.neigh=9, func="TPS", np=40, 
    dmax=0.3, n.sigma=6, P.T=TRUE)
}

\keyword{ spatial }
