\name{rbf}
\alias{rbf}

\title{
gaussian, exponential, trigonometric, thin plate spline, inverse multiquadratic, and multiquadratic radial basis function prediction
}
\description{               
Function for gaussian (GAU), exponential (EXPON), trigonometric (TRI), thin plate spline (TPS), completely regularized spline (CRS), 
spline with tension (ST), inverse multiquadratic (IM), and multiquadratic (M) radial basis function (\emph{rbf}),
where \emph{rbf} is in a local neighbourhood
}
\usage{
rbf(eta, z, coordinates, newdata, n.neigh, func)
}

\arguments{
\item{eta}{the optimal smoothing parameter, we recommend using the parameter
found by minimizing the root-mean-square prediction errors using cross-validation}
\item{z}{vector of observations associated with the dependent variable, also
called regionalized variable}
\item{coordinates}{spatial coordinates \emph{x} and \emph{y}}
\item{newdata}{data frame, or \emph{x} and \emph{y} locations, where you want to
generate new predictions. In the case of a single point, the object
must be of class matrix}
\item{n.neigh}{number of nearest observations that should be used for a \emph{rbf}
prediction, where nearest is defined in terms of the spatial locations}
\item{func}{radial basis function model type, e.g. "GAU", "EXPON", "TRI", "TPS",
"CRS","ST","IM" and "M", are currently available}
}
\details{
rbf function generates individual predictions from gaussian (GAU), exponential (EXPON), trigonometric (TRI)
thin plate spline (TPS), completely regularized spline (CRS), spline with tension (ST), 
inverse multiquadratic (IM), and multiquadratic (M) functions
}
\value{
Attributes columns contain coordinates and predictions, and the variance
column contains NA's
}
\examples{
\dontrun{ 
data(preci) 

# prediction case a point 
point <- matrix(c(3,4),nrow=1) 
rbf(eta=0.1460814, z=preci$prec, coordinates=preci[,2:3], newdata=point,
	n.neigh=10, func="TPS") 

# prediction case a grid of points 
puntos<-expand.grid(x=seq(min(preci$x),max(preci$x),0.05), y=seq(min(preci$y),
	max(preci$y),0.05)) 
pred.rbf <- rbf(eta=0.1460814, z=preci$prec, coordinates=preci[,2:3],
	newdata=puntos, n.neigh=10, func="TPS") 
coordinates(pred.rbf) = c("x", "y") 
gridded(pred.rbf) <- TRUE 

# show prediction map 
spplot(pred.rbf["var1.pred"], cuts=40, col.regions=bpy.colors(100), 
	main = "rainfall map TPS", key.space=list(space="right", cex=0.8)) 
}
}
\keyword{ spatial }
