\name{criterio.cv}
\alias{criterio.cv}

\title{
Cross-validation summaries
}
\description{
Generate a data frame of statistical values associated with cross-validation
}
\usage{
criterio.cv(m.cv)
}

\arguments{
  \item{m.cv}{
data frame containing: the coordinates of data, prediction columns,
prediction variance of cross-validation data points, observed values,
residuals, zscore (residual divided by kriging standard error), and
fold. In the case of using rbf.tcv function, the prediction variance
and zscore (residual divided by standard error) will have NA's
}
}
\value{
data frame containing: mean prediction errors (MPE), average kriging
standard error (ASEPE), root-mean-square prediction errors (RMSPE),
mean standardized prediction errors (MSPE), root-mean-square standardized
prediction errors (RMSSPE), and coefficient of determination (R2).
}
\examples{
data(meuse) 
coordinates(meuse) <- ~x+y 
m <- vgm(.59, "Sph", 874, .04) 

# leave-one-out cross validation: 
out <- krige.cv(log(zinc)~1, meuse, m, nmax = 40) 
criterio.cv(out)

# multiquadratic function 
data(preci)
attach(preci)

# optimal sigma
tab <- rbf.tcv(eta=1.488733, z=prec, coordinates=preci[,2:3],
    n.neigh=9, func="M") 
criterio.cv(tab)
}

\keyword{ spatial }
