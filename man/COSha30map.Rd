\name{COSha30map}
\alias{COSha30map}
\docType{data}
\title{
Map of total soil carbon stock (t/ha) at 0-30 cm depth
}
\description{
Map of total soil carbon stock (t ha\eqn{^{-1}}) at 0-30 cm depth at La Libertad Research Center. The map was obtained through ordinary kriging interpolation. Spatial reference system: UTM 18N
}
\usage{data(COSha30map)}
\format{
  The format is:
Formal class 'SpatialPixelsDataFrame' [package "sp"] 
}
\source{
Santacruz, A., 2010. \emph{Design of optimal spatial sampling networks for the monitoring of soil organic carbon at La Libertad Research Center through the application of genetic algorithms}. M.Sc. Thesis. National University of Colombia, Bogot�. 162 p. (In Spanish)
}
\references{
Santacruz, A., 2011. \emph{Evolutionary optimization of spatial sampling networks. An application of genetic algorithms and geostatistics for the monitoring of soil organic carbon}. Editorial Acad�mica Espa�ola. 183 p. ISBN: 978-3-8454-9815-7 (In Spanish) 
}
\seealso{
\code{\link{COSha30}}
}
\examples{
data(COSha30map)
data(lalib)
summary(COSha30map)
l1 = list("sp.polygons", lalib)
spplot(COSha30map, "var1.pred", main="Soil carbon stock (t/ha) at 0-30 cm depth", 
    col.regions=bpy.colors(100), scales = list(draw =TRUE), xlab ="East (m)", 
    ylab = "North (m)", sp.layout=list(l1))
}
\keyword{datasets}