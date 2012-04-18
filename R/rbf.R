
assign("rbf",
   function(sigma, z, coordinates, newdata, n.neigh, func){
   if(func=="TPS") library(limSolve)
   rbf.pred <- as.data.frame(matrix(NA,nrow= nrow(newdata), ncol=4))
   colnames(rbf.pred) <- c("x","y","var1.pred","var1.var")
rbf0 <- function(sigma, z, coordinates, newdata, n.neigh, func){   
   xy <- coordinates
   So <- newdata                                                               # Coordenadas del punto a estimar
   m.dist <- as.matrix(dist(rbind(xy,So)))                                     # matriz de distancias
   dist.So <- m.dist[nrow(m.dist),1:(ncol(m.dist)-1)]                          # vector de distancias al punto So.
   neigh.orden <- order(dist.So)                                               # vecinos ordenados
   dist.neigh.cerca <- dist.So[neigh.orden[1:n.neigh]]                         # vecinos mas cercanos
   m.dist.neigh <- as.matrix(dist(coordinates))[neigh.orden[1:n.neigh], neigh.orden[1:n.neigh]]
   phi <- RBF.phi(m.dist.neigh,sigma,func)
   one = rep(1,n.neigh)
   PHI.Matriz <- rbind(as.matrix(cbind(phi, one)),c(one,0))
   b <- RBF.phi(dist.neigh.cerca,sigma,func)
   PHI.Vector <- as.matrix(c(b,1))
   W.rbf <- if(func=="TPS") Solve(PHI.Matriz, PHI.Vector) else solve(PHI.Matriz, PHI.Vector)
   RBF.pred <- W.rbf[1:n.neigh]%*%z[as.numeric(colnames(phi))]
   RBF.pred
}
rbf.pred[,3] <- apply(newdata,1,rbf0, sigma=sigma, z=z, coordinates=coordinates, n.neigh=n.neigh, func=func)
rbf.pred[,1:2] <- newdata
rbf.pred
}
)