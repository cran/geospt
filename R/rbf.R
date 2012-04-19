
assign("rbf",
   function(eta, z, coordinates, newdata, n.neigh, func){
   if(func=="TPS") library(limSolve)
   rbf.pred <- as.data.frame(matrix(NA,nrow= nrow(newdata), ncol=4))
   colnames(rbf.pred) <- c("x","y","var1.pred","var1.var")
rbf0 <- function(eta, z, coordinates, newdata, n.neigh, func){   
   dist.newdata <- as.numeric(Dis(coordinates,newdata))
   neigh.orden <- order(dist.newdata)                                               # vecinos ordenados
   dist.neigh.cerca <- dist.newdata[neigh.orden[1:n.neigh]]                         # vecinos mas cercanos
   m.dist.neigh <- as.matrix(dist(coordinates[neigh.orden[1:n.neigh],])) 
   phi <- RBF.phi(m.dist.neigh,eta,func)
   PHI.Matriz <- rbind(as.matrix(cbind(phi, 1)),c(rep(1,n.neigh),0))
   b <- RBF.phi(dist.neigh.cerca,eta,func)
   PHI.Vector <- as.matrix(c(b,1))
   W.rbf <- if(func=="TPS") Solve(PHI.Matriz, PHI.Vector) else solve(PHI.Matriz, PHI.Vector)
   RBF.pred <- W.rbf[1:n.neigh]%*%z[as.numeric(colnames(phi))]
   RBF.pred
}
rbf.pred[,3] <- apply(newdata, 1, rbf0, eta=eta, z=z, coordinates=coordinates, n.neigh=n.neigh, func=func)
rbf.pred[,1:2] <- newdata
rbf.pred
}
)