
assign("rbf.cv",
  function(eta, z, coordinates, n.neigh, func){
  rbf.pred <- as.data.frame(matrix(NA,nrow= length(z), ncol=4))
  colnames(rbf.pred) <- c("x","y","var1.pred","var1.var")
  for(i in 1:(length(z))){                                                
  rbf.pred[i,3] <- rbf(eta, z, coordinates=coordinates[-i,], newdata=coordinates[i,], n.neigh, func)[,3]
  }
RMSPE <-  sqrt(sum((rbf.pred$var1.pred-z)^2)/length(z))
RMSPE
}
)