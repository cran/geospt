
assign("rbf.tcv",
   function(eta, z, coordinates, n.neigh, func){
   rbf.pred <- as.data.frame(matrix(NA,nrow= length(z), ncol=8))
   colnames(rbf.pred) <- c("var1.pred","var1.var","observed","residual","zscore","fold","x","y")
   for(i in 1:(length(z))){
   rbf.pred[i,1] <- rbf(eta=eta, z, coordinates=coordinates[-i,], newdata=coordinates[i,], n.neigh, func)[,3]
   rbf.pred[i,6] <- i
}
rbf.pred[,3]<- z
rbf.pred[,7:8]<-coordinates
rbf.pred[,4]<- rbf.pred[,3]-rbf.pred[,1]
rbf.pred
}
)