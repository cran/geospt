
assign("graph.rbf",
   function(z, coordinates, newdata, n.neigh, func, np, dmax, n.eta, P.T){
if(!is.logical(P.T))  stop(paste("P.T must be logical"))
Opt <- optimize(rbf.cv, c(0.00001,dmax), z=z, coordinates=coordinates, n.neigh=n.neigh, func=func)
Datos <- as.data.frame(matrix(NA,nrow= length(seq(0.01, dmax, length.out=np)), ncol=2))           
eta <-  seq(0.01, Opt$minimum*n.eta, length.out=np)                                             
colnames(Datos) <- c("P","RMSPE")
for(i in 1:np){
Datos[i,1] <- eta[i]
Datos[i,2] <- rbf.cv(eta=eta[i], z=z, coordinates=coordinates, n.neigh=n.neigh, func)
}
Table0 <- rbind(Datos,c(Opt$minimum,Opt$objective))
orden <- order(Table0$P)
Table <- Table0[orden,]
plot(Table, lty=3, ylab="RMSPE", col=3, xlab="ETA", type="l")
Optim <- Table[which.min(Table[,2]),] 
ifelse(P.T == T,list(print(Table), cat("Optimal eta RBF: ", func, "\n", "ETA  = ", Optim$P, "\n", "RMSPE   = ", Optim$RMSPE, "\n")),
list(cat("Optimal eta RBF: ", func, "\n", "ETA  = ", Optim$P, "\n", "RMSPE   = ", Optim$RMSPE, "\n")))
}
)
