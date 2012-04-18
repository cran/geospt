assign("graph.rbf",
   function(z, coordinates, newdata, n.neigh, func, np, dmax, n.sigma, P.T){

Opt <- optimize(rbf.cv, c(0.00001,dmax), z=z, coordinates=coordinates, n.neigh=n.neigh, func=func)
cat("Optimal sigma RBF: ", func, "\n", "SIGMA  = ", Opt$minimum, "\n", "RMSPE   = ", Opt$objective, "\n")

Datos <- as.data.frame(matrix(NA,nrow= length(seq(0.01, dmax, length.out=np)), ncol=2))           # dmax=1, "MC"
sigma <-  seq(0.01, Opt$minimum*n.sigma, length.out=np)                                              # M*6,  MI*1.1,  TPS*11,   EXP*3.1
colnames(Datos) <- c("P","RMSPE")
for(i in 1:np){
Datos[i,1] <- sigma[i]
Datos[i,2] <- rbf.cv(sigma=sigma[i], z=z, coordinates=coordinates, n.neigh=n.neigh, func)
}
Table0 <- rbind(Datos,c(Opt$minimum,Opt$objective))
orden <- order(Table0$P)
Table <- Table0[orden,]
ifelse(P.T == TRUE,print(Table),"")
plot(Table, lty=3, ylab="RMSPE", col=3, xlab="SIGMA", type="l")         # sub=paste("optimal sigma =",round(Opt$minimum,4),":","RMSPE=", round(Opt$objective,4))        # main=paste("Optimization Sigma", func)
}
)
