                       ############################################################################
                       #################          POCKET PLOT PROGRAM           ###################
                       ############################################################################

# The Pocket Plot (so named because of its use in detecting pockets of non-stationarity)
# is a necessary technique to identify a localized area atypical with respect to the stationarity model,
# and it is built to exploit the spatial nature of the data through the coordinates of rows and columns
# (east "X" and north "y" respectively).

# Program analysis of local stationarity
# PPR (Probabilities PocketPlot by rows, ie horizontal "south-north")
# PPC (Probabilities PocketPlot by columns, ie vertical "east-west")
# PVR (PocketPlot of variance by rows, ie horizontal "south-north")
# PVC (PocketPlot of variance by columns, ie vertical "east-west")


assign("pocket.plot",
   function(data, graph, X, Y, Z,...){
Pocket.plotA<-function(datos,statistical,X,Y,Z, direct, xlab){
grid.mat<-tapply(Z,list(factor(Y),factor(X)),mean)
grid.matt<-grid.mat[max(nrow(grid.mat)):1,]


vect<-function(grid.matt,j){
    n<-length(grid.matt)
    z<- matrix(numeric(n),nrow(grid.matt),)
    for(i in 1:(nrow(grid.matt)-1)){
    z[i,j]<-ifelse(grid.matt[i,j]&grid.matt[i+1,j]!="NA",(abs(grid.matt[i,j]-grid.matt[i+1,j]))^(0.5),NA)
    }
    z
    }
matriz<-vect(grid.matt,1:ncol(grid.matt))

b<-matrix(matriz[1:nrow(grid.matt)-1,],nrow=nrow(grid.matt)-1)
y.barra<-mean(b,na.rm=TRUE)


poc<-function(grid.matt,i,j){
    l<-length(grid.matt)
    L<- matrix(numeric(l),nrow(grid.matt),)
    for(k in 1:nrow(grid.matt)){
    L[k,]<-ifelse(grid.matt[i,]&grid.matt[k,]!="NA",(abs(grid.matt[i,]-grid.matt[k,]))^(0.5),NA)
    L[i,]<-rep(NA,ncol(grid.matt))
    }
    L
    }

arreglo<-function(grid.matt){
    W<-array(rep(0,length(grid.matt)),c(nrow(grid.matt), ncol(grid.matt),nrow(grid.matt)))
    for(k in 1:nrow(grid.matt)){
    W[,,k]<-poc(grid.matt,k,1:ncol(grid.matt))
    }
    W
    }

f<-arreglo(grid.matt)

media.conteo<-function(f,x){
    mean.f<- matrix(rep(0,nrow(f[,,1])*nrow(f[,,1])),nrow(f[,,1]),nrow(f[,,1]))
    U<- matrix(rep(0,nrow(f[,,1])*nrow(f[,,1])),nrow(f[,,1]),nrow(f[,,1]))
    for(j in 1:nrow(f[,,1])){
    mean.f[j,]<-apply(f[,,j],1,mean,na.rm=TRUE)
    U[j,]<- apply(f[,,j],1,sum,na.rm=TRUE)
    }
    switch(x,
    media.barra=mean.f,
    conteo=U/mean.f)
    }


d<-media.conteo(f,"media.barra")-y.barra
N<-media.conteo(f,"conteo")
tabla<-as.matrix(d)
tablaN<-as.matrix(N[max(nrow(N)):1,])


rotulo<-rownames(grid.matt)
colnames(tabla)<-rotulo
rownames(tabla)<-rotulo
tabla1<-tabla[max(nrow(tabla)):1,]
N1<-as.vector(t(tablaN))
tabla2<-data.frame(P=as.vector(t(tabla1)),group=rep(factor(colnames(tabla1),levels=colnames(tabla1)),nrow(tabla1)),group1=rep(factor(rownames(tabla1),levels=rownames(tabla1)),rep(ncol(tabla1),ncol(tabla1))),N1,Q=(N1^0.5)*((as.vector(t(tabla1+y.barra))/y.barra)-1))
Prob<-function(P){
boxplot(P~group1, data=tabla2, main = paste("POCKET-PLOT in", direct, "direction"), col.main="blue", col=4, cex=0.7, out=TRUE, ylab= "P(jk)", xlab=paste(xlab, "Number"), lwd=1.5, col.main="blue", col.lab="blue", cex.lab=1.2)
box(lty = 'solid', col = 'blue', lwd=2)
abline(h=0, col="green")
identify(tabla2$group1, tabla2$P, tabla2$group, cex=0.8)
}
                             #main=paste("Sigma graphic optimization", func)
Var<-function(V){
boxplot(Q~group1, data=tabla2, main = paste("POCKET-PLOT of Standardized Variance in", direct, "direction"), col.main="blue", col=4, cex=0.7, out=TRUE, ylab= "Q(jk)", xlab=paste(xlab, "Number"), lwd=1.5, col.main="blue", col.lab="blue", cex.lab=1.2)
box(lty = 'solid', col = 'blue', lwd=2)
abline(h=0, col="green")
identify(tabla2$group1, tabla2$Q, tabla2$group, cex=0.8)
}


#print(tabla2)
switch(statistical,
P=Prob(tabla2),
V=Var(tabla2)
)
}

switch(graph,
PPR=Pocket.plotA(datos, "P", X, Y, Z, direc="south-north", xlab="Row"),
PPC=Pocket.plotA(datos, "P", Y, X, Z, direc="east-west", xlab="Column"),
PVR=Pocket.plotA(datos, "V", X, Y, Z, direc="south-north", xlab="Row"),
PVC=Pocket.plotA(datos, "V", Y, X, Z, direc="east-west", xlab="Column"),
)

})
