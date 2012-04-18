seqPtsOptNet <- function(formula, locations, spDatF, fitmodel, n=1, prevSeqs=NULL, popSize, generations, xmin, ymin, xmax, ymax, plotMap=FALSE, spMap=NULL, ...){
	evaluate <- function(string=c()) {
		returnVal = NA;
		pts2 <- as.data.frame(matrix(0, ncol=2, nrow=n))
		for (i in 1:n){
			pts2[i,1] <- round(string[i], 1)
		}
		for (j in 1:n){
			pts2[j,2] <- round(string[n + j], 1)
		}
		names(pts2) <- c("x", "y")
		coordinates(pts2) = c("x", "y")
		if (is.null(prevSeqs)==FALSE){
			pts2 <- rbind(prevSeqs, pts2)	
		}
		if (plotMap==TRUE) {
			if(is.null(spMap)) stop(paste("if plotMap=TRUE, spMap must also be provided"))
			plot(spMap, xlim=c(bbox(spMap)[1],bbox(spMap)[3]), ylim=c(bbox(spMap)[2],bbox(spMap)[4]), ...)
		    plot(pts2, add=TRUE)
		}
		interp <- krige(formula, locations, data=spDatF, newdata=pts2, model=fitmodel, ...)
		returnVal <- sum(sqrt(interp[["var1.var"]]))/n
		returnVal
	}
	results <- rbga(as.matrix(c(rep(xmin,n), rep(ymin, n))), as.matrix(c(rep(xmax,n), rep(ymax, n))), popSize=popSize, evalFunc=evaluate, verbose=TRUE, iters=generations, ...)
}

