# if(sigma!= 0) stop(paste("sigma must be greater than 0"))
assign("RBF.phi",
 function(distance, sigma, func){
switch(func,
M = (sqrt(distance^2+ sigma^2))^1,
IM = (sqrt(distance^2+ sigma^2))^(-1),
TPS = ifelse(distance>0,((distance*sigma)^2)*log(distance*sigma),0),
GAU = exp(-sigma*(distance^2)),
EXPON = exp(-(distance*sigma)),
TRI = sin(distance*sigma)
)
}
)