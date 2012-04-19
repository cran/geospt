assign("Dis",
function(coordinates, newdata){
Dis <- rep(NA,nrow(coordinates))
for (i in 1:(nrow(coordinates)))
Dis[i] = dist(rbind(coordinates[i,],newdata))
Dis
}
)