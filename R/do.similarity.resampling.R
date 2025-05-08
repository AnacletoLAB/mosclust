"do.similarity.resampling" <-
function(X, c=2, nsub=100, f=0.8, s=sFM, alg.clust.sim=Hierarchical.sim.resampling, 
                                     distance="euclidean", hmethod="ward.D") {
 if (length(c) == 1)
   c <-2:c;
 num.clustering <- length(c);
 for (i in c)
   if (i<2) {
	   print("Number of clusters must be more than 2.");
		 return(0);
	 }
 sim.matrix <- matrix(numeric(num.clustering*nsub), nrow=num.clustering);
 j <- 0;
 for (num.clust in c) {
   j <- j + 1;
   sim.matrix[j,] <- alg.clust.sim (X, c = num.clust, nsub=nsub, f = f, s = s, distance=distance, hmethod=hmethod);
 }
 return(sim.matrix);
}

