"do.similarity.noise" <-
function(X, c=2, nnoisy=100, perc=0.5, seed=100, s=sFM, 
                                    alg.clust.sim=Hierarchical.sim.noise, distance="euclidean", hmethod="ward.D") {
 if (length(c) == 1)
   c <-2:c;
 num.clustering <- length(c);
 for (i in c)
   if (i<2) {
	   print("Number of clusters must be more than 2.");
		 return(0);
	 }
 set.seed(seed);
 sim.matrix <- matrix(numeric(num.clustering*nnoisy), nrow=num.clustering);
 j <- 0;
 for (num.clust in c) {
   j <- j + 1;
   sim.matrix[j,] <- alg.clust.sim (X, c = num.clust, nnoisy=nnoisy, perc=perc, s = s, distance=distance, hmethod=hmethod);
 }
 return(sim.matrix);
}

