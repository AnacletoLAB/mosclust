"do.similarity.projection" <-
function(X, c=2, nprojections=100, dim=2, pmethod="PMO", scale=TRUE, seed=100, s=sFM, 
                                    alg.clust.sim=Hierarchical.sim.projection, distance="euclidean", hmethod="ward.D") {
 if (length(c) == 1)
   c <-2:c;
 num.clustering <- length(c);
 for (i in c)
   if (i<2) {
	   stop("Number of clusters must be more than 2.");
		 #return(0);
	 }
 sim.matrix <- matrix(numeric(num.clustering*nprojections), nrow=num.clustering);
 j <- 0;
 for (num.clust in c) {
   j <- j + 1;
   sim.matrix[j,] <- alg.clust.sim (X, c = num.clust, nprojections=nprojections, dim = dim,  
	                                  pmethod=pmethod, scale=scale, seed=seed, s = s, distance=distance, hmethod=hmethod);
 }
 return(sim.matrix);
}

