"Hierarchical.sim.projection" <-
function(X, c = 2, nprojections=100, dim=2, pmethod="RS", 
                                        scale=TRUE, seed=100, s=sFM, distance="euclidean", hmethod="ward") {

 n <- ncol(X);
 sim.vector <- numeric(nprojections);
 cl <- Multiple.Random.hclustering (X, dim=dim, pmethod=pmethod, c=c, hmethod=hmethod, n=nprojections*2, scale=scale, 
                                    distance=distance, seed=seed);
 for (i in 1:nprojections)  {   
	 cl1 <- cl[[i]];
	 M1 <- Do.boolean.membership.matrix(cl1, n, 1:n);
	 cl2 <- cl[[i+nprojections]];
	 M2 <- Do.boolean.membership.matrix(cl2, n, 1:n);
	 sim.vector[i] <- s(M1,M2);
 }
 return(sim.vector);
}

