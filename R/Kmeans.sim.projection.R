"Kmeans.sim.projection" <-
function(X, c = 2, nprojections=100, dim=2, pmethod="PMO", 
                                        scale=TRUE, seed=100, s=sFM, distance="euclidean", hmethod="ward.D") {
 n <- ncol(X);
 sim.vector <- numeric(nprojections);
 cl <- Multiple.Random.kmeans (X, dim=dim, pmethod=pmethod, c=c, n=nprojections*2, scale=scale, seed=seed);
 for (i in 1:nprojections)  {   
	 cl1 <- cl[[i]];
	 M1 <- Do.boolean.membership.matrix(cl1, n, 1:n);
	 cl2 <- cl[[i+nprojections]];
	 M2 <- Do.boolean.membership.matrix(cl2, n, 1:n);
	 sim.vector[i] <- s(M1,M2);
 }
 return(sim.vector);
}

