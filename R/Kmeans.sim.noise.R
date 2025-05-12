"Kmeans.sim.noise" <-
function(X, c = 2, nnoisy=100, perc=0.5, s = sFM, distance="euclidean", hmethod=NULL) {

 n <- ncol(X);
 sim.vector <- numeric(nnoisy);
 
 for (i in 1:nnoisy)  {
   X1 <- perturb.by.noise(X, perc);	 	 
	 r <- stats::kmeans(t(X1), centers=c, iter.max = 1000);
	 cl1 <- clusterv::Transform.vector.to.list(r$cluster);
	 M1 <- Do.boolean.membership.matrix(cl1, n, 1:n);
	 
	 X2 <- perturb.by.noise(X, perc);		 
	 r <- stats::kmeans(t(X2), centers=c, iter.max = 1000);
	 cl2 <- clusterv::Transform.vector.to.list(r$cluster);
	 M2 <- Do.boolean.membership.matrix(cl2,  n, 1:n);

	 sim.vector[i] <- s(M1,M2);
 }
 return(sim.vector);
}

