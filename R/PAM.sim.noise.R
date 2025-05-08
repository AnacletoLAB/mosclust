"PAM.sim.noise" <-
function(X, c = 2, nnoisy=100, perc=0.5, s = sFM, distance="euclidean", hmethod=NULL) {

 n <- ncol(X);
 sim.vector <- numeric(nnoisy);
  
 for (i in 1:nnoisy)  {
   X1 <- perturb.by.noise(X, perc);	 
	 if (distance == "euclidean")
		  d <- dist (t(X1))
	 else 
			d <- as.dist(1 - cor(X1));
	 r <- pam (d,c,cluster.only=TRUE);
	 cl1 <- Transform.vector.to.list(r);
	 M1 <- Do.boolean.membership.matrix(cl1, n, 1:n);
	 
	 X2 <- perturb.by.noise(X, perc);	 
	 if (distance == "euclidean")
		  d <- dist (t(X2))
	 else 
			d <- as.dist(1 - cor(X2));
	 r <- pam (d,c,cluster.only=TRUE);
	 cl2 <- Transform.vector.to.list(r);
	 M2 <- Do.boolean.membership.matrix(cl2, n, 1:n);
	
	 sim.vector[i] <- s(M1,M2);
 }
 return(sim.vector);
}

