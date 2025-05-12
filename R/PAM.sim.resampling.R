"PAM.sim.resampling" <-
function(X, c = 2, nsub=100, f = 0.8, s = sFM, distance="euclidean", hmethod=NULL) {

 n <- ncol(X);
 n.sub.ex <- ceiling(n * f);
 sim.vector <- numeric(0);
 
 for (i in 1:nsub)  {
   sub1 <- sample(n, n.sub.ex);
   Xsub1 <- X[,sub1];
	 colnames(Xsub1)<-sub1;
	 
	 if (distance == "euclidean")
		  d <- stats::dist(t(Xsub1))
	 else 
			d <- stats::as.dist(1 - stats::cor(Xsub1));
	 r <- cluster::pam (d,c,cluster.only=TRUE);
	 cl1 <- clusterv::Transform.vector.to.list(r);
	 M1 <- Do.boolean.membership.matrix(cl1, n.sub.ex, sub1);
	 
	 sub2 <- sample(n, n.sub.ex);
   Xsub2 <- X[,sub2];
	 colnames(Xsub2)<-sub2;
	 
	 if (distance == "euclidean")
		  d <- stats::dist(t(Xsub2))
	 else 
			d <- stats::as.dist(1 - stats::cor(Xsub2));
	 r <- cluster::pam (d,c,cluster.only=TRUE);
	 cl2 <- clusterv::Transform.vector.to.list(r);
	 M2 <- Do.boolean.membership.matrix(cl2, n.sub.ex, sub2);
	
	 # examples common two the two subsamples
	 sub.common <- Intersect(sub1,sub2);
	 
	 # extract from the membership matrices the rows and columns that 
	 # correspond to the examples common two the two subsamples
	 label.examples <- as.character(sub.common);
	 M1 <- M1[label.examples, label.examples];
	 M2 <- M2[label.examples, label.examples];
	 sim.vector[i] <- s(M1,M2);
 }
 return(sim.vector);
}

