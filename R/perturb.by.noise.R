"perturb.by.noise" <-
function(X, perc = 0.5) {
  
	if ((perc<=0)||(perc>1))
	  stop("Percentile must be between 0 and 1");  
  n.var <- nrow(X);
	n.samples <- ncol(X);
	
	#sdev <- sort(stats::sd(t(X)));
	sdev <- apply(X, 1, stats::sd);
	noise.sd <- sdev[ceiling(n.var*perc)];
	
	X.noisy <- matrix(stats::rnorm(n.var*n.samples, sd=noise.sd), nrow=n.var);
	
	X.noisy <- X + X.noisy;
	
	return(X.noisy);
}

