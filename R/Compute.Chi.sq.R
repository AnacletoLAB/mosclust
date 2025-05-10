"Compute.Chi.sq" <-
function(M, s0 = 0.9) {
  n <- ncol(M);
	K <- nrow(M);
	x <- numeric(K);
	for (k in 1:K) 
	  for (j in 1:n)  
		  if (M[k,j] > s0)
			  x[k] <- x[k] + 1;	
	theta <- sum(x)/(n*K); # pooled estimate
	if ((theta == 0) || (theta==1)) # the proportions are equal
	  p.value = 1
	else {
	  chi.statistic <- sum((x-n*theta)^2)/(n*theta*(1-theta));
    p.value <- 1 - stats::pchisq(chi.statistic,K-1);
	}
	return (p.value);
}

