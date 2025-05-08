"Bernstein.compute.pvalues" <-
function(sim.matrix) {
	n.clusterings <- nrow(sim.matrix);
	n.measures <- ncol(sim.matrix);
	ordered.clusterings <- integer(n.clusterings);
	p.value <- numeric(n.clusterings);
	means <- numeric(n.clusterings);
	variance <- numeric(n.clusterings);
	
	means <- mean(as.data.frame(t(sim.matrix)));
	for (i in 1:n.clusterings)
	  variance[i] <- var(sim.matrix[i,]);
	sorted.means <- sort(means, decreasing=TRUE);
	sorted.indices <- order(means, decreasing=TRUE);
	means <- sorted.means;
	ordered.clusterings <- sorted.indices+1;
	variance <- variance[sorted.indices];
	p.value[1] <- 1;
	for (i in 2:n.clusterings)
	  p.value[i] <- Bernstein.p.value(n.measures, means[1]-means[i], variance[1]+variance[i]);
	for (i in (n.clusterings-1):2)
	  p.value[i] <- p.value[i] + p.value[i+1];
	p.value[p.value>1] <- 1;	
	d <- data.frame(ordered.clusterings=ordered.clusterings, p.value=p.value, means=means, variance=variance);
	rownames(d) <- 1:n.clusterings;
	return(d);
}

