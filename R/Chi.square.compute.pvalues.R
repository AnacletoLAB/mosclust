"Chi.square.compute.pvalues" <-
function(sim.matrix, s0 = 0.9) {
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
	ordered.sim.matrix <- sim.matrix[sorted.indices,];
	p.value[1] <- 1;
	for (k in 2:n.clusterings)
	  p.value[k] <- Compute.Chi.sq (ordered.sim.matrix[1:k,], s0);
	d <- data.frame(ordered.clusterings=ordered.clusterings, p.value=p.value, means=means, variance=variance);
	rownames(d) <- 1:n.clusterings;
	return(d);
}

