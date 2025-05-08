"Hypothesis.testing" <-
function(d, alpha=0.01) {
	n.clusterings <- nrow(d);
	significant.difference <- TRUE;
	for (i in n.clusterings:2) {
	  if (d$p.value[i] > alpha) {
		  significant.difference <- FALSE;
		  break;
		} 	
	}
	if (significant.difference == FALSE) # there is no significant difference between the first and ith clustering
	  significant.clusters <- i
	else 
	  significant.clusters <- 1;
	return(d[1:significant.clusters,]);
}

