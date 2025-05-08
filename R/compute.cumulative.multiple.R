"compute.cumulative.multiple" <-
function(sim.matrix) {
  list.F <- list();
  n <- nrow(sim.matrix);
	for (i in 1:n) 
	  list.F[[i]] <- ecdf(sim.matrix[i,]);
	return(list.F);
}

