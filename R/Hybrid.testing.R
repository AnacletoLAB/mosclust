"Hybrid.testing" <-
function(sim.matrix, alpha=0.01, s0=0.9) {
	F <- compute.cumulative.multiple(sim.matrix);
	n <- length(F);
	n.chi.sq.selected <- 0;
	n.Bernstein.selected <- 0;
	
	d.Bernstein <- Bernstein.compute.pvalues(sim.matrix);
	d.Bernstein.selected <- Hypothesis.testing(d.Bernstein, alpha=alpha);
	n.Bernstein.selected <- nrow(d.Bernstein.selected);
	if (n.Bernstein.selected>1) {
	  selected.clusterings <- d.Bernstein.selected$ordered.clusterings - 1;
	  d.chi.sq <- Chi.square.compute.pvalues(sim.matrix[selected.clusterings,], s0 = s0);
		d.chi.sq$ordered.clusterings <- d.Bernstein.selected$ordered.clusterings; # to preserve the correct labelings
		d.chi.sq.selected <- Hypothesis.testing(d.chi.sq, alpha=alpha);
		n.chi.sq.selected <- nrow(d.chi.sq.selected);
		d.selected <- d.chi.sq.selected;
	}
	else {
	  d.selected <- d.Bernstein.selected;	
		d.chi.sq <- NULL;
	}
	
	l <- list(n.Bernstein.selected=n.Bernstein.selected, n.chi.sq.selected=n.chi.sq.selected, 
	          Berstein.res = d.Bernstein, chi.sq.res = d.chi.sq, selected.res = d.selected, F=F);
	
	return(l);
}

