"cumulative.values" <-
function(F) {
  e <- environment(F);
	l <- list(x=e$x, y=e$y);
	return(l);
}

