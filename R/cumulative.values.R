"cumulative.values" <-
function(Fun) {
  e <- environment(Fun);
	l <- list(x=e$x, y=e$y);
	return(l);
}

