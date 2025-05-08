"sM" <-
function(M1,M2) {
  n <- nrow(M1);
  res <- 1 - sum((M1-M2)^2)/n^2;
	return(res);
}

