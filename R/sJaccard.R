"sJaccard" <-
function(M1,M2) {
  res <- sum(M1*M2) / (sum(M1*M1) + sum(M2*M2) - sum(M1*M2));
	return(res);
}

