"compute.integral" <-
function(F, subdivisions=1000) {
  l <- integrate(F, 0, 1, subdivisions=subdivisions);
	return(l$value);
}

