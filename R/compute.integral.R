"compute.integral" <-
function(F, subdivisions=1000) {
  l <- stats::integrate(F, 0, 1, subdivisions=subdivisions);
	return(l$value);
}

