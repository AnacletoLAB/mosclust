"compute.integral" <-
function(Fun, subdivisions=1000) {
  l <- stats::integrate(Fun, 0, 1, subdivisions=subdivisions);
	return(l$value);
}

