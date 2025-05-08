"compute.integral.from.similarity" <-
function(sim.matrix) {
  return(as.vector(1 - mean(as.data.frame(t(sim.matrix)))));
}

