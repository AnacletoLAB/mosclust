"Intersect" <-
function(sub1,sub2) {
  
	sub.int <- integer(0);
	n1 <- length(sub1);
	n2 <- length(sub2);
	k <- 0;
		
	for(i in 1:n1)
	  for(j in 1:n2) 
		  if (sub1[i] == sub2[j]) {
			  k <- k + 1;
			  sub.int[k] <- sub1[i];
				break;
			}		
	return(sub.int);
}

