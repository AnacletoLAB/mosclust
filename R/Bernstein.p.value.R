"Bernstein.p.value" <-
function(n, Delta, v) {
 if ((Delta==0) && (v==0))
   return (1)	
 else {
   p <- exp((-n*Delta^2)/(2*v+(2/3)*Delta));
   return(p);
 }
}

