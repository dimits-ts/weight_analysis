# FUNCTIONS

# generates bins for lm model homoscedascity  
quantcut <- function(x, digits=6) { 
  cut(x, breaks=quantile(x), include.lowest=TRUE, dig.lab = digits) 
}
