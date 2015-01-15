# Name: Nikoula, Latifah & Nikos
# Date: 13 January 2015

# This function computes the root mean squared error

rmse <- function( actual, predicted ) {
 rmse<- round(sqrt( mean(( actual - predicted ) ^ 2 )), digits=2)
}