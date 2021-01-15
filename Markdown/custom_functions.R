
# Improving the ggplotly layout per https://github.com/ropensci/plotly/issues/1224#issuecomment-488809271
layout_ggplotly <- function(gg, x = -0.02, y = -0.08){
  # The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
  gg[['x']][['layout']][['annotations']][[1]][['y']] <- x
  gg[['x']][['layout']][['annotations']][[2]][['x']] <- y
  gg
}


# Suppressing output from cat() by Hadeley Wickham per
# https://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-tp859876p859882.html
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

# To be potentially used if we used a MCMC model
HDIofMCMC = function( sampleVec , credMass=0.95 ) {
  # Written by John Kruschke, Indiana University
  # Author of Doing Bayesian Data Analysis, 2e
  # Available as part of DBDA2E-utilities at
  #     https://github.com/boboppie/kruschke-doing_bayesian_data_analysis
  # (This is an excellent book on Bayesian statistics ... SR)
  
  # Computes highest density interval from a sample of representative values,
  #   estimated as shortest credible interval.
  # Arguments:
  #   sampleVec
  #     is a vector of representative values from a probability distribution.
  #   credMass
  #     is a scalar between 0 and 1, indicating the mass within the credible
  #     interval that is to be estimated.
  # Value:
  #   HDIlim is a vector containing the limits of the HDI
  sortedPts = sort( sampleVec )
  ciIdxInc = ceiling( credMass * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
  }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim = c( HDImin , HDImax )
  return( HDIlim )
}