#' Creates a factor variable which is idenical to another factor variable changing only a specified proportion
#'
#' This function is primarily used to testing and creates a factor variable which is associated with
#' another factor variable passed as input, changing only a specified proportion ("propr" 0-100) of
#' the values.
#'
#' @param var_vector: the input factor variable
#' @param prop: the specified proportion of the variable to change (defaults to 10)
#' @param stdout: print a summary of comparison to stdout (default to TRUE)
#' @return A vector() factor object with the specified proportion of the factor variable changed
#' @export
related_factor_variable <- function( var_vector, prop = 10, stdout = TRUE )
{
  if ( base::length( var_vector ) < 1 ) {
    base::stop( '\n passed vector has length 0 \n')
  }
  if ( prop < 0 | prop > 100 ) {
    base::stop( '\n "prop" can be set from 0 to 100, you set: ', prop, ' \n', sep = '' )
  }
  if ( ! base::is.factor( var_vector ) ) {
    var_vector_copy <- base::factor(var_vector)
  } else {
    var_vector_copy <- var_vector
  }
  ### extract number to
  tot_num <- base::length( var_vector )
  prop_num <- base::floor( base::length( var_vector ) * prop / 100 )
  ### if stdout
  if ( base::isTRUE( stdout )) {
    cat( ' \n changing ', prop, '% of the variable: ', prop_num, ' \ ', tot_num, ' (n) \n', sep = '' )
  }
  counter <- 0
  while(TRUE)
  {
    counter <- counter + 1
    ### start from a random position in the vector
    prop_num_start <- base::sample.int( tot_num, 1 )
    ### check if it includes end
    prop_num_start_2 <- 0
    prop_num_end_2 <- 0
    if ( (prop_num_start+prop_num) > tot_num ) {
      prop_num_end <- tot_num
      prop_num_start_2 <- 1
      prop_num_end_2 <- (prop_num_start+prop_num) - tot_num
    } else {
      prop_num_end <- prop_num_start+prop_num
    }
    ### extract the proportion specified
    var_vector_prop_2 <- NULL
    var_vector_equal_2 <- NULL
    if ( prop_num_start_2 > 0 ) {
      var_vector_prop <- var_vector_copy[ (prop_num_start + 1) : prop_num_end ]
      var_vector_prop_2 <- var_vector_copy[ prop_num_start_2 : prop_num_end_2 ]
      var_vector_equal <- var_vector_copy[ (prop_num_end_2 + 1) : prop_num_start ]
      var_vector_prop_categories <- c( var_vector_prop, var_vector_prop_2 )
    } else {
      var_vector_prop <- var_vector_copy[ (prop_num_start + 1) : prop_num_end ]
      var_vector_equal <- var_vector_copy[ 1 : prop_num_start ]
      var_vector_equal_2 <- var_vector_copy[ (prop_num_end + 1) : tot_num ]
      var_vector_prop_categories <- var_vector_prop
    }
    ### if random position ended up in the middle
    if ( base::is.null( var_vector_prop_2 ) ) {
      out_vector <- c( var_vector_equal, base::sample(var_vector_prop), var_vector_equal_2 )
    } else {
      out_vector <- c( base::sample(var_vector_prop), var_vector_equal, base::sample(var_vector_prop_2) )
    }
    ### check that at least 2 categories ended up in prop
    if ( base::length( base::unique( var_vector_prop_categories )) >= 2 ) {
      break
    }
    if ( counter > 1000 ) {
      base::stop( '\n 1000 iterations were performed but no random sample could achieve at least 2 different observation categories, consider reduce "prop" \n ' )
    }
  }
  return( base::factor( out_vector ) )
}
