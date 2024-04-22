#' Convert an outcome variable to brms specification
#'
#' This function takes an outcome variable vector and reconvert it accordingly to
#' brms::brm() model specifications
#'
#' @param var_vector: a variable vector to be passed as brms::brm() model outcome
#' @param positive: the level of the variable to use as 1 in case of "binomia" family (optional)
#' @param family: a brms::brmsfamily() (defaults to "binomia")
#' @return A brms::brm() adapted outcome based on family specified
brms_outcome_var <- function( var_vector, positive = NULL, family = 'binomial' )
{
  ### if user asked for a logistic regression
  if ( family == 'binomial' ) {
    if ( base::is.factor( var_vector ) ) {
      var_vector <- var_vector
    ### if passed var_vector is numeric
    } else if ( base::is.numeric( var_vector ) ) {
      if ( base::length( base::unique(var_vector) ) != 2 ) {
        base::stop( ' \n outcome passed as a numeric variable with more than two different values, not suitable for logistic regression \n' )
      } else {
        base::cat( ' \n outcome passed as a numeric variable, converting it to factor ... \n' )
        var_vector <- base::factor( var_vector )
      }
    } else if ( base::is.character( var_vector ) ) {
      if ( base::length( base::unique(var_vector) ) != 2 ) {
        base::stop( ' \n outcome passed as a character variable with more than two different values, not suitable for logistic regression \n' )
      } else {
        base::cat( ' \n outcome passed as a character variable, converting it to factor ... \n' )
        var_vector <- base::factor( var_vector )
      }
    } else {
      base::stop( '\n "outcome" is of a unrecognized type, format it as factor for logistic regression \n ')
    }
    ### check conversion (or original factor)
    var_vector_levels <- base::levels(var_vector)
    if ( base::length( var_vector_levels ) != 2 ) {
      base::stop( ' \n "outcome" is a factor with more than 2 levels, not suitable for logistic regression \n' )
    } else {
      if ( ! base::is.null( positive ) ) {
        ### check positive is actually a level
        if ( ! positive %in% var_vector_levels ) {
          base::stop( ' \n factor passed:"', positive, '" not found in outcome levels \n', sep = '' )
        }
      ### if user did not specify any factor take first (or 1 if 0/1)
      } else {
        if ( base::setequal( var_vector_levels, c(0,1) ) ) {
          positive <- 1
        } else {
          positive <- var_vector_levels[1]
        }
        base::cat( ' - factor not specified, using:"', positive, '" as 1 and ', var_vector_levels[2], ' as 0, in outcome \n', sep = '' )
      }
    }
    ### convert to 1 and 0
    var_vector_out <- base::ifelse( var_vector == positive, 1, 0 )
  ### if user need to fit a Poisson model
  } else if ( family == 'poisson' ) {
    ### check passed variable is numeric
    if ( base::is.numeric( var_vector ) ) {
      ### check are all integers (count data)
      if ( base::all( var_vector - base::floor(var_vector) == 0 ) ) {
        base::cat( ' \n "outcome" passed is numeric and values are integers: suitable for Poisson regression: \n', sep = '' )
        base::print( riptw::numeric_variable_summary(var_vector) )
      } else {
        base::stop( ' \n "outcome" passed is numeric but some values are not integers: not suitable for Poisson regression \n', sep = '' )
      }
    } else {
      base::stop( ' \n "outcome" passed is not numeric, not suitable for Poisson regression \n', sep = '' )
    }
    var_vector_out <- var_vector
  ### if user asked for a linear regression
  } else if ( family == 'gaussian' ) {
    ### check passed variable is numeric
    if ( base::is.numeric( var_vector ) ) {
      base::cat( ' \n "outcome" passed is numeric: suitable for Linear regression: \n', sep = '' )
      base::print( riptw::numeric_variable_summary(var_vector) )
    } else {
      base::stop( ' \n "outcome" passed is not numeric, not suitable for Linear regression \n', sep = '' )
    }
    var_vector_out <- var_vector
  ### if user asked for a multinomial regression
  } else if ( family == 'multinomial' ) {
    if ( base::is.factor( var_vector ) ) {
      var_vector <- var_vector
    ### if passed var_vector is numeric
    } else if ( base::is.numeric( var_vector ) ) {
      if ( base::length( base::unique(var_vector) ) == 2 ) {
        base::warning( ' \n outcome passed as a numeric variable with two different values, you sure you did not want a logistic regression? \n' )
      } else {
        base::cat( ' \n outcome passed as a numeric variable, converting it to factor ... \n' )
        var_vector <- base::factor( var_vector )
      }
    } else if ( base::is.character( var_vector ) ) {
      if ( base::length( base::unique(var_vector) ) == 2 ) {
        base::warning( ' \n outcome passed as a character variable with two different values, you sure you did not want a logistic regression? \n' )
      } else {
        base::cat( ' \n outcome passed as a character variable, converting it to factor ... \n' )
        var_vector <- base::factor( var_vector )
      }
    } else {
      base::stop( '\n "outcome" is of a unrecognized type, format it as factor for Multinomial regression \n ')
    }
    ### check conversion (or original factor)
    var_vector_levels <- base::levels(var_vector)
    if ( base::length( var_vector_levels ) == 2 ) {
      base::warning( ' \n outcome passed as a factor variable with two different values, you sure you did not want a logistic regression? \n' )
    } else {
      if ( ! base::is.null( positive ) ) {
        ### check positive is actually a level
        if ( ! positive %in% var_vector_levels ) {
          base::stop( ' \n factor passed:"', positive, '" not found in outcome levels \n', sep = '' )
        }
      ### if user did not specify any factor take first (or 1 if 0/1)
      } else {
        if ( base::setequal( var_vector_levels, c(0,1) ) ) {
          positive <- 1
        } else {
          positive <- var_vector_levels[ base::order(var_vector_levels) ][1]
        }
        base::cat( ' - factor not specified, using:"', positive, '" as reference in outcome \n\n', sep = '' )
      }
    }
    ### convert to 1 and 0
    var_vector_out <- base::factor( var_vector, levels = var_vector_levels[ base::order(var_vector_levels) ] )
  }
  return( var_vector_out )
}
