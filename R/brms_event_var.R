#' Convert an event data.frame column coded as a Surv() event into a brms cens() event
#'
#' This function converts a time event column coded as a Surv() event:
#' - 0 and 1 (event)
#' - 1 and 2 (event)
#' - 0 (right censored), 1 (event), 2 (left censored) and 3 (interval censored)
#' into a brms::brmsformula cens() event:
#' - "right" (right censored), "none" (event), "left" (left censored) and "interval" (interval censored)
#'
#' @param data: the input data.frame object
#' @param event_var: the column name of the event variable (please note: it must be complete, no NA allowed)
#' @return A vector() object which is the brns::brmsformula coded event factor
#' @export
brms_event_var <- function( data, event_var )
{
  event_out <- vector()
  ### check event_var exists in data.frame
  if ( event_var %in% colnames(data) ) {
    event_data <- data[[event_var]]
    ### if event_var is coded as factor
    if (is.factor( event_data )){
      event_levels <- levels( event_data )
      ### check event_var has 2 levels
      if ( length(event_levels) < 2 ) {
        stop( paste( event_var, ' is coded as factor with only one level! Please check ...' ) )
      ### if event_var has exactly 2 levels
      } else if ( length(event_levels) == 2 ) {
        ### check if levels are 0 and 1
        if ( setequal( c(0,1), event_levels ) ) {
          event_out <- ifelse( event_data == 1, "none", "right" )
        } else if ( setequal( c(1,2), event_levels ) ) {
          event_out <- ifelse( event_data == 2, "none", "right" )
        } else {
          stop( paste( event_var, ' is coded as factor with two levels:', event_levels, 'which are not suitable for survival event codification. Please check ...' ) )
        }
      ### if event_var has more than 2 levels
      } else {
        if ( all(event_levels %in% c(0,1,2,3)) ) {
          for ( i in 1:length(event_data) )
          {
            event_data_i <- event_data[i]
            if (event_data_i == 0) {
              event_data_i_out <- "right"
            } else if ( event_data_i == 1 ) {
              event_data_i_out <- "none"
            } else if ( event_data_i == 2 ) {
              event_data_i_out <- "left"
            } else if ( event_data_i == 3 ) {
              event_data_i_out <- "interval"
            }
            event_out <- c( event_out, event_data_i_out )
          }
        } else {
          stop( paste( event_var, ' is coded as factor with >2 levels:', event_levels, 'which are not suitable for survival event codification. Please check ...' ) )
        }
      }
    }
  } else {
    stop( paste( event_var, 'not found in dataframe') )
  }
  return( factor(event_out) )
}
