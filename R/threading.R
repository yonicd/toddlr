#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param tw PARAM_DESCRIPTION
#' @param n PARAM_DESCRIPTION, Default: 1000
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[rtweet]{get_timeline}}
#' @rdname tweet_threading_fw
#' @export 
#' @importFrom rtweet get_timeline
tweet_threading_fw <- function(tw, n = 1000,...){
  
  timeline <- rtweet::get_timeline(tw$screen_name[1], n = n,...)
  from_id <- tw$status_id[1]
  found <- TRUE
  
  while(found){
    
    idx <- which(timeline$reply_to_status_id%in%from_id)
    found <- length(idx)>0
    tw <- rbind(timeline[idx,],tw)
    from_id <- timeline$status_id[idx]
    
  }
  
  tw
}