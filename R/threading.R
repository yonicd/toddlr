#' @title Forward update of twitter thread
#' @description Function to update twitter thread from last status
#' @param tw tibble, rtweet output
#' @param n numeric, timeline of user to fetch to look for thread elements, Default: 1000
#' @param \dots Arguments to pass to \code{\link[rtweet]{get_timeline}}
#' @return updated tibble, rtweet output
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  tweet_threading_fw(toc)
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