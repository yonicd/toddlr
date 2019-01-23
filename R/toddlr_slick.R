#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param twe_dat PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[rtweet]{tweet_embed}}
#'  \code{\link[slickR]{slickR}}
#' @rdname toddlr_slick
#' @export 
#' @importFrom purrr map2_chr
#' @importFrom rtweet tweet_embed
#' @importFrom slickR slickR
toddlr_slick <- function(twe_dat){

  twe <- purrr::map2_chr(twe_dat$screen_name, twe_dat$status_id,
                         rtweet::tweet_embed,hide_thread = TRUE, align = 'center')
  
  slickR::slickR(
    obj = twe,
    slideType = 'iframe',
    slickOpts = list(
      autoplay=TRUE,
      vertical = TRUE,
      dots = FALSE),
    width = '40%',
    height = '40%')

}
