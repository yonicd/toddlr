#' @title toddlr slick
#' @description Carousel that contains iframes with twitter embedded statuses
#' @param twe_dat tibble that contains screen_name and status_id
#' @param \dots arguments to pass to \code{\link[slickR]{slickR}}
#' @return slick
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  toc%>%
#'  dplyr::slice(1:5)%>%
#'  toddlr::toddlr_slick()
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
toddlr_slick <- function(twe_dat,...){

  twe <- purrr::map2_chr(twe_dat$screen_name, twe_dat$status_id,
                         rtweet::tweet_embed,hide_thread = TRUE, align = 'center')
  
  slickR::slickR(
    obj = twe,
    slideType = 'iframe',
    slickOpts = list(
      autoplay=TRUE,
      vertical = TRUE,
      dots = FALSE),
    ...)

}
