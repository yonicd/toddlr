#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param plot_dat PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname toddlr_plots
#' @import patchwork
#' @export 

toddlr_plots <- function(plot_dat){
  
  dat <- plot_dat$time
  
  snips <- plot_dat$snips
  
  p1 <- dat%>%plot_cumfreq()
  
  p2 <- dat%>%plot_freq()
  
  p3 <- snips%>%plot_snip()
  
  p3 + ( p1 / p2 )
}

#' @import ggplot2
plot_cumfreq <- function(dat){
  dat%>%
    ggplot2::ggplot(ggplot2::aes(x=i,y=nn)) +
    ggplot2::geom_smooth(method='loess',se=FALSE,colour='black',size=0.5) +
    ggplot2::geom_point(ggplot2::aes(colour = now),size=2,show.legend = FALSE) +
    ggplot2::scale_x_continuous(breaks = 1:nrow(dat),labels = dat$ym) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle=90)
    ) +
    ggplot2::labs(
      title = 'Trends',
      x = "Date",
      y = 'Statuses\n(Cumulative Frequency)'
    ) 
}

#' @import ggplot2
plot_freq <- function(dat){
  dat%>%
    ggplot2::ggplot(ggplot2::aes(x=ym,y=n)) +
    ggplot2::geom_point(
      ggplot2::aes(size = ratio ,colour = now)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle=90),
      legend.position = 'bottom'
    ) +
    ggplot2::labs(
      x = "Date",
      y = 'Statuses\n(Frequency)',
      size = 'Favorite/Retweet',
      caption = 'Created By: @yoniceedee\nSource Script: https://goo.gl/FJJv7g'
    ) +
    ggplot2::scale_colour_discrete(guide = 'none') 
}

#' @import ggplot2
plot_snip <- function(snips){
  snips%>%
    ggplot2::ggplot(
      ggplot2::aes(x=whoami,y=n)
    ) +
    ggplot2::geom_bar(stat='identity',ggplot2::aes(fill = prox)) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = 'Friends',
      y = 'Frequency',
      fill = 'Circle of Trust'
    ) +
    ggplot2::theme(legend.position = 'bottom')
}