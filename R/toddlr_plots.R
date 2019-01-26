#' @title Create toddlr output plot
#' @description Creates dashboard of plots from thread
#' @param plot_dat list of tibbles
#' @return ggplot
#' @details Uses internal functions to create input list
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
    ggplot2::ggplot(ggplot2::aes(x=i,y=nn,colour=prox)) +
    ggplot2::geom_line(show.legend = FALSE,alpha=0.3) +
    ggplot2::geom_point(show.legend = FALSE,size=0.5) +
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
    ggplot2::ggplot(ggplot2::aes(x=ym,y=n,colour=prox)) +
    ggplot2::geom_point(
      ggplot2::aes(size = ratio)
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
      caption = 'Created By: @yoniceedee\nSource Script: https://goo.gl/yo6ipg'
    ) +
    ggplot2::scale_colour_discrete(guide = 'none') 
}

#' @import ggplot2
plot_snip <- function(snips){
  snips%>%
    dplyr::mutate(staff = ifelse(prox=='staff','Staff','Non Staff'))%>%
    ggplot2::ggplot(
      ggplot2::aes(x=whoami,y=n)
    ) +
    ggplot2::geom_bar(stat='identity',ggplot2::aes(fill = prox)) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~ staff,ncol = 1,scales='free') +
    ggplot2::labs(
      title = 'Who stop thinking/talking/treating about him like a toddler?',
      y = 'Frequency',
      fill = 'Circle of Trust'
    ) +
    ggplot2::theme(legend.position = 'bottom')
}