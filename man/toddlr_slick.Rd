% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toddlr_slick.R
\name{toddlr_slick}
\alias{toddlr_slick}
\title{toddlr slick}
\usage{
toddlr_slick(twe_dat, ...)
}
\arguments{
\item{twe_dat}{tibble that contains screen_name and status_id}

\item{\dots}{arguments to pass to \code{\link[slickR]{slickR}}}
}
\value{
slick
}
\description{
Carousel that contains iframes with twitter embedded statuses
}
\examples{
\dontrun{
if(interactive()){
 toc\%>\%
 dplyr::slice(1:5)\%>\%
 toddlr::toddlr_slick()
 }
}
}
\seealso{
\code{\link[rtweet]{tweet_embed}}
 \code{\link[slickR]{slickR}}
}
