
create_whoami <- function(tw){
  
  tw$whoami <- gsub('^(.*?)when | like a toddler(.*?)$|stop talking about him|stop treating him','',tw$text)
  tw$whoami <- gsub('^(.*?)when','',tw$whoami)
  tw$whoami <- gsub('https(.*?)$','',tw$whoami)
  tw$whoami <- gsub('^\\s*|\\s*$|[.]','',tw$whoami)
  tw
  
}

#' @importFrom dplyr mutate case_when
create_prox <- function(tw){
  tw%>%dplyr::mutate(prox = 
                             dplyr::case_when(
                               grepl('staff|vice president|advisors|advisers',whoami,ignore.case = TRUE) ~ 'staff',
                               grepl('himself|he',whoami,ignore.case = TRUE) ~ 'himself',
                               grepl('wife|kids|children',whoami,ignore.case = TRUE) ~ 'family',
                               grepl('friends|confidants|associates',whoami,ignore.case = TRUE) ~ 'friends',
                               grepl('lawyers',whoami,ignore.case = TRUE) ~ 'lawyers',
                               grepl('republicans|gop',whoami,ignore.case = TRUE) ~ 'GOP',
                               grepl('supporters',whoami,ignore.case = TRUE) ~ 'supporters',
                               grepl('congress',whoami,ignore.case = TRUE) ~ 'congress',
                               grepl('media|camera|cameras|news',whoami,ignore.case = TRUE) ~ 'intel',
                               grepl('intelligence',whoami,ignore.case = TRUE) ~ 'intel',
                               grepl('allies',whoami,ignore.case = TRUE) ~ 'allies'
                             ))
}

#' @importFrom dplyr mutate count
toddlr_status <- function(tw){
  tw%>%
    dplyr::mutate(
      date  = as.Date(created_at),
      ym = strftime(date,'%Y-%m')
    )%>%
    dplyr::count(ym)
}

#' @importFrom dplyr mutate group_by summarise ungroup
toddlr_stats <- function(tw){
  tw%>%
    dplyr::mutate(
      date  = as.Date(created_at),
      ym = strftime(date,'%Y-%m')
    )%>%
    dplyr::group_by(ym)%>%
    dplyr::summarise(
      ratio = sum(favorite_count)/sum(retweet_count)
    )%>%
    dplyr::ungroup()
}