
create_whoami <- function(tw){
  
  #phrases
  tw$whoami <- gsub("^(.*?)when | like a toddler(.*?)$| like at toddler(.*?)$| like he's a toddler(.*?)$|stop thinking about him|stops talking about him|stop talking about him|stop treating him|stops treating him",'',tw$text)
  tw$whoami <- gsub('^(.*?)when','',tw$whoami)
  
  #links
  tw$whoami <- gsub('https(.*?)$','',tw$whoami)
  
  #punctuation
  tw$whoami <- gsub('[.,]','',tw$whoami)
  
  #spelling
  tw$whoami <- gsub('\\bIke\\b','like',tw$whoami)
  tw$whoami <- gsub('\\blike a toddler\\b','',tw$whoami)
  
  tw$whoami <- gsub('\\balking\\b|\\btaking\\b','talking',tw$whoami)
  tw$whoami <- gsub('\\badvisors\\b','advisers',tw$whoami)
  
  #spaces
  tw$whoami <- gsub('^\\s*|\\s*$','',tw$whoami)
  tw$whoami <- gsub('\\s+',' ',tw$whoami)
  
  tw
  
}

#' @importFrom dplyr mutate case_when
create_prox <- function(tw){
  tw%>%dplyr::mutate(prox = 
                             dplyr::case_when(
                               grepl('Republicans|republicans|gop|lawmakers',whoami,ignore.case = TRUE) ~ 'GOP',
                               grepl('staff|vice president|advisors|advisers|aides|physician',whoami,ignore.case = TRUE) ~ 'staff',
                               grepl('wife|kids|children|first lady',whoami,ignore.case = TRUE) ~ 'family',
                               grepl('friends|confidants|associates',whoami,ignore.case = TRUE) ~ 'friends',
                               grepl('lawyer|lawyers',whoami,ignore.case = TRUE) ~ 'lawyers',
                               grepl('supporters',whoami,ignore.case = TRUE) ~ 'supporters',
                               grepl('congress',whoami,ignore.case = TRUE) ~ 'congress',
                               grepl('media|camera|cameras|news',whoami,ignore.case = TRUE) ~ 'media',
                               grepl('intelligence|military',whoami,ignore.case = TRUE) ~ 'intel',
                               grepl('allies',whoami,ignore.case = TRUE) ~ 'allies',
                               grepl('\\bhimself\\b|he',whoami,ignore.case = TRUE) ~ 'himself'
                             ))
}

#' @importFrom dplyr mutate count
toddlr_status <- function(tw){
  tw%>%
    dplyr::mutate(
      date  = as.Date(created_at),
      ym = strftime(date,'%Y-%m')
    )%>%
    dplyr::count(ym,prox)
}

#' @importFrom dplyr mutate group_by summarise ungroup
toddlr_stats <- function(tw){
  tw%>%
    dplyr::mutate(
      date  = as.Date(created_at),
      ym = strftime(date,'%Y-%m')
    )%>%
    dplyr::group_by(ym,prox)%>%
    dplyr::summarise(
      ratio = sum(favorite_count)/sum(retweet_count)
    )%>%
    dplyr::ungroup()
}