#' Read Lines
#' 
#' This function reads lines of a txt file and eliminates empty lines from it.
#'
#' @param con path to file
#'
#' @return character vector
#' @export
my_readlines <- function(con){
  tmp <- readLines(con, skipNul = T)
  tmp <- tmp[which(tmp != "")]
  return(tmp)
}


#' Remove articles
#' 
#' This function removes all articles from a character vector. Use it to simplify
#' a text for better tokenization.
#'
#' @param txt character vector
#'
#' @return character vector
#' @export
remove_articles <- function(txt){
  return(
    stringr::str_replace_all(tolower(txt), "(\\s+)(a|an|and|the)(\\s+)", " ")
  )
}


#' Remove prepositions
#' 
#' Removes preposition and other words clutter from a string
#'
#' @param txt input string
#' @param prep path to file with words to exlude
#'
#' @return character vector
#' @export
remove_prepositions <- function(txt, prep){
  preps <- paste(prep, collapse = '|')
  preps <- paste0('(\\s+)(', preps, ')', "(\\s+)")
  return(
    stringr::str_replace_all(tolower(txt), preps, " ")
  )
}


#' Read text
#' 
#' This function reads a text and returns its clean version
#'
#' @param txt_con path to text
#' @param prep_con path to list of words to exclude
#'
#' @return character vector
#' @export
my_readtext <- function(txt_con, prep_con){
  return(
    remove_prepositions(
      txt  = remove_articles(tolower(my_readlines(txt_con))),
      prep = unique(my_readlines(prep_con))
    )
  )
}


#' Build tokens
#' 
#' This function takes character vector and convert it into tokens
#'
#' @param txt character vector
#' @param n_grams max n grams
#' @param simplify 
#'
#' @return token list
#' @export
build_tokens <- function(txt, n_grams = 1, simplify = T){
  return(
    quanteda::tokens(
      txt,
      remove_punct      = T,
      remove_symbols    = T,
      remove_numbers    = T,
      remove_url        = T,
      remove_separators = T,
      split_hyphens     = T
    ) %>% 
      quanteda::tokens_tolower(.) %>% 
      quanteda::tokens_ngrams(., n = n_grams, concatenator = " ")
  )
}


#' Make frequencies
#' 
#' This function creates numeric vector of token frequencies
#'
#' @param txt input tokens
#' @param min_freq minimal frequency
#' @param min_doc minimal length of document
#'
#' @return numeric vector
#' @export
make_freq <- function(txt, min_freq = 1, min_doc = 1) {
  output_matrix <- 
    quanteda::dfm(txt) %>% 
    quanteda::dfm_trim(., min_termfreq = min_freq, min_docfreq = min_doc)
  return(quanteda::colSums(output_matrix))
}


#' Make word cloud plot
#' 
#' This function returns word cloud plot
#'
#' @param txt input tokens
#' @param min_freq cut off rate of frequency
#' @param min_doc minimal document length
#'
#' @return
#' @export
make_wd_plot <- function(txt, min_freq = 1, min_doc = 1){
  tmp <- 
    quanteda::dfm(txt) %>% 
    quanteda::dfm_trim(., min_termfreq = min_freq, min_docfreq = min_doc)
  return(
    quanteda.textplots::textplot_wordcloud(tmp)
  )
}


#' Convert to data table
#' 
#' This function converts words frequency to data table format handy for 
#' text comparisons
#'
#' @param dt input data table in wide format
#'
#' @return data
#' @export
convert_to_dt <- function(dt){
  tmp <- data.table::melt(dt, 
                          variable.name = 'words', 
                          measure.vars = colnames(dt))
  return(tmp[order(-value)])
}
