#' Parse the text extracted from a webpage
#'
#' @export
#' @importFrom dplyr "%>%"
#' @param text URL to retrieve sentences from
#' @examples parse_html_text(get_sentences_url(url='http://nytimes.com/es', node='body', process=FALSE))
parse_html_text <- function(text) {
  stringr::str_split(text, "\n|   |\\.") %>%
    unlist() %>%
    stringi::stri_enc_toutf8(validate = TRUE) %>%
    stringr::str_trim(side = "both") %>%
    .[nchar(.) > 0 & nchar(.) < 500] %>%
    .[stringr::str_count(., "\\S+") >= 7] %>%
    .[!stringr::str_detect(., "var|&&|\\|\\||//|else if|\\{|=|/\\*|\\.log|module\\.")]
}
