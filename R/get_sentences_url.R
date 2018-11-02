#' Get sentences from a part of a website
#'
#' @export
#' @importFrom dplyr "%>%"
#' @param url URL to retrieve sentences from
#' @param node Part of webpage to extract text from
#' @param process Also try to remove non-language text and extract sentences
#' @examples
#'  get_sentences_url('http://nytimes.com/', 'body', process=FALSE)
#'
#
get_sentences_url <- function(url, node, process = TRUE) {
  htmltext <- xml2::read_html(url) %>%
    rvest::html_node(node) %>%
    rvest::html_text()

  if (isTRUE(process)) {
    parse_html_text(htmltext)
  } else {
    htmltext
  }
}
