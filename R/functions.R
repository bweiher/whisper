# functions

utils::globalVariables(c('.','candidateLanguage1', 'domain', 'percentScore1', 'rows', 'sentences_evaluated'))


#' Parse the text extracted from a webpage
#'
#' @export
#' @importFrom dplyr "%>%"
#' @param text URL to retrieve sentences from
#' @examples parse_html_text(get_sentences_url(url='http://nytimes.com/es', node='body', process=FALSE))
parse_html_text <- function(text ) {

    stringr::str_split(text, '\n|   ') %>%
    unlist() %>%
    stringi::stri_enc_toutf8(validate = TRUE) %>%
    stringr::str_trim(side = 'both') %>%
    .[nchar(.)>0 & nchar(.)<500] %>%
    .[stringr::str_count(., "\\S+") >= 7] %>%
    .[!stringr::str_detect(., 'var|&&|\\|\\||//|else if|\\{|=|/\\*|\\.log|module\\.')]
  }

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
get_sentences_url <- function(url, node, process=TRUE) {
   htmltext <-  xml2::read_html(url) %>%
    rvest::html_node(node) %>%
    rvest::html_text()

  if(isTRUE(process)) {
    htmltext %>%
    parse_html_text()

  } else {
    htmltext
}

}




#' Determine language of website
#'
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom dplyr "n"
#' @param urls A character vector of URLs to iterate over
#' @param show_url Print URL of current iteration
#' @param show_progress Show overall progress periodically
#' @param wait_time Time to wait before moving from bad URL
#' @examples
#'  get_language_url(c('http://nytimes.com', 'http://nytimes.com/es'))
#'
get_language_url <- function(urls, show_url=TRUE, show_progress=TRUE, wait_time = 5) {

  len_seq <- length(unique(urls))


  # loop over each domain;
  # return a data_frame as the output for each URL
  purrr::map_df(1:len_seq, function(x) {



  url <- urls[x] #  URL of current iteration


  # Define messaging preferences
  if(isTRUE(show_url)) {
    message(url)
  }

  # If progress should be printed every 10 URLs
  if(isTRUE(show_progress)) {

    if(x %% 10 == 0 ) {
      iteration_pct <- round( (100 * x / len_seq),2) %>% paste0('%')
    message(iteration_pct)
    }
  }

  # Make the request to the URL, and wrap errors in NA
  # Define wait_time, for time to wait before moving on
  req  <- tryCatch(
    R.utils::evalWithTimeout({  xml2::read_html(url ) }, timeout= wait_time),
    error = function(e){NA}
  )



  if(is.na(req)) {

#  if request returned with an error
#  then  output table with NAs and record reason

    tmp_df <-
    tibble::tibble(
      language =     NA,
      rows =         NA,
      confidence =   NA,
      score =        NA,
      total =        NA,
      share =        NA,
      nlp_failure_reason = 'Invalid Request'
      )


  } else {

     # take some sentences from the body of the webpage
      sentances <- req %>%
      rvest::html_node('body') %>%
      rvest::html_text() %>%
      parse_html_text()


    if(length(sentances)==0) {

      # if the sentances have no lines left after the regex
      # output tibble with this reason
      tmp_df <- tibble::tibble(
        language = NA,
        rows =   NA,
        confidence =   NA,
        score =   NA,
        total =   NA,
        share =   NA,
        nlp_failure_reason = 'No Body Text')

    }  else {

      # if we have some sentances left to make a prediction on the language, do so
      # and aggregate for each domain, since many lines may be pulled
      # and we can make multiple predictions per domain this way

      tmp_df <-
        sentances %>%
          cldr::detectLanguage(isPlainText=T) %>%
          tibble::as_tibble() %>%
          dplyr::group_by(language = candidateLanguage1) %>%
          dplyr::summarise(
            rows = n(),
            confidence = stats::median(percentScore1, na.rm=T)
          ) %>%
        dplyr::mutate(sentences_evaluated =  sum(rows),
                 share = round((100 * rows / sentences_evaluated) ) %>%  as.integer()) %>%
        dplyr::arrange(dplyr::desc(rows)) %>%
        dplyr::slice(1) %>%
        dplyr::mutate(nlp_failure_reason = NA)

    }





  }



  # finally, find the html attributes of the webpage
  # using the original request from above
 tmp_df %>%
 dplyr::mutate(
  domain=url,
  lang_html_attr = tryCatch(rvest::html_attr(req ,'lang'), error=function(e){NA})
  ) %>%
 dplyr::select(domain, dplyr::everything()) %>%
 dplyr::select(-rows)






})

}


# Method 2  HTML headers ---------------------

#' Query the response header of a webpage
#' @export
#' @importFrom dplyr "%>%"
#' @param urls A character vector of URLs to iterate over
#' @examples
#'  get_html_response_url(c('http://nytimes.com', 'http://nytimes.com/es'))
#'
get_html_response_url   <- function(urls) {

  seq_len <- length(unique(urls))


  purrr::map_df(1:seq_len, function(x) {

  url <- urls[x]

  # Message to inform over uncaptured errors
  message(url)  ; message(paste0(x,' out of ', length(urls)))

  # test if URL exists
  if(RCurl::url.exists(url)) {


    h <- RCurl::basicHeaderGatherer()
    # if we don't get an error, get URI
    doc <- tryCatch(RCurl::getURI(url , headerfunction = h$update), error=function(e){NA})

    # if the output is not NA (error checker above, proceed)
    if(!is.na(doc)) {

      output <- h$value()
      element <-  stringr::str_detect(names(output), 'Content-Language')


      # if `Content-Language` is detected in the names of the HTML header
      if(sum(element)==0) {
        html_header <-  tibble::tibble( html_response_header = NA,
                                failure_reason = "Not Present")

      } else {
      # record the language if present
      html_header <- tibble::tibble(html_response_header = as.character(output[element]),
                            failure_reason = NA)

      }


    } else {
      # invalid URL
      # not sure why
      html_header <- tibble::tibble(html_response_header = NA,
                            failure_reason = 'Invalid Request to valid URL')

    }

  } else {
  #
    html_header <- tibble::tibble( html_response_header = NA,
                           failure_reason = 'URL doesn\'t exist')

  }

  html_header %>%
    dplyr::mutate(domain = url)  %>%
    dplyr::select(domain, dplyr::everything())

  })




}



