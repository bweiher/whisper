#' Determine language of website
#'
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom dplyr "n"
#' @param urls A character vector of URLs to iterate over
#' @param show_url Print URL of current iteration
#' @param show_progress Show overall progress periodically
#' @param parallelize Whether or not to parallelize the scraping
#' @examples
#'  get_language_url(c('http://nytimes.com', 'http://nytimes.com/es'))
#'
get_language_url <- function(urls, show_url = TRUE, show_progress = TRUE, parallelize = FALSE) {
  
  if(sum(!stringr::str_detect(stringr::str_to_lower(urls), "^http")) > 0) stop("You need to start the URL with http://www or https://www")
  
  
  if (parallelize == TRUE) { # use future to parallelize
    future::plan(future::multiprocess)
    fn <- furrr::future_map_dfr
  } else {
    fn <- purrr::map_df
  }

  len_seq <- length(unique(urls))


  # loop over each domain;
  # return a data_frame as the output for each URL
  fn(1:len_seq, function(x) {
    url <- urls[x] #  URL of current iteration


    # Define messaging preferences
    if (isTRUE(show_url)) {
      message(url)
    }

    # If progress should be printed every 10 URLs
    if (isTRUE(show_progress)) {
      if (x %% 10 == 0) {
        iteration_pct <- round((100 * x / len_seq), 2) %>% paste0("%")
        message(iteration_pct)
      }
    }

    # Make the request to the URL, and wrap errors in NA
    # Define wait_time, for time to wait before moving on
    req <- tryCatch(
      # R.utils::evalWithTimeout({
      xml2::read_html(url),
      # }, timeout = wait_time),
      error = function(e) {
        NA
      }
    )



    if (is.na(req)) {

      #  if request returned with an error
      #  then  output table with NAs and record reason

      tmp_df <- tibble::tibble(
        total_sentences_evaluated = NA_real_,
        first_language = NA_character_,
        first_prop = NA_real_,
        second_language = NA_character_,
        second_prop = NA_real_,
        third_language = NA_character_,
        third_prop = NA_real_,
        nlp_failure_reason = "Invalid Request"
      )
    } else {

      # take some sentences from the body of the webpage
      sentences <- req %>%
        rvest::html_node("body") %>%
        rvest::html_text() %>%
        parse_html_text()


      if (length(sentences) == 0) {

        # if the sentences have no lines left after the regex
        # output tibble with this reason
        tmp_df <- tibble::tibble(
          total_sentences_evaluated = NA_real_,
          first_language = NA_character_,
          first_prop = NA_real_,
          second_language = NA_character_,
          second_prop = NA_real_,
          third_language = NA_character_,
          third_prop = NA_real_,
          nlp_failure_reason = "No Sentences to Parse"
        )
      } else {

        # if we have some sentences left to make a prediction on the language, do so
        # and aggregate for each domain, since many lines may be pulled
        # and we can make multiple predictions per domain this way

        tmp_df <- sentences %>%
          cld3::detect_language() %>%
          tibble::as_tibble() %>%
          stats::setNames("language") %>%
          dplyr::group_by(language) %>%
          dplyr::summarise(rows = n()) %>%
          dplyr::mutate(
            prop = rows / sum(rows, na.rm = TRUE),
            total_sentences = sum(rows, na.rm = TRUE)
          ) %>%
          dplyr::arrange(-prop) %>%
          dplyr::mutate(
            second_language = lead(language),
            second_prop = lead(prop),
            third_language = lead(language, 2),
            third_prop = lead(prop, 2)
          ) %>%
          dplyr::rename(first_language = language, first_prop = prop) %>%
          dplyr::select(
            total_sentences_evaluated = total_sentences,
            first_language,
            first_prop,
            second_language,
            second_prop,
            third_language,
            third_prop
          ) %>%
          dplyr::slice(1) %>%
          dplyr::mutate_at(c(
            "total_sentences_evaluated",
            "first_prop",
            "second_prop",
            "third_prop"
          ), as.numeric) %>%
          dplyr::mutate_at(c(
            "first_language",
            "second_language",
            "third_language"
          ), as.character) %>%
          dplyr::mutate(nlp_failure_reason = NA_character_)
      }
    }



    # finally, find the html attributes of the webpage
    # using the original request from above
    tmp_df %>%
      dplyr::mutate(
        domain = url,
        lang_html_attr = tryCatch(rvest::html_attr(req, "lang"), error = function(e) {
          NA
        })
      ) %>%
      dplyr::select(domain, dplyr::everything())
  })
}
