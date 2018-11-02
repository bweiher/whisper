
#' Query the response header of a webpage
#' @export
#' @importFrom dplyr "%>%"
#' @param urls A character vector of URLs to iterate over
#' @examples
#'  get_html_response_url(c('http://nytimes.com', 'http://nytimes.com/es'))
#'
get_html_response_url <- function(urls) {
  seq_len <- length(unique(urls))


  purrr::map_df(1:seq_len, function(x) {
    url <- urls[x]

    # Message to inform over uncaptured errors
    message(url)
    message(paste0(x, " out of ", length(urls)))

    # test if URL exists
    if (RCurl::url.exists(url)) {
      h <- RCurl::basicHeaderGatherer()
      # if we don't get an error, get URI
      doc <- tryCatch(RCurl::getURI(url, headerfunction = h$update), error = function(e) {
        NA
      })

      # if the output is not NA (error checker above, proceed)
      if (!is.na(doc)) {
        output <- h$value()
        element <- stringr::str_detect(names(output), "Content-Language")


        # if `Content-Language` is detected in the names of the HTML header
        if (sum(element) == 0) {
          html_header <- tibble::tibble(
            html_response_header = NA,
            failure_reason = "Not Present"
          )
        } else {
          # record the language if present
          html_header <- tibble::tibble(
            html_response_header = as.character(output[element]),
            failure_reason = NA
          )
        }
      } else {
        # invalid URL
        # not sure why
        html_header <- tibble::tibble(
          html_response_header = NA,
          failure_reason = "Invalid Request to valid URL"
        )
      }
    } else {
      #
      html_header <- tibble::tibble(
        html_response_header = NA,
        failure_reason = "URL doesn't exist"
      )
    }

    html_header %>%
      dplyr::mutate(domain = url) %>%
      dplyr::select(domain, dplyr::everything())
  })
}
