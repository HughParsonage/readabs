#' Download, extract, and tidy ABS time series spreadsheets
#'
#' \code{read_abs()} downloads ABS time series spreadsheets,
#' then extracts the data from those spreadsheets,
#' then tidies the data. The result is a single
#' data frame (tibble) containing tidied data.
#'
#' @param cat_no ABS catalogue number, as a string, including the extension.
#' For example, "6202.0".
#'
#' @param tables numeric. Time series tables in `cat_no`` to download and extract. Default is "all",
#' which will read all time series in `cat_no`. Specify `tables` to
#' download and import specific tables(s) - eg. `tables = 1` or `tables = c(1, 5)`.
#'
#' @param series_id (optional) character. Supply an ABS unique time series
#' identifier (such as "A2325807L") to get only that series. This is an alternative
#' to specifying `cat_no`.
#'
#' @param path Local directory in which to save downloaded ABS time series
#' spreadsheets. By default, `path` takes the value set in the environment variable
#' "R_READABS_PATH". To check the value of this variable, run \code{Sys.getenv("R_READABS_PATH")}.
#' If this variable is not set, the files downloaded by read_abs()
#' will be stored in a temporary directory (\code{tempdir()}).
#'
#' @param metadata logical. If `TRUE` (the default), a tidy data frame including
#' ABS metadata (series name, table name, etc.) is included in the output. If
#' `FALSE`, metadata is dropped.
#'
#' @param show_progress_bars TRUE by default. If set to FALSE, progress bars
#' will not be shown when ABS spreadsheets are downloading.
#'
#' @param retain_files when TRUE (the default), the spreadsheets downloaded from the
#' ABS website will be saved in the directory specified with `path`. If set to `FALSE`,
#' the files will be stored in a temporary directory.
#'
#' @return A data frame (tibble) containing the tidied data from the ABS time
#' series table(s).
#'
#' @examples
#'
#' # Download and tidy all time series spreadsheets from the Wage Price Index (6345.0)
#'
#' \donttest{wpi <- read_abs("6345.0")}
#'
#' # Get two specific time series, based on their time series IDs
#'
#' \donttest{cpi <- read_abs(series_id = c("A2325806K", "A2325807L"))}
#'
#' @importFrom purrr walk walk2 map map_dfr map2
#' @importFrom curl nslookup
#' @importFrom dplyr group_by filter
#' @name read_abs
#' @export

read_abs <- function(cat_no = NULL,
                     tables = "all",
                     series_id = NULL,
                     path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                     metadata = TRUE,
                     show_progress_bars = TRUE,
                     retain_files = TRUE){

  if(!is.logical(retain_files)){
    stop("The `retain_files` argument to `read_abs()` must be either TRUE or FALSE.")
  }

  if(is.null(cat_no) & is.null(series_id)){
    stop("read_abs() requires either an ABS catalogue number, such as '6202.0' or '6401.0', or an ABS time series ID like 'A84423127L'.")
  }

  if(!is.null(cat_no) & !is.null(series_id)) {
    stop("Please specify either the cat_no OR the series_id, not both.")
  }

  if(!is.null(cat_no)) {
    if(nchar(cat_no) < 6) {
    message(paste0("Please ensure you include the cat_no extension.\n`read_abs()` will assume you meant \"", cat_no, ".0\"", " rather than ", cat_no))
    cat_no <- paste0(cat_no, ".0")
    }
  }

  if(!is.null(cat_no) & is.null(tables)){
    message(paste0("`tables` not specified; attempting to fetch all tables from ", cat_no))
    tables <- "all"
  }

  if(!is.logical(metadata)){
    stop("`metadata` argument must be either TRUE or FALSE")
  }

  # create temp directory to temporarily store spreadsheets if retain_files == FALSE
  if(!retain_files){
    path <- tempdir()
  }

  # satisfy CRAN
  ProductReleaseDate=SeriesID=NULL

  # create a subdirectory of 'path' corresponding to the catalogue number if specified
  if(retain_files & !is.null(cat_no)){
    path <- file.path(path, cat_no)
  }

  # check that R has access to the internet

  if(is.null(curl::nslookup("abs.gov.au", error = FALSE))){

    stop("R cannot access the ABS website. `read_abs()` requires access to the ABS site.
         Please check your internet connection and security settings." )

  }

  xml_urls <- form_abs_tsd_url(cat_no = cat_no,
                               tables = tables,
                               series_id = series_id)


  # find spreadsheet URLs from cat_no in the Time Series Directory

  download_message <- ifelse(!is.null(cat_no), paste0("catalogue ", cat_no),
                             "series ID ")

  message(paste0("Finding filenames for tables corresponding to ABS ", download_message))

  xml_dfs <- purrr::map_dfr(xml_urls,
                            .f = get_abs_xml_metadata,
                            release_dates = "latest")

  # the same Series ID can appear in multiple spreadsheets; we just want one (the latest)

  if(!is.null(series_id)) {
    xml_dfs <- xml_dfs %>%
      dplyr::group_by(SeriesID) %>%
      dplyr::filter(ProductReleaseDate == max(ProductReleaseDate)) %>%
      dplyr::filter(row_number() == 1)
  }

  urls <- unique(xml_dfs$TableURL)
  urls <- gsub(".test", "", urls)

  table_titles <- unique(xml_dfs$TableTitle)

  # download tables corresponding to URLs
  message(paste0("Attempting to download files from ", download_message,
                 ", ", xml_dfs$ProductTitle[1]))
  purrr::walk(urls, download_abs, path = path, show_progress_bars = show_progress_bars)

  # extract the sheets to a list
  filenames <- base::basename(urls)
  message("Extracting data from downloaded spreadsheets")
  sheets <- purrr::map2(filenames, table_titles,
                       .f = extract_abs_sheets, path = path)

  # remove one 'layer' of the list, so that each sheet is its own element in the list
  sheets <- unlist(sheets, recursive = FALSE)

  # tidy the sheets
  sheet <- tidy_abs_list(sheets, metadata = metadata)

  # remove spreadsheets from disk if `retain_files` == FALSE
  if(!retain_files){
    # delete downloaded files
    file.remove(file.path(path, filenames))
  }

  # if series_id is specified, remove all other series_ids

  if(!is.null(series_id)) {

    users_series_id <- series_id

    sheet <- sheet %>%
      dplyr::filter(series_id %in% users_series_id)

  }

  # return a data frame
  sheet

}

#' Download, extract, and tidy ABS time series spreadsheets (deprecated)
#'
#' \code{get_abs()} is deprecated. Please use \code{read_abs()} instead. It has identical functionality.
#'
#' @export

get_abs <- function(){
  .Deprecated(old = "get_abs()",
              new = "read_abs()",
              msg = "get_abs() is deprecated.\nPlease use read_abs() instead.")
}
