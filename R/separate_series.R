#' Separate the series column in a tidy ABS time series data frame
#'
#' Separate the 'series' column in a data frame (tibble)
#' downloaded using \code{read_abs()} into multiple columns using the ";" separator.
#'
#' @param data A data frame (tibble) containing tidied data from the ABS time
#' series table(s).
#' @param column_names (optional) character vector. Supply a vector of column names, such as
#' \code{c("group_name", "variable","gender")}. If not supplied, columns will be named "series_1" etc.
#' @param remove_totals logical. FALSE by default. If set to TRUE, any series rows that contain the word "total" will be removed.
#' @param remove_nas locical. FALSE by default. If set to TRUE, any rows containining an NA in at least one of the separated series columns will be removed.
#'
#' @return A data frame (tibble) containing the tidied data from the ABS time
#' series table(s).
#'
#' @export
#'
#' @examples
#'
#' \donttest{motor_vehicles <- read_abs("9314.0") %>% separate_series()}
#'
#' @importFrom stringr str_extract str_replace_all str_squish str_count
#' @importFrom dplyr filter mutate_at syms
#' @importFrom tidyr separate
#' @importFrom purrr map_dfr

separate_series <- function(data,
                            column_names = NULL,
                            remove_totals = FALSE,
                            remove_nas = FALSE) {

  # satisfy R CMD check's "no visible binding" error
  series <- NULL
  original_series <- NULL

  # Create copy of series column to restore later
  data <- mutate(data,
                 original_series = series)

  # Minor data cleaning of series column
  data <- mutate(data,
                 series = str_extract(series, ".+(?= ;)"), #Extract everything before trailing ;
                 series = str_replace_all(series, pattern = ">", replacement = ""),
                 series = str_squish(series))

  # Filter totals if specified
  if(remove_totals){
    data <- filter(data,
                   !grepl("total", series, ignore.case = T))
  }

  # Determine number of ; separators in series column
  n_seps <- max(str_count(data$series, ";"))

  # Determine number of columns to split series into
  n_columns <- n_seps + 1

  # Create new column names if not specified
  if(is.null(column_names)){
    column_names <- paste("series", 1:n_columns, sep = "_")
  }

  # Separate columns
  data_separated <- separate(data, series,
                             into = column_names,
                             sep = ";",
                             remove = FALSE,
                             fill = "left") %>%
    mutate_at(.vars = vars(column_names), str_squish)


  # check for columns with NAs
  na_columns <- paste0(colnames(data_separated)[colSums(is.na(data_separated)) > 0],
                       collapse = ", ")

  if(na_columns != "" & !remove_nas){warning(na_columns, " column(s) have NA values.", call. = TRUE)}

  # filter NAs in new series columns

  if(remove_nas & na_columns != "") {

    na_col_syms <- dplyr::syms(na_columns)

    remove_na_fn <- function(data, column) {

      data %>%
        filter(!is.na( {{ column }} ))

    }

    data_separated <- purrr::map_dfr(.x = na_col_syms,
        .f = remove_na_fn,
        data = data_separated)

    message("Rows with NAs in separated series column(s) have been removed.")

  }


  # Replace original series column
  data_separated <- mutate(data_separated,
                           series = original_series)

  data_separated <- select(data_separated,
                           -original_series)

  return(data_separated)

}





