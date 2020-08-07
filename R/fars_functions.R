#' Read file
#'
#' This function reads data saved as csv.bz2 format.
#'
#' @param  filename Dataset name as character string.
#'
#' @return This function returns a single dataset with
#'    classes of 'tbl_df', 'tbl' and 'data.frame'.
#'
#' @examples \dontrun{
#' fars_read(filename = 'accident_2013.csv.bz2')
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df %>%
#' @importFrom tibble as_tibble
#'
#' @note Function throws an error if selected file can not be found.
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  tibble::as_tibble(data)
}

#' Make file name
#'
#' This function makes a filename by adding a year to
#'    default data file name "accident_YYYY.csv.bz2".
#'
#' @param  year Year as a character string or numeric value.
#'
#' @return Filename as string.
#'
#' @examples \dontrun{
#' make_filename(2014)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read yearly accident data
#'
#' This function returns US traffic accidents data by year and month.
#'
#' @param years One or several years in a vector of character string
#'    or numeric values.
#'
#' @return This function returns the months and years of accidents.
#'    Data is retured as yearly lists and with
#'    classes of 'tbl_df', 'tbl' and 'data.frame'.
#'
#' @examples \dontrun{
#' fars_read_years(c(2014))
#' fars_read_years(c(2013, 2014, 2015))
#' }
#' @references  US National Highway Traffic Safety Administration
#'
#' @importFrom dplyr mutate select %>%
#'
#' @note Years from 2013 to 2015 are available, function throws an error if
#'    year(s) outside these years are selected.
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Make summary of accidents
#'
#' This function summarizes number of US traffic accidents
#'    by year and month.
#'
#' @inheritParams  fars_read_years
#'
#' @return Lists of accidents by month and year.
#'
#' @examples \dontrun{
#' fars_summarize_years(2014)
#' fars_summarize_years(c(2013, 2014, 2015))
#' }
#' @seealso \code{\link{fars_map_state}} for map plots.
#'
#' @references  US National Highway Traffic Safety Administration
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @note Years from 2013 to 2015 are available, function throws an error if
#'    year(s) outside 2013-2015 is selected.
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Draw a map of accident locations
#'
#' This function crates a state map with US traffic accident locations
#'    at selected year.
#'
#' @param state.num State number as a character string or numeric value.
#'
#' @inheritParams make_filename
#'
#' @return State map with accidents locations.
#' @examples \dontrun{
#' fars_map_state(1, 2014)
#' }
#' @seealso \code{\link{fars_summarize_years}} for summaries.
#'
#' @references  US National Highway Traffic Safety Administration
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @note  Maps can be produced only for one state and year at the time.
#'  Years from 2013 to 2015 are available, function throws an error if
#'    year(s) outside 2013-2015 or invalid state number is selected.
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
