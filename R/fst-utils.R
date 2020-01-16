#' Internal functions for working with the fst cache
#' @noRd


catno2fst <- function(cat_no,
                      table = integer(0L),
                      path = Sys.getenv("R_READABS_PATH", unset = tempdir())) {
  if (length(table) > 1L) {
    stop("Internal error (catno2fst): length(table) > 1 at this time. Please report.")
  }
  basename.fst <- gsub(".", "-", cat_no, fixed = TRUE)
  if (length(table) == 0L || identical(table, "all")) {
    basename.fst <- paste0(basename.fst, ".fst")
  } else {
    basename.fst <- paste0(basename.fst, sprintf("T%02d", table), ".fst")
  }
  fullname.fst <- file.path(path, "fst", basename.fst)
  hutils::provide.file(fullname.fst)
}

fst_available <- function(cat_no,
                          table = integer(0L),
                          path = Sys.getenv("R_READABS_PATH", unset = tempdir())) {
  if (!requireNamespace("fst", quietly = TRUE) ||
      !dir.exists(path)) {
    return(FALSE)
  }

  file.fst <- catno2fst(cat_no, table = table, path)

  if (!file.exists(file.fst)) {
    return(FALSE)
  }

  # fst may be damaged. If it is, (i.e. fst metadata returns an error) return FALSE
  # nocov start
  out <- tryCatch(inherits(fst::fst.metadata(file.fst), "fstmetadata"),
                  error = function(e) FALSE,
                  warning = function(e) FALSE)
  # nocov end
  out
}

ext2ext <- function(file, new.ext) {
  paste0(tools::file_path_sans_ext(file), new.ext)
}










