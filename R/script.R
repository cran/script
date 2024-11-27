#' Identify Script Name
#'
#' Identifies script name in a variety of contexts,
#' e.g. interactively or when script is sourced.
#' Attempts to support RStudio environment.
#'
#' @export
#' @importFrom rstudioapi getActiveDocumentContext getSourceEditorContext
#' @importFrom knitr current_input
#' @return character: path to current file, or empty string if indeterminate
#' @examples
#' script()
script <- function() {
  # qmd currently returns .rmarkdown
  this <- knitr::current_input()
  if(!(is.null(this))){
    if(file.exists(this)){
      this <- sub('\\.rmarkdown$','.qmd', this)
      return(normalizePath(this))
    }
  }
  # http://stackoverflow.com/a/32016824/2292993
  cmdArgs = commandArgs(trailingOnly = FALSE)
  needle = "--file="
  match = grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript via command line
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  }
  ls_vars = ls(sys.frames()[[1]])
  if ("input" %in% ls_vars) {
    # Source'd via RStudio
    return(normalizePath(sys.frames()[[1]]$input))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    # Source'd via R console
    return(normalizePath(sys.frames()[[1]]$ofile))
  }
  # RStudio Run Selection
  # http://stackoverflow.com/a/35842176/2292993
  pth <- ''
  tryCatch({
    pth = rstudioapi::getActiveDocumentContext()$path
    if(pth != '') pth = normalizePath(pth)
  }, error = function(e){
    pth = ''
  }
  )
  if (pth!='') {
    return(pth)
  }
  # RStudio Console
  tryCatch({
    pth = rstudioapi::getSourceEditorContext()$path
    if(pth != '') pth = normalizePath(pth)
  }, error = function(e){
    pth = ''
  }
  )
  return(pth)
}

#' Identify Script base Name
#'
#' Identifies script base name using \code{\link{script}}.
#' if \code{ext} is supplied (and \code{script() is not empty string}),
#' the last dot (and anything that follows it) is removed if present;
#'  \code{ext} is appended.
#' @param ext replacement extension
#' @export
#' @return character: current filename, or empty string if indeterminate
#' @examples
#' scriptbase('.csv')
scriptbase <- function(ext = NULL){
  x <- script()
  x <- basename(x)
  if(x == '') return(x)
  if(is.null(ext)) return(x)
  stopifnot(is.character(ext), !is.na(ext), length(ext) == 1)
  x <- sub('[.][^.]*$','',x)
  x <- paste0(x, ext)
  return(x)
}

