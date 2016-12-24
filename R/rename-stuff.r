##' Rename characters or factors based on their name, as determined by the supplied  regular expression.
##'
##' Inspired by "revalue" from plyr package
##' @param named named character vector or list. Syntax is: "to"="from"
##' @param stuff character vector to be renamed
##' @return a character vector
##' @export
reclassify <- function(stuff, named) {
  label.names <- lapply(named, grep, stuff, value=TRUE)
  labels <- rep(names(named),
                times=sapply(label.names, length))
  names(labels) <- unlist(label.names)
  labels
}

##' Rename characters or factors based on their name, as determined by the supplied  regular expression.
##'
##' Inspired by "revalue" from plyr package
##' @param named named character vector or list. Syntax is: "to"="from"
##' @param stuff character vector to be renamed
##' @return a character vector
##' @export
rerevalue <- function (x, replace = NULL, warn_missing = TRUE, ...) 
{
  if (!is.null(x) && !is.factor(x) && !is.character(x)) {
    stop("`x` is not a factor or a character vector.")
  }
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.")
  }
  if (is.factor(x)) {
    levels(x) <- rerevalue(levels(x), replace, warn_missing, ...)
    return(x)
  }
  if (warn_missing) {
    not_found <- sapply(lapply(replace, grep, x), length)
    if (length(not_found_index <- which(not_found == 0L) != 0L)) {
      message("The following `replace` values were not present in `x`: \\n", 
              paste(replace[not_found_index], collapse = ", "))
    }
  }
  for (i in seq_along(replace)) {
    x <- gsub(replace[i], names(replace)[i], x, ...)
  }
  x
}
