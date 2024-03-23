#' Internal: check for valid computational direction arguments
#'
#' @param Xdir a character or numeric indicating the direction
#' @param Xopts a character indicating the names of the vertices
#' @param normalize a logical if true Xdir is normalized to have an Aitchison norm of 1
#' @return a numeric vector
#'
#' @author Lukas Dargel
#' @keywords internal
check_Xdir <- function(Xdir, Xopts, normalize = FALSE) {

  stopifnot(is.numeric(Xdir) || is.character(Xdir),
            is.character(Xopts),
            is.logical(normalize))
  Dx <- length(Xopts)

  if (is.numeric(Xdir)) {
    check <- "When Xdir is numeric it musst be a non-negative vector of dimension %s!"
    if (length(Xdir) != Dx || any(Xdir < 0)) stop(sprintf(check, Dx))
    if (normalize) {
      Xdir <- ilr(Xdir)
      Xdir <- as(ilrInv(Xdir/sqrt(sum(Xdir^2))),"vector")
    } else Xdir <- Xdir/sum(Xdir)
    return(Xdir)
  }

  if (is.character(Xdir)) {
    check <- "When Xdir is character it musst be one of %s!"
    Xbin <- Xopts %in% Xdir
    if (sum(Xbin) != 1) stop(sprintf(check, list(Xopts)))

    Xdir <- exp(Xbin)
    if (normalize) Xdir[Xbin] <- Xdir[Xbin]^sqrt(Dx/(Dx-1))
    return(Xdir/sum(Xdir))
  }
}

#' Internal: check for valid name of Xvar
#'
#' Users should always specify Xvar as "NAME_SIMPLEX",
#' which means before log-ratio transformations.
#'
#' @param Xvar a character or numeric indicating the direction
#' @param trSry a character indicating the names of the vertices
#' @return a single integer or character
#'
#' @author Lukas Dargel
#' @keywords internal
check_Xvar <- function(Xvar, trSry, return_type = c("NAME_SIMPLEX", "NAME_COORD", "pos")[1]) {

  stopifnot(is.character(Xvar) && length(Xvar) == 1,
            is.data.frame(trSry),
            isTRUE(return_type %in% c("NAME_COORD", "NAME_SIMPLEX", "pos")))


  Anames <- unlist(trSry$NAME_SIMPLEX, use.names = FALSE)
  Xnames <- setdiff(Anames[-1], "(Intercept)")
  is_Xvar <- gsub(" ", "", Xvar) == gsub(" ", "", Xnames)   # tolerate blanks
  if (sum(is_Xvar) != 1) stop("Xvar musst be one of ", list(Xnames), "!")

  Xvar <- Xnames[is_Xvar]
  Xvar <- switch(return_type,
    "NAME_SIMPLEX" = Xvar,
    "NAME_COORD"   = trSry$NAME_COORD[Xvar == Anames],
    "pos"          = which(Anames == Xvar))
  Xvar <- c(Xvar,recursive = TRUE, use.names = FALSE)
  return(Xvar)
}

#' Format numbers to percentages
#' This code copied from stats:::format.perc(), to avoid notes about the ::: operator.
#' @keywords internal
pct <- function (probs, digits = 10) {
  paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits), "%")
}

