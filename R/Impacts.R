#' @title Computation of elasticities in CoDa regression models
#'
#' @description
#' This function computes elasticities and semi-elasticities for CoDa
#' regression model.
#' where we have to distinguish four cases:
#'  - Y and X are both compositional: this leads to an elasticity
#'  - Y is compositional and X is scalar: this leads to a semi-elasticity
#'  - Y is scalar and X is compositional: this leads to a semi-elasticity
#'  - Y and X are both scalar: this case is not implemented as it leads to constant marginal effects
#'
#' @details
#' The mathematical foundation for elasticity computations in CoDa model come
#' from Morais and Thomas-Agnan (2021).
#' Dargel and Thomas-Agnan (2024) present further results and illustrations.
#'
#' @param object an object of class "lmCoDa"
#' @param Xvar a character indicating the name of one explanatory variable
#' @param obs a numeric that refers to the indicator of one observation
#'
#' @return a matrix
#' @author
#'   - Lukas Dargel
#'   - Rodrigue Nasr
#' @export
#' @rdname Impacts
#' @references
#'   - Dargel, Lukas and Christine Thomas-Agnan, “Pairwise share ratio interpretations of compositional regression models”, Computational Statistics & Data Analysis 195 (2024), p. 107945
#'   - Morais, Joanna and Christine Thomas-Agnan. "Impact of covariates in compositional models and simplicial derivatives." Austrian Journal of Statistics 50.2 (2021): 1-15.
#' @examples
#' res <- lmCoDa(YIELD ~ PRECIPITATION + ilr(TEMPERATURES), data = head(rice_yields,20))
#' Impacts(res, Xvar = "TEMPERATURES")
#'
Impacts <- function(object, Xvar=NULL, obs=1) {

  stopifnot(is(object, "lmCoDa"),
            is.character(Xvar) || length(Xvar) == 1,
            is.numeric(obs) && isTRUE(obs >= 1) && obs <= nobs(object))

  trSry <- object$trSry
  Xvar <- check_Xvar(Xvar, trSry,return_type = "pos")
  Xcoef <- trSry$COEF_CLR[[Xvar]]

  check <- "Impacts are only meaningful if X or Y are compositional!"
  YX_is_compo <- c("" != trSry$LR_TRAN[c(1, Xvar)], use.names = FALSE)
  if (identical(YX_is_compo, c(FALSE, FALSE)))
    stop(check)

  if (identical(YX_is_compo, c(FALSE, TRUE)))
    return(Xcoef)

  # when Y is compositional
  Dy <- trSry$D[[1]]
  Wz <- diag(Dy) - as(fitted(object, space = "simplex")[rep(obs,Dy), ], "matrix")
  imp <- Xcoef %*% t(Wz)
  colnames(imp) <- colnames(Xcoef)
  attr(imp, "obs") <- obs
  return(imp)
}



