# ---- constructors -----------------------------------------------------------

#' Estimating CoDa regression models
#'
#' This is a thin wrapper around [lm()] followed by [ToSimplex()], which
#' allows to create a lmCoDa object in one step.
#'
#' @param formula as in [lm()]
#' @param data as in [lm()]
#' @param ... arguments passed on to [lm()]
#' @return an object of class "lm" and "lmCoDa" if the formula include at least
#'   one log-transformation
#'
#' @author Lukas Dargel
#' @seealso [lm()], [ToSimplex()], [compositions::ilr()], [compositions::alr()]
#' @export
#' @examples
#'
#' # XY-compositional model
#' res <- lmCoDa(
#'   ilr(cbind(left, right, extreme_right)) ~
#'   ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)),
#'   data =  head(election, 20))
#'
#' # X-compositional model
#' res <- lmCoDa(YIELD ~ PRECIPITATION + ilr(TEMPERATURES), data = head(rice_yields, 20))
#'
lmCoDa <- function(formula, data, ...) {
  cl <- match.call()
  cl[[1]] <- as.symbol("lm")
  res <- eval(cl)
  cl[[1]] <- as.symbol("lmCoDa")
  res$call <- cl
  return(ToSimplex(res))
}


#' Converting Linear Models to CoDa models
#'
#' @description
#' The function converts the output of a "lm" to the "lmCoDa" class, which
#' offers additional tools for the interpretation of a CoDa regression models.
#' Most of the work is done by the [transformationSummary()] function, which
#' has its own documentation page, but should be reserved for internal use.
#'
#'
#' @inherit lmCoDa return examples
#' @inheritParams VariationScenario
#' @seealso [lm()], [lmCoDa()]
#' @author
#'   - Lukas Dargel
#'   - Rodrigue Nasr
#' @export
#' @examples
#'
#' # XY-compositional model
#' res <- lm(
#'   ilr(cbind(left, right, extreme_right)) ~
#'   ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)),
#'   data =  head(election, 20))
#' res <- ToSimplex(res)
#'
#' # X-compositional model
#' res <- lm(YIELD ~ PRECIPITATION + ilr(TEMPERATURES), data = head(rice_yields, 20))
#' res <- ToSimplex(res)
ToSimplex <- function(object){

  trSry <- transformationSummary(object)
  if (all(trSry[,'LR_TRAN'] == ""))
    return(object)

  object$trSry <- trSry
  class(object) <- c('lmCoDa', class(object))
  if (trSry[1,'LR_TRAN'] == "")
    return(object)

  invTran <- match.fun(paste0(trSry[1,'LR_TRAN'],"Inv"))
  attributes(object$fitted.values)$orig <- invTran(object$fitted.values)
  attributes(object$residuals)$orig     <- invTran(object$residuals)
  return(object)
}


# ---- methods ----------------------------------------------------------------
#' @inherit predict.lmCoDa title description details params
#' @param split logical, if `TRUE` the coefficients are reported as a list instead
#'   of a matrix, where list structure reflects the explanatory variables of the model
#' @param ... not used
#' @return a matrix
#'
#' @author Lukas Dargel
#' @exportS3Method
coef.lmCoDa <- function(object, space = NULL, split = FALSE, ...) {
  stopifnot(is.null(space) || space %in% c("clr", "simplex"))
  type <- if (is.null(space)) "COEF_COORD" else c("clr" = "COEF_CLR", "simplex" = "COEF_SIMPLEX")[space]
  cfs <- object$trSry[[type]][-1]
  cfs <- if (split) cfs else Reduce("rbind", cfs)
  return(cfs)
}


#' @inherit predict.lmCoDa title description details params
#' @return matrix or vector
#' @importFrom stats fitted
#' @author Lukas Dargel
#' @exportS3Method
fitted.lmCoDa <- function(object, space = NULL, ...) {

  stopifnot(is.null(space) || space %in% c("clr", "simplex"))
  fity <- object$fitted.values
  if (is.null(space))
    return(fity)

  Ky <- object$trSry[["LR_BASE_K"]][[1]]
  if (length(Ky) == 0)
    stop("The space argument can only be used for Y compositional models!")

  if (space == "simplex")
    return(attr(fity, "orig"))

  if (space == "clr")
    return(fity %*% t(Ky))
}


#' Predictions, fitted values, residuals, and coefficients in CoDa models
#'
#' These functions work as in the usual lm object.
#' They additionally offer the possibility use the `space` argument
#' which transforms them into directly into clr space or in the simplex.
#'
#' @param object class "lmCoDa"
#' @param space a character indicating in which space the prediction should
#'   be returned. Supported are the options `c("clr", "simplex")`.
#' @param ... passed on to [predict.lm()]
#' @return matrix or vector
#' @author Lukas Dargel
#' @exportS3Method
predict.lmCoDa <- function(object, space = NULL, ...) {

  stopifnot(is.null(space) || space %in% c("clr", "simplex"))
  pred <- NextMethod("predict")
  if (is.null(space))
    return(pred)

  Ky <- object$trSry[["LR_BASE_K"]][[1]]
  if (length(Ky) == 0)
    stop("The space argument can only be used for Y compositional models!")

  if (space == "simplex")
    return(clrInv(pred %*% t(Ky)))

  if (space == "clr")
    return(pred %*% t(Ky))
}

#' @inherit predict.lmCoDa title description details params
#' @return matrix or vector
#' @author Lukas Dargel
#' @exportS3Method
residuals.lmCoDa <- function(object, space = NULL, ...) {

  stopifnot(is.null(space) || space %in% c("clr", "simplex"))
  resi <- object$residuals
  if (is.null(space))
    return(resi)

  Ky <- object$trSry[["LR_BASE_K"]][[1]]
  if (length(Ky) == 0)
    stop("The space argument can only be used for Y compositional models!")

  if (space == "simplex")
    return(attr(resi, "orig"))

  if (space == "clr")
    return(resi %*% t(Ky))
}
