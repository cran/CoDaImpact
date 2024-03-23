#' @title Confidence Intervals for CoDa Models
#'
#' @description
#' Dargel and Thomas-Agnan (2024) show to compute variances and confidence
#' intervals for parameters of CoDa models in log-ratio spaces.
#'
#' Of particular interest are the clr parameters since they can be directly
#' interpreted as differences from an average elasticity.
#'
#' Another option is interpret the difference in clr parameters as these
#' coincide with the difference in elasticities.
#'
#'
#' @details
#' Since CoDa models are often multivariate this function only allows to
#' specify one explanatory variable at a time.
#' The output is also more complex than the usual one for "lm" classes, because
#' we have to indicate the component of Y and X.
#' With [confint.lm()] it is still possible to compute the usual the confidence
#' intervals.
#'
#' @param object class "lmCoDa"
#' @param parm a character, indicating the name of one explanatory variable
#' @param level a numeric, indicating the confidence level required
#' @param y_ref an optional argument that indicates the reference component of
#'   the response variable using its name or its position. \cr
#'   This argument is only used in the Y-compositional model.
#'   If it is supplied confidence intervals of difference are used instead of
#'   the direct intervals of the parameters.
#' @param obs an optional integer that indicates one observation
#'   when this argument is supplied the function return the observation
#'   dependent elasticity
#' @param ... passed on to confit()
#' @return data.frame
#'
#' @exportS3Method
#' @importFrom stats qt
#' @author Lukas Dargel
#' @references
#'   - Dargel, Lukas and Christine Thomas-Agnan, “Pairwise share ratio interpretations of compositional regression models”, Computational Statistics & Data Analysis 195 (2024), p. 107945
#' @examples
#'
#' ## ==== Y-compositional model ====
#' res <- lmCoDa(
#'   ilr(cbind(left, right, extreme_right)) ~
#'   ilr(cbind(Age_1839, Age_4064)) +
#'   ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
#'   unemp_rate,
#'   data = head(election, 20))
#'
#' ## ---- CI for scalar X
#' # CI for clr parameters
#' confint(res, "unemp_rate")
#' # CI for difference in clr parameters (coincides with difference in the semi elasticity)
#' confint(res, "unemp_rate", y_ref = 1)
#'
#' ## ---- CI for compositional X
#' # CI for clr parameters
#' confint(res, "cbind(Age_1839, Age_4064)")
#'
#' # CI for difference in clr parameters (coincides with difference in the elasticity)
#' confint(res, "cbind(Age_1839, Age_4064)", y_ref = 1)
#'
#'
confint.lmCoDa <- function(
    object,
    parm,
    level = .95,
    y_ref = NULL,
    obs = NULL,
    ...) {

  stopifnot(level > 0 && level < 1,
            is.character(parm) && length(parm) == 1,
            is.null(y_ref) || length(y_ref) == 1)

  trSry <- object$trSry
  Xnames <- c(object$trSry$NAME_SIMPLEX[-1],use.names = FALSE)
  if (!parm %in% Xnames) stop("parm must be one of ", list(Xnames))
  Xpos <- c(which(parm == object$trSry$NAME_SIMPLEX),use.names = FALSE)

  if (all(0 == c(trSry$D[[1]], trSry$D[[Xpos]])))
    return(stats::confint.lm(object, rownames(trSry)[Xpos], level))

  est_coef <- t(trSry[["COEF_CLR"]][[Xpos]])
  vcov_x <- trSry[["VARCOV_CLR"]][[Xpos]]
  vcov_y <- trSry[["VARCOV_CLR"]][[1]]
  y_vars <- colnames(vcov_y)

  df_eq <- nobs(object) - ncol(t(coef(object)))
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  qlevel <- qt(a, df_eq)

  # 3 cases
  if (ncol(vcov_y) < 2) y_ref <- obs <- NULL # when scalar y we only need clr
  clr_intervals <- is.null(y_ref) & is.null(obs)
  diff_intervals <- !is.null(y_ref)
  elast_intervals <- !is.null(obs)

  if (clr_intervals) {
    sd_coef <- lapply(sqrt(diag(vcov_x)), "*", sqrt(diag(vcov_y)))
    sd_coef <- t(t(do.call("cbind", sd_coef)))

    result <- vector("list", length = ncol(est_coef))
    for (i in seq_along(result)) {
      result[[i]] <- data.frame(
        "Y"   = colnames(vcov_y),
        "X"   = colnames(est_coef)[i],
        "est" = est_coef[,i],
        "sd"  = sd_coef[,i],
        "qLo" = sd_coef[,i] * qlevel[1] + est_coef[,i],
        "qHi" = sd_coef[,i] * qlevel[2] + est_coef[,i],row.names = NULL)
    }
    result <- Reduce("rbind", result)
    colnames(result) <- c("Y","X","EST","SD", pct(a))
    attr(result, "type") <- "CI for clr coeffiecients"
    return(result)
  }

  if (diff_intervals) {
    if (is.character(y_ref)) y_ref <- which(y_ref == y_vars)
    if (!any(y_ref %in% seq_len(ncol(vcov_y)))) stop("y_ref is not identifyable!")
    diff_coef <- est_coef - est_coef[rep(y_ref,length(y_vars)), ]
    sd_dy <- sqrt(diag(vcov_y) + vcov_y[y_ref,y_ref] - 2*vcov_y[y_ref,])

    result <- vector("list", length = ncol(est_coef))
    for (i in seq_along(result)) {
      sd_dcoef <- sqrt(vcov_x[i,i]) * sd_dy

      result[[i]] <- data.frame(
        "Y_ref" = colnames(vcov_y)[y_ref],
        "Y"   = colnames(vcov_y),
        "X"   = colnames(est_coef)[i],
        "dif" = diff_coef[,i],
        "sd"  = sd_dcoef,
        "qLo" = sd_dcoef * qlevel[1] + diff_coef[,i],
        "qHi" = sd_dcoef * qlevel[2] + diff_coef[,i],row.names = NULL)
    }
    result <- Reduce("rbind", result)
    colnames(result) <- c("Y_ref","Y","X","DIFF","SD", pct(a))
    attr(result, "type") <- "CI for differences in impacts"
    return(result)
  }


  if (elast_intervals) {
    stop("CI for observation dependent elastities is not yet implemented!") #...
    # ....it would require a Monte Carlo approach
    # ....or an approximation by Delta method might work too
    }
}
