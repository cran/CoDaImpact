#' @title Scenarios for variation in CoDa regressions models
#'
#' @description
#' Scenarios of this type are illustrated in Dargel and Thomas-Agnan (2024).
#' They allow to evaluate how the response variable (Y) in a CoDa model would evolve under a hypothetical scenario for linear changes in one explanatory variable (X).
#' When the changing explanatory variable is compositional the term "linear" is understood with respect to the geometry of the simplex.
#'
#' @details
#' The linear scenario for X is computed with [seq()] in the scalar case and with [CoDa_seq()] in the compositional case.
#' The corresponding changes in Y are computed with the prediction formula, where we exploit the fact that only in one variable is changing.
#'
#'
#' @param object an object of class "lmCoDa"
#' @param Xvar a character indicating the name of the explanatory variable that changes
#' @param Xdir either character or numeric, to indicate the direction in which Xvar should change
#'   - when character this should be one of the components of X, in which case the direction is the corresponding vertex of the simplex
#'   - when numeric this argument is coerced to a unit vector in the simplex
#'   - (when Xvar refers to a scalar variable this argument is ignored)
#' @param obs a numeric indicating the observation used for the scenario
#' @param inc_size a numeric indicating the distance between each point in the scenario of X
#' @param n_steps a numeric indicating the number of points in the scenario
#' @param add_opposite a logical, if `TRUE` the scenario also includes changes in the opposite direction
#' @param normalize_Xdir a logical, if `TRUE` the direction `Xdir` scaled to have an Aitchison norm of 1, allowing to interpret `inc_size` as the Aitchison distance
#' @return a data.frame containing the scenario of X and the corresponding predicted values of Y
#' @author Lukas Dargel
#' @references
#'   - Dargel, Lukas and Christine Thomas-Agnan, “Pairwise share ratio interpretations of compositional regression models”, Computational Statistics & Data Analysis 195 (2024), p. 107945
#' @export
#' @examples
#'
#' # ---- model with scalar response ----
#' res <- lmCoDa(YIELD ~ PRECIPITATION + ilr(TEMPERATURES), data = head(rice_yields,20))
#' VariationScenario(res, Xvar = "TEMPERATURES", Xdir = "MEDIUM", n_steps = 5)
#' VariationScenario(res, Xvar = "PRECIPITATION", n_steps = 5)
#'
#'
#' # ---- model with compositional response ----
#' res <- lmCoDa(ilr(cbind(left, right, extreme_right)) ~
#'                 ilr(cbind(Age_1839, Age_4064)) +
#'                 ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
#'                 log(unemp_rate),
#'               data = head(election))
#'
#' VariationScenario(res, Xvar ="cbind(Age_1839,Age_4064)",Xdir = "Age_1839", n_steps = 5)
#' VariationScenario(res, "log(unemp_rate)", n_steps = 5)
#'
VariationScenario <- function(
    object,
    Xvar,
    Xdir,
    obs = 1,
    inc_size = .1,
    n_steps = 100,
    add_opposite = TRUE,
    normalize_Xdir = TRUE) {

  stopifnot(is(object, "lmCoDa"),
            is.character(Xvar) && length(Xvar) == 1,
            missing(Xdir) || is.character(Xdir) || is.numeric(Xdir),
            is.numeric(obs) && isTRUE(obs >= 1) && obs <= nobs(object),
            is.numeric(inc_size) && length(inc_size) == 1,
            is.numeric(n_steps) && length(n_steps) == 1,
            isTRUE(add_opposite) || isFALSE(add_opposite))

  clo2 <- function(x) x/rowSums(x)
  trSry <- object$trSry

  # get X0
  Xvar <- check_Xvar(Xvar, trSry, "NAME_COORD")
  Dx <- trSry$D[[Xvar]]
  scalar_x <- Dx == 0
  Kx <- trSry$LR_BASE_K[[Xvar]]
  X0 <- object$model[[Xvar]]
  X0 <- if (scalar_x) X0[obs] else t(X0[obs,]) %*% t(Kx) # work with clr in compositional case

  # get Y0
  scalar_y <- trSry$D[[1]] == 0
  Ky <- trSry$LR_BASE_K[[1]]
  Y0 <- fitted(object)
  Y0 <- if (scalar_y) Y0[obs] else Y0[obs,] %*% t(Ky) # work with clr in compositional case

  inc_seq <- seq(from = -n_steps*add_opposite, to = n_steps)
  if (scalar_x) {
    # IDEA allow for "log-linear" sequences
    Xcoef <- trSry$COEF_CLR[[Xvar]]
    Xscenario <- inc_seq*inc_size
    Yscenario <- if (scalar_y) Y0 else Y0[rep(1,length(Xscenario)),,drop=FALSE]
    Yscenario <- Xscenario %*% Xcoef + Yscenario
    Xscenario <- X0 + Xscenario
  }

  if (!scalar_x) {
    Xdir <- check_Xdir(Xdir, colnames(X0), normalize_Xdir)
    Xcoef <- trSry$COEF_CLR[[Xvar]]

    Xscenario <- CoDa_path(rep(1/Dx, Dx), comp_direc = Xdir, step_size = inc_size, n_steps = n_steps, add_opposite = add_opposite)
    Xscenario <- log(as.matrix(Xscenario)) %*% clrBase(Dx)
    Yscenario <- if (scalar_y) Y0 else Y0[rep(1,nrow(Xscenario)),]
    Yscenario <- Xscenario %*% Xcoef + Yscenario
    Xscenario <- clo2(exp(X0[rep(1,nrow(Xscenario)),] + Xscenario))
  }

  if (!scalar_x) Xvar <- "X"
  if (!scalar_y) Yvar <- "Y" else Yvar <- rownames(trSry)[1]
  if (!scalar_y) Yscenario <- clo2(exp(Yscenario))
  result <- data.frame(row.names = as.character(inc_seq))
  result[[Yvar]] <- Yscenario
  result[[Xvar]] <- Xscenario
  attr(result, "inc_size") <- inc_size
  attr(result, "obs") <- obs
  return(result)
}

