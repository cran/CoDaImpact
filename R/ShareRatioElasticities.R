#' Compute share ratio elasticities for CoDa models
#'
#' In CoDa models with compositional dependent variable (Y) share ratio elasticities (SRE)
#' allow to interpret the influence of compositional explanatory variables (X).
#' The interpretation is analogous to usual elasticities:
#'   - When the share ratio of X increases by 1% the share ratio of Y increases by SRE%
#'   - The main difference to usual elasticities that, since X is compositional
#'     the change of X musts be specified in terms of a direction in the simplex.
#'
#'
#' @details
#' More details on this interpretation can be found in Dargel and Thomas-Agnan (2024) and in the accompanying vignette.
#'
#' @inheritParams VariationScenario
#' @param Xdir a numeric vector, a single character, or `NULL`:
#'   - if numeric `Xdir` is taken as a fixed direction in the simplex
#'   - if character `Xdir` is interpreted as one summit of the X composition and converted to the fixed direction towards this summit
#'   - if `NULL` the share ratio elasticities are computed for variable directions corresponding the example in Dargel and Thomas-Agnan (2024 Lukas Dargel & Christine Thomas-Agnan (2024) The link between multiplicative competitive interaction models and compositional data regression with a total, Journal of Applied Statistics, DOI: 10.1080/02664763.2024.2329923 )
#' @return a data.frame
#'
#' @author Lukas Dargel
#' @references
#'   - Dargel, Lukas and Christine Thomas-Agnan, “Pairwise share ratio interpretations of compositional regression models”, Computational Statistics & Data Analysis 195 (2024), p. 107945
#' @export
#' @examples
#'
#' ### XY-compositional model
#' res <- lmCoDa(
#'   ilr(cbind(left, right, extreme_right)) ~
#'   ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)),
#'   data =  head(election, 20))
#'
#' ## Focus on changes in the education composition
#' educ_comp <- "cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)"
#'
#' ## case 1
#' ## changes towards the summit "Educ_Higher" as (fixed) direction
#' SRE1 <- ShareRatioElasticities(res, Xvar = educ_comp, Xdir = "Educ_Higher")
#'
#' SRE1[1,]
#' # Result: SRE=Inf
#' # cannot be interpreted because, for this direction,
#' # the relative change in the share ratio of X (Highschool / BeforeHighschool) is zero
#' SRE1[7,]
#' # Result: SRE=0.9
#' # when the ratio of X (Higher / BeforeHighschool) increases by 1%
#' # the ratio of Y (right / left) increases by about 0.9%
#'
#' ## case 2
#' ## numeric vector as (fixed) direction
#' SRE2 <- ShareRatioElasticities(res, Xvar = educ_comp, Xdir = exp(c(0,0,1)))
#' identical(SRE1,SRE2) # exp(c(0,0,1)) is the direction that points to the third summit
#'
#' ## case 3
#' ## variable directions with Xdir = NULL
#' ## In this case the direction depends components used for the share ratio of X
#' ## In particular the component of X in the numerator grows
#' ## by the same rate as the denominator decreases
#' SRE3 <- ShareRatioElasticities(res, Xvar = educ_comp, Xdir = NULL)
#' SRE3[1,]
#' # Result: SRE=-2.8
#' # when the ratio of X (Highschool / BeforeHighschool) increases by 1%
#' # the ratio of Y (right / left) decreases by about -2.8%
ShareRatioElasticities <- function(
    object,
    Xvar,
    Xdir = NULL){

  stopifnot(is(object, "lmCoDa"),
            is.character(Xvar) && length(Xvar) == 1,
            is.null(Xdir) || is.character(Xdir) || is.numeric(Xdir))

  trSry <- object$trSry
  Xpos <- check_Xvar(Xvar, trSry, "pos")

  check <- "Share ratio elasticities are only meaningful when X and Y are compositional!"
  Dx <- trSry$D[[Xpos]]
  Dy <- trSry$D[[1]]
  if (Dx == 0 || Dy == 0) stop(check)

  # allocate for results in vectorized form
  resX  <- expand.grid(Xj = seq(Dx), Xl = seq(Dx))
  resY  <- expand.grid(Yj = seq(Dy), Yl = seq(Dy))
  resXY <- merge(resY[resY$Yj != resY$Yl,], resX[resX$Xj != resX$Xl,])
  resXY <- cbind(resXY, "SRE" = NA_real_)

  y_names <- colnames(trSry$COEF_CLR[[Xpos]])
  x_names <- rownames(trSry$COEF_CLR[[Xpos]])

  if (is.null(Xdir)) {
    # no direction specified:
    # use relative compensation of different components of X by default
    B  <- t(trSry$COEF_CLR[[Xpos]])
    resXY$SRE <- unlist(Map(
      function(Xnum, Xden, Ynum, Yden) B[Ynum, Xnum] - B[Yden, Xnum] - B[Ynum, Xden] + B[Yden, Xden],
      Xnum = resXY$Xj, Xden = resXY$Xl, Ynum = resXY$Yj, Yden = resXY$Yl))/2
  }

  if (!is.null(Xdir)) {
    Xdir <- check_Xdir(Xdir = Xdir, Xopts = x_names, normalize = FALSE)
    u  <- log(Xdir)
    uB <- u %*% trSry$COEF_CLR[[Xpos]]
    resXY$SRE <- (uB[resXY$Yj] - uB[resXY$Yl]) / (u[resXY$Xj] - u[resXY$Xl])
  }

  resXY$Yj <- y_names[resXY$Yj]
  resXY$Yl <- y_names[resXY$Yl]
  resXY$Xj <- x_names[resXY$Xj]
  resXY$Xl <- x_names[resXY$Xl]
  return(resXY)
}
