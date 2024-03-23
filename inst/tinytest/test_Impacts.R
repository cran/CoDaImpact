# ---- Y - compositional ------------------------------------------------------
res <- lmCoDa(
  ilr(cbind(left, right, extreme_right)) ~
    ilr(cbind(Age_1839, Age_4064)) +
    ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
    unemp_rate,
  data = election[1:20,])

expect_equal(
  Impacts(res, Xvar = "cbind(Age_1839, Age_4064)",obs =  10)*0, {
  res0 <- matrix(0,2,3,dimnames = list(c("Age_1839", "Age_4064"), c("left", "right", "extreme_right")))
  attr(res0, "obs") <- 10
  res0})

expect_equal(
  Impacts(res, Xvar =  "unemp_rate",obs =  10)*0, {
  res0 <- matrix(0,1,3,dimnames = list(c("unemp_rate"), c("left", "right", "extreme_right")))
  attr(res0, "obs") <- 10
  res0})

# ---- Y - scalar -------------------------------------------------------------
res <- lmCoDa(YIELD ~ PRECIPITATION + ilr(TEMPERATURES), data = rice_yields[1:20,])

expect_error(Impacts(res, Xvar = "PRECIPITATION"), pattern = "Impacts")

expect_equal(
  Impacts(res, Xvar = "TEMPERATURES",obs =  10)*0, {
    res0 <- matrix(0,3,1,dimnames = list(c("LOW", "MEDIUM", "HIGH"), c("YIELD")))
    res0})

