library("compositions")

# ---- methods ----------------------------------------------------------------
ddd <- election[1:20,]
res <- ToSimplex(lm(
  ilr(cbind(left, right, extreme_right)) ~
    ilr(cbind(Age_1839, Age_4064)) +
    ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
    unemp_rate, data = ddd))

expect_equal(
  ilrInv(res$fitted.values),
  fitted(res, space = "simplex"),
  info = "fit")

expect_equal(
  ilrInv(predict(res)),
  predict(res, space = "simplex"),
  info = "pred ")

expect_equal(
  ilrInv(resid(res)),
  resid(res, space = "simplex"))

expect_equal({
  cf <- coef(res)
  list(dim(cf) + c(2,1),
       c(1, 0, 0, 0, 0, 0, 1))

},{
  cf <- coef(res, space = "simplex")
  list(dim(cf),
       unname(round(rowSums(cf),12)))
})

expect_equal({
  cf <- coef(res)
  list(dim(cf) + c(2,1),
       c(0, 0, 0, 0, 0, 0, 0))

},{
  cf <- coef(res, space = "clr")
  list(dim(cf),
       unname(round(rowSums(cf),12)))
})

