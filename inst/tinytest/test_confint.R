library("compositions")

# ---- confint (univariate) ---------------------------------------------------
# Y-compositional
res <- lmCoDa(
  ilr(cbind(left, right, extreme_right)) ~
    ilr(cbind(Age_1839, Age_4064)) +
    ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
    unemp_rate,
  data = election[1:20,])
expect_true(is.data.frame(confint(res, "unemp_rate")))
expect_true(is.data.frame(confint(res, "cbind(Age_1839, Age_4064)")))
expect_true(is.data.frame(confint(res, "cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)")))

res2 <- confint(res, "cbind(Age_1839, Age_4064)")
expect_true(all(res2$EST > res2$Q025) && all(res2$EST < res2$Q975))
expect_equal(
  res2[,c("Y","X")],
  expand.grid(Y = c("left", "right", "extreme_right"),
              X = c("Age_1839", "Age_4064"), stringsAsFactors = FALSE),
  check.attributes = FALSE)
rm(res, res2)

# Y-scalar
res <- lmCoDa(YIELD ~ PRECIPITATION + ilr(TEMPERATURES), data = rice_yields[1:20,])
expect_true(is.data.frame(confint(res, "TEMPERATURES")))
expect_equal(confint(res, "TEMPERATURES"),
             confint(res, "TEMPERATURES",y_ref = 3))
rm(res)

# ---- confint (differences) --------------------------------------------------
expect_true({
  res <- confint(lmCoDa(
    ilr(cbind(left, right, extreme_right)) ~
      ilr(cbind(Age_1839, Age_4064)) +
      ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
      unemp_rate, data = election[1:20,]),
    parm = "cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)",
    y_ref = 1)
  all(res[res$Y_ref == res$Y, c("DIFF", "SD", "2.5 %", "97.5 %")] == 0)
},info = "Y compo - X comp")


expect_true({
  res <- confint(lmCoDa(
    ilr(cbind(left, right, extreme_right)) ~
      ilr(cbind(Age_1839, Age_4064)) +
      ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
      unemp_rate, data = election[1:20,]),
    parm = "unemp_rate",
    y_ref = 1)
  all(res[res$Y_ref == res$Y, c("DIFF", "SD", "2.5 %", "97.5 %")] == 0)
},info = "Y compo - X scalar")

# ---- confint (elasticity) ---------------------------------------------------
# Y compo
res <- lmCoDa(
  ilr(cbind(left, right, extreme_right)) ~
    ilr(cbind(Age_1839, Age_4064)) +
    ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
    unemp_rate, data = election[1:20,])

expect_error({
  ci <- confint(res,parm = "cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)", obs = 1)},
  pattern = "implemented")
# expect_equivalent({
#   ci <- confint(res,parm = "unemp_rate", obs = 10)
#   sum(ci$IMPACT)}, 0, info = "Y compo - X scalar")

