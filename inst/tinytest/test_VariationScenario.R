expect_equal({
  res <- lmCoDa(
    ilr(cbind(left, right, extreme_right)) ~
      ilr(cbind(Age_1839, Age_4064)) +
      ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
      log(unemp_rate),
    data = head(election,20))

  list(
    VariationScenario(res, "cbind(Age_1839, Age_4064)",Xdir = "Age_1839", n_steps = 5, add_opposite = FALSE),
    VariationScenario(res, "log(unemp_rate)", n_steps = 5, add_opposite = FALSE))
},
{
  res <- lmCoDa(
    alr(cbind(left, right, extreme_right)) ~
      alr(cbind(Age_1839, Age_4064)) +
      alr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
      log(unemp_rate),
    data = head(election,20))
  list(
    VariationScenario(res, "cbind(Age_1839, Age_4064)",Xdir = "Age_1839", add_opposite = FALSE, n_steps = 5),
    VariationScenario(res, "log(unemp_rate)",Xdir = "Age_1839",add_opposite = FALSE,n_steps = 5))
},info = "Y compo & (X compo + X scalar)")

expect_equal({
  res <- lmCoDa(YIELD ~ PRECIPITATION + ilr(TEMPERATURES), data = head(rice_yields,20))
  list(
    VariationScenario(res, "TEMPERATURES", Xdir = "MEDIUM", n_steps = 5, add_opposite = FALSE),
    VariationScenario(res, "PRECIPITATION", n_steps = 5, add_opposite = FALSE))
},
{
  res <- lmCoDa(YIELD ~ PRECIPITATION + alr(TEMPERATURES), data = head(rice_yields,20))
  list(
    VariationScenario(res, "TEMPERATURES", Xdir = "MEDIUM", n_steps = 5, add_opposite = FALSE),
    VariationScenario(res, "PRECIPITATION", n_steps = 5, add_opposite = FALSE))
},info = "Y scalar & (X compo + X scalar)")

