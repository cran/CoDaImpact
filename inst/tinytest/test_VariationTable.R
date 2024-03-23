# ---- Y - compositional case ------------------------------------------------
res <- lmCoDa(
  ilr(cbind(left, right, extreme_right)) ~
    ilr(cbind(Age_1839, Age_4064)) +
    ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
    log(unemp_rate),
  data = head(election, 20))

# compositional X
expect_equal(
  VariationTable(res, Xvar = "cbind(Age_1839, Age_4064)", Xdir = "Age_1839"),
  VariationTable(res, Xvar = "cbind(Age_1839, Age_4064)", Xdir = c(2,1)),
  info = "Coherence between preferential and general direction.",
  check.attributes = FALSE)

resX <- VariationTable(res, Xvar = "cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)", Xdir = c(.5,.4,.1))
expect_true("elasticity" %in% tolower(rownames(resX)))
expect_true(sum(attr(resX, "X(0)")) == 1)
expect_true(sum(attr(resX, "X(h)")) == 1)
expect_true(sum(attr(resX, "Xdir")) == 1)
expect_true(sum(resX["Initial parts",]) == 1)
expect_equivalent(sum(resX["New parts",]),1)
expect_equivalent(sum(resX["Variation in % points",]),0)
rm(resX)

# scalar X
resZ <- VariationTable(res, Xvar = "log(unemp_rate)")
expect_true("semi elasticity" %in% tolower(rownames(resZ)))
expect_equivalent(sum(resZ["Initial parts",]), 1)
expect_equivalent(sum(resZ["New parts",]), 1)
expect_equivalent(sum(resZ["Variation in % points",]),0)
rm(resZ, res)


# ---- Y scalar ---------------------------------------------------------------
res <- lmCoDa(YIELD ~ PRECIPITATION + ilr(TEMPERATURES), data = head(rice_yields, 20))

# compositional X
resX <- VariationTable(res, Xvar = "TEMPERATURES", Xdir = "HIGH")
expect_true(colnames(resX) == "YIELD")
expect_true("semi elasticity" %in% tolower(rownames(resX)))
expect_true(sum(attr(resX, "X(0)")) == 1)
expect_true(sum(attr(resX, "Xdir")) == 1)
expect_true(resX["Initial value",] == res$fitted.values[[1]])
expect_true(isTRUE(abs(resX["Variation in units",]) > 0))
rm(resX)


# scalar X
expect_error(VariationTable(res, Xvar = "PRECIPITATION"),pattern =  "meaningful")
