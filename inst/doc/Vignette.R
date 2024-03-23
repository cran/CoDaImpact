## ----setup0, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, error = TRUE)
oopt <- options(scipen = 999)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
# remotes::install_github("LukeCe/CoDaImpact")
library(CoDaImpact)
library(compositions)

## -----------------------------------------------------------------------------
summary(rice_yields)

## -----------------------------------------------------------------------------
whitered_pal <- colorRampPalette(rev(c("beige","orange","red","darkred")))

top100 <- head(rice_yields, 250)
whitered_col <- whitered_pal(length(top_100$YIELD))
yield_top100 <- rank(top_100$YIELD,ties.method = "first")
ylcol_top100 <- whitered_col[yield_top100]


layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
plot(acomp(top_100$TEMPERATURES),
     col = ylcol_top100,
     pch = 16)


legend_image <- as.raster(matrix(whitered_pal(20), ncol=1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Rice yield')
yield_range <- range(top_100$YIELD)
yield_range <- seq(min(yield_range), max(yield_range), l = 5)
text(x=1.5, y = seq(0,1,l=5), labels = (round(yield_range,1)))
rasterImage(legend_image, 0, 0, 1,1)

## -----------------------------------------------------------------------------
sapply(rice_yields, class)

## -----------------------------------------------------------------------------
rice_yields$TEMPERATURES[1,] #LOW, MEDIUM, and HIGH, are grouped in TEMPERATURES

## -----------------------------------------------------------------------------
fit_X_compo <- lm(YIELD ~ PRECIPITATION + alr(TEMPERATURES),data = rice_yields)
class(fit_X_compo)

## -----------------------------------------------------------------------------
fit_X_compo <- ToSimplex(fit_X_compo)
class(fit_X_compo)

## -----------------------------------------------------------------------------
fit_X_compo <- lmCoDa(YIELD ~ PRECIPITATION + alr(TEMPERATURES),data = rice_yields)
class(fit_X_compo)

## -----------------------------------------------------------------------------
coef(fit_X_compo,space = "simplex")
coef(fit_X_compo,space = "clr")
coef(fit_X_compo) # by default use the log-ratio of the estimation, here alr

## -----------------------------------------------------------------------------
head(fitted(fit_X_compo))

## -----------------------------------------------------------------------------
head(resid(fit_X_compo))

## -----------------------------------------------------------------------------
V_TEMP   <- ilrBase(D = 3)
V_TEMP

## -----------------------------------------------------------------------------
clr_TEMP <- coef(fit_X_compo, space = "clr", split = TRUE)[["alr(TEMPERATURES)"]]
ilr_TEMP <- t(V_TEMP) %*% clr_TEMP
ilr_TEMP

## -----------------------------------------------------------------------------
clr_TEMP

## -----------------------------------------------------------------------------
VariationScenario(
  fit_X_compo,
  Xvar = "TEMPERATURES",
  Xdir = c(0.2, 0.5, 0.3),
  inc_size = 2,
  n_steps = 5,
  add_opposite = TRUE,
  obs=43)

## -----------------------------------------------------------------------------
VS2 <- VariationScenario(
  fit_X_compo,
  Xvar = "TEMPERATURES",
  Xdir = c(0.2, 0.5, 0.3),
  inc_size = .1,
  n_steps = 40,
  add_opposite = TRUE,
  obs=43)

yield_color <- whitered_pal(length(VS2$YIELD))
yield_color <- yield_color[rank(VS2$YIELD,ties.method = "first")]
plot(acomp(VS2$X), col = yield_color, pch = 16)
plot(acomp(VS2["0", "X"]), add = TRUE, pch = 16)


## -----------------------------------------------------------------------------
RY_Impacts <- Impacts(fit_X_compo, Xvar = "TEMPERATURES")
RY_Impacts # here, the semi-elasticity is computed for the variable TEMPERATURES 

## -----------------------------------------------------------------------------
VariationTable(
  fit_X_compo,
  obs      =  1,             # indicator of the observation (1 is default)
  Xvar     = "TEMPERATURES", # covariate for which the impact is computed
  Xdir     = 'HIGH',         # vertex in the covariate simplex 
  inc_rate = 0.05)           # signed intensity 

## -----------------------------------------------------------------------------
fit_X_compo.varTab <- VariationTable(
  fit_X_compo,
  obs      = 10,               # indicator of the observation
  Xvar     = "TEMPERATURES",   # covariate for which the impact is computed
  Xdir     = c(0.2,0.55,0.25), # general direction in the covariate simplex
  inc_size = 0.05)             # signed intensity

fit_X_compo.varTab

## -----------------------------------------------------------------------------
attributes(fit_X_compo.varTab)

## -----------------------------------------------------------------------------
summary(car_market)

## -----------------------------------------------------------------------------
opar <- par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(x = car_market$DATE, y = car_market$SEG_A,type = "l", col = "red",
     main = "French vehicles market shares from 2003 to 2015", 
     xlab = "DATE", ylab = "VALUE", ylim = c(0,0.5))
lines(x = car_market$DATE, y = car_market$SEG_B,type = "l", col = "blue" )
lines(x = car_market$DATE, y = car_market$SEG_C,type = "l", col = "green")
lines(x = car_market$DATE, y = car_market$SEG_D,type = "l", col = "orange")
lines(x = car_market$DATE, y = car_market$SEG_E,type = "l", col = "black")
legend("topright",
       legend = paste0("Segment ", LETTERS[1:5]),
       col = c("red", "blue", "green", "orange", "black"),
       lty = 1,
       inset=c(-0.35,0))
par(opar)

## -----------------------------------------------------------------------------
car_market$SEG <- as.matrix(car_market[,c("SEG_A","SEG_B","SEG_C","SEG_D","SEG_E")])

## -----------------------------------------------------------------------------
fit_Y_compo <- lmCoDa(
  ilr(SEG) ~ HOUSEHOLD_EXPENDITURE + GDP + GAS_PRICE + SCRAPPING_SUBSIDY,
  data=car_market)

## -----------------------------------------------------------------------------
coef(fit_Y_compo, space = "simplex")  # coefficients in the simplex
coef(fit_Y_compo)                     # ... in ilr space

## -----------------------------------------------------------------------------
head(fitted(fit_Y_compo, space = "simplex"))  # coefficients in the simplex
head(fitted(fit_Y_compo, space = "clr"))      # ... in clr space
head(fitted(fit_Y_compo))                     # ... in ilr space (as in the estimation)

## -----------------------------------------------------------------------------
head(resid(fit_Y_compo, space = "simplex"))  # coefficients in the simplex
head(resid(fit_Y_compo, space = "clr"))      # ... in clr space
head(resid(fit_Y_compo))                     # ... in ilr space (as in the estimation)

## -----------------------------------------------------------------------------
coef(fit_Y_compo)

## -----------------------------------------------------------------------------
ilrBase(D = 5) # default contrast matrix

## -----------------------------------------------------------------------------
vs_exp2 <- VariationScenario(
  fit_Y_compo,
  Xvar = "HOUSEHOLD_EXPENDITURE",
  obs = 1,
  inc_size = 100,
  n_steps = 150,
  add_opposite = TRUE)

## -----------------------------------------------------------------------------
plot(x = vs_exp2$HOUSEHOLD_EXPENDITURE, y = vs_exp2$Y[,1],type = "l", col = "red",
     main = "Variation scenario of houshold expenditure for observation 1",
     xlab = "Household expenditure", ylab = "Market share of segment")
lines(x = vs_exp2$HOUSEHOLD_EXPENDITURE, y = vs_exp2$Y[,2],type = "l", col = "blue" )
lines(x = vs_exp2$HOUSEHOLD_EXPENDITURE, y = vs_exp2$Y[,3],type = "l", col = "green")
lines(x = vs_exp2$HOUSEHOLD_EXPENDITURE, y = vs_exp2$Y[,4],type = "l", col = "orange")
lines(x = vs_exp2$HOUSEHOLD_EXPENDITURE, y = vs_exp2$Y[,5],type = "l", col = "black")
legend("topleft",
       legend = paste0("SEG_", LETTERS[1:5]),
       col = c("red", "blue", "green", "orange", "black"),
       lty = 1)

## -----------------------------------------------------------------------------
Impacts(fit_Y_compo,
        Xvar = "HOUSEHOLD_EXPENDITURE",
        obs=4)

## -----------------------------------------------------------------------------
fit_Y_compo.VarTab <- VariationTable(
  fit_Y_compo,                      # model output
  Xvar = "HOUSEHOLD_EXPENDITURE",   # variable to be changed
  inc_size = 2500,                  # additive increment of X
  obs = 1)                          # observation index

fit_Y_compo.VarTab

## -----------------------------------------------------------------------------
barplot(as.matrix(fit_Y_compo.VarTab[5,]),col = "cyan")
title("Market segment shares variations")

## -----------------------------------------------------------------------------
data("election")
summary(election[1:3])
summary(election[4:6])
summary(election[7:9])
summary(election[10:13])

## -----------------------------------------------------------------------------
election$VOTE <- as.matrix(election[,c("left","right","extreme_right")])
election$AGE  <- as.matrix(election[,c("Age_1839","Age_4064","Age_65plus")])
election$EDUC <- as.matrix(election[,c("Educ_BeforeHighschool","Educ_Highschool","Educ_Higher")])

## -----------------------------------------------------------------------------
fit_YX_compo <- lmCoDa(
  ilr(VOTE) ~
  alr(AGE) + unemp_rate + asset_owner_rate +
  ilr(EDUC) + income_taxpayer_rate + forgeigner_rate,
  data = election)

## -----------------------------------------------------------------------------
coef(fit_YX_compo)
coef(fit_YX_compo, space = "simplex")

## -----------------------------------------------------------------------------
confint(fit_YX_compo, parm = "AGE")

## -----------------------------------------------------------------------------
confint(fit_YX_compo, parm = "AGE", y_ref = "left")

## -----------------------------------------------------------------------------
confint(fit_YX_compo, parm = "unemp_rate")

## -----------------------------------------------------------------------------
confint(fit_YX_compo, parm = "unemp_rate", y_ref = "left")

## -----------------------------------------------------------------------------
ilrBase(D = 3)

## -----------------------------------------------------------------------------
coef(fit_YX_compo, split = TRUE)[["ilr(EDUC)"]]

## ----fig.width=10, fig.height=10----------------------------------------------
VS_election<-VariationScenario(
  fit_YX_compo,
  Xvar = "AGE",
  Xdir = "Age_1839",
  n_steps = 100,
  obs = 1)


opar <- par(mfrow = c(2,1), mar = c(5,4,1,2))
plot(x = VS_election$X[,1],  y = VS_election$Y[,1], col = "Orange",
     xlim = c(0,1), ylim = c(0,1), xlab = "% Age_1839", ylab = "% VOTE")
lines(x = VS_election$X[,1],  y = VS_election$Y[,1], lwd = 1.5, col = "orange")
lines(x = VS_election$X[,1],  y = VS_election$Y[,2], lwd = 1.5, col = "darkblue")
lines(x = VS_election$X[,1],  y = VS_election$Y[,3], lwd = 1.5, col = "red")
legend("top",
       legend = c("left", "right", "extreme_right"),
       col = c("orange", "darkblue", "red"),
       lty = 1)


plot(x = VS_election$X[,1],  y = VS_election$X[,1], col = "Orange",
     xlim = c(0,1), ylim = c(0,1), xlab = "% Age_1839", ylab = "% AGE")
lines(x = VS_election$X[,1],  y = VS_election$X[,1], lwd = 1.5, col = "orange")
lines(x = VS_election$X[,1],  y = VS_election$X[,2], lwd = 1.5, col = "darkblue")
lines(x = VS_election$X[,1],  y = VS_election$X[,3], lwd = 1.5, col = "red")
legend("top",
       legend = c("18-39","40-64", "65_plus"),
       col = c("orange", "darkblue", "red"),
       lty = 1)
par(opar)

## -----------------------------------------------------------------------------
Impacts(fit_YX_compo, Xvar = 'AGE', obs = 3)

## -----------------------------------------------------------------------------
VariationTable(
  fit_YX_compo,
  Xvar = "AGE",
  Xdir = 'Age_1839',
  Ytotal = 100000,
  inc_rate = 0.05)

## -----------------------------------------------------------------------------
VT <- VariationTable(
  fit_YX_compo,
  Xvar = "AGE",
  Xdir = c(0.45,0.2,0.35) ,
  inc_size=0.1,
  Ytotal = 100000)
VT

## -----------------------------------------------------------------------------
attributes(VT)

## ----include=FALSE------------------------------------------------------------
options(oopt)

