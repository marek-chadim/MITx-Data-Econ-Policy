?lm

#create data
height <- c(168, 175, 155, 160, 170, 180, 165, 158, 172, 169)
bodymass <- c(78, 80, 72, 75, 77, 85, 68, 65, 79, 76)

#fit linear model
fit <- lm(bodymass ~ height)

class(fit)
summary(fit)
names(fit)

plot(height, bodymass)
abline(fit, col = 'red')

##-- Continuing the  lm(.) example:
coef(fit) # the bare coefficients

## The 2 basic regression diagnostic plots [plot.lm(.) is preferred]
plot(resid(fit), fitted(fit)) # Tukey-Anscombe's
qqnorm(residuals(fit))

require(graphics)
plot(fit)

## 4 plots on 1 page;
## allow room for printing model formula in outer margin:
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0)) -> opar
plot(fit)
plot(fit, id.n = NULL)                 # no id's
plot(fit, id.n = 5, labels.id = NULL)  # 5 id numbers

## Was default in R <= 2.1.x:
## Cook's distances instead of Residual-Leverage plot
plot(fit, which = 1:4)

## All the above fit a smooth curve where applicable
## by default unless "add.smooth" is changed.
## Give a smoother curve by increasing the lowess span :
plot(fit, panel = function(x, y) panel.smooth(x, y, span = 1))

par(mfrow = c(2,1)) # same oma as above
plot(fit, which = 1:2)

## Cook's distance tweaking
par(mfrow = c(2,3)) # same oma ...
plot(fit, which = 1:6, cook.col = "royalblue")

## A case where over plotting of the "legend" is to be avoided:
if(dev.interactive(TRUE)) getOption("device")(height = 6, width = 4)
par(mfrow = c(3,1), mar = c(5,5,4,2)/2 +.1, mgp = c(1.4, .5, 0))
plot(fit, which = 5, extend.ylim.f = c(0.2, 0.08))
plot(fit, which = 5, cook.lty = "dotdash",
     cook.legendChanges = list(x = "bottomright", legend = "Cook"))
plot(fit, which = 5, cook.legendChanges = NULL)  # no "legend"


par(opar) # reset par()s
