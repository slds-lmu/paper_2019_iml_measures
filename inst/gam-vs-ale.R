devtools::load_all()
library("mgcv")


# Construct example with correlated features?
n = 100

x1 = rnorm(n)
x2 = rnorm(n)

X = data.frame(x1, x2)
cor(x1, x2)
f = function(model = NULL, newdata) {
  1 *  (2 * newdata[,1] > 0)
}

y = f(newdata = X)

dat = cbind(y, X)
mod.gam = gam(y ~ s(x1) + s(x2), data = dat)

mod.gam

plot(mod.gam)

summary(mod.gam)


pred = Predictor$new(predict.fun = f, data = X)

fes = FeatureEffects$new(pred)
plot(fes)
