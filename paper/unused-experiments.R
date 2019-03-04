library(Cubist)

dat = data.frame(x = fc2$approx_models$x1$.__enclos_env__$private$x)
y = fc2$approx_models$x1$.__enclos_env__$private$ale_values
model_tree <- cubist(x = dat, y = y, control = cubistControl(rules = 2, unbiased = TRUE, extrapolation = 0))
pred = predict(model_tree, dat, extra)
var(pred - y)/var(y)

ddf = data.frame(x = dat$x, y = y, pred = pred)
ggplot(ddf) + geom_line(aes(x = x, y = y)) + geom_line(aes(x = x, y = pred), color = "red") +
  scale_y_continuous(limits = c(-4, 5))


dat = data.frame(x = rnorm(100))
y = dat$x^2
y[2] = -2

model_tree <- cubist(x = dat, y = y, control = cubistControl(rules = 2, unbiased = TRUE, extrapolation = 100))
pred = predict(model_tree, dat)
#var(pred - y)/var(y)

ddf = data.frame(x = dat$x, y = y, pred = pred)
ggplot(ddf) + geom_line(aes(x = x, y = y)) + geom_line(aes(x = x, y = pred), color = "red")



aic = function(tab){
  k = nrow(tab) + 1
  2 * k - 2 * ln(lf)
}

library(Cubist)

dat = data.frame(x = fc2$approx_models$x$.__enclos_env__$private$x)
y = fc2$approx_models$x$.__enclos_env__$private$ale_values
model_tree <- cubist(x = dat, y = y, control = cubistControl(rules = 3))
pred = predict(model_tree, dat)
var(pred - y)/var(y)

ddf = data.frame(x = dat$x, y = y, pred = pred)
ggplot(ddf) + geom_line(aes(x = x, y = y)) + geom_line(aes(x = x, y = pred), color = "red")

# TEst splines
n.segs = 10
x = rnorm(1000)
y = x^2
bbreaks =  seq(from = min(x), to = max(x), length.out = 1 + n.segs)
segs = cut(x, breaks = bbreaks, include.lowest = TRUE)
x.intercept = model.matrix(y ~ segs - 1)
x.intercept = t(apply(x.intercept, 1, function(x) cummax(x)))
x.slope = x.intercept * x
colnames(x.slope) = sprintf("slope:%s", colnames(x.slope))
X = cbind( x.intercept, x.slope)
head(X)


library(glmnet)
glmmod = glmnet(X,y)
plot(glmmod)

lambda = 0.02

pred = predict(glmmod,newx=X,s=c(lambda))[,1]
var(pred - y)/var(y)

coef(glmmod,s=lambda) # extract coefficients at a single value of lambda
plot(x, pred)
