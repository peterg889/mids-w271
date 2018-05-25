challenger = read.csv("/Users/petergrabowski/Desktop/w271/challenger.csv", header=TRUE)


####### Q4 ##########
challenger$ratio = challenger$O.ring / challenger$Number
plot(x=challenger$ratio, y=challenger$Temp)
plot(x=challenger$ratio, y=challenger$Pressure)

head(challenger)

####### 4B ##########
mod.fit = glm(formula = ratio ~ Temp + Pressure, family=binomial(link=logit), data=challenger)
# Warning message:
# In eval(family$initialize) : non-integer #successes in a binomial glm!

mod.fit
summary(mod.fit)


####### 4C ##########
library(package = car)

Anova(mod.fit, test = "LR")

anova(mod.fit, test = "Chisq")

####### Q5 ##########
####### 5A ##########
mod.fit2 = glm(formula = ratio ~ Temp , family=binomial(link=logit), data=challenger)
summary(mod.fit2)


####### 5B ##########
b0 = mod.fit2$coefficients[["(Intercept)"]]
b1 = mod.fit2$coefficients[["Temp"]]

eq1 = function(x){exp(b0 + b1*x) / (1+ exp( b0 + b1*x))}
curve(eq1, from=31, to=81, col='blue')

# TODO -- plot expected number of failures

####### 5C ##########
# page 89
predict.data = data.frame(Temp = seq(from = 31, to = 81))
# linear.pred = predict(object = mod.fit2, newdata = predict.data, type = "link" , se = TRUE)
# linear.pred

alpha = 0.05
ci.pi = function(newdata, mod.fit.obj, alpha) {
  linear.pred = predict(object = mod.fit.obj, newdata = newdata, type = "link", se= TRUE)
  CI.lin.pred.lower = linear.pred$fit - qnorm(p = 1-alpha/2) * linear.pred$se
  CI.lin.pred.upper = linear.pred$fit + qnorm(p = 1-alpha/2) * linear.pred$se
  CI.pi.lower = exp(CI.lin.pred.lower) / (1+ exp(CI.lin.pred.lower))
  CI.pi.upper = exp(CI.lin.pred.upper) / (1+ exp(CI.lin.pred.upper))
  list(lower = CI.pi.lower, upper = CI.pi.upper)
}

curve(expr=ci.pi(newdata = data.frame(Temp = x), mod.fit.obj = mod.fit2, alpha=0.05)$lower, lty = "dotdash", add = TRUE, xlim=c(31,81))
curve(expr=ci.pi(newdata = data.frame(Temp = x), mod.fit.obj = mod.fit2, alpha=0.05)$upper, lty = "dotdash", add = TRUE, xlim=c(31,81))
curve(expr=ci.pi(newdata = data.frame(Temp = x), mod.fit.obj = mod.fit2, alpha=0.05)$upper, lty = "dotdash", add = TRUE, xlim=c(31,81))
legend(locator(1), legend = c("Logistic regression model", "95% individual C.I."), lty=c("solid", "dotdash"), col = c("blue", "blue"), bty = "n")

####### 5C ##########

