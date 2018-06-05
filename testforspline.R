library(stats)
require(rms)

x <- 1:11
y <- c(0.2,0.40, 0.6, 0.75, 0.88, 0.99, 1.1, 1.15, 1.16, 1.16, 1.16 )
plot(x, y)
spline <- smooth.spline(x,y)
lines(spline)

require(rms)
x <- 1:11
y <- c(0.2,0.40, 0.6, 0.75, 0.88, 0.99, 1.1, 1.15, 1.16, 1.16, 1.16 )
dd <- datadist(x); options(datadist='dd')

f <- ols(y ~ rcs(x, c(3, 5, 7, 9)))

anova(f)

ggplot(Predict(f)) + geom_point(aes(x=x, y=y), data=data.frame(x,y))
Function(f)   ## if have latex installed can also use latex(f)
