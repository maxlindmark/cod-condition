# https://www.statology.org/percentile-vs-quartile-vs-quantile/
x <- c(1,6,4,2,12,15,8,7)
w <- c(1,1,1,3,3, 3, 3,3)

mean(x)
median(x)

x2 <- c(1,6,4,2,2,2,12,12,12,15,15,15,8,8,8,7,7,7)

mean(x2)
weighted.mean(x, w)
# Same

# What if we have a continuous weight? How do we repeat
w2 <- c(1,1,1,1.5,1.5,1.5,1.5,1.5)
x3 <- c(1,6,4,2,  12, 15, 8,  7)

n <- sum(w2)

(sum(x3*w2))/n

weighted.mean(x, w2)

# Looks correct...
#install.packages("hutils")
library(hutils)
weighted_quantile(x)
median(x)

# Same.. now apply weights!
median(x2)
weighted_quantile(v=x, w=w, p=0.5)

# Not the same... maybe because it's even numbers?
#install.packages("MetricsWeighted")
library(MetricsWeighted)
MetricsWeighted::weighted_quantile(x=x, w=w, probs=c(0.25, 0.5, 0.75))
hutils::weighted_quantile(v=x, w=w, p=c(0.25, 0.5, 0.75))

# Now test with odd number..
x4 <- c(1,6,4,2,12,15,8)
w4 <- c(1,1,1,3,3, 3, 3)

x5 <- c(1,6,4,2,2,2,12,12,12,15,15,15,8,8,8)

median(x5)
MetricsWeighted::weighted_quantile(x=x4, w=w4, probs=c(0.25, 0.5, 0.75))
hutils::weighted_quantile(v=x4, w=w4, p=c(0.25, 0.5, 0.75))



