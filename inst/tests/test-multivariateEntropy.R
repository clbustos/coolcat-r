# Multivariate entropy
d.f=data.frame(c=factor(c("r","r","b")),w=factor(c("h","m","l")))
expect_that(multivariateEntropy(d.f),equals(entropy(d.f$c)+entropy(d.f$w)))

expect_that(multivariateEntropy(d.f[c(1,2),]),equals(1))
expect_that(multivariateEntropy(d.f[c(1,3),]),equals(2))
expect_that(multivariateEntropy(d.f[2:3,]),equals(2))

d.f.2=data.frame(c=factor(c(NA,"r","r","b")),w=factor(c("h","m","l",NA)))
expect_that(multivariateEntropy(d.f.2),equals(entropy(d.f$c)+entropy(d.f$w)))
