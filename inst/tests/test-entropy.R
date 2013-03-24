expect_that(entropy(c(1,1,2,2)),equals(1))
# NA omited from results
expect_that(entropy(c(1,1,2,2,NA)),equals(1))
expect_that(entropy(c(1,1,1,1)),equals(0))
#
expect_that(entropy(c(1,1,1,1,NA)),equals(0))

d.f=data.frame(c=factor(c("r","r","b")),w=factor(c("h","m","l")))
# Multivariate entropy
expect_that(multivariateEntropy(d.f[c(1,2),]),equals(1))
expect_that(multivariateEntropy(d.f[c(1,3),]),equals(2))
expect_that(multivariateEntropy(d.f[2:3,]),equals(2))
# Expected entropy
expect_that(expectedEntropy(d.f,c(1,1,2)),equals(2/3))
expect_that(expectedEntropy(d.f,c(1,2,1)),equals(4/3))
expect_that(expectedEntropy(d.f,c(1,2,2)),equals(4/3))
d.f.2=data.frame(a=factor(runif(100)>0.5),b=factor(runif(100)>0.9))
expect_that(multivariateEntropy(d.f.2),equals(multivariateEntropy.r(d.f.2)))
