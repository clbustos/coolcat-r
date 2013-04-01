# Expected entropy
d.f=data.frame(c=factor(c("r","r","b")),w=factor(c("h","m","l")))
expect_that(expectedEntropy(d.f,factor(c(1,1,2))),equals(2/3))
expect_that(expectedEntropy(d.f,factor(c(1,2,1))),equals(4/3))
expect_that(expectedEntropy(d.f,factor(c(1,2,2))),equals(4/3))
d.f.2=data.frame(a=factor(runif(100)>0.5),b=factor(runif(100)>0.9))
g=factor(runif(100)>0.2)
expect_that(multivariateEntropy(d.f.2),equals(multivariateEntropy.r(d.f.2)))
expect_that(expectedEntropy(d.f.2,g),equals(expectedEntropy.r(d.f.2,g)))
