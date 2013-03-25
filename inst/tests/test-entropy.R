#Testing entropy function
expect_that(entropy(c(1,1,2,2)),equals(1))
# NA omited from results
expect_that(entropy(c(1,1,2,2,NA)),equals(1))
expect_that(entropy(c(1,1,1,1)),equals(0))
#
expect_that(entropy(c(1,1,1,1,NA)),equals(0))


