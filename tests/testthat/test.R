library(easyboot)

test_that("Assess whether 'boot' statistic functions work as expected", {
    # Set seed
    set.seed(1234)

    # Generate data
    univariate_continuous <- data.frame(data = iris[1:50, 'Petal.Width'])
    bivariate_categorical <- data.frame(data = iris[1:80, 'Species'])
    bivariate_categorical$data <- as.character(bivariate_categorical$data)

    # Test boot_mean
    expect_equal(object = boot::boot(data = univariate_continuous,
                                     statistic = boot_mean,
                                     R = 1000,
                                     stype = 'i')$t0,
                 expected = mean(univariate_continuous$data),
                 tolerance = 0.001)

    # Test boot_geometric
    expect_equal(object = boot::boot(data = univariate_continuous,
                                     statistic = boot_geometric,
                                     R = 1000,
                                     stype = 'i')$t0,
                 expected = prod(univariate_continuous$data)^(1/length(univariate_continuous$data)),
                 tolerance = 0.001)

    # Test boot_median
    expect_equal(object = boot::boot(data = univariate_continuous,
                                     statistic = boot_median,
                                     R = 1000,
                                     stype = 'i')$t0,
                 expected = median(univariate_continuous$data),
                 tolerance = 0.001)

    # Test boot_mode
    expect_equal(object = boot::boot(data = univariate_continuous,
                                     statistic = boot_mode,
                                     R = 1000,
                                     stype = 'i')$t0,
                 expected = unique(univariate_continuous$data)[which.max(tabulate(match(univariate_continuous$data, unique(univariate_continuous$data))))],
                 tolerance = 0.001)

    # Test boot_prop
    expect_equal(object = boot::boot(data = bivariate_categorical,
                                     statistic = boot_prop,
                                     R = 1000,
                                     stype = 'i')$t0,
                 expected = length(bivariate_categorical[bivariate_categorical$data == 'versicolor', ])/length(bivariate_categorical$data),
                 tolerance = 0.001)
    })
