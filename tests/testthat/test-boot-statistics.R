library(BootCentral)

test_that("Assess whether 'boot' statistic functions work as expected", {

    # Generate univariate data (including an NA value)
    foo <- data.frame(data = c(NA, iris[1:50, 'Petal.Width']))
    bar <- data.frame(data = iris[1:80, 'Species'])
    bar$data <- as.character(bar$data)
    bar$data[[1]] <- NA
    # Generate bivariate data (including an NA value)
    baz <- iris[1:100, c('Petal.Width', 'Species')]
    baz$Species <- as.character(baz$Species)
    baz$Petal.Width[[1]] <- NA
    qux <- baz
    qux$Petal.Width[qux$Petal.Width < 1] <- 'small'
    qux$Petal.Width[grepl('1.?.?', qux$Petal.Width)] <- 'large'

    # Test boot_mean
    expect_equal(object = boot::boot(data = foo,
                                     statistic = boot_mean,
                                     R = 1000,
                                     stype = 'i')$t0,
                 expected = 0.246,
                 tolerance = 0.001)

    # Test boot_geometric
    expect_equal(object = boot::boot(data = foo,
                                     statistic = boot_geometric,
                                     R = 1000,
                                     stype = 'i')$t0,
                 expected = 0.2265819,
                 tolerance = 0.001)


    # Test boot_harmonic
    expect_equal(object = boot::boot(data = foo,
                                     statistic = boot_harmonic,
                                     R = 1000,
                                     stype = 'i')$t0,
                 expected = 0.2087683,
                 tolerance = 0.001)

    # Test boot_median
    expect_equal(object = boot::boot(data = foo,
                                     statistic = boot_median,
                                     R = 1000,
                                     stype = 'i')$t0,
                 expected = median(foo$data,
                                   na.rm = TRUE),
                 tolerance = 0.001)

    # Test boot_mode
    expect_equal(object = boot::boot(data = foo,
                                     statistic = boot_mode,
                                     R = 1000,
                                     stype = 'i')$t0,
                 expected = 0.2,
                 tolerance = 0.001)

    # Test boot_prop
    expect_equal(object = boot::boot(data = bar,
                                     statistic = boot_prop,
                                     R = 1000,
                                     stype = 'i')$t0,
                 expected = 0.6202532,
                 tolerance = 0.001)

    # Test difference between arithmetic means (boot_delta_mean)
    expect_equal(object = boot::boot(data = baz,
                                     statistic = boot_delta_mean,
                                     R = 1000,
                                     stype = 'i')$t0,
                 expected = -1.079061,
                 tolerance = 0.001)

    # Test the difference between geometric means (boot_delta_geometric)
    expect_equal(object = boot::boot(data = baz,
                                     statistic = boot_delta_geometric,
                                     R = 1000,
                                     stype = 'i')$t0,
                 expected = -1.084028,
                 tolerance = 0.001)

    # Test difference between harmonic means (boot_delta_harmonic)
    expect_equal(object = boot::boot(data = baz,
                                     statistic = boot_delta_harmonic,
                                     R = 1000,
                                     stype = 'i')$t0,
                 expected = -1.087073,
                 tolerance = 0.001)

    # Test the difference between medians (boot_delta_median)
    expect_equal(object = boot::boot(data = baz,
                                     statistic = boot_delta_median,
                                     R = 1000,
                                     stype = 'i')$t0,
                 expected = -1.1,
                 tolerance = 0.001)

    # Test the odds ratio (boot_OR)
    expect_equal(object = boot::boot(data = qux,
                                     statistic = boot_OR,
                                     R = 1000,
                                     stype = 'i')$t0,
                 expected = 0,
                 tolerance = 0.001)
    })
