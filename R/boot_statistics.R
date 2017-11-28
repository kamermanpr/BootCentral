#' @title 'boot' statistic functions
#'
#' @description A series of functions for internal use within \code{\link[boot]{boot}} only. The functions calculate measures of central tendency (arithmetic mean, geometric mean, harmonic mean, median, and mode), differences in measures of central tendency (difference in arithmetic means, difference in geometric means, difference in harmonic means, and difference in medians), proportions, and odds ratios.
#'
#' @param d A data frame.
#'
#'    For `boot_mean`, `boot_geometric`, `boot_harmonic`, `boot_median`, and `boot_mode`, the data frame must consist of a single column of numeric data \emph{(dimensions: n x 1)}.
#'
#'    For `boot_prop`, the data frame must consist of a single column of binomial categorical data \emph{(dimensions: n x 1)}.
#'
#'    For `boot_delta_mean`, `boot_delta_geometric`, `boot_delta_harmonic`, and `boot_delta_median`, the data frame must consist of a two columns. The first column must be numeric data, and the second column must be a two-level grouping variable \emph{(dimensions: n x 2)}.
#'
#'    For `boot_OR` the data frame must consist of a two columns. The first column must be binomial categorical data, and the second column must be a two-level grouping variable \emph{(dimensions: n x 2)}.
#'
#' @param i Internal indexing parameter used by \code{\link[boot]{boot}} for resampling. \strong{Do not change}.
#
#' @return Returns and object of class "boot".
#'
#' @describeIn boot_mean Calculates an arithmetic mean.
#' @export
boot_mean <- function(d, i){
    # Sample
    df <- d[i, ]
    # Remove NA
    df <- df[!is.na(df)]
    # Calculate arithmetic mean
    mean(df)
}

#' @describeIn boot_mean Calculates a geometric mean
#' @export
boot_geometric <- function(d, i){
    # Sample
    df <- d[i, ]
    # Remove NA
    df <- df[!is.na(df)]
    # Calculate geometric mean
    n <- length(df)
    prod(df)^(1/n)
}

#' @describeIn boot_mean Calculates a harmonic mean.
#' @export
boot_harmonic <- function(d, i){
    # Sample
    df <- d[i, ]
    # Remove NA
    df <- df[!is.na(df)]
    # Calculate harmonic mean
    1 / mean(1 / df)
}

#' @describeIn boot_mean Calculates a median
#' @export
boot_median <- function(d, i){
    # Sample
    df <- d[i, ]
    # Remove NA
    df <- df[!is.na(df)]
    # Calculate median
    median(df)
}

#' @describeIn boot_mean Calculates a mode
#' @export
boot_mode <- function(d, i){
    # Sample
    df <- d[i, ]
    # Remove NA
    df <- df[!is.na(df)]
    # Calculate mode
    uniqv <- unique(df)
    uniqv[which.max(tabulate(match(df, uniqv)))]
}

#' @describeIn boot_mean Calculates a proportion
#' @export
boot_prop <- function(d, i){
    # Sample
    df <- d[i, ]
    # Remove NA
    df <- df[!is.na(df)]
    # Calculate proportion
    tab <- table(df,
                 useNA = 'no')
    prop.table(tab)[[1]]
}

#' @describeIn boot_mean Calculates the difference between arithmetic means
#' @export
boot_delta_mean <- function(d, i){
    # Sample
    df <- d[i, ]
    # Remove NA
    df <- df[complete.cases(df), ]
    # Get unique values for grouping variable
    uniq <- unique(df[[2]])
    # Calculate delta means
    mean(df[1][df[2] == uniq[[1]]]) - mean(df[1][df[2] == uniq[[2]]])
}

#' @describeIn boot_mean Calculates the difference between geometric means
#' @export
boot_delta_geometric <- function(d, i){
    # Sample
    df <- d[i, ]
    # Remove NA
    df <- df[complete.cases(df), ]
    # Get unique values for grouping variable
    uniq <- unique(df[[2]])
    # Seperate by grouping variable
    df_1 <- df[1][df[2] == uniq[[1]]]
    df_2 <- df[1][df[2] == uniq[[2]]]
    # Calculate delta geometric mean
    n_1 <- length(df_1)
    n_2 <- length(df_2)
    geo_1 <- prod(df_1)^(1/n_1)
    geo_2 <- prod(df_2)^(1/n_2)
    geo_1 - geo_2
}

#' @describeIn boot_mean Calculates the difference between harmonic means
#' @export
boot_delta_harmonic <- function(d, i){
    # Sample
    df <- d[i, ]
    # Remove NA
    df <- df[complete.cases(df), ]
    # Get unique values for grouping variable
    uniq <- unique(df[[2]])
    # Calculate delta means
    (1 / mean(1/ df[1][df[2] == uniq[[1]]])) -
        (1 / mean(1 / df[1][df[2] == uniq[[2]]]))
}

#' @describeIn boot_mean Calculates the difference between medians
#' @export
boot_delta_median <- function(d, i){
    # Sample
    df <- d[i, ]
    # Remove NA
    df <- df[complete.cases(df), ]
    # Get unique values for grouping variable
    uniq <- unique(df[[2]])
    # Calculate delta medians
    median(df[1][df[2] == uniq[[1]]]) - median(df[1][df[2] == uniq[[2]]])
}

#' @describeIn boot_mean Calculates an odds ratio
#' @export
boot_OR <- function(d, i){
    # Sample
    df <- d[i, ]
    # Remove NA
    df <- df[complete.cases(df), ]
    # xtabulate
    x_tab <- table(df)
    # Calculate odds ratio
    x_odds <- fisher.test(x_tab)$estimate
    # Remove name
    names(x_odds) <- NULL
    # Print
    x_odds
}
