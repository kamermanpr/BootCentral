#' Boot functions for measures of central tendancy
#'
#' A series of internal functions used within \code{\link[boot]{boot}} to
#' calculate measures of central tendancy (mean, median, mode), and proportions.
#'
#' @param d A one-column dataframe.
#'
#' @param i Internal indexing parameter used by \code{\link[boot]{boot}}.
#' \strong{Do not change}.
#'
#'
#' @return Returns and object of class "boot".
#'
#' @name boot_statistics
NULL
#'
#' @rdname boot_statistics
#' @export
boot_mean <- function(d, i){
    # Sample
    df <- d[i, ]
    # Rename columns
    colnames(df) <- c('x')
    # Calculate mean
    mean(df$x, na.rm = TRUE)
}
#'
#' @rdname boot_statistics
#' @export
boot_median <- function(d, i){
    # Sample
    df <- d[i, ]
    # Rename columns
    colnames(df) <- c('x')
    # Calculate median
    median(df$x, na.rm = TRUE)
}
#'
#' @rdname boot_statistics
#' @export
boot_mode <- function(d, i){
    # Sample
    df <- d[i, ]
    # Rename columns
    colnames(df) <- c('x')
    uniqv <- unique(df$x)
    uniqv[which.max(tabulate(match(df$x, uniqv)))]
}
#'
#' @rdname boot_statistics
#' @export
boot_prop <- function(d, i){
    # Sample
    df <- d[i, ]
    # Rename columns
    colnames(df) <- c('x')
    # Calculate proportion
    tab <- table(df$x)
    prop.table(tab)[[2]]
}
