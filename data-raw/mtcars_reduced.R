############################################################
#                                                          #
#   Simplified version of mtcars for tests and examples    #
#                                                          #
############################################################

# Simplify 'mtcars' to columns: cyl and disp
mtcars_reduced <- mtcars[ , c('cyl', 'disp')]

# Convert cyl and disp to two-level categorical variables
mtcars_reduced$cyl[mtcars_reduced$cyl == 4] <- 'v4'
mtcars_reduced$cyl[mtcars_reduced$cyl != 'v4'] <- 'v6+'
mtcars_reduced$disp[mtcars_reduced$disp < 100] <- '<100'
mtcars_reduced$disp[mtcars_reduced$disp != '<100'] <- '>100'

# Save to data
devtools::use_data(mtcars_reduced,
                   overwrite = TRUE)
