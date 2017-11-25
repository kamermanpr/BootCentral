############################################################
#                                                          #
#    Simplified version of iris for tests and examples     #
#                                                          #
############################################################

# Simplify 'iris' to two species
iris_reduced <- iris[iris$Species %in% c('setosa', 'versicolor'), ]

# Convert factors to character
iris_reduced$Species <- as.character(iris_reduced$Species)

# Save to data
devtools::use_data(iris_reduced,
                   overwrite = TRUE)
