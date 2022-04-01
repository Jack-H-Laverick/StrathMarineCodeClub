
#--# Title: Building an R package, 01/04/2022

library(devtools)

create("./sudokusolver", open = TRUE) # Get the package started

# Update DESCRIPTION file

# Move across your functions into the R directory

# Document the functions using roxygen2, including adding an export tag

document()  # Build help pages

install()   # Install our package ready for use 