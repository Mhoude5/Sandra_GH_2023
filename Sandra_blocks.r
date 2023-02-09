
library(edibble)

#n = number of tmts
#nr = number of rows
#nc = number of columns
#dim = vector of integers to indicate number of elements in each dimension


#Salinity Split Plot Design ----

# Salinity treatment allocation: 12 whole plots in 4 rows and 3 cols with 3 trts
# A 4-row 3-col 3-trt latin_rectangle

set.seed(743)
salinity<- latin_rectangle(nr = 4, nc = 3, nt = 3)
salinity

#Species assignment

set.seed(750)
(t1 <- latin_rectangle(nr = 8, nc = 4, nt = 4))
set.seed(130)
(t2 <- latin_rectangle(nr = 8, nc = 2, nt = 4))
(species <- cbind(t1, t2))
species





