
<!-- README.md is generated from README.Rmd. Please edit that file -->

# harmonizer

<!-- badges: start -->
<!-- badges: end -->

Innovation is a major engine of economic growth. To compare products
over time, harmonization of product codes is mandatory. This package
provides an *easy-to-use* approach to harmonize product codes. Moreover,
it offers an application that allows finding all new and dropped
products for given firm-level data based on harmonized product codes.
Details will be provided in the upcoming paper (REFERENCE).

## Installation

You can install the released version of harmonizer from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("harmonizer")
```

## Examples

These examples show how to use the functions of the package. It is
recommended to use the default settings of the main functions. However,
it is possible to use modified versions of the functions. For more
information, check `vignette("harmonizer")` and also the according help
files.

``` r
library("harmonizer")

### default use of the main functions
my_harmonization <- harmonize_cn8(b = 2008, e = 2012)
my_harmonization <- harmonize_pc8(b = 2008, e = 2011)

### modified versions of the functions

### example for CN8 product code harmonization
hist_cn8 <- history_matrix_cn8(b = 2008, e = 2012)
bec_cn8 <- cn8_to_bec(b = 2008, e = 2012, historymatrix = hist_cn8)
my_harmonization <- harmonize_cn8(b = 2008, e = 2012, progress = TRUE, historymatrix = hist_cn8)
# however, the argument "historymatrix" is not mandatory and is calculated by default

### example for PC8 product code harmonization
hist_pc8 <- history_matrix_pc8(b = 2008, e = 2011)
bec_pc8 <- pc8_to_bec(b = 2008, e = 2011, historymatrix = hist_pc8)
my_harmonization <- harmonize_pc8(b = 2008, e = 2011, historymatrix = hist_pc8)
# however, the argument "historymatrix" is not mandatory and is calculated by default

### additional functions
get_data_directory()

get_data_directory(path = FALSE, open_explorer = TRUE, show_data = "CN8")

### harmonized analyses

# CN8 product codes
sampledata <- read_table("my_firmdata.txt", sep = ";", header = TRUE, colClasses = "character")
my_harmonization <- harmonize_cn8(b = 2008, e = 2014)
my_product_changes <- utilize_cn8(firm_data = sampledata, harmonized_data = my_harmonization, 
                                  b = 2008, e = 2014, output = "all")

changes <- my_product_changes[[1]]
merged_data <- my_product_changes[[2]]

my_product_changes <- utilize_cn8(firm_data = sampledata, harmonized_data = my_harmonization, 
                                  b = 2008, e = 2014, output = "product.changes", value = TRUE)
# however, the argument "harmonized_data" is not mandatory and is calculated by default

# PC8 product codes
sampledata <- read.table("my_firmdata.csv", sep = ";", header = TRUE, colClasses = "character")
my_harmonization <- harmonize_pc8(b = 2009, e = 2014)
my_product_changes <- utilize_pc8(firm_data = sampledata, harmonized_data = my_harmonization,
                                  b = 2009, e = 2014, output = "all")

changes <- my_product_changes[[1]]
merged_data <- my_product_changes[[2]]

my_product_changes <- utilize_pc8(firm_data = sampledata, harmonized_data = my_harmonization,
                                  b = 2009, e = 2014, output = "product.changes", value = TRUE)
# however, the argument "harmonized_data" is not mandatory and is calculated by default


### help files
?history_matrix_cn8
?cn8_to_bec
?harmonize_cn8

?history_matrix_pc8
?pc8_to_bec
?harmonize_pc8

?get_data_directory

?utilize_cn8
?utilize_pc8

### others
citation("harmonizer")
vignette("harmonizer")
news(package = "harmonizer")
```
