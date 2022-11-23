# Update to version 0.3.1

* fixed a bug in function history_martrix_cn8() that crashed this function and harmonize_cn8()

# Update to version 0.3.0

* harmonize_cn8() and harmonize_pc8() have a new argument "harmonize.to", which allows the user to define if harmonization is done towards the first or last year of the period of interest.

* utilize_cn8() and utilize_pc8() have a new argument "base", which allows the user to define which plus-codes (CN8plus or HS6plus) are used as a base for calculating added/dropped/same products and their corresponding values.

* Huge data update: PC8 data is now available from 2001 to 2021; CN8 data is now available from 1995 to 2022; BEC Rev. 5 is now available

* more background information about idea of the harmonization was added to the vignette

# Update to version 0.2.1

* updated citation of the package

# Update to version 0.2.0

* renamed all functions; "." replaced by "_"

* minor bug fixes in harmonize_pc8(), affecting HS6plus codes

* default value of utilize_cn8() and utilize_pc8() changed

* new argument "value" of utilize_cn8() and utilize_pc8() for including various quantities, user can obtain values of same/added/dropped products

# Update to version 0.1.0

* first release of the harmonizer package on CRAN
