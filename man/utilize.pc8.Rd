\name{utilize.pc8}
\alias{utilize.pc8}
\title{A possible utilization of harmonized PC8 products codes}
\usage{
utilize.pc8(b, e, firm.data, harmonized.data = NULL,
                progress = TRUE, output = "product.changes")
}
\description{
Provide an application of the data frames obtained by the main function, \code{harmonize.pc8}. To use these additional functions, data on firm-level is required, which is data that is not provided by the package.
}
\arguments{
\item{b}{first year of interest}
\item{e}{last year of interest}
\item{firm.data}{Data on firm level which must provide the following columns: "firmID", "year" and "PC8".}
\item{harmonized.data}{Harmonized data of PC8 product codes. Provided by harmonize.pc8(). By default \emph{NULL}; the function computes the needed harmonized data.}
\item{progress}{logical, determines whether progress is printed in console or not.}
\item{output}{Defines which dataframe is returend. It may take the following values:
      \itemize{
          \item{"product.changes", returns all changed PC8 product codes per firm per year (see description of (a) above)}
          \item{"merged.firm.data", returns entered firm data, extended by harmonized data (see description of (b) above)}
          \item{"all", returns both dataframes as a list}
      }
  }
}

\value{
Provides two possible data frames: \describe{
  \item{(a)}{ One dataframe that contains all changed PC8 product codes per firm per year. In more detail, this means how many products remained the same, were added, were dropped, how many products were produced by a certain firm in a given year, and how many products were produced in the year after.}
  \item{(b)}{ One dataframe that is based on the entered firm data. The entered firm data is extended by harmonized data (that is "PC8plus", "flag", "flagyear", "HS6plus", "BEC", "BEC_agr", "BEC_basic_class").}
  }

Table that summarizes the output, described by the notation (a) above:

\tabular{ll}{
Variable \tab	Explanation \cr
\code{firmID}	\tab character;	specific code that describes a firm over the years (this code does not change over time) \cr
\code{period_UL}	\tab character;	lower limit of the time period \cr
\code{period}	\tab character;	time period in which the product was produced \cr
\code{gap}	\tab numeric;	indicating if the time period is greater than one (i.e. upper limit - lower limit > 1) \cr
\code{same_products} numeric;	\tab	number of products that were produced in both years (i.e. remained in the product portfolio of this firm) \cr
\code{new_products}	\tab numeric;	number of added products in the upper limit of the time period (i.e. added to the product portfolio of this firm) \cr
\code{dropped_products}	\tab numeric;	number of dropped products in the upper limit of the time period (i.e. removed of the product portfolio of this firm) \cr
\code{nbr_of_products_period_LL}	\tab numeric;	number of all products produced in the lower limit of the time period (i.e. entire product portfolio of this firm) \cr
\code{nbr_of_products_period_UL}	\tab numeric;	number of all products produced in the upper limit of the time period (i.e. entire product portfolio of this firm)
}

Table that summarizes the output, described by the notation (b) above:

\tabular{ll}{
Variable	\tab	Explanation \cr
\code{firmID}	\tab character;	specific code that describes a firm over the years (this code does not change over time, provided by user) \cr
\code{year}	\tab numeric;	year in which the firm produced a product (provided by user) \cr
\code{PC8}	\tab character;	PC8 code of firm product (provided by user) \cr
\code{...}	\tab character;	additional columns from original firm data (provided by user) \cr
\code{PC8plus}	\tab character;	final harmonization, which refers to the last year of the time period \cr
\code{flag}	\tab character;	either 0 or 1; 1 indicates that this code remained the same in notation over the whole time period but was split or merged in addition \cr
\code{HS6}	\tab character;	provides the HS6 classification of the PC8plus code \cr
\code{HS6plus}	\tab character;	also adjusts for the change lists of HS6 \cr
\code{BEC}	\tab character;	provides the BEC classification on a high aggregated level (1 digit) \cr
\code{BEC_agr}	\tab character;	provides the BEC classification on a less aggregated level (up to 3 digits) \cr
\code{BEC_basic_class}	\tab character;	provides information if the code is classified as consumption, capital or intermediate good in BEC
}
}

\examples{
\donttest{
sampledata <- read.table(paste0(system.file("extdata", package = "harmonizer"),
                         "/sampledata/pc8sample.txt"), sep = ";",
                         header = TRUE , colClasses = "character")

utilize.pc8(b = 2011, e = 2013, firm.data = sampledata)

newdata <- utilize.pc8(b = 2011, e = 2013, firm.data = sampledata,
                       output = "all")
changes <- newdata[[1]]
merged_data <- newdata[[2]]
}
}
