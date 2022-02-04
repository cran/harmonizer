\name{utilize_cn8}
\alias{utilize_cn8}
\title{A possible utilization of harmonized CN8 products codes}
\usage{
utilize_cn8(b, e, firm_data, harmonized_data = NULL,
            progress = TRUE, output = "merged.firm.data",
            value = FALSE)
}
\description{
Provide an application of the data frames obtained by the main function, \code{harmonize_cn8}. To use these additional functions, data on firm-level is required, which is data that is not provided by the package.
}
\arguments{
\item{b}{first year of interest}
\item{e}{last year of interest}
\item{firm_data}{Data on firm level which must provide the following columns: "firmID", "year" and "CN8".}
\item{harmonized_data}{Harmonized data of CN8 product codes. Provided by harmonize_cn8(). By default \emph{NULL}; the function computes the needed harmonized data.}
\item{progress}{logical, determines whether progress is printed in console or not.}
\item{output}{Defines which dataframe is returned. It may take the following values:
      \itemize{
          \item{"product.changes", returns all changed CN8 product codes per firm per year (see description of (a) above)}
          \item{"merged.firm.data", returns entered firm data, extended by harmonized data (see description of (b) above)}
          \item{"all", returns both dataframes as a list}
      }
  }
\item{value}{logical, determines whether value is calculated for same/new/dropped products. Only possible if data contains a column: "value". Value may contain different quantities (e.g. sales [Euro] or weight [kg]).}
}

\value{
Provides two possible data frames: \describe{
  \item{(a)}{ One dataframe that contains all changed CN8 product codes per firm per year. In more detail, this means how many products remained the same, were added or dropped - the value of the same/added/dropped products - how many products were produced by a certain firm in a given year, and how many products were produced in the year after.}
  \item{(b)}{ One dataframe that is based on the entered firm data. The entered firm data is extended by harmonized data (that is "CN8plus", "flag", "flagyear", "HS6plus", "BEC", "BEC_agr", "BEC_basic_class").}
  }

Table that summarizes the output, described by the notation (a) above:

\tabular{ll}{
Variable \tab	Explanation \cr
\code{firmID}	\tab	character; specific code that describes a firm over the years (this code does not change over time) \cr
\code{period_UL}	\tab character;	lower limit of the time period \cr
\code{period}	\tab character;	time period in which the product was produced \cr
\code{gap}	\tab numeric;	indicating if the time period is greater than one (i.e. upper limit - lower limit > 1) \cr
\code{same_products}	\tab numeric;	number of products that were produced in both years (i.e. remained in the product portfolio of this firm) \cr
\code{value_same_products} \tab numeric;	value of products that were produced in both years (i.e. remained in the product portfolio of this firm); the value is calculated in the upper limit of the time period \cr
\code{new_products}	\tab numeric;	number of added products in the upper limit of the time period (i.e. added to the product portfolio of this firm) \cr
\code{value_new_products}	\tab numeric;	value of added products in the upper limit of the time period (i.e. added to the product portfolio of this firm) \cr
\code{dropped_products}	\tab numeric;	number of dropped products in the upper limit of the time period (i.e. removed of the product portfolio of this firm) \cr
\code{value_dropped_products}	\tab numeric;	value of dropped products in the upper limit of the time period (i.e. removed of the product portfolio of this firm); the value is calculated in the lower limit of the time period \cr
\code{nbr_of_products_period_LL}	\tab numeric;	number of all products produced in the lower limit of the time period (i.e. entire product portfolio of this firm) \cr
\code{nbr_of_products_period_UL}	\tab numeric;	number of all products produced in the upper limit of the time period (i.e. entire product portfolio of this firm)
}

Table that summarizes the output, described by the notation (b) above:

\tabular{ll}{
Variable	\tab	Explanation \cr
\code{firmID}	\tab character;	specific code that describes a firm over the years (this code does not change over time, provided by user) \cr
\code{year}	\tab numeric;	year in which the firm produced a product (provided by user) \cr
\code{CN8}	\tab character;	CN8 code of firm product (provided by user) \cr
\code{(value)}	\tab numeric; value of the corresponding product code (may be provided by user) \cr
\code{...}	\tab character;	additional columns from original firm data (provided by user) \cr
\code{CN8plus}	\tab character;	final harmonization, which refers to the last year of the time period \cr
\code{flag}	\tab character;	either 0 or 1; 1 indicates that this code remained the same in notation over the whole time period but was split or merged in addition \cr
\code{HS6}	\tab character;	provides the HS6 classification of the CN8plus code \cr
\code{HS6plus}	\tab character;	also adjusts for the change lists of HS6 \cr
\code{BEC}	\tab character;	provides the BEC classification on a high aggregated level (1 digit) \cr
\code{BEC_agr}	\tab character;	provides the BEC classification on a less aggregated level (up to 3 digits) \cr
\code{BEC_basic_class}	\tab character;	provides information if the code is classified as consumption, capital or intermediate good in BEC
}
}

\examples{
\donttest{
sampledata <- read.table(paste0(system.file("extdata", package = "harmonizer"),
                         "/sampledata/cn8sample.txt"), sep = ";",
                         header = TRUE, colClasses = "character")

utilize_cn8(b = 2008, e = 2010, firm_data = sampledata)

newdata <- utilize_cn8(b = 2008, e = 2010, firm_data = sampledata,
                       output = "all")
changes <- newdata[[1]]
merged_data <- newdata[[2]]
}
}

