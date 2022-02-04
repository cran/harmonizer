\name{harmonize_pc8}
\alias{harmonize_pc8}
\title{Harmonization of PC8 product codes}
\usage{
harmonize_pc8(b, e, historymatrix = NULL,
              HS6breaks = c(1992, 1996, 2002, 2007, 2012, 2017),
              progress = TRUE)
}
\description{
Provides a dataframe which contains all PC8 product codes and their history in the demanded time period, as well as harmonized PC8plus code, harmonized HS6plus code and BEC classification.
}
\arguments{
\item{b}{first year of interest}
\item{e}{last year of interest}
\item{historymatrix}{History matrix of PC8 product codes. Provided by history_matrix_pc8(). By default \emph{NULL}; the function computes the needed harmonized data.}
\item{HS6breaks}{Vector of years, where HS6 codes where changed.}
\item{progress}{logical, determines whether progress is printed in console or not.}
}

\value{
A data frame that contains all PC8 product codes and their history, harmonized PC8plus codes, harmonized HS6plus codes, and BEC classification. The 'plus-codes' are the main outcome of the function. They provide harmonized information of the product codes, i.e. comparable codes. Every harmonization refers to the last year of interest. The following table offers an overview of all provided variables.

\tabular{ll}{
Variable \tab	Explanation \cr
\code{PC8_xxxx}	\tab character; a specific PC8 code in a given year \cr
\code{PC8plus}	\tab character; the harmonization code for PC8, which refers to the last year of the time period \cr
\code{HS6plus}	\tab character; the harmonization code of HS6, which refers to the last year of the time period \cr
\code{BEC}	\tab character; provides the BEC classification at a high aggregation level (1 digit) \cr
\code{BEC_agr} \tab	character; provides the BEC classification at a lower aggregation level (up to 3 digits) \cr
\code{BEC_basic_class} \tab	character; provides information if the code is classified as consumption, capital or intermediate good in BEC \cr
\code{flag}	\tab numeric; either 0 or 1; 1 indicates that this code remained the same in notation over the whole time period but was split or merged in addition \cr
\code{flagyear} \tab numeric;	indicates the first year in which the flag was set to 1
}
}

\examples{
\donttest{
harmonize_pc8(b = 2009, e = 2011)

mydata <- history_matrix_pc8(b = 2015, e = 2017)
myharmonization <- harmonize_pc8(b = 2015, e = 2017,
                                 historymatrix = mydata)
}
}