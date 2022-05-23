\name{harmonize_cn8}
\alias{harmonize_cn8}
\title{Harmonization of CN8 product codes}
\usage{
harmonize_cn8(b, e, historymatrix, harmonize.to = "e",
              HS6breaks = c(1992, 1996, 2002, 2007, 2012, 2017),
              progress = TRUE)
}
\description{
Provides a dataframe which contains all CN8 product codes and their history in the demanded time period, as well as harmonized CN8plus code, harmonized HS6plus code and BEC classification.
}
\arguments{
\item{b}{first year of interest}
\item{e}{last year of interest}
\item{historymatrix}{History matrix of CN8 product codes. Provided by history_matrix_cn8(). By default \emph{NULL}; the function computes the needed harmonized data.}
\item{harmonize.to}{Defines which year for harmonization is used. It may take the following values:
      \itemize{
          \item{"e", harmonizes product codes towards year e}
          \item{"b", harmonizes product codes towards year b}
      }
  }
\item{HS6breaks}{Vector of years, where HS6 codes were changed. Do not edit, unless additional break is needed.}
\item{progress}{logical, determines whether progress is printed in console or not.}
}

\value{
A data frame that contains all CN8 product codes and their history, harmonized CN8plus codes, harmonized HS6plus codes, and BEC classification. The 'plus-codes' are the main outcome of the function. They provide harmonized information of the product codes, i.e. comparable codes. Every harmonization refers to the last year of interest. The following table offers an overview of all provided variables.

\tabular{ll}{
Variable \tab	Explanation \cr
\code{CN8_xxxx}	\tab character; a specific CN8 code in a given year \cr
\code{CN8plus}	\tab character; the harmonization code for CN8, which refers to the last/first year of the time period \cr
\code{HS6plus}	\tab character; the harmonization code of HS6, which refers to the last/first year of the time period \cr
\code{BEC}	\tab character; provides the BEC classification at a high aggregation level (1 digit) \cr
\code{BEC_agr} \tab	character; provides the BEC classification at a lower aggregation level (up to 3 digits) \cr
\code{SNA_basic_class} \tab	character; provides information if the code is classified as consumption, capital or intermediate good in SNA \cr
\code{flag}	\tab numeric; either 0 or 1; 1 indicates that this code remained the same in notation over the whole time period but was split or merged in addition \cr
\code{flagyear} \tab numeric;	indicates the first year in which the flag was set to 1
}
}

\examples{
\donttest{
harmonize_cn8(b = 2008, e = 2010)

mydata <- history_matrix_cn8(b = 2016, e = 2018)
myharmonization <- harmonize_cn8(b = 2016, e = 2018,
                                 historymatrix = mydata)
}
}
