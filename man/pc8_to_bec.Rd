\name{pc8_to_bec}
\alias{pc8_to_bec}
\title{concordance list between PC8 and BEC}
\usage{
pc8_to_bec(b, e, historymatrix = NULL, progress = TRUE)
}
\description{
Provides a dataframe which contains all PC8 product codes and related BEC codes in the demanded time period.
}
\arguments{
\item{b}{first year of interest}
\item{e}{last year of interest}
\item{historymatrix}{History matrix of PC8 product codes. Provided by history_matrix_pc8().}
\item{progress}{logical, determines whether progress is printed in console or not.}
}

\value{
A data frame that contains all PC8 product codes and related BEC and HS6 codes in a given time period. The following table offers an overview of all provided variables.

\tabular{ll}{
Variable \tab	Explanation \cr
\code{PC8}	\tab character; a specific PC8 code \cr
\code{HS6} \tab character;	provides the HS6 classification of the PC8plus code \cr
\code{BEC}	\tab character; provides the BEC classification on a high aggregation level (1 digit) \cr
\code{BEC_agr} \tab character;	provides the BEC classification on a lower aggregation level (up to 3 digits)

}
}

\examples{
\donttest{
pc8_to_bec(b = 2008, e = 2010)
}
}
