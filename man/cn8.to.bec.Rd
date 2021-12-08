\name{cn8.to.bec}
\alias{cn8.to.bec}
\title{Concordance list between CN8 and BEC}
\usage{
cn8.to.bec(b, e, historymatrix = NULL, progress = TRUE)
}
\description{
Provides a dataframe which contains all CN8 product codes and related BEC codes in a given time period.
}
\arguments{
\item{b}{first year of interest}
\item{e}{last year of interest}
\item{historymatrix}{History matrix of CN8 product codes. Provided by history.martix.cn8().}
\item{progress}{logical, determines wether progress is printed in console or not.}
}

\value{
A data frame that contains all CN8 product codes and related BEC and HS6 codes in a given time period. The following table offers an overview of all provided variables.

\tabular{ll}{
Variable \tab	Explanation \cr
\code{CN8}	\tab character; a specific CN8 code \cr
\code{HS6} \tab character;	provides the HS6 classification of the CN8plus code \cr
\code{BEC}	\tab character; provides the BEC classification on a high aggregation level (1 digit) \cr
\code{BEC_agr} \tab character;	provides the BEC classification on a lower aggregation level (up to 3 digits)

}
}

\examples{
\donttest{
cn8.to.bec(b = 2008, e = 2010)
}
}
