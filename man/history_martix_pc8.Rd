\name{history_matrix_pc8}
\alias{history_matrix_pc8}
\title{History matrix of PC8 product codes}
\usage{
history_matrix_pc8(b, e, progress = TRUE)
}
\description{
Provides a dataframe which contains all PC8 product codes and their history in a given time period.
}
\arguments{
\item{b}{first year of interest}
\item{e}{last year of interest}
\item{progress}{logical, determines whether progress is printed in console or not.}
}

\value{
A data frame that contains all PC8 product codes and their history over time for the demanded time period. This dataset is the basis for the main function \code{harmonize_pc8()} and can be obtained therewith as well. The following table offers an overview of all provided variables.

\tabular{ll}{
Variable	\tab Explanation \cr
\code{PC8_xxxx}	\tab character; a specific PC8 code in a given year \cr
\code{flag} \tab	numeric; either 0 or 1; 1 indicates that this code remained the same in notation over the whole time period but was split or merged in addition \cr
\code{flagyear}	\tab numeric; indicates the first year in which the flag was set to 1
}
}

\examples{
\donttest{
history_matrix_pc8(b = 2008, e = 2010)
}
}
