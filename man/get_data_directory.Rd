\name{get_data_directory}
\alias{get_data_directory}
\title{Data path for custom data}
\usage{
get_data_directory(path = TRUE, open_explorer = FALSE,
                   show_data = NULL)
}
\description{
Provides the directory where custom data must be stored and the used data (e.g., concordance lists, list of codes) can be edited.
}
\arguments{
\item{path}{logical, determines whether the path is printed in the console}
\item{open_explorer}{logical, determines whether an explorer is opened in addition. Only executable if the directory path does not contain any blanks.}
\item{show_data}{character string, which must take one of the following values: "CN8", "HS6", "PC8" or "HS6toBEC". All available data in in the given directory is printed in the console. Only executable if the directory path does not contain any blanks.}
}

\value{
Returns the path (character), of the directory where custom data must be stored and the used data (e.g., concordance lists, list of codes) can be edited.
}

\examples{

get_data_directory()

get_data_directory(path = FALSE, show_data = "CN8")
}
