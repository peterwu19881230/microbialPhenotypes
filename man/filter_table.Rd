\name{filter_table}
\alias{filter_table}
\title{
Remove all missing values from a table
}
\description{
Filter the matrix or dataframe so that all rows/columns with NA/NAN/NULL/"" are removed
}
\usage{
filter_table(table)
}
\arguments{
  \item{table}{
A matrix or dataframe that contains missing values (NA/NAN/NULL/"")
}
}
\author{
Peter I-Fan Wu
}
\examples{
mat=matrix(c(NA,2:11,NA),nrow=3)
filter_table(mat)
}

