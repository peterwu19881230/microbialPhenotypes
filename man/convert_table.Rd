\name{convert_table}
\alias{convert_table}
\title{
Convert types of data in a table
}
\description{
Convert every element in a matrix or dataframe to another type (eg. integer -> character)
}
\usage{
convert_table(dat, function_new_type)
}
\arguments{
  \item{dat}{
A matrix or dataframe
}
  \item{function_new_type}{
The transformation function that converts the type of an object to another (eg. as.character())
}
}
\author{
Peter I-Fan Wu
}
\examples{
dat=data.frame(number=1:5,letter=letters[1:5])
str(dat)
new_dat=convert_table(dat, as.character)
str(new_dat)
}
