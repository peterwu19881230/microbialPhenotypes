\name{one_attr}
\alias{one_attr}
\title{
Generate a table that shows whether pairs of elements in a list share the same attribute(s)
}
\description{
Compute the vecotr of "at least 1 same attribute". NA is accepted for attribute.
}
\usage{
one_attr(attribute_list)
}
\arguments{
  \item{attribute_list}{
A named list where the elements list the attributes
}
}
\author{
Peter I-Fan Wu
}
\examples{
attribute_list=list(x=1:5,y=4:7,z=letters[1:5])
one_attr(attribute_list)
}
