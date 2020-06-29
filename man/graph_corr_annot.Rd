\name{graph_corr_annot}
\alias{graph_corr_annot}
\title{
Graph the result generated from get_confusionMatrix() 
}
\description{
Takes the output from get_confusionMatrix() to plot the result to see whether there is enrichment for sensitivity, specificity, precision, or accuracy
}
\usage{
graph_corr_annot(confusionMatrix_obj, metric, similarity, subset, cols, x_lab = "High similarity -- Gene pairs ranked by similarity -- Low similarity", ylim, lwd)
}
\arguments{
  \item{confusionMatrix_obj}{
An object generated from get_confusionMatrix() 
}
  \item{metric}{
One metric from "sensitivity", "specificity", "precision", or "accuracy""
}
  \item{similarity}{
Name for the similarity metric (To used for the y axis)
}
  \item{subset}{
Subset of pairs to be plotted
}
  \item{cols}{
The color vector
}
  \item{x_lab}{
The label on the x axis 
}
  \item{ylim}{
Range of y axis
}
  \item{lwd}{
Thickness of the lines
}
}
\author{
Peter I-Fan Wu
}


