% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/color-assessment.R
\name{dist_cie76}
\alias{dist_cie76}
\title{CIE 76 Distance between a set of colors}
\usage{
dist_cie76(X)
}
\arguments{
\item{X}{vector (or matrix) of colors.}
}
\value{
distance matrix of the colors
}
\description{
The CIE 76 distance between two colors is defined as the Euclidean distance of
these colors in LAB space. CIE 76 is a perception based distance and is designed to be
device indpendent.
A distance of 1 is supposed to be a 'Just noticeable difference' for 50% of the population.
A value of 5 is considered to be visible by most. \url{https://web.archive.org/web/20080705054344/http://www.aim-dtp.net/aim/evaluation/cie_de/index.htm}
For large differences (beyond 10) the CIE 76 formula is known to be unreliable (cite?).
Great source of information: \url{https://en.wikipedia.org/wiki/Color_difference#cite_note-10}
}
