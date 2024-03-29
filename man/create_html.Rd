% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report.R
\name{create_html}
\alias{create_html}
\title{Create html table with the results of the evaluation}
\usage{
create_html(table)
}
\arguments{
\item{table}{\code{dataframe} generated by evaluate function}
}
\value{
\code{html} table
}
\description{
Create html table with the results of the evaluation
}
\examples{
library(survey)
library(dplyr)

hogar <- epf_personas \%>\%
  group_by(folio) \%>\%
  slice(1)
dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = hogar, weights = ~fe)
table <- assess(create_prop("ocupado", domains = "zona+sexo", design = dc))
}
