% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_external.R
\name{create_size}
\alias{create_size}
\title{Create the inputs to evaluate the quality of total estimations}
\usage{
create_size(
  var,
  domains = NULL,
  subpop = NULL,
  design,
  ci = FALSE,
  ess = FALSE,
  ajuste_ene = FALSE,
  standard_eval = FALSE,
  rm.na = FALSE,
  deff = FALSE,
  rel_error = FALSE,
  unweighted = FALSE,
  df_type = c("chile", "eclac"),
  eclac_input = FALSE
)
}
\arguments{
\item{var}{numeric variable within the \code{dataframe}. When the domain parameter is not used,
it is possible to include more than one variable using the + separator. When a value is introduced
in the domain parameter, the estimation variable must be a dummy variable.}

\item{domains}{domains to be estimated separated by the + character.}

\item{subpop}{integer dummy variable to filter the dataframe.}

\item{design}{complex design created by \code{survey} package.}

\item{ci}{\code{boolean} indicating if the confidence intervals must be calculated.}

\item{ess}{\code{boolean} effective sample size.}

\item{ajuste_ene}{\code{boolean} indicating if an adjustment for the sampling-frame transition period must be used.}

\item{standard_eval}{\code{boolean} indicating if the function is wrapped inside another function, if \code{TRUE} avoid lazy eval errors.}

\item{rm.na}{\code{boolean} remove NA values if required.}

\item{deff}{\code{boolean} design effect.}

\item{rel_error}{\code{boolean} relative error.}

\item{unweighted}{\code{boolean} add non-weighted count if required.}

\item{df_type}{\code{character} use degrees of freedom calculation approach from INE Chile or CEPAL. Options are "chile" or "eclac".}

\item{eclac_input}{\code{boolean} return eclac inputs}
}
\value{
\code{dataframe} that contains the inputs and all domains to be evaluated.
}
\description{
\code{create_size} generates a \code{dataframe} with the following elements: sum,
degrees of freedom, sample size, and coefficient of variation. The function allows
grouping in several domains.
}
\examples{
dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
create_size("ocupado", "zona+sexo", design = dc)
}
