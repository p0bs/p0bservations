% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_income_net.R
\name{calc_income_net}
\alias{calc_income_net}
\title{Calculate income net of UK tax and National Insurance}
\usage{
calc_income_net(income_taxable)
}
\arguments{
\item{income_taxable}{The taxable income level (i.e. after deductions for things like pension contributions).}
}
\description{
This function applies the prevailing tax bands and rates to give a simple calculation for income after UK tax and National Insurance (to the nearest couple of pounds) for FY20/21.
}
\examples{
\dontrun{
calc_income_net(income_taxable = 38000)
}
}
\keyword{Tax}
