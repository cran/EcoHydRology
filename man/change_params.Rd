\name{change_params}
\alias{change_params}
\docType{data}
\title{
Example dataframe of parameters, ranges, and search strings for optimizing swat2005
}
\description{
Example dataframe of parameters, ranges, and search strings for optimizing swat2005
}
\usage{data(change_params)}
\format{
  A data frame with 15 observations on the following 11 variables.
  \describe{
    \item{\code{filetype}}{a factor with levels \code{0*.gw} \code{0*.mgt} \code{0*.sol} \code{basins.bsn}}
    \item{\code{parameter}}{a factor with levels \code{ALPHA_BF} \code{Ave\. AW} \code{Bulk Density} \code{CN2} \code{Depth} \code{ESCO} \code{GW_DELAY} \code{GWQMN} \code{Ksat\. \(est\.\)} \code{SFTMP} \code{SMFMN} \code{SMFMX} \code{SMTMP} \code{SURLAG} \code{TIMP}}
    \item{\code{alter_type}}{a factor with levels \code{new} \code{percent}}
    \item{\code{min}}{a numeric vector}
    \item{\code{max}}{a numeric vector}
    \item{\code{current}}{a numeric vector}
    \item{\code{multi}}{a logical vector}
    \item{\code{frformat}}{a factor with levels \code{} \code{A27,10f12}}
    \item{\code{fwformat}}{a factor with levels \code{F16.4} \code{F20}}
  }
}

\examples{
data(change_params)
## maybe str(change_params) ; plot(change_params) ...
}
\keyword{datasets}
