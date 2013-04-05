\name{PTpet}
\alias{PTpet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Priestley-Taylor potential evapotranspiration
}
\description{
Returns potential evapotranspiration in m/day caculated with the Priestley-Taylor equation
}
\usage{
PTpet(Rn, T_C, PTconstant = 1.26)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Rn}{
Net daily radiation [kJ/m2/d]
}
  \item{T_C}{
Average daily temperature [C]
}
  \item{PTconstant}{
Priestley-Taylor constant, usually 1.26 [-]
}
}

\references{
Priestley and Taylor (1972).  On the assessment of surface heat flux and evaporation using large-scale parameters. 
Mon. Weath. Rev. 100: 81-92
}
\author{
Josephine Archibald
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

