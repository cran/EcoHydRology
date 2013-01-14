\name{swat_objective_function_rch}
\alias{swat_objective_function_rch}
\title{
An example objective function for calibrating SWAT2005 model.
}
\description{
An example objective function for calibrating SWAT2005 model.
}
\usage{
swat_objective_function_rch(x, calib_range, calib_params, flowgage, rch, save_results = F)
}
\arguments{
  \item{x}{
Numeric vector similar to change_params$current defining the scalar states of the parameters to be optimized.
}
  \item{calib_range}{
2 place vector of begin and end dates for calibration.
}
  \item{calib_params}{
Dataframe in the format of change_params limited to those rows of parameters desired to calibrate. See change_params.
}
  \item{flowgage}{
list of the format of the list returned by the function get_usgs_gage.
}
  \item{rch}{
Number of the reach in the output.rch file you desire to calibrate against
}
  \item{save_results}{
Logical to express if you want to copy the optimial solution to the current directory, overwriting the swat input files.
}
}
\value{
abs\(NS - 1\) as needed to be able to minimize to optimal as needed for DEoptim function.

}
\author{
Daniel R. Fuka
}

