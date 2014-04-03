\name{Lumped_VSA_model}
\alias{Lumped_VSA_model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lumped Variable Source Area (VSA) Watershed Model
}
\description{
This model calculates streamflow and approximate saturated area percentage contributing to overland flow.  It was developed in saturation-excess dominated watersheds, and is based on the Thornthwaite-Mather water budget and SCS Curve Number approach for overland runoff. 
}
\usage{
Lumped_VSA_model(dateSeries, P, Tmax, Tmin, Depth = NULL, SATper = NULL, AWCper = NULL, 
percentImpervious = 0, no_wet_class = 10, Tp = 5, latitudeDegrees = 42.38, albedo = 0.23, 
StartCond = "avg", PETin = NULL, AWC = Depth * AWCper, SAT = Depth * SATper, SW1 = NULL, 
BF1 = 1, PETcap = 5, rec_coef = 0.1, Se_min = 78, C1 = 3.1, Ia_coef = 0.05, 
PreviousOutput = NULL, runoff_breakdown = RunoffBreakdown(Tp, HrPrcDelay = (Tp/2 - 4)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dateSeries}{
Daily date series in the format "2013-05-21"
}
  \item{P}{
Rain + Snowmelt (mm)
}
  \item{Tmax}{
Maximum daily T (C)
}
  \item{Tmin}{
Minimum daily T (C)
}
  \item{Depth}{
Average watershed soil depth (mm)
Not needed if SAT and AWC depth entered directly
}
  \item{SATper}{
Porosity of the soil (volumetric fraction, 0-1)
Not needed if SAT (porosity depth) entered directly
}
  \item{AWCper}{
Available water capacity, Field capacity - wilting point (volumetric fraction, 0-1)
Not needed if AWC entered directly
}
  \item{percentImpervious}{
Percent of the watershed that is impervious (percentage, 0-100%)
}
  \item{no_wet_class}{
Number of wetness classes to distribute runoff over. Default is 10.
}
  \item{Tp}{
Time to peak of hydrograph (hours)
}
  \item{latitudeDegrees}{
latitude (degrees)
}
  \item{albedo}{
Average surface albedo, defaults to average 0.23
}
  \item{StartCond}{
Watershed conditions before first day of run (options are "wet", "dry", "avg")
}
  \item{PETin}{
# User has the option to enter PET values (mm/day), otherwise this will be estimated from Priestley-Taylor equation, estimating radiation from temperature
}
  \item{AWC}{
# AWC depth (mm)
}
  \item{SAT}{
Porosity depth (mm)
}
  \item{SW1}{
Soil water on the first day (depth, mm)
}
  \item{BF1}{
Baseflow on the first day (mm/day)
}
  \item{PETcap}{
Cutoff for maximal PET allowed per day (mm)
}
  \item{rec_coef}{
Baseflow recession coefficient
}
  \item{Se_min}{
Minimal daily CN S value. (mm)
}
  \item{C1}{
Coefficient relating daily Curve Number S to soil water 
}
  \item{Ia_coef}{
Initial abstraction coefficient for CN-equation.  (range ~ 0.05 - 0.2)
}
  \item{PreviousOutput}{
If the model is run repeatedly, previous output can be provided so that the model only needs to calculate from that point forward.
}
  \item{runoff_breakdown}{
The proportion of runoff that reaches the outlet on a given day after the storm event.  Can be calculated from Tp
}
}
\details{
The model expects continuous input on a daily time-step, since the soil-water is calculated over time, and affects the amount of runoff that will be generated after a storm.  
Also, note that precipitation values are actually Rain + Snowmelt (mm).  Users can use the snowmelt function to determine this if needed.
}
\value{
Returns a data frame with modeled streamflow, baseflow, ET, and maximum wetness class generating runoff for all dates.  Soil water and other modeled intermediate results are also returned.  All flow values (modeled_flow, baseflow, OverlandFlow, ShallowInterflow, totQ, quickflow_combined, impervRunoff, excess) are in depth of flow per day (mm/d)
}
\references{
Archibald, J.A., B.P. Buchanan, D.R. Fuka, C.B Georgakakos, S.W. Lyon, M.T. Walter. A simple, regionally parameterized model for predicting nonpoint source areas in the Northeastern US. Submitted to Journal of Hydrology: Regional Studies  

Schneiderman EM. Steenhuis TS, Thongs DJ, Easton ZM, Zion MS, Neal AL, Mendoza GF, Walter MT.  2007.  Incorporating variable source area hydrology into a curve-number-based watershed model.  Hydrological Processes.  21: 3420-3430

Shaw, SB, MT Walter.  2009.  Improving runoff risk estimates: Formulating runoff as a bivariate process using the SCS curve number method.  Water Resources Research.  45

Thornthwaite CW, JR Mather.  1957 Instructions for computing potential evapotranspiration and water balance.  Publ Climatol 3: 183-311

United States Department of Agriculture (USDA).  (1986).  SCS publication Technical Release 55: Urban Hydrology for Small Watersheds.  Natural Resources Conservation Service

Weiler K. Unpublished.  Determination of the Linear Bedrock Coefficient From Historical Flow Data
}
\author{
Josephine Archibald
}
\note{
This function is under development and might change substantially in further versions.  
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning}{
This function cannot handle NA values in input, and can only be run for a daily time-step.
If Tx < Tn for any day, this will produce an error.
Currently, the crop coefficients used to estimate PET are specific for deciduous northeastern USA.
}


\seealso{
PET_fromTemp
}
\examples{
data(OwascoInlet)
# First get rain and snow-melt input: 
rsm <- SnowMelt(Date=OwascoInlet$date, precip_mm=OwascoInlet$P_mm, Tmax_C=OwascoInlet$Tmax_C, 
Tmin_C=OwascoInlet$Tmin_C, lat_deg=42.66)
# Calculate streamflow based on watershed characteristics:
Results <- Lumped_VSA_model(dateSeries = OwascoInlet$date, 	P = rsm$Rain_mm+rsm$SnowMelt_mm, 
Tmax=OwascoInlet$Tmax_C, Tmin = OwascoInlet$Tmin_C, latitudeDegrees=42.66, Tp = 5.8, Depth = 2010, 
SATper = 0.27, AWCper = 0.13, StartCond = "wet")
#  View results:
hydrograph(streamflow=ConvertFlowUnits(cms=OwascoInlet$Streamflow_m3s, WA=106, AREAunits="mi2"), 
timeSeries=OwascoInlet$date, streamflow2=Results$modeled_flow, precip=rsm$Rain_mm+rsm$SnowMelt_mm)
}
