% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotcor.R
\name{plotcor}
\alias{plotcor}
\title{Visualise climate cross correlation or autocorrelation.}
\usage{
plotcor(cor.output, type, arrow = FALSE)
}
\arguments{
\item{cor.output}{Output of \code{\link{autowin}} or
\code{\link{crosswin}}}

\item{type}{Should be either "A" for data generated by \code{\link{autowin}}
or "C" for data generated by \code{\link{crosswin}}.}

\item{arrow}{TRUE or FALSE. Add arrows to plots to pinpoint best window.}
}
\value{
Will generate a colour plot to visualise the correlation data.
}
\description{
Create a colour plot to visualise the results of \code{\link{autowin}} or
\code{\link{crosswin}}. Displays correlation across all desired climate
windows.
}
\examples{

#Simple test example
#Create data from a subset of our test dataset
#Just use two years
biol_data <- Mass[1:2, ]
clim_data <- MassClimate[grep(pattern = "1979|1986", x = MassClimate$Date), ]

single <- singlewin(xvar = list(Temp = clim_data$Temp),
                   cdate = clim_data$Date, 
                   bdate = biol_data$Date, 
                   baseline = lm(Mass ~ 1, data = biol_data),
                   range = c(1, 0), 
                   type = "relative", stat = "mean", 
                   func = c("lin"), cmissing = FALSE, cinterval = "day")
                   
auto <- autowin(reference = single,
                xvar  = list(Temp = clim_data$Temp),
                cdate = clim_data$Date, bdate = biol_data$Date,
                baseline = lm(Mass ~ 1, data = biol_data), range = c(1, 0), 
                stat = "mean", func = "lin", 
                type = "relative",
                cmissing = FALSE, cinterval = "day")
                
plotcor(auto, type = "A")

\dontrun{
# Full working example
# Visualise climate autocorrelation

data(Mass)
data(MassClimate)

# Fit a single climate window using the datasets Mass and MassClimate.

single <- singlewin(xvar = list(Temp = MassClimate$Temp), 
                   cdate = MassClimate$Date, bdate = Mass$Date,
                   baseline = lm(Mass ~ 1, data = Mass), 
                   range = c(72, 15),
                   stat = "mean", func = "lin",
                   type = "absolute", refday = c(20, 5),
                   cmissing = FALSE, cinterval = "day")            

# Test the autocorrelation between the climate in this single window and other climate windows.

auto <- autowin(reference = single,
               xvar  = list(Temp = MassClimate$Temp), 
               cdate = MassClimate$Date, bdate = Mass$Date,
               baseline = lm(Mass ~ 1, data = Mass), 
               range = c(365, 0), 
               stat = "mean", func = "lin",
               type = "absolute", refday = c(20, 5),
               cmissing = FALSE, cinterval = "day")
                
# Plot the auto-correlation data

plotcor(auto, type = "A")
}
}
\author{
Liam D. Bailey and Martijn van de Pol
}
