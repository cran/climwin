## ----message = FALSE-----------------------------------------------------
library(climwin)

## ---- eval = FALSE-------------------------------------------------------
#  
#  MassWin <- slidingwin(xvar = list(Temp = MassClimate$Temp),
#                        cdate = MassClimate$Date,
#                        bdate = Mass$Date,
#                        baseline = lm(Mass ~ 1, data = Mass),
#                        cinterval = "day",
#                        range = c(150, 0),
#                        upper = 0, binary = TRUE,
#                        type = "absolute", refday = c(20, 05),
#                        stat = "sum",
#                        func = "lin")
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#      head(MassWin[[1]]$BestModelData)
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  MassWin <- slidingwin(xvar = list(Temp = Climate$Temp),
#                        cdate = Climate$Date,
#                        bdate = Biol$Date,
#                        baseline = lm(Mass ~ 1, data = Biol),
#                        cinterval = "day",
#                        range = c(150, 0),
#                        type = "absolute", refday = c(20, 05),
#                        stat = "mean",
#                        func = "lin", spatial = list(Biol$SiteID, Climate$SiteID))
#  

## ---- echo = FALSE, fig.width = 5, fig.height = 5------------------------

Unweight <- data.frame(Time = seq(0, 100, 1), Weight = c(rep(0, times = 25), rep(1, times = 50), rep(0, 26)))
Unweight$Weight <- Unweight$Weight/sum(Unweight$Weight)

par(mar = c(5, 4.25, 4, 2) + 0.1)
plot(x = Unweight$Time, y = Unweight$Weight, type = "l", ylab = "Weight", xlab = "Time", ylim = c(0, 0.05),
     yaxt = "n", xaxt = "n",
     lwd = 2, 
     cex.lab = 1.25, cex.axis = 1.25, cex = 1.5, 
     #mgp = c(4, 1.75, 0)
     )
axis(2, cex.axis = 1.25, cex.lab = 1.25, yaxp = c(0, 0.05, 2))
axis(1, cex.axis = 1.5, cex.lab = 1.25, xaxp = c(0, 100, 2),
     mgp = c(2, 1.5, 0))


## ---- echo = FALSE, fig.width = 8, fig.height = 4------------------------

par(mfrow = c(1, 2))
duration <- 365
j        <- seq(1:duration) / duration
k        <- seq(-10, 10, by = (2 * 10 / duration))
weight   <- 3 / 0.2 * ((j[1:duration] - 0) / 0.2) ^ (3 - 1) * exp( - ((j[1:duration] - 0) / 0.2) ^ 3)
plot((weight / sum(weight)), type = "l", ylab = "Weight", xlab = "Day", cex.lab = 1.5, cex.axis = 1.5, main = "Weibull distribution")

weight <- evd::dgev(k[1:duration], loc = 1, scale = 2, shape = -1, log = FALSE)
plot((weight / sum(weight)), type = "l", ylab = "Weight", xlab = "Day", cex.lab = 1.5, cex.axis = 1.5, main = "GEV distribution")


## ---- eval = FALSE-------------------------------------------------------
#  
#  weight <- weightwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date,
#                      bdate = Mass$Date,
#                      baseline = lm(Mass ~ 1, data = Mass),
#                      range = c(150, 0),
#                      func = "lin", type = "absolute",
#                      refday = c(20, 5),
#                      weightfunc = "W", cinterval = "day",
#                      par = c(3, 0.2, 0))
#  

## ---- echo = FALSE, fig.width = 5, fig.height = 5------------------------

explore(weightfunc = "W", shape = 2.17, scale = 0.35, loc = 0)


## ---- eval = FALSE-------------------------------------------------------
#  
#  weight$WeightedOutput$deltaAICc
#  

