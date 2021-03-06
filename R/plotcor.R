#' Visualise climate cross correlation or autocorrelation.
#' 
#' Create a colour plot to visualise the results of \code{\link{autowin}} or
#' \code{\link{crosswin}}. Displays correlation across all desired climate
#' windows.
#' @param cor.output Output of \code{\link{autowin}} or
#'   \code{\link{crosswin}}
#' @param type Should be either "A" for data generated by \code{\link{autowin}}
#'   or "C" for data generated by \code{\link{crosswin}}.
#'@param arrow TRUE or FALSE. Add arrows to plots to pinpoint best window.
#' @return Will generate a colour plot to visualise the correlation data.
#' @author Liam D. Bailey and Martijn van de Pol
#' @import ggplot2
#' @examples
#' 
#'#Simple test example
#'#Create data from a subset of our test dataset
#'#Just use two years
#'biol_data <- Mass[1:2, ]
#'clim_data <- MassClimate[grep(pattern = "1979|1986", x = MassClimate$Date), ]
#'
#'single <- singlewin(xvar = list(Temp = clim_data$Temp),
#'                    cdate = clim_data$Date, 
#'                    bdate = biol_data$Date, 
#'                    baseline = lm(Mass ~ 1, data = biol_data),
#'                    range = c(1, 0), 
#'                    type = "relative", stat = "mean", 
#'                    func = c("lin"), cmissing = FALSE, cinterval = "day")
#'                    
#'auto <- autowin(reference = single,
#'                 xvar  = list(Temp = clim_data$Temp),
#'                 cdate = clim_data$Date, bdate = biol_data$Date,
#'                 baseline = lm(Mass ~ 1, data = biol_data), range = c(1, 0), 
#'                 stat = "mean", func = "lin", 
#'                 type = "relative",
#'                 cmissing = FALSE, cinterval = "day")
#'                 
#'plotcor(auto, type = "A")
#' 
#' \dontrun{
#'# Full working example
#'# Visualise climate autocorrelation
#'
#'data(Mass)
#'data(MassClimate)
#' 
#'# Fit a single climate window using the datasets Mass and MassClimate.
#' 
#'single <- singlewin(xvar = list(Temp = MassClimate$Temp), 
#'                    cdate = MassClimate$Date, bdate = Mass$Date,
#'                    baseline = lm(Mass ~ 1, data = Mass), 
#'                    range = c(72, 15),
#'                    stat = "mean", func = "lin",
#'                    type = "absolute", refday = c(20, 5),
#'                    cmissing = FALSE, cinterval = "day")            
#' 
#'# Test the autocorrelation between the climate in this single window and other climate windows.
#' 
#'auto <- autowin(reference = single,
#'                xvar  = list(Temp = MassClimate$Temp), 
#'                cdate = MassClimate$Date, bdate = Mass$Date,
#'                baseline = lm(Mass ~ 1, data = Mass), 
#'                range = c(365, 0), 
#'                stat = "mean", func = "lin",
#'                type = "absolute", refday = c(20, 5),
#'                cmissing = FALSE, cinterval = "day")
#'                 
#'# Plot the auto-correlation data
#' 
#'plotcor(auto, type = "A")
#'}
#'@export


plotcor <- function(cor.output, type, arrow = FALSE){
  ifelse (type == "A", title <- "Correlation between single window and all other windows", 
          title <- "Correlation between 2 climate variables in all windows")
  with(cor.output, {
    if(type == "C"){
      ggplot(cor.output, aes(x = WindowClose, y = WindowOpen, z = cor))+
        geom_tile(aes(fill = cor))+
        scale_fill_gradient2(low = "red", mid = "yellow", high = "blue", 
                             midpoint = mean(cor.output$cor), name = "")+
        theme_classic()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(size = 0.25, colour = "black"),
              plot.title = element_text(size = 16, hjust = 0.5))+
        ggtitle(title)+
        ylab("Window open")+
        xlab("Window close")
    } else if(type == "A"){
      if(arrow == FALSE){
        ggplot(cor.output, aes(x = WindowClose, y = WindowOpen, z = cor))+
          geom_tile(aes(fill = cor))+
          scale_fill_gradient2(low = "red", mid = "yellow", high = "blue", 
                               midpoint = mean(cor.output$cor), name = "")+
          theme_classic()+
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(size = 0.25, colour = "black"),
                plot.title = element_text(size = 16, hjust = 0.5))+
          ggtitle(title)+
          ylab("Window open")+
          xlab("Window close")
      } else {
        
        CIRC <- circle(centre = c(cor.output$BestWindowClose[1], cor.output$BestWindowOpen[1]), diameter = 5, npoints = 1000)
        colnames(CIRC) <- c("WindowClose", "WindowOpen")
        CIRC$cor <- 0
        
        ggplot(cor.output, aes(x = WindowClose, y = WindowOpen, z = cor))+
          geom_tile(aes(fill = cor))+
          scale_fill_gradient2(low = "red", mid = "yellow", high = "blue", 
                               midpoint = mean(cor.output$cor), name = "")+
          theme_classic()+
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(size = 0.25, colour = "black"),
                plot.title = element_text(size = 16, hjust = 0.5))+
          ggtitle(title)+
          ylab("Window open")+
          xlab("Window close")+
          geom_path(data = CIRC, aes(x = WindowClose, y = WindowOpen), size = 1.2, colour = "black")+
          geom_segment(aes(x = BestWindowClose[1], y = 0, xend = BestWindowClose[1], yend = (BestWindowOpen[1] - 2.5)), 
                       size = 1, linetype = "dashed") +
          geom_segment(aes(x = 0, y = BestWindowOpen[1], xend = (BestWindowClose[1] - 2.5), yend = BestWindowOpen[1]),
                       size = 1, linetype = "dashed")
      }
    }
  }
  )
}