theme_pm <- function(...){
  theme (
    plot.title = element_text (vjust = 3, size = 20), # plot title attrib.
    plot.margin = unit (c(3, 3, 3, 3), "lines"), # plot margins
    panel.border = element_rect (colour = "black", fill = F, size = 1), # axis colour = black
    panel.grid.major = element_line (color='grey98'), # remove major grid
    panel.grid.minor = element_line (color='grey99'), # remove minor grid
    panel.background = element_rect (fill = "white"), # background colour
    legend.background = element_rect (color='grey98',fill = "white"), # background colour
    legend.justification=c(0, 0), # lock point for legend
    #legend.position = c(0, 0), # put the legend INSIDE the plot area
    legend.key = element_blank (), # switch off the rectangle around symbols in the legend
    legend.title = element_blank (), # switch off the legend title
    legend.text = element_text (size = 10), # sets the attributes of the legend text
    axis.title.x = element_text (vjust = -1, size = 15), # change the axis title
    axis.title.y = element_text (vjust = -0.1, angle = 90, size = 15), # change the axis title
    axis.text.x = element_text (size = 12.5, vjust = -0.25, colour = "black"),# change the axis label font attributes
    axis.text.y = element_text (size = 12.5, hjust = 1, colour = "black"), # change the axis label font attributes
    axis.ticks = element_line (colour = "black", size = 0.5), # sets the thickness and colour of axis ticks
    axis.ticks.length = unit(-0.25 , "cm"), # -ve length = inside ticks
    axis.ticks.margin = unit(0.5, "cm"),# margin between the ticks and the text
    ...
  )
}