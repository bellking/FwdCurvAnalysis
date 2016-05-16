drawFwdCurvesWithPrice <- function(fwdStrCalInfo,spotPriceData)
{
  if (missing(spotPriceData))
  {
    spotPriceData <- fwdStrCalInfo[,c('QDate','SpotPrice')]
  }
  
  plotFwdCurves <- 
    ggplot(data = fwdStrCalInfo , aes(x = as.Date(FwdYM), y = FwdPrice, group=QDate, color= LongTermStr)) +
    geom_line(size=0.5) +
    geom_line(data = spotPriceData, size=0.5,  aes(x = as.Date(QDate), y = SpotPrice, group=NULL), color="blue") +
    scale_y_continuous(breaks = seq(0,150,10))
}



ggplot_dual_axis <- function(plot1, plot2, which.axis = "y") {
  
  grid.newpage()
  
  # Increase right margin if which.axis == "y"
  if(which.axis == "y") plot1 = plot1 + theme(plot.margin = unit(c(0.7, 1.5, 0.4, 0.4), "cm"))
  
  # Extract gtable
  g1 = ggplot_gtable(ggplot_build(plot1))
  
  g2 = ggplot_gtable(ggplot_build(plot2))
  
  # Overlap the panel of the second plot on that of the first
  pp = c(subset(g1$layout, name == "panel", se = t:r))
  
  g = gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  
  # Steal axis from second plot and modify
  axis.lab = ifelse(which.axis == "x", "axis-b", "axis-l")
  
  ia = which(g2$layout$name == axis.lab)
  
  ga = g2$grobs[[ia]]
  
  ax = ga$children[[2]]
  
  # Switch position of ticks and labels
  if(which.axis == "x") ax$heights = rev(ax$heights) else ax$widths = rev(ax$widths)
  
  ax$grobs = rev(ax$grobs)
  
  if(which.axis == "x") 
    
    ax$grobs[[2]]$y = ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm") else
      
      ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  
  # Modify existing row to be tall enough for axis
  if(which.axis == "x") g$heights[[2]] = g$heights[g2$layout[ia,]$t]
  
  # Add new row or column for axis label
  if(which.axis == "x") {
    
    g = gtable_add_grob(g, ax, 2, 4, 2, 4) 
    
    g = gtable_add_rows(g, g2$heights[1], 1)
    
    g = gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
    
  } else {
    
    g = gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    
    g = gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b) 
    
    g = gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b - 1)
    
  }
  
  #g
  # Draw it
 grid.draw(g)
  
  
}


drawContourCurve <- function(fwdStrBasicInfo,interestPeriod.start,interestPeriod.end)
{
  
  if(missing(interestPeriod.start))
  {
    interestPeriod.start <- min(fwdStrBasicInfo$QDate)
  }
  
  if(missing(interestPeriod.end))
  {
    interestPeriod.end <- max(fwdStrBasicInfo$QDate)
  }
  
  
  fwdStrBasicInfo <- fwdStrBasicInfo[fwdStrBasicInfo$QDate >= interestPeriod.start & fwdStrBasicInfo$QDate <= interestPeriod.end  , ] 
  
  p1 <- ggplot(data = fwdStrBasicInfo, aes(x = QDate, y = FwdDelta, group=MMDiff, color= sprintf("%02d",MMDiff))) +
    geom_line(size=0.2) +
    geom_line(data = fwdStrBasicInfo , aes(x = QDate, y = SpotPrice / 10, group = NULL), size=1, color = 'blue') +
    scale_x_date(date_breaks = "6 months" , date_labels = "%y/%b") +
    scale_y_continuous(breaks = seq(-50,50,2.5))
  
  #p2 <- ggplot(data = fwdStrBasicInfo , aes(x = QDate, y = SpotPrice, group = NULL)) +
  #  geom_line(size=0.7, color = 'blue') +
  #  scale_x_date(date_breaks = "6 months" , date_labels = "%y/%b")
  
  ##pp <- ggplot_dual_axis( p1, p2)
  
}


drawFwdPrice <- function(fwdStrBasicInfo,interestPeriod.start,interestPeriod.end)
{
  
  if(missing(interestPeriod.start))
  {
    interestPeriod.start <- min(fwdStrBasicInfo$QDate)
  }
  
  if(missing(interestPeriod.end))
  {
    interestPeriod.end <- max(fwdStrBasicInfo$QDate)
  }
  
  fwdStrBasicInfo <- fwdStrBasicInfo[fwdStrBasicInfo$QDate >= interestPeriod.start & fwdStrBasicInfo$QDate <= interestPeriod.end  , ] 
  
  p1 <- ggplot(data = fwdStrBasicInfo, aes(x = QDate, y = FwdPrice, group=MMDiff, color= sprintf("%02d",MMDiff))) +
    geom_line(size=0.2) +
    geom_line(data = fwdStrBasicInfo , aes(x = QDate, y = SpotPrice, group = NULL), size=1, color = 'blue') +
    scale_x_date(date_breaks = "6 months" , date_labels = "%y/%b") +
    scale_y_continuous(breaks = seq(0,200,5))
}

drawCorrelationTrend <- function(correlationInfo, spotPriceInfo, displayMM)
{
    corelationTrend <- ggplot(subset(correlationInfo, MMDiff %in% displayMM), 
                            aes(x = QDate, y = Estimate , group = MMDiff, col = sprintf("%02d",MMDiff))) +
    geom_line(size=0.5) +
    geom_point(size=1.2) +
    scale_x_date(date_breaks = "6 months" , date_labels = "%y/%b") +
    geom_line(data = spotPriceInfo, size = 1, aes(x = QDate, y = SpotPrice / 100, group = NULL), col = 'red' )
}


drawSlopeTrend <- function(slopeTrend.Plot.Data)
{
  slopeTrend.Plot <- ggplot(data = slopeTrend.Plot.Data , 
                            aes(x = QDate, y = PWSlope , group = MMDiff, col = sprintf("%02d" , MMDiff))) +
    geom_line(size=0.5) +
    #geom_point(size=1.2) +
    geom_line(size = 1, aes(y = SpotPrice / 100, group = NULL), col = 'red' ) +
    scale_x_date(date_breaks = "6 months" , date_labels = "%y/%b")
}


