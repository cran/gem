PlotMetadata = function(M, xlim = range(M$t)){
    t = M$t
    temp = M$temp
    batt = M$batt
    gps = M$gpsOn

    ylim = min(temp) + (max(temp) - min(temp)) * c(-0.05, 1.15)
    par(mar = c(4,4,4,4))
    plot(t, temp, type = 'l', col = 'red', yaxt = 'n', main = 'Battery and Temperature', xlab = 'Time (day of year)', ylab = 'Battery (V)', ylim = ylim, xlim = xlim)
    lines(t, rescale(batt, range(temp)), col='black')
    labels = pretty(range(batt))
    axis(2, at = rescale(labels, ylim, range(batt)), labels = labels, col.axis = 'black', col.ticks = 'black')
    axis(4, at = pretty(range(temp)), col.axis = 'red', col.ticks = 'red')
    mtext('Temperature (deg C)', 4, col = 'red', line = 2.5)
    legend(x = 'topright', pch = c(15,15,15), col = c(1,2,3), legend = c('Battery', 'Temperature', 'GPS'))

    lines(t, gps * 0.02*(ylim[2]-ylim[1]) + ylim[1], col = 3)
}

rescale = function(x, newlim, oldlim = range(x)){
    (x - oldlim[1])/(oldlim[2] - oldlim[1]) * (newlim[2] - newlim[1]) + newlim[1]
}
