library(pacman)
p_load(pracma)

getwd()
setwd("/home/vika/Documents/uni/4sem/LAB_4/4-Gaismas-interference/4data")
getwd()

alldata = list.files(pattern="*.csv")

csvInput = function(i){
	# ielasa .csv failu
	# alldata[i] vietā ielikt i-to lista alldata elementu
	colNames = c('dateTime', 'Time', 'lightIntensity', 'relativeIntensity', 'angle',
		'angularVelocity', 'angularAcceleration', 'position', 'velocity', 'acceleration')
	filename = read.csv(alldata[i], skip = 2, header = FALSE, col.names = colNames, sep = ";")
}

patternSearch = function(filename, parameter){
	for (char in 1:length(filename)){
		for (i in 1:length(parameter)){
			if (grepl(parameter[i], filename[char])){
				return(parameter[i])
			}
		}
	}
}

filenameInterpret = function(filename){
	#define possible patterns
	a = c('004','008')
	b = c('025','055','05','005')
	colors = c('g', 'r')
	distances = c('340','440','580')
	measurementCounter=c(sprintf("_%.f", seq(1,4,1)))

	#call patternSearch function
	color = patternSearch(filename, colors)
	ai = patternSearch(filename, a)
	bi = patternSearch(filename, b)
	distance = patternSearch(filename, distances)
	turn = patternSearch(filename, measurementCounter)
	return(c(color, ai, bi, distance, turn))
}


processData = function(){
	position = (filename[, 'position'])
	relativeIntensity = (filename[, 'relativeIntensity'])
	dataRaw = data.frame(position, relativeIntensity)
	data = na.omit(dataRaw)

	position = data[, 'position']
	relativeIntensity = data[, 'relativeIntensity']
	# cut vectors smaller to avoid noise
	# possible improvement: find the first and last peak of min height 0.01 and cut 5 indexes before that
	begin = 10
	end = length(position)/2
	position = position[begin:end]
	relativeIntensity = relativeIntensity[begin:end]
	data = data.frame(position, relativeIntensity)
}

plotData = function(i, data, peaks, peakPositions){
	# splains = smooth.spline(position, relativeIntensity)
	# splains = spline(position, relativeIntensity)
	# splains = spline(position, relativeIntensity, method = "natural")
	position = as.numeric(unlist(data[1]))
	relativeIntensity = as.numeric(unlist(data[2]))

	splains = smooth.spline(position, relativeIntensity, spar = 0.001, all.knots=TRUE)

	plot.new()
	jpeg(paste('rplot', toString(i), '.jpeg', sep=""), width = 1000, height = 500, units = "px", pointsize = 15)
	plot(position, relativeIntensity, col="gray40", xlab = "Position", ylab ="Relative intensity")
	points(peakPositions, peaks, col = 'orangered', pch=19)
	lines(splains, col = "purple", lwd = 2)
	title(main = 'Junga dubultsprauga', cex.main = 2, font.main= 4, col.main= "black")
	abline(v=(seq(0,0.020,0.002)), col="burlywood2", lty="dotted")
	abline(h=(seq(0,3,0.2)), col="burlywood4", lty="dotted")
	dev.off()
}

magicBox = function(relativeIntensity, peakCount){
	# peaks = findpeaks(relativeIntensity, nups = 10, ndowns = nups,
	# 	minpeakheight = 0.1, minpeakdistance = 10, npeaks = peakCount)
	relativeIntensity = as.numeric(unlist(data[2]))
	peaks = findpeaks(relativeIntensity, minpeakdistance = 5, threshold = 0.51, npeaks = peakCount)
	return(peaks)
}

plotPeaks = function(peaks, position){
	# The first column gives the height,
 	# the second the position/index where the maximum is reached,
 	# the third and forth the indices of where the peak begins and ends
 	# --- in the sense of where the pattern starts and ends.
 	peakPositionIndex = vector(mode="numeric", length=0)
 	peakPositions = vector(mode="numeric", length=0)
 		# this finds the peak by finding the middle point of where the peak begins and ends
		for (i in 1:nrow(peaks)){
			peakPositionIndex_i = (peaks[,3][i] + peaks[,4][i]) / 2
			peakPositionIndex = c(peakPositionIndex, peakPositionIndex_i)
		}
		# this conects the index of the position of the peak with the value of the position
		for (i in 1:length(peakPositionIndex)){
			index = peakPositionIndex[i]
			peakPosition = position[index]
			peakPositions = c(peakPositions, peakPosition)
		}
	return(peakPositions)
}


for (i in 1:2){
	# izsauc visas funkcijas
	filename = csvInput(i)
	interpretation = filenameInterpret(alldata[i])
	data = processData()
	peakData = magicBox(data, 5)
	peaks = peakData[,1]
	position = as.numeric(unlist(data[1]))
	peakPositions = plotPeaks(peakData, position)
	print(peaks)
	print(peakPositions)
	# print(head(data))
	# plotData(i, data, peaks, peakPositions)
}

# high threshold - viens pīķis
# low threshold - divi pīķi

# list = filenameInterpret(filename)
# names(list) <- c("color", "ai", "bi", "turn")

# print(list)
# names(list)