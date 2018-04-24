library(pacman)
p_load(pracma)
p_load(stats)

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

	# get rid of rows with NA
	data = na.omit(dataRaw)
	position = data[, 'position']
	relativeIntensity = data[, 'relativeIntensity']

	begin = 10
	for (i in 1:length(position)){
		if (position[i] == max(position)){
			end = i
			print(end)
			break
		}			
	}

	relativeIntensity = relativeIntensity[begin:end]
	position = position[begin:end]
	data = data.frame(position, relativeIntensity)
	# print(data)

	# sort according to position
	data = data[with(data, order(position)), ]
	position = data[, 'position']
	relativeIntensity = data[, 'relativeIntensity']
	# print(data)
	print('shit is bout to get real')
	print(max(relativeIntensity))

	if (max(relativeIntensity) > 2){
		# xlimit = (max(relativeIntensity)/2) - (max(relativeIntensity)/10)
		xlimit = (max(relativeIntensity)/6)-(max(relativeIntensity)/10)
	}
	else{
		# xlimit = (max(relativeIntensity)/7 - (max(relativeIntensity)/10))
		xlimit = (max(relativeIntensity)/5)
	}
	for (i in 1:length(position)){
		if (relativeIntensity[i] > xlimit){
			terminateBegin = i
			print(terminateBegin)
			print(relativeIntensity[i])
			break
		}
	}

	position = position[terminateBegin:length(position)]
	relativeIntensity = relativeIntensity[terminateBegin:length(relativeIntensity)]

	for (i in length(position):1){
		if (relativeIntensity[i] > xlimit){
			terminateEnd = i
			print(terminateEnd)
			print(relativeIntensity[i])
			break
		}
	}

	position = position[1:terminateEnd]
	relativeIntensity = relativeIntensity[1:terminateEnd]
	data = data.frame(position, relativeIntensity)
}

approxData = function(data){
	# currently disabled. wanted to migrate spline from plot to get to work with it
	position = as.numeric(unlist(data[1]))
	relativeIntensity = as.numeric(unlist(data[2]))
	# splains = smooth.spline(position, relativeIntensity)
	# splains = spline(position, relativeIntensity)
	# splains = spline(position, relativeIntensity, method = "natural")
	# splains = smooth.spline(position, relativeIntensity, spar = 0.0001, all.knots=TRUE)
	# splains = smooth.spline(position, relativeIntensity, df = 2, spar = 0.0001, all.knots = TRUE)
	# splains = smooth.spline(position, relativeIntensity, df = 50, spar = 1e-7, all.knots = TRUE)
	splains = smooth.spline(position, relativeIntensity, spar = 1e-7, tol = 1e-6)
	# print(splains)
	# peaks = findpeaks(splains, minpeakdistance = 2, threshold = 0.1, npeaks = peakCount)
	# print(peaks)
}


plotData = function(i, data){
	position = as.numeric(unlist(data[1]))
	relativeIntensity = as.numeric(unlist(data[2]))
	# print(position)
	splains = smooth.spline(position, relativeIntensity, spar = 1e-7, tol = 1e-6)
	# print(splains)
	print(typeof(splains))
	plot.new()
	jpeg(paste('choprplot', toString(i), '.jpeg', sep=""), width = 1000, height = 500, units = "px", pointsize = 15)
	plot(position, relativeIntensity, col="gray35", xlab = "Position", ylab ="Relative intensity")
	# points(peakPositions, peaks, col = 'orangered', pch=19)
	lines(splains, col = "purple", lwd = 2)
	title(main = 'Junga dubultsprauga', cex.main = 2, font.main= 4, col.main= "black")

	incrementY = round(max(relativeIntensity) / 10, digits = 2)
	incrementX = round((max(position)-min(position)) / 10, digits = 3)
	beginX = round(min(position), digits = 2)
	abline(v=(seq(beginX, (max(relativeIntensity)+10), incrementX)), col="burlywood4", lty="dotted")
	abline(h=(seq(0, (max(position)+10), incrementY)), col="burlywood4", lty="dotted")
	dev.off()
	return(splains)
}

findPeaks = function(relativeIntensity, peakCount){
	# peaks = findpeaks(relativeIntensity, nups = 10, ndowns = nups,
	# 	minpeakheight = 0.1, minpeakdistance = 10, npeaks = peakCount)
	relativeIntensity = as.numeric(unlist(data[2]))
	peaks = findpeaks(relativeIntensity, minpeakdistance = 2, threshold = 0.1, npeaks = peakCount)
	# splains = as.numeric(unlist(splains))
	# peaks = findpeaks(splains, minpeakdistance = 2, threshold = 0.1, npeaks = peakCount)
	return(peaks)
}

# splineMagicBox = function(splains, peakCount){
# 	splains = as.numeric(unlist(splains))
# 	peaks = findpeaks(splains, minpeakdistance = 2, threshold = 0.1, npeaks = peakCount)
# }

peakPositionsToPlot = function(peaks, position){
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


for (i in 4:5){
	# izsauc visas funkcijas
	filename = csvInput(i)
	interpretation = filenameInterpret(alldata[i])
	data = processData()
	# peaks2 = approxData(data)
	# peakData = findPeaks(data, 10)
	# peakDataSplain = splineMagicBox(splains, 10)
	# peaks = peakData[,1]
	# position = as.numeric(unlist(data[1]))
	# peakPositions = peakPositionsToPlot(peakData, position)
	# print(peaks)
	# print(peakPositions)
	plotData(i, data)
}