getwd()
setwd("/home/vika/Documents/uni/4sem/LAB_4/4-Gaismas-interference/4data")
getwd()

alldata = list.files(pattern="*.csv")
# print(length(alldata))

csvInput = function(i){
	setwd("/home/vika/Documents/uni/4sem/LAB_4/4-Gaismas-interference/4data")
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
	# get index of the maximum position
	begin = 10
	for (i in 1:length(position)){
		if (position[i] == max(position)){
			end = i
			# print(end)
			break
		}			
	}
	# chop the vector when max position is reached
	# physical interpretation - sensor goes backwards
	relativeIntensity = relativeIntensity[begin:end]
	position = position[begin:end]
	data = data.frame(position, relativeIntensity)
	# sort according to position
	data = data[with(data, order(position)), ]
	position = data[, 'position']
	relativeIntensity = data[, 'relativeIntensity']
	# filter out noise - small relative intensities at the begining and end
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
			# print(terminateBegin)
			# print(relativeIntensity[i])
			break
		}
	}
	position = position[terminateBegin:length(position)]
	relativeIntensity = relativeIntensity[terminateBegin:length(relativeIntensity)]

	for (i in length(position):1){
		if (relativeIntensity[i] > xlimit){
			terminateEnd = i
			# print(terminateEnd)
			# print(relativeIntensity[i])
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
	splains = smooth.spline(position, relativeIntensity, spar = 1e-7, tol = 1e-6)
	# peaks = findpeaks(splains, minpeakdistance = 2, threshold = 0.1, npeaks = peakCount)
	# print(peaks)
}


plotData = function(i, data){
	setwd("/home/vika/Documents/uni/4sem/LAB_4/4-Gaismas-interference/plotoutput")
	position = as.numeric(unlist(data[1]))
	relativeIntensity = as.numeric(unlist(data[2]))
	# splains = smooth.spline(position, relativeIntensity, spar = 1e-7, tol = 1e-6)
	fit1 = nls(relativeIntensity~(A*position^5 + B*position^4 + C*position^3 + D*position^2 + E*position + F),
		data=data, start=list("A"=0.000001, "B"=0.00001, "C"=0.001,"D"=0.01, "E"=0.1, "F"=1))
	# print(summary(fit1))

	# fitfun = function(A,B,C,D,E,F,position){A*position^5 + B*position^4 + C*position^3 + D*position^2 + E*position + F}
	# a1=coef(fit1)[1]
	# print(fit1)
	# print(splains)
	# print(typeof(splains))

	# plot.new()
	# jpeg(paste('choprplot', toString(i), '.jpeg', sep=""), width = 1000, height = 500, units = "px", pointsize = 15)
	# plot(position, relativeIntensity, col="gray35", xlab = "Position", ylab ="Relative intensity")
	# # points(peakPositions, peaks, col = 'orangered', pch=19)
	# lines(splains, col = "purple", lwd = 2)
	# lines(fit1, col = "green", lwd = 2)
	# title(main = 'Junga dubultsprauga', cex.main = 2, font.main= 4, col.main= "black")

	# incrementY = round(max(relativeIntensity) / 10, digits = 2)
	# incrementX = round((max(position)-min(position)) / 10, digits = 3)
	# beginX = round(min(position), digits = 2)
	# abline(v=(seq(beginX, (max(relativeIntensity)+10), incrementX)), col="burlywood4", lty="dotted")
	# abline(h=(seq(0, (max(position)+10), incrementY)), col="burlywood4", lty="dotted")
	# dev.off()
	# return(splains)
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

determineLambda = function(color){
	if (color == 'g'){
		lambda = 532 * 10^(-9)
	}
	else{
		lambda = 650 * 10^(-9)
	}
}

determineLinearSeparation = function(L, d, lambda){
	linSep = L * lambda / d
}


peakFinder = function(data, relativeIntensity, position, linSep){
	indexofmax = which.max(relativeIntensity)
	max1 = position[indexofmax]
	newposition = max1 - (linSep/2)
	indexofmin = match(newposition, position)

	# problem - what to do if there is no this exact position?
	# find closest existing position to newposition
	if (is.na(indexofmin)){
		indexofmin = which.min(abs(position - newposition))
		# print(indexofmin)
		min1 = position[indexofmin] 
	}
	else {
		min1 = position[indexofmin]
	}
	# print(min1)
	# print(indexofmin)
	position = position[1:indexofmin]
	relativeIntensity = relativeIntensity[1:indexofmin]
	indexofmax2 = which.max(relativeIntensity)
	max2 = position[indexofmax2]
	maximums = c(max1, max2)
	indexofmaximums = c(indexofmax, indexofmax2)
	data_peaks = data.frame(maximums, indexofmaximums)
	return(data_peaks)
}

calculateResults = function(d, diffMax, L){
	cal_lambda = (d * diffMax) / (L * 1)
}

for (i in 1:2){
	# izsauc visas funkcijas
	filename = csvInput(i)
	interpretation = filenameInterpret(alldata[i])
	print(alldata[i])
	print(interpretation)
	d = as.numeric(interpretation[3])/100
	data = processData()
	position = as.numeric(unlist(data[1]))
	relativeIntensity = as.numeric(unlist(data[2]))
	lambda = determineLambda(interpretation[1])
	# print(lambda)
	L = as.numeric(interpretation[4])
	print(L)
	linSep = determineLinearSeparation(L, d, lambda)
	# print(linSep)
	peaks = peakFinder(data, relativeIntensity, position, linSep)
	print(peaks)
	maximums = peaks[, 'maximums']
	diffMax = abs(maximums[1] - maximums[2])
	print(diffMax)
	cal_lambda = calculateResults(d, diffMax, L)
	print(cal_lambda)
	# name = sprintf("%s%s%s%s%s.csv", interpretation[1], interpretation[2], interpretation[3], interpretation[4], interpretation[5])
	# print(name)
	# data = processData()
	# setwd("/home/vika/Documents/uni/4sem/LAB_4/4-Gaismas-interference/clearcsv")
	# write.csv(x=data, file=paste(sprintf("%s%s%s%s%s.csv", interpretation[1], interpretation[2], interpretation[3], interpretation[4], interpretation[5])))
	# plotData(i, data)

	# peaks2 = approxData(data)
	# peakData = findPeaks(data, 10)
	# peakDataSplain = splineMagicBox(splains, 10)
	# peaks = peakData[,1]
	# position = as.numeric(unlist(data[1]))
	# peakPositions = peakPositionsToPlot(peakData, position)
	# print(peaks)
	# print(peakPositions)
	# plotData(i, data)
}