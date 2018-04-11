library(pacman)
# p_load(quantmod)
p_load(pracma)

getwd()
setwd("/home/vika/Documents/uni/4sem/LAB_4/4-Gaismas-interference/4data")
getwd()

alldata = list.files(pattern="*.csv")

csvInput = function(i){
	#ielasa .csv failu
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
	begin = 10
	end = length(position)/2
	position = position[begin:end]
	relativeIntensity = relativeIntensity[begin:end]
	data = data.frame(position, relativeIntensity)
	# print(typeof(data))
	# print(typeof(position))
	# return(c(position, relativeIntensity))
}

plotData = function(i, data){
	# splains = smooth.spline(position, relativeIntensity)
	# splains = spline(position, relativeIntensity)
	# splains = spline(position, relativeIntensity, method = "natural")
	position = as.numeric(unlist(data[1]))
	relativeIntensity = as.numeric(unlist(data[2]))
	print(typeof(position))
	splains = smooth.spline(position, relativeIntensity, spar = 0.001, all.knots=TRUE)
	plot.new()
	jpeg(paste('rplot', toString(i), '.jpeg', sep=""), width = 1000, height = 500, units = "px", pointsize = 10)
	plot(position, relativeIntensity, xlab = "Position", ylab ="Relative intensity")
	lines(splains, col = "blue")
	title(main = 'Junga dubultsprauga', cex.main = 2, font.main= 4, col.main= "black")
	abline(v=(seq(0,0.020,0.002)), col="burlywood2", lty="dotted")
	abline(h=(seq(0,3,0.2)), col="burlywood4", lty="dotted")
	dev.off()
	# return(relativeIntensity)
}

# plot(position, relativeIntensity, pch=20, xlim =c(0,3500)

magicBox = function(relativeIntensity, peakCount){
	# this one calculates 3 lokālos maksimumus kur pirmais ir centra maksimums,
	# un tad ir pārējie maksimumi
	# peaks2 = findpeaks(relativeIntensity, nups = 10, ndowns = nups,
	# 	minpeakheight = 0.1, minpeakdistance = 10, npeaks = peakCount)
	peaks2 = findpeaks(relativeIntensity, minpeakdistance = 10, threshold = 1, npeaks = peakCount)
	return(peaks2)
}

# _plotPeaks = function(){

# }

# i in 1:length(data)
for (i in 1:2){
	# izsauc visas funkcijas
	filename = csvInput(i)
	interpretation = filenameInterpret(alldata[i])
	data = processData()
	relativeIntensity = plotData(i, data)
	# print(relativeIntensity)
	# peaks = magicBox(relativeIntensity, 5)
	# print(peaks)
}

 # The first column gives the height,
 # the second the position/index where the maximum is reached,
 # the third and forth the indices of where the peak begins and ends
 # --- in the sense of where the pattern starts and ends. 
# high threshold - viens pīķis
# low threshold - divi pīķi

# filename = csvInput(1)
# interpretation = filenameInterpret(filename)

# print(head(filename))
# print(head(alldata[1]))
# print(interpretation)

# list = filenameInterpret(filename)
# names(list) <- c("color", "ai", "bi", "turn")

# print(list)
# names(list)