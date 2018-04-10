library(pacman)
p_load(quantmod)
p_load(pracma)

getwd()
setwd("/home/vika/Documents/uni/4sem/LAB_4/4-Gaismas-interference/4data")
getwd()

alldata = list.files(pattern="*.csv")
# filename = "a004b025r340_1.csv"

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

plotData = function(i){
	position = (filename[, 'position'])
	relativeIntensity = (filename[, 'relativeIntensity'])
	dataRaw = data.frame(position, relativeIntensity)
	data = na.omit(dataRaw)

	position = data[, 'position']
	relativeIntensity = data[, 'relativeIntensity']
	#print(data[, 'position'])
	# splains = smooth.spline(position, relativeIntensity)
	# splains = spline(position, relativeIntensity)
	# splains = spline(position, relativeIntensity, method = "natural")
	splains = smooth.spline(position, relativeIntensity, spar = 0.001, all.knots=TRUE)
	plot.new()
	jpeg(paste('rplot', toString(i), '.jpeg', sep=""), width = 1000, height = 500, units = "px", pointsize = 10)
	plot(position, relativeIntensity, xlab = "Position", ylab ="Relative intensity")
	lines(splains, col = "blue")
	title(main = 'Junga dubultsprauga', cex.main = 2, font.main= 4, col.main= "black")
	# abline(v=(seq(0,3500,100)), col="burlywood4", lty="dotted")
	# abline(h=(seq(0,12,0.5)), col="burlywood2", lty="dotted")
	dev.off()
	return(relativeIntensity)
}

# plot(position, relativeIntensity, pch=20, xlim =c(0,3500)

magicBox = function(relativeIntensity, peakCount){
# this one calculates 3 lokālos maksimumus kur pirmais ir centra maksimums, un tad ir pārējie maksimumi
# atrod the maksimumu
# apgriež tur pa vidu
# peaks1=findPeaks(position, peakCount)
peaks2 = findpeaks(relativeIntensity, nups = 10, ndowns = nups, minpeakheight = 0.1, minpeakdistance = 10, npeaks = peakCount)
}

# i in 1:length(data)
for (i in 1:2){
	# izsauc visas funkcijas
	filename = csvInput(i)
	interpretation = filenameInterpret(alldata[i])
	allineed = plotData(i)
	# peaks = magicBox(relativeIntensity, 5)
	print(relativeIntensity)
}


# filename = csvInput(1)
# interpretation = filenameInterpret(filename)

# print(head(filename))
# print(head(alldata[1]))
# print(interpretation)

# list = filenameInterpret(filename)
# names(list) <- c("color", "ai", "bi", "turn")

# print(list)
# names(list)