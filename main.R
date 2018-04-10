getwd()
setwd("/home/vika/Documents/uni/4sem/LAB_4/4-Gaismas-interference/4data")
getwd()

alldata = list.files(pattern="*.csv")
# filename = "a004b025r340_1.csv"

csvInput = function(i){
#ielasa .csv failu
colNames = c('dateTime', 'Time', 'lightIntensity', 'relativeIntensity', 'angle', 'angularVelocity',
	'angularAcceleration', 'position', 'velocity', 'acceleration')
# alldata[i] vietā ielikt i-to lista alldata elementu
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

destroyTrash = function(filename){
# input: variable - filename; value - filename ielasītie dati
# output: divi vektori - pozīcija un relativeIntensity
# piešķir jaunu mainīgo relevantajām kolonām vai piešķir jaunu vērtību esošajiem mainīgajiem (tikai šīs 2 kolonas)
# relativeIntensity = data$relativeIntensity
# position = data$position
# [position, relativeIntensity] = filename[,c('position','relativeIntensity')]
# vai arī šis ir lieks un plotojot vnk lietot filename tabulu tā it kā es saprotu abstrakcijas
position = (filename[, 'position'])
relativeIntensity = (filename[, 'relativeIntensity'])
return(c(position, relativeIntensity))
}

plot = function(i){
# input ir destroyTrash output
# output = destroyTrash(filename)
# x = output[1]
# y = output[2]
# # print(head(output))
# print(output)

position = (filename[, 'position'])
relativeIntensity = (filename[, 'relativeIntensity'])
data = data.frame(position, relativeIntensity)
print(head(data))
newdata = na.omit(data)

print(head(newdata))

# create new dataset without missing data
# newdata <- na.omit(position) 
# print(length(newdata))
# print(length(relativeIntensity))

# print(is.na(relativeIntensity))
# print(newdata)
# print(position)
# print(head(filename))
# print(filename[, 'position'])
# print(filename[, 'relativeIntensity'])
# print(relativeIntensity)
# print(x)
# print(y)
# splains = smooth.spline(position, relativeIntensity)
# plot.new()
# jpeg(paste('rplot', toString(i), '.jpeg', sep=""), width = 900, height = 500, units = "px", pointsize = 10)
# plot(position, relativeIntensity, pch=20, xlim =c(0,3500), col = "gray25", xlab = "Position",
# 	ylab="Relative intensity", cex.axis = 1.5, cex.lab=1.5)
# lines(splains, col = "blue")
# title(main = 'Junga dubultsprauga', cex.main = 2, font.main= 4, col.main= "black")
# abline(v=(seq(0,3500,100)), col="burlywood4", lty="dotted")
# abline(h=(seq(0,12,0.5)), col="burlywood2", lty="dotted")
# dev.off()
}

magicBox = function(){
# this one calculates 3 lokālos maksimumus kur pirmais ir centra maksimums, un tad ir pārējie maksimumi
# atrod the maksimumu
# apgriež tur pa vidu

}

# i in 1:length(data)
for (i in 1:2){
	# izsauc visas funkcijas
	filename = csvInput(i)
	interpretation = filenameInterpret(alldata[i])
	allineed = plot(i)
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