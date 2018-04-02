getwd()
setwd("/home/vika/Documents/uni/4sem/LAB_4/4-Gaismas-interference/4data")
getwd()

alldata = list.files(pattern="*.csv")

csvInput = function(filename){
#ielasa .csv failu
}

filenameInterpret = function(filename){
}

destroyTrash = function(vertibafilenamemainigajam){
# piešķir jaunu mainīgo relevantajām kolonām vai
# piešķir jaunu vērtību esošajiem mainīgajiem (tikai šīs 2 kolonas)
# relativeIntensity = data$relativeIntensity
# position = data$position
}

plot = function(){
# input ir destroyTrash output
# taisa grafiku un splainus
}

magicBox = function(){
# this one calculates 3 lokālos maksimumus kur pirmais ir centra maksimums, un tad ir pārējie maksimumi
}


# for (i in 1:length(data)){
	# izsauc visas funkcijas
# }

word = "a004b025r340_1.csv"
a = c('004','008')
b = c('025','055','05','005')
colors = c('g', 'r')
distances = c('340','440','580')
measurementCounter=c(sprintf("_%.f", seq(1,4,1)))

shit = function(word, parameter){
	for (char in 1:length(word)){
		for (i in 1:length(parameter)){
			if (grepl(parameter[i], word[char])){
				return(parameter[i])
			}
		}
	}
}


color = shit(word, colors)
ai = shit(word, a)
bi = shit(word, b)
distance = shit(word, distances)
turn = shit(word, measurementCounter)

print(word)
print(color)
print(ai)
print(bi)
print(distance)
print(turn)