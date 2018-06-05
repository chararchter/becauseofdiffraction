dd <- data.frame(b = factor(c("Hi", "Med", "Hi", "Low"), 
      levels = c("Low", "Med", "Hi"), ordered = TRUE),
      x = c("A", "D", "A", "C"), y = c(8, 3, 9, 9),
      z = c(1, 1, 1, 2))

# sorto y kolonu augoši
sorted = dd[with(dd, order(y)), ]

print(dd)
print(sorted)

# n = 4
# for (i in length(dd)){
# 	gabals = 0
# 	while gabals < n {
		
# 	}
# }


a = c(3,4,5,6,7)
print(a)
for (i in length(a)){
	if (a[i] > 6){
		print(i)
		print(a[i])
	} 
}


print(a[2:length(a)])

# from code:
# 	sort vectors
# 	data = data[with(data, order(data[1])), ]
# 	cut vectors smaller to avoid noise
# 	possible improvement: find the first and last peak of min height 0.01 and cut 5 indexes before that
# 	# hoe don't do that works horrible
# 	position = data[, 'position']
# 	relativeIntensity = data[, 'relativeIntensity']
# 	skaits = 4
# 	relativeIntensity  = aggregate(relativeIntensity, by=list(0:(length(relativeIntensity)-1) %/% skaits), mean)
# 	position  = aggregate(position, by=list(0:(length(position)-1) %/% skaits), mean)

# high threshold - viens pīķis
# low threshold - divi pīķi

# list = filenameInterpret(filename)
# names(list) <- c("color", "ai", "bi", "turn")

# print(list)
# names(list)
