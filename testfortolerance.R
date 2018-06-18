# for (i in 1:10){
# 	print(i)
# }

# for (i in seq(1, 10, by=0.5)) {print(i)}

l <- 1:1000
mean(l[l %% 3 == 0 | l %% 5 == 0])
newposition = 10
l = seq(newposition-0.01, newposition+0.01, by=0.001)
mean(l)