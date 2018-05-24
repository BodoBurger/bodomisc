# variable v1 is coded 1, 2 or 3
# we want to attach value labels 1=red, 2=blue, 3=green

v1 = c(2,3,1,1,1,3,3,2,2,1)
v1 = factor(v1, levels = c(1,2,3), labels = c("red", "blue", "green"))

v11 = c(2,3,1,1,1,3,3,2,2,1)
v11 = factor(v11, levels = c(3,2,1), labels = c("red", "blue", "green"))


v111 = factor(c(2,3,1,1,1,3,3,2,2,1), labels = c("red", "blue", "green"))

v1111 = factor(c(2,3,1,1,1,3,3,2,2,1))
v1111 = factor(c(4,3,1,1,1,3,3,4,4,1))

# variable y is coded 1, 3 or 5
# we want to attach value labels 1=Low, 3=Medium, 5=High
v2 = c(3,3,3,1,1,1,1,5,5,3,1)
v2 = ordered(v2, levels = c(1,3, 5), labels = c("Low", "Medium", "High"))
v2


test <- factor(c("yes","no"))
levels(test)
[1] "no"  "yes"
as.numeric(test)
[1] 2 1

factorValueMapping = function(x) {
  data.frame(levels = unique(x), value = as.numeric(unique(x)))
}


