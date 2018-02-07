### IGR-workshop script

library(dplR)

### Data loading --------------------------------------------------------------------------------------------------
list.files("workshop/allan")

metadata <- read.table("workshop/allan/MLPS_META.txt")
head(metadata)

cat.vec <- as.factor(c(rep("E", 20), rep("I", 20)))

plot(metadata$DBH.cm ~ cat.vec)

### Basal Area

BA <- (metadata$DBH.cm/2)^2*pi
plot(BA ~ cat.vec)

### Normality testing ---------------------------------------------------------------------------------------------

### distribution normality test > variance homoenity > t.test

tapply(metadata$DBH.cm, cat.vec, shapiro.test) # both subgroups are normally distributed
var.test(metadata$DBH.cm[which(cat.vec == "E")], metadata$DBH.cm[which(cat.vec == "I")])
t.test(metadata$DBH.cm[which(cat.vec == "E")], metadata$DBH.cm[which(cat.vec == "I")])

### if data not normally distributed 
wilcox.test(metadata$DBH.cm[which(cat.vec == "E")], metadata$DBH.cm[which(cat.vec == "I")])

### Tree rings -----------------------------------------------------------------------------------------------------

### Loading data
rwl.data <- read.rwl("workshop/allan/MLPS.rwl")
tail(rwl.data)

spag.plot(rwl.data) # spaghetti plot

# many chronologies in one plot
plot(rwl.data[, 1] ~ rownames(rwl.data), type = "l", xlab = "year", ylab = "ringwidth [mm]", 
     ylim = range(rwl.data, na.rm = TRUE))

for(i in 1:80){
  lines(rwl.data[, i] ~ rownames(rwl.data))
}

# adding some colors

cat.vec.rwl <- as.factor(substr(colnames(rwl.data), 5, 5))

for(i in which(cat.vec.rwl == "E")){
  lines(rwl.data[, i] ~ rownames(rwl.data), col = "red")
}

for(i in which(cat.vec.rwl == "I")){
  lines(rwl.data[, i] ~ rownames(rwl.data), col = "blue")
}
