### IGR-workshop script

### Libraries -----------------------------------------------------------------------------------------------------
library(dplR)

### Functions -----------------------------------------------------------------------------------------------------
# load functions from file R_code
source("workshop/allan/R_code.R")

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

#### master chronology for edge and interior

edge.chron <- chron(rwl.data[, which(cat.vec.rwl == "E")]) 
int.chron <- chron(rwl.data[, which(cat.vec.rwl == "I")])

plot(edge.chron)
plot(int.chron)

plot(edge.chron[, 1] ~ rownames(edge.chron), type = "l", xlab = "year", ylab = "ringwidth [mm]", col = "red")
lines(int.chron[, 1] ~ rownames(int.chron), col = "blue")

### Basal Area Increment calculations and plotting

head(metadata)
#transformation of the id
metadata$ID[1:20] <- paste0(substr(metadata$ID[1:20], 1, 4), "E", substr(metadata$ID[1:20], 5, 6)) # not working

diam.frame <- as.data.frame(matrix(nrow = 40, ncol = 2))
colnames(diam.frame) <- c("ID", "DBH.cm")

diam.frame[1:20, 1] <- paste0(substr(metadata$ID[1:20], 1, 4), "E", substr(metadata$ID[1:20], 5, 6))
diam.frame[21:40, 1] <- paste0(substr(metadata$ID[21:40], 1, 4), "I", substr(metadata$ID[21:40], 5, 6))
diam.frame[, 2] <- metadata$DBH.cm * 10 - 20 # bark thickness

### mean ring-width per tree

av.rw <- chron.by.stem(rwl.data, 1, 7)
tail(av.rw)

BAI <- bai.out(av.rw, diam.frame)
BAI.E <- bai.out(av.rw[, 1:20], diam = diam.frame[1:20,])
BAI.I <- bai.out(av.rw[, 21:40], diam = diam.frame[21:40,])


plot(BAI.E[, 1] ~ rownames(BAI.E), type = "l", xlab = "year", ylab = "BAI [mm]", col = "red",
     ylim = range(BAI.E, na.rm = TRUE))
for(i in 1:20){
  lines(BAI.E[, i] ~ rownames(BAI.E), col = "red")
}
for(i in 1:20){
  lines(BAI.I[, i] ~ rownames(BAI.E), col = "blue")
}

edge.chron.bai <- chron(BAI.E)
int.chron.bai <- chron(BAI.I)

plot(edge.chron.bai)
plot(int.chron.bai)

plot(edge.chron.bai[, 1] ~ rownames(edge.chron.bai), type = "l", xlab = "year", ylab = "BAI [mm^2]", 
     ylim = range(int.chron.bai, na.rm = TRUE), col = "red")
lines(int.chron.bai[, 1] ~ rownames(int.chron.bai), col = "blue")

### Common overlap period -------------------------------------------------------------------------------------------------
# we will focus on annual difference between edge and interior and only the common interval

cat.vec.BAI <- c(rep("E", 20), rep("I", 20))
BAI.ci <- common.interval(BAI)
BAI.comp <- matrix(nrow = 20, ncol = 142)

for(i in 1:71){
  BAI.comp[, ((i - 1) * 2 + 1)] <- as.numeric(BAI.ci[i, which(cat.vec.BAI == "E")])
  BAI.comp[, (i  * 2)] <- as.numeric(BAI.ci[i, which(cat.vec.BAI == "I")])
}

boxplot(BAI.comp, col = c("red", "blue"), outcol = c("red", "blue"), xlab = "years", ylab = "BAI", xaxt = "n")
axis(side = 1, at = seq(-1,  140, 10), labels = seq(1940, 2010, 5))

p.vals.BAI <- vector(mode = "numeric")
for(i in 1:71){
  p.vals.BAI[i] <- wilcox.test(BAI.comp[, i * 2 - 1], BAI.comp[, i * 2])$p.val
}

points(x = which(p.vals.BAI < 0.05) * 2 - 0.5, y = rep(-100, length(which(p.vals.BAI < 0.05))), pch = 16)
which(p.vals.BAI < 0.05) + 1940 # które lata?

### Principal Component Gradient Analysis PCGA ----------------------------------------------------------------------------
d.av.rw <- detrend(av.rw, method = "Ar")

PCGA.out <- PCGA(d.av.rw)
names(PCGA.out)
PCGA.out$imp
plot.PCGA(PCGA.out) # default rainbow
col_vec <- vector(mode = "character")
col_vec[which(substr(colnames(PCGA.out$pop), 5, 5) == "E")] <- "red"
col_vec[which(substr(colnames(PCGA.out$pop), 5, 5) == "I")] <- "blue"
plot.PCGA(PCGA.out, col.vec = col_vec)

# North versus East cores in whole group
plot(rwl.data)
d.rw <- detrend(rwl.data, method = "Ar")
PCGA.rw <- PCGA(d.rw)
col_rw <- vector(mode = "character")
col_rw[which(substr(colnames(PCGA.rw$pop), 8, 8) == "E")] <- "green"
col_rw[which(substr(colnames(PCGA.rw$pop), 8, 8) == "N")] <- "pink"

plot.PCGA(PCGA.rw, col.vec = col_rw)

### edge only
edge.rwl <- rwl.data[which(substr(colnames(rwl.data), 5, 5) == "E")]
d.edge.rw <- detrend(edge.rwl, method = "Ar")
PCGA.edge.rw <- PCGA(d.edge.rw)
col_edge.rw <- vector(mode = "character")
col_edge.rw[which(substr(colnames(PCGA.edge.rw$pop), 8, 8) == "E")] <- "red4"
col_edge.rw[which(substr(colnames(PCGA.edge.rw$pop), 8, 8) == "N")] <- "tomato"

plot.PCGA(PCGA.edge.rw, col.vec = col_edge.rw)

###interior  only
int.rwl <- rwl.data[which(substr(colnames(rwl.data), 5, 5) == "I")]
d.int.rw <- detrend(int.rwl, method = "Ar")
PCGA.int.rw <- PCGA(d.int.rw)
col_int.rw <- vector(mode = "character")
col_int.rw[which(substr(colnames(PCGA.int.rw$pop), 8, 8) == "E")] <- "navy"
col_int.rw[which(substr(colnames(PCGA.int.rw$pop), 8, 8) == "N")] <- "royalblue"

plot.PCGA(PCGA.int.rw, col.vec = col_int.rw)

# plotting edge and interior on the same panel
par(mfrow = c(2, 1))
plot.PCGA(PCGA.edge.rw, col.vec = col_edge.rw)
plot.PCGA(PCGA.int.rw, col.vec = col_int.rw)
par(mfrow = c(1, 1))

#### Early wood --------------------------------------------------------------------------------------------------------
### Loading data
early.data <- read.rwl("workshop/allan/MLPS_EW.rwl")
tail(early.data)
names(early.data)

# many chronologies in one plot
plot(early.data[, 1] ~ rownames(early.data), type = "l", xlab = "year", ylab = "earlywood [mm]", 
     ylim = range(early.data, na.rm = TRUE))

for(i in 1:80){
  lines(early.data[, i] ~ rownames(early.data))
}

cat.vec.rwl <- as.factor(substr(colnames(rwl.data), 5, 5))

for(i in which(cat.vec.rwl == "E")){
  lines(early.data[, i] ~ rownames(early.data), col = "red")
}

for(i in which(cat.vec.rwl == "I")){
  lines(early.data[, i] ~ rownames(early.data), col = "blue")
}

#### master chronology for edge and interior

early.edge.chron <- chron(early.data[, which(cat.vec.rwl == "E")]) 
early.int.chron <- chron(early.data[, which(cat.vec.rwl == "I")])

plot(early.edge.chron[, 1] ~ rownames(early.edge.chron), type = "l", xlab = "year", ylab = "earlywood [mm]", col = "red")
lines(early.int.chron[, 1] ~ rownames(early.int.chron), col = "blue")

d.early.rw <- detrend(early.data, method = "Ar")
PCGA.early <- PCGA(d.early.rw)
col_early <- vector(mode = "character")
col_early[which(substr(colnames(PCGA.early$pop), 5, 5) == "E")] <- "red"
col_early[which(substr(colnames(PCGA.early$pop), 5, 5) == "I")] <- "blue"
plot.PCGA(PCGA.early, col.vec = col_early)


#### Late wood --------------------------------------------------------------------------------------------------------
### Loading data
late.data <- read.rwl("workshop/allan/MLPS_LW.rwl")
tail(late.data)
names(late.data)

# many chronologies in one plot
plot(late.data[, 1] ~ rownames(late.data), type = "l", xlab = "year", ylab = "latewood [mm]", 
     ylim = range(late.data, na.rm = TRUE))

for(i in 1:80){
  lines(late.data[, i] ~ rownames(late.data))
}

cat.vec.rwl <- as.factor(substr(colnames(rwl.data), 5, 5))

for(i in which(cat.vec.rwl == "E")){
  lines(late.data[, i] ~ rownames(late.data), col = "red")
}

for(i in which(cat.vec.rwl == "I")){
  lines(late.data[, i] ~ rownames(late.data), col = "blue")
}

#### master chronology for edge and interior

late.edge.chron <- chron(late.data[, which(cat.vec.rwl == "E")]) 
late.int.chron <- chron(late.data[, which(cat.vec.rwl == "I")])

plot(late.int.chron[, 1] ~ rownames(late.int.chron), type = "l", xlab = "year", ylab = "latewood [mm]", col = "blue")
lines(late.edge.chron[, 1] ~ rownames(late.edge.chron), col = "red")

d.late.rw <- detrend(late.data, method = "Ar")
PCGA.late <- PCGA(d.late.rw)
col_late <- vector(mode = "character")
col_late[which(substr(colnames(PCGA.late$pop), 5, 5) == "E")] <- "red"
col_late[which(substr(colnames(PCGA.late$pop), 5, 5) == "I")] <- "blue"
plot.PCGA(PCGA.late, col.vec = col_late)

### Chronos --------------------------------------------------------------------------------------------------------------
# two different ones
chron.east <- chron(rwl.data[, which(substr(colnames(rwl.data), 8, 8) == "E")])
chron.north <- chron(rwl.data[, which(substr(colnames(rwl.data), 8, 8) == "N")])

plot(chron.east[, 1] ~ rownames(chron.east), type = "l", xlab = "year", ylab = "RW [mm]", col = "dodgerblue")
lines(chron.north[, 1] ~ rownames(chron.north), col = "orange")

# four different ones
east <- rwl.data[, which(substr(colnames(rwl.data), 8, 8) == "E")]
north <- rwl.data[, which(substr(colnames(rwl.data), 8, 8) == "N")]

east.edge <- east[, which(substr(colnames(east), 5, 5) == "E")]
east.int <- east[, which(substr(colnames(east), 5, 5) == "I")]
north.edge <- north[, which(substr(colnames(north), 5, 5) == "E")]
north.int <- north[, which(substr(colnames(north), 5, 5) == "I")]

chron.east.edge <- chron(east.edge)
chron.east.int <- chron(east.int)
chron.north.edge <- chron(north.edge)
chron.north.int <- chron(north.int)


plot(chron.east.edge[, 1] ~ rownames(chron.east.edge), type = "l", xlab = "year", ylab = "RW [mm]", col = "red")
lines(chron.east.int[, 1] ~ rownames(chron.east.int), col = "orange")
lines(chron.north.edge[, 1] ~ rownames(chron.north.edge), col = "blue")
lines(chron.north.int[, 1] ~ rownames(chron.north.int), col = "pink")

### Climate -----------------------------------------------------------------------------------------------------------
climate <- read.table("workshop/allan/SPEI3_MLPS.txt")
head(climate)
cor.spei.mat <- cor(cbind(PCGA.out$pop[PCGA.out$period,], climate[PCGA.out$period,]), use = "pairwise.complete", 
                    method = "spearman")[1:ncol(PCGA.out$pop), (ncol(PCGA.out$pop)+1):(ncol(PCGA.out$pop)+ncol(climate))]

corrplot::corrplot(cor.spei.mat)
corrplot::corrplot(cor.spei.mat, order = "alphabet")
boxplot(cor.spei.mat)
barplot(cor.spei.mat[PCGA.out$rank, "jun"], col = col.vec[PCGA.out$rank])
cor.test(cor.spei.mat[PCGA.out$rank, "jun"], 1:40, method = "spearman")
