### IGR-workshop own work script - OMPS stand

### Libraries -----------------------------------------------------------------------------------------------------
library(dplR)
library(tidyverse)
library(ggplot2)
library(SPEI)

### Functions -----------------------------------------------------------------------------------------------------
# load functions from file R_code
source("workshop/allan/R_code.R")

### Data loading --------------------------------------------------------------------------------------------------
list.files("workshop/data")

OMPS_metadata <- read_csv2("workshop/data/OMPS_metadata.csv")

OMPS_metadata %>%
  mutate(crown.length = height - crown) -> OMPS_metadata

cat.vec <- as.factor(c(rep("E", 30), rep("I", 30)))

plot(OMPS_metadata$dbh ~ cat.vec)
wilcox.test(OMPS_metadata$dbh[which(cat.vec == "E")], OMPS_metadata$dbh[which(cat.vec == "I")])

plot(OMPS_metadata$height ~ cat.vec)
wilcox.test(OMPS_metadata$height[which(cat.vec == "E")], OMPS_metadata$height[which(cat.vec == "I")])

plot(OMPS_metadata$crown ~ cat.vec)
wilcox.test(OMPS_metadata$crown[which(cat.vec == "E")], OMPS_metadata$crown[which(cat.vec == "I")])

plot(OMPS_metadata$crown.length ~ cat.vec)
wilcox.test(OMPS_metadata$crown.length[which(cat.vec == "E")], OMPS_metadata$crown.length[which(cat.vec == "I")])

### Tree rings -----------------------------------------------------------------------------------------------------

### Loading data
interior_data <- read.rwl("workshop/data/OMPSI.rwl")
edge_data <- read.rwl("workshop/data/OMPSE.rwl")

om_data <- combine.rwl(edge_data, interior_data)

# many chronologies in one plot
plot(om_data[, 1] ~ rownames(om_data), type = "l", xlab = "year", ylab = "ringwidth [mm]", 
     ylim = range(om_data, na.rm = TRUE))

cat.vec.rwl <- as.factor(substr(colnames(om_data), 5, 5))

for(i in which(cat.vec.rwl == "E")){
  lines(om_data[, i] ~ rownames(om_data), col = "red")
}

for(i in which(cat.vec.rwl == "I")){
  lines(om_data[, i] ~ rownames(om_data), col = "blue")
}

#### master chronology for edge and interior

edge_chron <- chron(edge_data)
interior_chron <- chron(interior_data)

plot(edge_chron[, 1] ~ rownames(edge_chron), type = "l", xlab = "year", ylab = "ringwidth [mm]", col = "red")
lines(interior_chron[, 1] ~ rownames(interior_chron), col = "blue")

### Basal Area Increment calculations and plotting
### mean ring-width per tree
av_rw <- chron.by.stem(om_data, 1, 7)
tail(av_rw)

OMPS_metadata %>%
  mutate(ID = substr(TreeID, 1, 7)) %>%
  group_by(ID) %>%
  summarise(av_dbh = mean(dbh) * 10 - 30) -> diam_frame

diam_frame <- as.data.frame(diam_frame)

BAI <- bai.out(av_rw, diam_frame)
BAI_E <- bai.out(av_rw[, 1:15], diam = diam_frame[1:15,])
BAI_I <- bai.out(av_rw[, 16:30], diam = diam_frame[16:30,])

plot(BAI_E[, 1] ~ rownames(BAI_E), type = "l", xlab = "year", ylab = "BAI [mm]", col = "red",
     ylim = range(BAI_E, na.rm = TRUE))
for(i in 1:14){
  lines(BAI_E[, i] ~ rownames(BAI_E), col = "red")
}
for(i in 1:14){
  lines(BAI_I[, i] ~ rownames(BAI_I), col = "blue")
}

edge_chron_bai <- chron(BAI_E)
int_chron_bai <- chron(BAI_I)

plot(edge_chron_bai[, 1] ~ rownames(edge_chron_bai), type = "l", xlab = "year", ylab = "BAI [mm^2]", 
     col = "red", ylim = range(0, 4000))
lines(int_chron_bai[, 1] ~ rownames(int_chron_bai), col = "blue")

### Common overlap period -------------------------------------------------------------------------------------------------
# we will focus on annual difference between edge and interior and only the common interval

cat_vec_BAI <- c(rep("E", 15), rep("I", 15))
BAI_ci <- common.interval(BAI)
BAI_comp <- matrix(nrow = 15, ncol = 132) # rows = number of trees and cols = double common years

for(i in 1:66){
  BAI_comp[, ((i - 1) * 2 + 1)] <- as.numeric(BAI_ci[i, which(cat_vec_BAI == "E")])
  BAI_comp[, (i  * 2)] <- as.numeric(BAI_ci[i, which(cat_vec_BAI == "I")])
}

boxplot(BAI_comp, col = c("red", "blue"), outcol = c("red", "blue"), xlab = "years", ylab = "BAI", xaxt = "n")
axis(side = 1, at = seq(-2, 140, 10), labels = seq(1950, 2020, 5))

p_vals_BAI <- vector(mode = "numeric")
for(i in 1:58){
  p_vals_BAI[i] <- wilcox.test(BAI_comp[, i * 2 - 1], BAI_comp[, i * 2])$p.val
}

points(x = which(p_vals_BAI < 0.05) * 2 - 0.5, y = rep(0, length(which(p_vals_BAI < 0.05))), pch = 16)
which(p_vals_BAI < 0.05) + 1940 # które lata?

### Chronos --------------------------------------------------------------------------------------------------------------
# two different ones
chron_east <- chron(om_data[, which(substr(colnames(om_data), 8, 8) == "E")])
chron_south <- chron(om_data[, which(substr(colnames(om_data), 8, 8) == "S")])

plot(chron_east[, 1] ~ rownames(chron_east), type = "l", xlab = "year", ylab = "RW [mm]", col = "dodgerblue")
lines(chron_south[, 1] ~ rownames(chron_south), col = "orange")

# four different ones
east <- om_data[, which(substr(colnames(om_data), 8, 8) == "E")]
south <- om_data[, which(substr(colnames(om_data), 8, 8) == "S")]

east_edge <- east[, which(substr(colnames(east), 5, 5) == "E")]
east_int <- east[, which(substr(colnames(east), 5, 5) == "I")]
south_edge <- south[, which(substr(colnames(south), 5, 5) == "E")]
south_int <- south[, which(substr(colnames(south), 5, 5) == "I")]

chron_east_edge <- chron(east_edge)
chron_east_int <- chron(east_int)
chron_south_edge <- chron(south_edge)
chron_south_int <- chron(south_int)

plot(chron_east_edge[, 1] ~ rownames(chron_east_edge), type = "l", xlab = "year", ylab = "RW [mm]", 
     col = "red4", lwd = 3, ylim = range(1:8))
lines(chron_east_int[, 1] ~ rownames(chron_east_int), col = "navy", lwd = 3)
lines(chron_south_edge[, 1] ~ rownames(chron_south_edge), col = "tomato", lwd = 3)
lines(chron_south_int[, 1] ~ rownames(chron_south_int), col = "royalblue", lwd = 3)

### Principal Component Gradient Analysis PCGA ----------------------------------------------------------------------------
d_av_rw <- detrend(av_rw, method = "Ar")
PCGA_out <- PCGA(d_av_rw)
PCGA_out$imp
col_vec <- vector(mode = "character")
col_vec[which(substr(colnames(PCGA_out$pop), 5, 5) == "E")] <- "red"
col_vec[which(substr(colnames(PCGA_out$pop), 5, 5) == "I")] <- "blue"
plot.PCGA(PCGA_out, col.vec = col_vec)

# North versus East cores in whole group
d_rw <- detrend(om_data, method = "Ar")
PCGA_rw <- PCGA(d_rw)
col_rw <- vector(mode = "character")
col_rw[which(substr(colnames(PCGA_rw$pop), 8, 8) == "E")] <- "green"
col_rw[which(substr(colnames(PCGA_rw$pop), 8, 8) == "S")] <- "pink"
plot.PCGA(PCGA_rw, col.vec = col_rw)

### edge only
d_edge_rw <- detrend(edge_data, method = "Ar")
PCGA_edge_rw <- PCGA(d_edge_rw)
col_edge_rw <- vector(mode = "character")
col_edge_rw[which(substr(colnames(PCGA_edge_rw$pop), 8, 8) == "E")] <- "red4"
col_edge_rw[which(substr(colnames(PCGA_edge_rw$pop), 8, 8) == "S")] <- "tomato"
plot.PCGA(PCGA_edge_rw, col.vec = col_edge_rw)

###interior  only
d_int_rw <- detrend(interior_data, method = "Ar")
PCGA_int_rw <- PCGA(d_int_rw)
col_int_rw <- vector(mode = "character")
col_int_rw[which(substr(colnames(PCGA_int_rw$pop), 8, 8) == "E")] <- "navy"
col_int_rw[which(substr(colnames(PCGA_int_rw$pop), 8, 8) == "S")] <- "royalblue"
plot.PCGA(PCGA_int_rw, col.vec = col_int_rw)

# plotting edge and interior on the same panel
par(mfrow = c(1, 2))
plot.PCGA(PCGA_edge_rw, col.vec = col_edge_rw)
plot.PCGA(PCGA_int_rw, col.vec = col_int_rw)
par(mfrow = c(1, 1))

### Climate -----------------------------------------------------------------------------------------------------------
om_climate <- read.table("workshop/climate/OMPS_climate.txt")
head(om_climate)
# 
# gb_climate %>%
#   select(T.1:T.12) %>%
#   mutate(year = rownames(.)) %>%
#   gather(month, temp, T.1:T.12) %>%
#   mutate(month = substr(.$month, 3, 4)) %>%
#   arrange(year, month) -> gb_temp
# 
# gb_climate %>%
#   select(P.1:P.12) %>%
#   mutate(year = rownames(.)) %>%
#   gather(month, prcp, P.1:P.12) %>%
#   mutate(month = substr(.$month, 3, 4)) %>%
#   arrange(year, month) -> gb_prcp

t_mat <- om_climate[, 1:12]
p_mat <- om_climate[, 13:24]

PET <- thornthwaite(as.numeric(t(t_mat)), 53.958694)

spei3 <- spei(as.numeric(t(p_mat)) - PET, scale = 3)

spei_mat <- matrix(ncol = 12, nrow = nrow(om_climate))
for(i in 1:12){
  spei_mat[, i] <- as.numeric(spei3$fitted)[seq(i, length(as.numeric(spei3$fitted)), 12)]
}

head(spei_mat)
colnames(spei_mat) <- paste0("spei", 1:12)
rownames(spei_mat) <- rownames(om_climate)

cor_spei_mat_climate <- cor(cbind(PCGA_out$pop[PCGA_out$period,], om_climate[PCGA_out$period,]), 
                    use = "pairwise.complete", 
                    method = "spearman")[1:ncol(PCGA_out$pop), 
                                         (ncol(PCGA_out$pop)+1):(ncol(PCGA_out$pop)+ncol(om_climate))]

period <- intersect(rownames(om_climate), PCGA_out$period)
cor_spei_mat_spei <- cor(cbind(PCGA_out$pop[period,], spei_mat[period,]), 
                            use = "pairwise.complete", 
                            method = "spearman")[1:ncol(PCGA_out$pop), 
                                                 (ncol(PCGA_out$pop)+1):(ncol(PCGA_out$pop)+ncol(spei_mat))]
corrplot::corrplot(cor_spei_mat_climate)

# pval <- psych::corr.test(cbind(PCGA_out$pop[period,], spei_mat[period,]), method = "spearman", adjust="holm")$p
corrplot::corrplot(cor_spei_mat_spei, method = "ellipse")
corrplot::corrplot(cor_spei_mat_spei, method = "pie")

corrplot::corrplot(cor_spei_mat, order = "alphabet")
boxplot(cor_spei_mat_spei)
barplot(cor_spei_mat_spei[PCGA_out$rank, "spei7"], col = col_vec[PCGA_out$rank])
cor.test(cor_spei_mat_spei[PCGA_out$rank, "spei7"], 1:30, method = "spearman")

