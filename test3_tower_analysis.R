library(readr)
library(lubridate)
library(dplyr)
library(openair)

setwd("~/Documents/ADMSUrban/test3/tower_snaqs/output")
######################################
#This section processes raw output

data <- read_csv("./test3_pst.csv")
names(data)[9:16] <- c("NOx_as_NO2", "NO2", "O3", "CO", "VOC", "SO2", "PM2.5", "PM10")

sdata <- split(data, data$'Receptor name')

receptors <- levels(as.factor(data$'Receptor name')) 
mod <- paste(receptors, "_mod", sep = "")
obs <- paste(receptors, "_obs", sep = "")

date <- seq(from=as.POSIXct("2016-11-1 00:00:00",tz="Asia/Shanghai"),
            to=as.POSIXct("2016-12-31 23:00:00",tz="Asia/Shanghai"),by="hour")

#bgd <- cbind(date, read.table("cams_2016.11-12_ADMS.bgd", skip = 22, sep = ",")[-c(1:3)])
#names(bgd)[-1] <- c("pm10", "pm2.5", "co", "so2", "o3", "voc", "no2", "nox") 

#separate the list into 6 data frames, rename cols etc. 
for (i in 1:length(receptors)) {
  x <- as.data.frame(sdata[i])
  names(x) <- names(data)
  x <- cbind(date,x)[-c(2:5,7:9)]
  x$NO <- (x$NOx_as_NO2 - x$NO2) / 46 * 30
  x$NOx_true <- x$NO + x$NO2
  assign(mod[i], x)
  rm(x)
}


for (i in 1:length(receptors)) {
  y <- read_csv(file = paste("./obs/", receptors[i], "_obs.csv", sep = ""))
  y$date <- force_tz(y$date, tzone = "Asia/Shanghai")
  x <- get(mod[i])
  z <- inner_join(y, x, by = "date")
  write.csv(z, paste(receptors[i], "_obs_mod.csv", sep = ""), row.names = F)
  assign(receptors[i], z)
  rm(x, y, z)
}

################################################################
#This section plots PBLH

date1 <- seq(from=as.POSIXct("2016-11-1 00:00:00",tz="Asia/Shanghai"),
            to=as.POSIXct("2016-12-31 23:00:00",tz="Asia/Shanghai"),by="hour")
mop <- data.frame(date1, read_csv("test3_MOP.csv"))

date2 <- seq(from=as.POSIXct("2016-10-1 00:00:00",tz="Asia/Shanghai"),
             to=as.POSIXct("2016-11-30 23:00:00",tz="Asia/Shanghai"),by="hour")
old <- data.frame(date2, read_csv("~/Documents/ADMSUrban/test2/tower_snaq/output/test2_MOP.csv"))

obs <- read_csv("~/Documents/beijing/boundary_layer/MLH_IAP_2016-2017_V1.0.csv") %>%
  rename(date = `TIME [CST]`) %>% 
  timeAverage(avg.time = "hour")
obs$date <- force_tz(obs$date, tzone = "Asia/Shanghai")

data <- data.frame(date = obs$date[1:648], obs = obs$`MLH [m a.g.l.]`[1:648],
                     run3 = mop$H_1[73:720], run2 = old$H_1[817:1464])

breaks <- seq(as.POSIXct("2016-11-4 0:00", tz="Asia/Shanghai"), as.POSIXct("2016-12-31 0:00", tz="Asia/Shanghai"), by = "day")
text <- format(breaks, "%b %d")

dev.new()
plot(data$date, data$obs, type = "l", ylim = c(0, 2500), 
     xlab = "", ylab = "PBLH (m)", xaxt = "n")
axis(side = 1, at = breaks, labels = text)
lines(data$date, data$run2, col = "blue")
lines(data$date, data$run3, col = "forestgreen")
legend("topright", lty = c(1,1,1), col = c("black", "forestgreen", "blue"), 
       legend = c("measurements", "run 3", "run 2"), text.col = c("black", "forestgreen", "blue"))
dev.print(pdf, "PBLH.pdf")

modStats(data, obs = "obs", mod = "run2")
modStats(data, obs = "obs", mod = "run3")


#########################################
#This section plots timeseries

#load run 3
files <- list.files(pattern = "obs_mod.csv", full.names = F)
receptors <- paste("SNAQ", c(39,40,41,44,45,46), sep = "")

start <- as.POSIXct("2016-11-9 12:00:00", tz = "Asia/Shanghai")
end <- as.POSIXct("2016-11-23 6:00:00", tz = "Asia/Shanghai")

for (i in 1 : 6) {
  x <- read_csv(files[i])
  x$date <- force_tz(x$date, tzone = "Asia/Shanghai")
  x <- subset(x, date >= start & date <= end)
  assign(receptors[i], x)
}

#load run 2
for (i in 1:6) {
  x <- read_csv(paste("../../../test2/tower_snaq/output/", files[i], sep = ""))
  x$date <- force_tz(x$date, tzone = "Asia/Shanghai")
  x <- subset(x, date >= start & date <= end)
  assign(paste(receptors[i], "_old", sep = ""), x)
}

#for plotting
receptors_new <- receptors[c(1:3,6,5,4)]
receptors_names <- c("8m", "32m", "102m", "160m", "260m", "320m")
cols <- c("blue", "forestgreen", "red", "dimgrey", "darkgoldenrod", "purple")
breaks <- seq(as.POSIXct("2016-11-10 0:00", tz="Asia/Shanghai"), as.POSIXct("2016-11-23 0:00", tz="Asia/Shanghai"), by = "day")
text <- format(breaks, "%b %d")

dev.new()
par(mfrow = c(3,1))#, lwd = 1.5, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
plot(SNAQ39$date, SNAQ39$co, type = "l",  ylim = c(0,5.5), xaxt = "n",
     xlab = "", ylab = "CO (mg/m³)", col = cols[1], main = "Observations")
axis(side = 1, at = breaks, labels = text)
#add lines
for (i in 2:6) {
  y <- get(receptors_new[i])
  points(y$date, y$co, type = "l", col = cols[i])
}
legend(x = "topright", c(receptors_names), text.col=c(cols))

plot(SNAQ39_old$date, SNAQ39_old$CO, type = "l", col = cols[1], xaxt = "n",
     xlab = "", ylab = "CO (mg/m³)", ylim = c(0,5.5), main = "Model run 2 (MEIC2010)")
axis(side = 1, at = breaks, labels = text)
for (i in 2:6) {
  x <- get(paste(receptors_new[i], "_old", sep = ""))
  points(x$date, x$CO, type = "l", col = cols[i])
}
legend(x = "topright", c(receptors_names), text.col=c(cols))

plot(SNAQ39$date, SNAQ39$CO, type = "l", col = cols[1], xaxt = "n",
     xlab = "", ylab = "CO (mg/m³)", ylim = c(0,5.5), main = "Model run 3 (MEIC2013)")
axis(side = 1, at = breaks, labels = text)
for (i in 2:6) {
  x <- get(receptors_new[i])
  points(x$date, x$CO, type = "l", col = cols[i])
}
legend(x = "topright", c(receptors_names), text.col=c(cols))

dev.print(pdf, "CO_obs_mod.pdf")


dev.new()
par(mfrow = c(3,1))#, lwd = 1.5, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
plot(SNAQ39$date, SNAQ39$nox_as_no2, type = "l",  ylim = c(0,720), xaxt = "n",
     xlab = "", ylab = "NOx (µg/m³)", col = cols[1], main = "Observations")
axis(side = 1, at = breaks, labels = text)
for (i in 2:6) {
  y <- get(receptors_new[i])
  points(y$date, y$nox_as_no2, type = "l", col = cols[i])
}
legend(x = "topright", c(receptors_names), text.col=c(cols))

plot(SNAQ39_old$date, SNAQ39_old$NOx_as_NO2, type = "l", col = cols[1], xaxt = "n",
     xlab = "", ylab = "NOx (µg/m³)", ylim = c(0,720), main = "Model run 2 (MEIC2010)")
axis(side = 1, at = breaks, labels = text)
for (i in 2:6) {
  x <- get(paste(receptors_new[i], "_old", sep = ""))
  points(x$date, x$NOx_as_NO2, type = "l", col = cols[i])
}
legend(x = "topright", c(receptors_names), text.col=c(cols))

plot(SNAQ39$date, SNAQ39$NOx_as_NO2, type = "l", col = cols[1], xaxt = "n",
     xlab = "", ylab = "NOx (µg/m³)", ylim = c(0,720), main = "Model run 3 (MEIC2013)")
axis(side = 1, at = breaks, labels = text)
for (i in 2:6) {
  x <- get(receptors_new[i])
  points(x$date, x$NOx_as_NO2, type = "l", col = cols[i])
}
legend(x = "topright", c(receptors_names), text.col=c(cols))

dev.print(pdf, "NOx_obs_mod.pdf")

dev.new()
par(mfrow = c(3,1))#, lwd = 1.5, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
plot(SNAQ39$date, SNAQ39$no, type = "l",  ylim = c(0,420), xaxt = "n",
     xlab = "", ylab = "NO (µg/m³)", col = cols[1], main = "Observations")
axis(side = 1, at = breaks, labels = text)
for (i in 2:6) {
  y <- get(receptors_new[i])
  points(y$date, y$no, type = "l", col = cols[i])
}
legend(x = "topright", c(receptors_names), text.col=c(cols))

plot(SNAQ39_old$date, SNAQ39_old$NO, type = "l", col = cols[1], xaxt = "n",
     xlab = "", ylab = "NO (µg/m³)", ylim = c(0,420), main = "Model run 2 (MEIC2010)")
axis(side = 1, at = breaks, labels = text)
for (i in 2:6) {
  x <- get(paste(receptors_new[i], "_old", sep = ""))
  points(x$date, x$NO, type = "l", col = cols[i])
}
legend(x = "topright", c(receptors_names), text.col=c(cols))

plot(SNAQ39$date, SNAQ39$NO, type = "l", col = cols[1], xaxt = "n",
     xlab = "", ylab = "NO (µg/m³)", ylim = c(0,420), main = "Model run 3 (MEIC2013)")
axis(side = 1, at = breaks, labels = text)
for (i in 2:6) {
  x <- get(receptors_new[i])
  points(x$date, x$NO, type = "l", col = cols[i])
}
legend(x = "topright", c(receptors_names), text.col=c(cols))

dev.print(pdf, "NO_obs_mod.pdf")

dev.new()
par(mfrow = c(3,1))#, lwd = 1.5, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
plot(SNAQ39$date, SNAQ39$no2, type = "l",  ylim = c(0,170), xaxt = "n",
     xlab = "", ylab = "NO2 (µg/m³)", col = cols[1], main = "Observations")
axis(side = 1, at = breaks, labels = text)
for (i in 2:6) {
  y <- get(receptors_new[i])
  points(y$date, y$no2, type = "l", col = cols[i])
}
legend(x = "topright", c(receptors_names), text.col=c(cols))

plot(SNAQ39_old$date, SNAQ39_old$NO2, type = "l", col = cols[1], xaxt = "n",
     xlab = "", ylab = "NO2 (µg/m³)", ylim = c(0,170), main = "Model run 2 (MEIC2010)")
axis(side = 1, at = breaks, labels = text)
for (i in 2:6) {
  x <- get(paste(receptors_new[i], "_old", sep = ""))
  points(x$date, x$NO2, type = "l", col = cols[i])
}
legend(x = "topright", c(receptors_names), text.col=c(cols))

plot(SNAQ39$date, SNAQ39$NO2, type = "l", col = cols[1], xaxt = "n",
     xlab = "", ylab = "NO2 (µg/m³)", ylim = c(0,170), main = "Model run 3 (MEIC2013)")
axis(side = 1, at = breaks, labels = text)
for (i in 2:6) {
  x <- get(receptors_new[i])
  points(x$date, x$NO2, type = "l", col = cols[i])
}
legend(x = "topright", c(receptors_names), text.col=c(cols))
dev.print(pdf, "NO2_obs_mod.pdf")

dev.new()
par(mfrow = c(3,1))#, lwd = 1.5, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
plot(SNAQ39$date, SNAQ39$o3, type = "l",  ylim = c(0,100), xaxt = "n",
     xlab = "", ylab = "O3 (µg/m³)", col = cols[1], main = "Observations")
axis(side = 1, at = breaks, labels = text)
for (i in 2:6) {
  y <- get(receptors_new[i])
  points(y$date, y$o3, type = "l", col = cols[i])
}
legend(x = "topright", c(receptors_names), text.col=c(cols))

plot(SNAQ39_old$date, SNAQ39_old$O3, type = "l", col = cols[1], xaxt = "n",
     xlab = "", ylab = "O3 (µg/m³)", ylim = c(0,100), main = "Model run 2 (MEIC2010)")
axis(side = 1, at = breaks, labels = text)
for (i in 2:6) {
  x <- get(paste(receptors_new[i], "_old", sep = ""))
  points(x$date, x$O3, type = "l", col = cols[i])
}
legend(x = "topright", c(receptors_names), text.col=c(cols))

plot(SNAQ39$date, SNAQ39$O3, type = "l", col = cols[1], xaxt = "n",
     xlab = "", ylab = "O3 (µg/m³)", ylim = c(0,100), main = "Model run 3 (MEIC2013)")
axis(side = 1, at = breaks, labels = text)
for (i in 2:6) {
  x <- get(receptors_new[i])
  points(x$date, x$O3, type = "l", col = cols[i])
}
legend(x = "topright", c(receptors_names), text.col=c(cols))
dev.print(pdf, "O3_obs_mod.pdf")

####################################################################
#This section calculates stats

#load run 3
files <- list.files(pattern = "obs_mod.csv", full.names = F)
receptors <- paste("SNAQ", c(39,40,41,44,45,46), sep = "")

start <- as.POSIXct("2016-11-9 12:00:00", tz = "Asia/Shanghai")
end <- as.POSIXct("2016-11-23 6:00:00", tz = "Asia/Shanghai")

for (i in 1 : 6) {
  x <- read_csv(files[i])
  x$date <- force_tz(x$date, tzone = "Asia/Shanghai")
  x <- subset(x, date >= start & date <= end)
  assign(receptors[i], x)
}

data <- do.call(rbind, list(SNAQ39, SNAQ40, SNAQ41, SNAQ44, SNAQ45, SNAQ46))

#load run 2
for (i in 1:6) {
  x <- read_csv(paste("../../../test2/tower_snaq/output/", files[i], sep = ""))
  x$date <- force_tz(x$date, tzone = "Asia/Shanghai")
  x <- subset(x, date >= start & date <= end)
  assign(paste(receptors[i], "_old", sep = ""), x)
}

data_old <- do.call(rbind, list(SNAQ39_old, SNAQ40_old, SNAQ41_old, SNAQ44_old, SNAQ45_old, SNAQ46_old))

obs <- c("co", "nox_as_no2", "no", "no2", "o3")
mod <- toupper(obs)
mod[2] <- "NOx_as_NO2"

receptors_new <- receptors[c(1:3,6,5,4)]

for (i in 1: length(obs)){
  x <- modStats(data, obs = obs[i], mod = mod[i], type = "Receptor name")
  x <- x[match(receptors_new, x$'Receptor name'), ]
  y <- modStats(data, obs = obs[i], mod = mod[i])
  names(y)[1] <- "Receptor name"
  z <- rbind(y, x)
  z[-1] <- round(z[-1], digits = 2)
  write.table(z[, c(1,3,6,9)], paste(mod[i], ".csv", sep = ""), row.names = F)
  
}

for (i in 1: length(obs)){
  x <- modStats(data_old, obs = obs[i], mod = mod[i], type = "Receptor name")
  x <- x[match(receptors_new, x$'Receptor name'), ]
  y <- modStats(data_old, obs = obs[i], mod = mod[i])
  names(y)[1] <- "Receptor name"
  z <- rbind(y, x)
  z[-1] <- round(z[-1], digits = 2)
  write.table(z[, c(1,3,6,9)], paste(mod[i], "_test2.csv", sep = ""), row.names = F)
  
}


########################################################################


p1 <- scatterPlot(SNAQ39, x = "co", y = "CO", col = "forestgreen", mod.line = T, xlim = c(0, 5.5), ylim = c(0, 5.5),
            linear = T, main = "SNAQ39", xlab = "obs", ylab = "mod")
p2 <- scatterPlot(SNAQ41, x = "co", y = "CO", col = "forestgreen",mod.line = T, xlim = c(0, 5.5), ylim = c(0, 5.5),
            linear = T, main = "SNAQ41", xlab = "obs", ylab = "mod")
p3 <- scatterPlot(SNAQ44, x = "co", y = "CO", col = "forestgreen",mod.line = T, xlim = c(0, 5.5), ylim = c(0, 5.5),
            linear = T, main = "SNAQ44", xlab = "obs", ylab = "mod")
p4 <- scatterPlot(SNAQ39_old, x = "co", y = "CO", col = "blue", mod.line = T, xlim = c(0, 5.5), ylim = c(0, 5.5),
                  linear = T, main = "SNAQ39", xlab = "obs", ylab = "mod")
p5 <- scatterPlot(SNAQ41_old, x = "co", y = "CO", col = "blue", mod.line = T, xlim = c(0, 5.5), ylim = c(0, 5.5),
                  linear = T, main = "SNAQ41", xlab = "obs", ylab = "mod")
p6 <- scatterPlot(SNAQ44_old, x = "co", y = "CO", col = "blue", mod.line = T, xlim = c(0, 5.5), ylim = c(0, 5.5),
                  linear = T, main = "SNAQ44", xlab = "obs", ylab = "mod")

p1 <- scatterPlot(SNAQ39, x = "nox_as_no2", y = "NOx_as_NO2", col = "forestgreen", mod.line = T, xlim = c(0, 720), ylim = c(0, 720),
                  linear = T, main = "SNAQ39", xlab = "obs", ylab = "mod")
p2 <- scatterPlot(SNAQ41, x = "nox_as_no2", y = "NOx_as_NO2", col = "forestgreen",mod.line = T, xlim = c(0, 720), ylim = c(0, 720),
                  linear = T, main = "SNAQ41", xlab = "obs", ylab = "mod")
p3 <- scatterPlot(SNAQ44, x = "nox_as_no2", y = "NOx_as_NO2", col = "forestgreen",mod.line = T, xlim = c(0, 720), ylim = c(0, 720),
                  linear = T, main = "SNAQ44", xlab = "obs", ylab = "mod")
p4 <- scatterPlot(SNAQ39_old, x = "nox_as_no2", y = "NOx_as_NO2", col = "blue", mod.line = T, xlim = c(0, 720), ylim = c(0, 720),
                  linear = T, main = "SNAQ39", xlab = "obs", ylab = "mod")
p5 <- scatterPlot(SNAQ41_old, x = "nox_as_no2", y = "NOx_as_NO2", col = "blue", mod.line = T, xlim = c(0, 720), ylim = c(0, 720),
                  linear = T, main = "SNAQ41", xlab = "obs", ylab = "mod")
p6 <- scatterPlot(SNAQ44_old, x = "nox_as_no2", y = "NOx_as_NO2", col = "blue", mod.line = T, xlim = c(0, 720), ylim = c(0, 720),
                  linear = T, main = "SNAQ44", xlab = "obs", ylab = "mod")
dev.new()
print(p1, split = c(1,1,2,3))
print(p2, split = c(1,2,2,3), newpage = F)
print(p3, split = c(1,3,2,3), newpage = F)
print(p4, split = c(2,1,2,3), newpage = F)
print(p5, split = c(2,2,2,3), newpage = F)
print(p6, split = c(2,3,2,3), newpage = F)
dev.print(pdf, "NOx_scatter.pdf")

scatterPlot(SNAQ39_old, x = "co", y = "CO", mod.line = T, xlim = c(0, 5.5), ylim = c(0, 5.5),
            linear = T, main = "SNAQ39", xlab = "obs", ylab = "mod")



