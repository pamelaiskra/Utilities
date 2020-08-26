#Created by Iskra Mejía Estrada. August 2020.
install.packages("ncdf4")
library(ncdf4)
install.packages("VIM")
library(VIM)

# ---- Step 1
mync <- read.table("data.txt")
myt <- data.frame(mync)
#Select pertinent columns
myt <- myt[,c(2,4)]

# ---- Step 1
thresh <- 1
durations <- c(3,5)
months_name <- month.abb
months_loc <- as.numeric(myt[,1])

# ---- Step 3
myt[(which(myt[,2] <= thresh)),2] <- NA 

# ---- Step 4
vindex <- which(is.na(myt[,2]))

# ---- Step 5
index_events <- function(myvec, nday) {
  indexx <- list()
  contador <- 1
  for (i in 2:length(myvec)) {
    diff <- myvec[i]-myvec[i-1]
    if (diff == (nday + 1)) {
      indexx[[contador]] <- myvec[i-1] +1
      contador <- contador + 1
    }
  }
  result <- do.call(rbind,indexx)
  return(result)
}

# ---- Step 6
num_events_season <- function(myvec) {
  indexes <- index_events(myvec, nday)
  # ---- Step 6a
  seasonal <- months_loc[indexes]
  # ---- Step 6b
  DJF <- sum(seasonal == months_name[12]) +
    sum(seasonal == months_name[1]) + sum(seasonal == months_name[2])
  # ---- Step 6c
  MAM <- sum(seasonal == months_name[3]) + 
    sum(seasonal == months_name[4]) + sum(seasonal == months_name[5])
  # ---- Step 6d
  JJA <- sum(seasonal == months_name[6]) + 
    sum(seasonal == months_name[7]) + sum(seasonal == months_name[8])
  # ---- Step 6e
  SON <- sum(seasonal == months_name[9]) + 
    sum(seasonal == months_name[10]) + sum(seasonal == months_name[11])
  
  seasonal <- c(DJF,MAM,JJA,SON)
  return(seasonal)
}

# ---- ACTUAL CALCULATIONS
matrix_results <- matrix(NA,nrow=length(durations),ncol=5)
for (j in 1:length(durations)) {
  matrix_results[j,1] <- durations[j]
  nday <- durations[j]
  matrix_results[j,2:5] <- num_events_season(vindex)
}

colnames(matrix_results) <- c("Duration of event","DJF","MAM","JJA","SON")

# ---- Step 7
cat("For",nday,"-day duration events, there are",seasonal[1],
    "events in DJF,",seasonal[2],"in MAM,", seasonal[3],
    "in JJA and",seasonal[4], "in SON")

# ---- Step 8

labelss <- c("DJF","MAM","JJA","SON")
colourss <- c("blue","red","black","green")

# Create legend
legendd <- c(1:length(durations))
for (m in 1:length(durations)) {
  legendd[m] <- paste0(durations[m],"-day events")
}

# Plot
plot(matrix_results[1,2:5], xaxt ="n", xlab="Season",
     ylab="# of n-day events", ylim=c(0,max(matrix_results)),
     pch=16,col=colourss[1])
axis(1,at=seq_along(labelss),tick = TRUE,labels = labelss)
for (k in 2:nrow(matrix_results)) {
  points(matrix_results[k,2:5],pch=16,col=colourss[k])
}
legend("center", legendd,fill=colourss[1:length(durations)])
