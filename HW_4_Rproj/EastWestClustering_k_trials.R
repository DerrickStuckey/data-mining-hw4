## Derrick Stuckey
## Clustering Analysis of East-West Airlines customer data

EastWestAirlinesCluster <- read.csv("~/Desktop/GW/Data Mining/Assignment 4/EastWestAirlinesCluster.csv")

# create new dataframe, initialized with variables that don't need transforming
xformed_data <- data.frame(EastWestAirlinesCluster[c("cc1_miles", "cc2_miles", "cc3_miles", "Days_since_enroll", "Award")])

# scale exponentially distributed variables w/ log transform
# use add-1 smoothing to get real values in case of 0
xformed_data$Balance_Log <- log(EastWestAirlinesCluster$Balance+1)
xformed_data$Qual_miles_Log <- log(EastWestAirlinesCluster$Qual_miles+1) #should be binary?
xformed_data$Bonus_miles_Log <- log(EastWestAirlinesCluster$Bonus_miles+1)
xformed_data$Bonus_trans_Log <- log(EastWestAirlinesCluster$Bonus_trans+1)
xformed_data$Flight_miles_12mo_Log <- log(EastWestAirlinesCluster$Flight_miles_12mo+1)
xformed_data$Flight_trans_12_Log <- log(EastWestAirlinesCluster$Flight_trans_12+1)

# scale data by (difference from mean) / sd for each column
xformed_data <- scale(xformed_data, center=TRUE, scale=TRUE)
#xformed_data$Days_since_enroll <- xformed_data$Days_since_enroll

# returns rsq value for k-means clustering w/ k=k
get_rsq <- function(k) {
  kmeans_fit <- kmeans(xformed_data, centers=k)
  rsq <- kmeans_fit$betweenss / kmeans_fit$totss
  return(rsq)
}

# try for a few different values of k
k_vals <- c(2,3,4,5,6,7,8,9)
rsq_vals <- lapply(k_vals, get_rsq)

# plot rsq vs k
plot(k_vals, rsq_vals, xlab="k", ylab="R-squared", main="K-means Performance by # of Clusters", col="blue")
