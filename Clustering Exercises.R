# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

library("cluster")
install.packages(c("rattle.data","NbClust"))
library("rattle.data")

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
wine2 <- wine[-1]
wine2 <- scale(wine2)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total 'within-groups sums of squares' against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine2)

# Exercise 2:
#   * How many clusters does this method suggest?
# This method suggests that 3 clusters will be the best choice.
#   * Why does this method work? What's the intuition behind it?
# For each iteration each data point gets closer to a cluster's centroid.  The iteration process improves the overall outcome by checking and rechecking each data point and finding the best clusters.
#   * Look at the code for wssplot() and figure out how it works
# The apply function in wssplot function calculates the variance of each column of the data set and returns a vector of these values.

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library("NbClust")
set.seed(1234)
nc <- NbClust(wine2, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,]) # added for reference
barplot(table(nc$Best.n[1,]),
	          xlab="Number of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")



# Exercise 3: How many clusters does this method suggest?
# "According to the majority rule, the best number of clusters is 3 (15 indices proposed 3 clusters)"

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(wine2, 3, nstart = 25) # data frame, 3 clusters, start at 25
fit.km$size # Gives [1] 51 65 62

fit.km$centers # Gives the centers of each cluster for each column

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

aggregate(wine[-1], by=list(cluster = fit.km$cluster), mean)
wine$Type

ct.km <- table(wine$Type, fit.km$cluster)
ct.km # I have the same numbers but in a different order (i.e. in row 1: 59 0 0 is 0 0 59 for me.  Row 2 is identical and row 3 is reversed)
# The entries appear to be very well grouped considering the 0 values.

# Exercise 6:
# * Visualize these clusters using function clusplot() from the cluster library
# * Would you consider this a good clustering?

library("flexclust")
randIndex(ct.km) # 1 is perfect agreement, 0.897 is pretty good

clusplot(pam(wine2, 3))
# One of the clusters looks good, but the other two have significant overlap and I would have assigned some points in each to the other cluster.  The plot reads "These two components explain 55.41% of the point variability."

clusplot(clara(wine2, 3))
# Clara looks better than pam.  There is much less overlap of the same two clusters and the note at the bottom is the same.

clusplot(fanny(wine2, 3))
# Fanny reduces the data frame to 2 clusters with a little overlap. Previous analyses indicated that 3 was a better choice.

# From a visual perspective, clara appears to be the better choice for "partitioning" the data.


