#Use the in-built "mtcars" data of R and do as follows in R studio with R script:

# 1. Perform the principal component analysis in the data and exact the dimensions
# based on components with eigenvalues >1, check it with screeplot as well 
# and interpret the result carefully

# Load the mtcars dataset
data(mtcars)

# 1. Principal Component Analysis (PCA)
pca <- prcomp(mtcars, scale = TRUE)  # Perform PCA
summary(pca)  # Display summary

# Screeplot
screeplot(pca, type = "line", main = "Screeplot")  # Screeplot
abline(h = 1, col = "red")  # Add line at eigenvalue 1
str(pca)

# Extract dimensions based on eigenvalues > 1
eigenvalues <- pca[[1]]^2
dimensions <- sum(eigenvalues > 1)
cat("Number of dimensions with eigenvalues > 1:", dimensions, "\n")

# Interpretation:
# The screeplot displays the eigenvalues of each principal component. 
# Eigenvalues represent the amount of variance explained by each component.
# We look for the "elbow" in the screeplot, where eigenvalues drop significantly. 
# In this case, it seems to be around the second component.
# Thus, we choose the dimensions with eigenvalues greater than 1 as they 
#explain a significant amount of variance in the data.
# In this case, the number of dimensions with eigenvalues > 1 is 2 
#(the first 2 components).

# 2. Perform the principal component analysis with varimax rotation in the data 
# and exact the dimensions based on eigenvalue >1 and check it with Screeplot 
# as well and interpret the result carefully

# Perform PCA with varimax rotation
library(psych)  # Load 'psych' package for varimax rotation
pca_varimax <- principal(mtcars, nfactors = length(mtcars), rotate = "varimax")
summary(pca_varimax)

#Extract dimensions based on eigenvalues > 1
eigenvalues_varimax <- pca_varimax$values
dimensions_varimax <- sum(eigenvalues > 1)
cat("Number of dimensions with eigenvalues > 1:", dimensions_varimax, "\n")

str(pca_varimax)

# Screeplot
plot(1:length(eigenvalues_varimax), eigenvalues_varimax, type = "b",
     xlab = "Principal Component", ylab = "Eigenvalue",
     main = "Screeplot of PCA with Varimax Rotation")
abline(h = 1, col = "red")

# Interpretation
cat("The screeplot shows the eigenvalues for each principal 
    component after varimax rotation.\n")
cat("Eigenvalues represent the amount of variance 
    explained by each component.\n")
cat("We select the dimensions based on eigenvalues greater than 1.\n")
cat("In this case,", dimensions, "dimensions have 
    eigenvalues greater than 1,\n")
cat("indicating that they explain more variance than 
    a single original variable.\n")
cat("Varimax rotation simplifies the interpretation of 
    components by maximizing\n")
cat("the variance of each component and providing a clearer 
    factor loading structure.\n")


# 3. Perform the classical multidimensional scaling in the data, revise the 
#results using stress values and interpret the result carefully 
# Perform Classical Multidimensional Scaling (MDS)
mds <- cmdscale(dist(mtcars))

# Plot the MDS solution
plot(mds, type = "n", xlab = "Dimension 1", ylab = "Dimension 2")
text(mds, labels = row.names(mtcars))

# Calculate stress values
dist_orig <- dist(mtcars)

#custom stress function 
stress <- function(mds, dist_orig) {
  n <- nrow(mds)
  dist_mds <- as.matrix(dist(mds))
  sum_diff <- sum((dist_orig - dist_mds)^2)
  sum_orig <- sum(dist_orig^2)
  stress_val <- sqrt(sum_diff / sum_orig)
  return(stress_val)
}

stress <- stress(mds, dist_orig)

# Print stress value
cat("Stress value:", stress, "\n")

# Interpretation

cat("The classical MDS represents the dissimilarity between 
    observations in a lower-dimensional space.\n")
cat("The MDS solution is plotted in a two-dimensional space, 
    with the points representing the observations.\n")
cat("Stress is a measure of the discrepancy between the original
    dissimilarity matrix and the distances in the MDS solution.\n")
cat("A lower stress value indicates a better fit between the original 
    distances and the distances in the reduced space.\n")
cat("In this case, the stress value is", round(stress, 4), "which 
    indicates the goodness of fit for the MDS solution.\n")
cat("Interpretation of the MDS plot should consider the proximity of points, 
    with closer points representing similar observations.\n")


#####################################3
#In R, the cmdscale() function does not directly provide a stress value. 
#Instead, the stress value is typically calculated using an external package 
#called isoMDS from the MASS library. Here's the revised script that performs 
#classical multidimensional scaling (MDS) using the isoMDS function, 
#calculates the stress value, and provides interpretation

# Load the required package
library(MASS)

# Perform Classical Multidimensional Scaling (MDS)
mds <- isoMDS(dist(mtcars))

# Plot the MDS solution
plot(mds$points, type = "n", xlab = "Dimension 1", ylab = "Dimension 2")
text(mds$points, labels = row.names(mtcars))

#make the plot Alternative approach with Sammon's stres better 
mds_2 <- MASS::sammon(dist_orig, trace = FALSE)
plot(mds_2$points, pch = 19)
abline(h=0, v=0, lty=2)
text(mds_2$points, pos = 4, labels = rownames(mtcars))

# Compare with PCA (first two PCs):
arrows(x0 = mds_2$points[,1], y0 = 
      mds_2$points[,2], x1 = pca$x[,1], y1 = pca$x[,2], col='red', pch=19, cex=0.5)


# Calculate stress value
stress <- mds$stress

# Print stress value
cat("Stress value:", stress, "\n")

# Interpretation
cat("The classical MDS represents the dissimilarity between observations 
    in a lower-dimensional space.\n")
cat("The MDS solution is plotted in a two-dimensional space, with the 
    points representing the observations.\n")
cat("Stress is a measure of the discrepancy between the original dissimilarity 
    matrix and the distances in the MDS solution.\n")
cat("A lower stress value indicates a better fit between the original 
    distances and the distances in the reduced space.\n")
cat("In this case, the stress value is", round(stress, 4), "which 
    indicates the goodness of fit for the MDS solution.\n")
cat("Interpretation of the MDS plot should consider the proximity of 
    points, with closer points representing similar observations.\n")



# 4. Perform the hierarchical cluster analysis in the data and determine 
#the number of clusters to exact using the dendogram and cut at the various 
#distances with justification

# Perform Hierarchical Cluster Analysis
hc <- hclust(dist(mtcars))

# Plot the Dendrogram
plot(hc, main = "Dendrogram of Hierarchical Clustering")

# Determine the number of clusters using the dendrogram
clusters <- cutree(hc, k = 2:length(mtcars))
cluster_counts <- table(clusters)
print(cluster_counts)

# Determine the number of clusters using the dendrogram
num_clusters <- length(unique(cutree(hc, k = length(mtcars))))
cat("Number of clusters:", num_clusters, "\n")

# Cut the dendrogram at various distances
cut_distances <- c(10, 15, 20)  # Adjust the distances as needed
for (distance in cut_distances) {
  clusters <- cutree(hc, h = distance)
  num_clusters <- length(unique(clusters))
  cat("Number of clusters at distance", distance, ":", num_clusters, "\n")
  cat("Cluster sizes:", table(clusters), "\n\n")
}

# 5. Perform the k-means cluster analysis in the data based on the number of 
#clusters identified using dendogram and interpret the result carefully

## Perform K-means Cluster Analysis
kmeans_result <- kmeans(scale(mtcars), centers = num_clusters)

# Add Cluster Labels to the Dataset
mtcars$cluster <- as.factor(kmeans_result$cluster)

# Interpretation
cat("Number of clusters identified:", num_clusters, "\n")
cat("Cluster Sizes:", table(mtcars$cluster), "\n\n")

# Summary of Cluster Centers
cluster_centers <- data.frame(cluster = 1:num_clusters, kmeans_result$centers)
print(cluster_centers)

# The script then performs k-means cluster analysis using the kmeans() function 
#on the scaled "mtcars" dataset. The resulting clusters are assigned to each
#observation, and the cluster labels are added to the dataset.
# 
# The interpretation section displays the number of clusters identified 
#and the sizes of each cluster using the table() function. It provides an 
#overview of the distribution of observations in each cluster.
# 
# Additionally, the script presents a summary of the cluster centers, which 
#represent the mean values of the variables within each cluster. 
#The cluster_centers data frame displays the cluster number along with 
#the corresponding center values for each variable.

#Interpreting the results requires analyzing the characteristics of 
#each cluster based on the cluster centers and understanding the context of 
#the data. You can examine the variables that contribute most to the 
#differences between the clusters and interpret the clusters based 
#on their specific characteristics and similarities.