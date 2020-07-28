##DOING CLUSTER ANALYSIS##
#Read the dataset
SNFCLUSTER= read.csv("D:/Arghavan/Ph.D-20190508T193213Z-001/R/COMPARING/TEST1.csv")
View(SNFCLUSTER)

# find distance matrix 
d <- dist(as.matrix(SNFCLUSTER))

# apply hirarchical clustering
hc <- hclust(d)

# plot the dendrogram
plot(hc)

##FINISH##

#HCPC ANALYSIS#
install.packages(c("FactoMineR", "factoextra"))
library(factoextra)
library(FactoMineR)

HCPC(SNFCLUSTER, nb.clust = 0, min = 3, max = NULL, graph = TRUE)

###PCA FINAL###

res.pca <- PCA(SNFCLUSTER, ncp = 3, graph = FALSE)
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(res.pca, graph = FALSE)

fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)


plot(res.hcpc, choice = "3D.map")

head(res.hcpc$data.clust, 16)
head(res.hcpc$desc.ind, 16)
head(res.hcpc$desc.axes, 16)
head(res.hcpc$desc.var, 16)


install.packages("writexl")
library("writexl")

m= write.table(res.hcpc$desc.ind$para$`3`, file = "results.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
m
library("readr")
install.packages("readr")
write_tsv(z$dist, path = "results.txt")
res.hcpc$desc.ind
