library(data.table)
#load the data

Geneexpression1 <- fread("GeneExpressionNE2021.txt", data.table = F)

# 1. Read the data
data <- read.table("GeneExpressionNE2021.txt", header = FALSE)  # Adjust file name and parameters as needed

# 2. Create a vector of observation names you want to extract
vec <- c("NS501","Mo3", "CO245","K64","NS701","LH222","DKMBZA","Pa392","PHB09","MS1334","LH195","MS221","N542","NC338","PHVJ4","Ill.Hy","A648","NK807","NC264","DKIB02","PHW52","H5","A797NW","PHW80")
#taxa of bad quality  ,"LH195","MS221","N542","NC338","PHVJ4","Ill.Hy","A648","NK807","NC264","DKIB02","PHW52","H5","A797NW","PHW80")


# 3. Subset the data based on the names
sub_df_2021<- Geneexpression1[Geneexpression1$taxa %in% vec,] 
print ("extracted_data") 
print (sub_df_2021)



# Saving subset DataFrame to a CSV file
write.csv(sub_df_2021, "sub_df_2021.csv", row.names = FALSE)
# Loading the subset DataFrame from the CSV file
sub_df_2021 <- read.csv("sub_df_2021.csv")
# Loading the subset DataFrame from the CSV file
sub_df_2021 <- read.csv("sub_df_2021.csv")

str(sub_df_2021)
summary(sub_df_2021)
data.frame(sub_df_2021)

#vlad code
install.packages("ggplot2")
library(ggplot2)
install.packages("factoextra")
library(factoextra)

#this will load the data from the package "factoextra"

decathlon2.active <- sub_df_2021[, 2:39757]


head(decathlon2.active[, 1:6])


#you will need to remove all 0s prior the analysis
xdata2021 <- decathlon2.active[, which(apply(decathlon2.active, 2, var) != 0)] 

# remove all columns that has zero value
xdata_without_zero2021 <- xdata2021[, colSums(xdata != 0) > 0]

#this is the formula calculating the PCA
res.pca <- prcomp(xdata_without_zero2021, scale = TRUE)

fviz_eig(res.pca) # Show the percentage of variances explained by each principal component.

# you dont need to color code
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates


data <- data.frame(res.ind$coord)
plot(data$Dim.1, data$Dim.2)
