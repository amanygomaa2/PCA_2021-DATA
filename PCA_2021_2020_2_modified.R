#1. read the 2020 original data
library(data.table)
#load the data


data_2020 <- fread("counts.NE2020.693.filtered_V2.txt", data.table = F)

# 2. Create a vector of observation names you want to extract
vec <- c("NS501","Mo3", "CO245","K64","NS701","LH222","DKMBZA","Pa392","PHB09","MS1334","LH195","MS221","N542","NC338","PHVJ4","Ill.Hy","A648","NK807","NC264","DKIB02","PHW52","H5","A797NW","PHW80")
#taxa of bad quality  ,"LH195","MS221","N542","NC338","PHVJ4","Ill.Hy","A648","NK807","NC264","DKIB02","PHW52","H5","A797NW","PHW80")


# 3. Subset the data based on the names
sub_df_2020<- data_2020[data_2020$taxa %in% vec,] 
print ("extracted_data") 
print (sub_df_2020)

# 4. read 2021 original data
data_2021 <- fread("counts.NE2021.734.filtered-2.txt", data.table = F)
sub_df_2021<- data_2021[data_2021$taxa %in% vec,] 
print ("extracted_data") 
print (sub_df_2021)

dta2020 <- sub_df_2020
dta2021 <- sub_df_2021
#taxa of bad quality  ,"LH195","MS221","N542","NC338","PHVJ4","Ill.Hy","A648","NK807","NC264","DKIB02","PHW52","H5","A797NW","PHW80")
dta2021$taxa[which(dta2021$taxa == "LH195")] <- paste0("LH195","_2020_b")




badtaxa <- c("LH195","MS221","N542","NC338","PHVJ4","Ill.Hy","A648","NK807","NC264","DKIB02","PHW52","H5","A797NW","PHW80")
intersect(dta2021$taxa, badtaxa)
length(intersect(dta2021$taxa, badtaxa))

for (i in badtaxa) {
  dta2021$taxa[which(dta2021$taxa == i)] <- paste0(i,"_2021_b")
}



goodtaxa <- c("NS501","Mo3", "CO245","K64","NS701","LH222","DKMBZA","Pa392","PHB09","MS1334")
intersect(dta2021$taxa, goodtaxa)
length(intersect(dta2021$taxa, goodtaxa))

for (i in goodtaxa) {
  dta2021$taxa[which(dta2021$taxa == i)] <- paste0(i,"_2021_g")
}

### now change the names for 2020
for (i in badtaxa) {
  dta2020$taxa[which(dta2020$taxa == i)] <- paste0(i,"_2020_b")
}

for (i in goodtaxa) {
  dta2020$taxa[which(dta2020$taxa == i)] <- paste0(i,"_2020_g")
}


# Find differing column names
diff_cols <- setdiff(colnames(dta2020), colnames(dta2021))

# Subset dta2020 to remove differing columns
dta2020 <- dta2020[, !colnames(dta2020) %in% diff_cols]

diff_cols2 <- setdiff(colnames(dta2021), colnames(dta2020))
dta2021 <- dta2021[, !colnames(dta2021) %in% diff_cols2]
#combine datasets https://www.digitalocean.com/community/tutorials/rbind-function-r
dta <- rbind(dta2020, dta2021)


row.names(dta) <- dta$taxa
dta <- dta[,-1]

# remove all columns that has zero value
dta_without_zero <- dta[, colSums(dta != 0) > 0]

#you will need to remove all 0s prior the analysis
xdata <- dta_without_zero[, which(apply(dta_without_zero, 2, var) != 0)] 


#this is the formula calculating the PCA
res.pca <- prcomp(xdata, scale = TRUE)
install.packages("factoextra") 
library(factoextra)            
fviz_eig(res.pca) # Show the percentage of variances explained by each principal component.

# you dont need to color code
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


fviz_pca_ind(res.pca) +
  labs(title ="PCA", x = "PC1", y = "PC2")


eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates


data <- data.frame(res.ind$coord)



plot(data$Dim.1, data$Dim.2)




