library(data.table)
#load the data
data_2021 <- fread("counts.NE2021.734.filtered-2.txt", data.table = F)
vec <- c("NS501","Mo3", "CO245","K64","NS701","LH222","DKMBZA","Pa392","PHB09","MS1334",
         "PHW03","Mo44","11430","Oh43E","PHT22","PHWG5","Va38","A15","LH284","NC326")
  

sub_df_2021<- data_2021[data_2021$taxa %in% vec,] 
print ("extracted_data") 
print (sub_df_2021)

dta2021 <- sub_df_2021
# 2. Create a vector of observation names you want to extract
#taxa of QS5_6 "PHW03","Mo44","11430","Oh43E","PHT22","PHWG5","Va38","A15","LH284",NC326")
qs5_6taxa <- c("PHW03","Mo44","11430","Oh43E","PHT22","PHWG5","Va38","A15","LH284","NC326")
intersect(dta2021$taxa, qs5_6taxa)
length(intersect(dta2021$taxa, qs5_6taxa))

for (i in qs5_6taxa) {
  dta2021$taxa[which(dta2021$taxa == i)] <- paste0(i,"_2021_qs5_6")
}

qs1taxa <- c("NS501","Mo3", "CO245","K64","NS701","LH222","DKMBZA","Pa392","PHB09","MS1334")
intersect(dta2021$taxa, qs1taxa)
length(intersect(dta2021$taxa, qs1taxa))

for (i in qs1taxa) {
  dta2021$taxa[which(dta2021$taxa == i)] <- paste0(i,"_2021_qs1")
}

row.names(dta2021) <- dta2021$taxa
dta <- dta2021[,-1]
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
             repel = TRUE    # Avoid text overlapping
)


fviz_pca_ind(res.pca) +
  labs(title ="PCA", x = "PC1", y = "PC2")


eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for individuals
res.ind <- get_pca_ind(res.pca)

res.ind$coord          # Coordinates


data <- data.frame(res.ind$coord)
data2 <- cbind(rownames(xdata),data)
colnames(data2)[1] <- "taxa" #assigns the name "taxa" to the first column of the dataframe data2

grep("_qs5_6",data2$taxa) # will give the row numbers with a '_b' in the column taxa
data2$group <- NA #creating a new column with NAs

data2$group[grep("_qs5_6",data2$taxa)] <- "bad"
data2$group[-grep("_qs5_6",data2$taxa)] <- "good"

hist(data2$Dim.1, fill = data2$group)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
p1 <- data2 %>%
  ggplot( aes(x=Dim.1, fill=group)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="", title = "pc1, 2021_bad=qs5_6,good=qs1")
p1
#boxplot
b1 <- data2 %>%
  ggplot(aes(x = group, y = Dim.1, fill = group)) +
  geom_boxplot(color = "#e9ecef", alpha = 0.6) +
  scale_fill_manual(values = c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill = "",  title = "pc1, 2021- qs1=good, qs5_6=bad")
b1

p2 <- data2 %>%
  ggplot( aes(x=Dim.2, fill=group)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="", title = "pc2, 2021- qs1=good, qs5_6=bad")
p2
b2 <- data2 %>%
  ggplot(aes(x = group, y = Dim.2, fill = group)) +
  geom_boxplot(color = "#e9ecef", alpha = 0.6) +
  scale_fill_manual(values = c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill = "",  title = "pc2, 2021- qs1=good, qs5_6=bad")
b2

p15 <- data2 %>%
  ggplot( aes(x=Dim.15, fill=group)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
p15





