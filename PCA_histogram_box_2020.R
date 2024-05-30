library(data.table)
#load the data
data_2020 <- fread("counts.NE2020.693.filtered_V2.txt", data.table = F)
vec <- c("NS501","Mo3", "CO245","K64","NS701","LH222","DKMBZA","Pa392","PHB09","MS1334","LH195","MS221","N542","NC338","PHVJ4","Ill.Hy","A648","NK807","NC264","DKIB02","PHW52","H5","A797NW","PHW80")

sub_df_2020<- data_2020[data_2020$taxa %in% vec,] 
print ("extracted_data") 
print (sub_df_2020)

dta2020 <- sub_df_2020
# 2. Create a vector of observation names you want to extract
#taxa of bad quality  ,"LH195","MS221","N542","NC338","PHVJ4","Ill.Hy","A648","NK807","NC264","DKIB02","PHW52","H5","A797NW","PHW80")
badtaxa <- c("LH195","MS221","N542","NC338","PHVJ4","Ill.Hy","A648","NK807","NC264","DKIB02","PHW52","H5","A797NW","PHW80")
intersect(dta2020$taxa, badtaxa)
length(intersect(dta2020$taxa, badtaxa))

for (i in badtaxa) {
  dta2020$taxa[which(dta2020$taxa == i)] <- paste0(i,"_2020_b")
}

goodtaxa <- c("NS501","Mo3", "CO245","K64","NS701","LH222","DKMBZA","Pa392","PHB09","MS1334")
intersect(dta2020$taxa, goodtaxa)
length(intersect(dta2020$taxa, goodtaxa))

for (i in goodtaxa) {
  dta2020$taxa[which(dta2020$taxa == i)] <- paste0(i,"_2020_g")
}

row.names(dta2020) <- dta2020$taxa
dta <- dta2020[,-1]
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


grep("_b",data2$taxa) # will give the row numbers with a '_b' in the column taxa
data2$group <- NA #creating a new column with NAs

data2$group[grep("_b",data2$taxa)] <- "bad"
data2$group[-grep("_b",data2$taxa)] <- "good"

hist(data2$Dim.1, fill = data2$group)
library(ggplot2)
library(dplyr)
library(hrbrthemes)

p1 <- data2 %>%
  ggplot( aes(x=Dim.1, fill=group)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="", title = "2020")

p1
#boxplot
b1 <- data2 %>%
  ggplot(aes(x = group, y = Dim.1, fill = group)) +
  geom_boxplot(color = "#e9ecef", alpha = 0.6) +
  scale_fill_manual(values = c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill = "", title = "2020")
b1

p2 <- data2 %>%
  ggplot( aes(x=Dim.2, fill=group)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="", title = "PC2 - 2020")
p2

b2 <- data2 %>%
  ggplot(aes(x = group, y = Dim.2, fill = group)) +
  geom_boxplot(color = "#e9ecef", alpha = 0.6) +
  scale_fill_manual(values = c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill = "", title = "PC2 -2020")
b2

p15 <- data2 %>%
  ggplot( aes(x=Dim.15, fill=group)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
p15


#plot(data$Dim.1, data$Dim.2) I 

data2 %>%
  ggplot(aes(x = group, y = Dim.2, fill = group)) +
  geom_boxplot(color = "#e9ecef", alpha = 0.6) +
  scale_fill_manual(values = c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill = "")


