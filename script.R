
# Load necessary libraries
library(tidyverse)
library(cluster)  # For clustering algorithms
library(factoextra)  # For PCA visualization and clustering validation
library(NbClust)  # For determining the number of clusters
library(ggplot2)
library(patchwork)



df <- read.csv("data/spotify_most_streamed_2024.csv")
df <- df %>% filter(!is.na(Spotify.Streams) & Spotify.Streams != "")
df <- df %>% filter(!is.na(YouTube.Views) & YouTube.Views != "")
df <- df %>% filter(!is.na(TikTok.Views) & TikTok.Views != "")

df$Spotify.Streams <- as.numeric(gsub("[^0-9.-]", "", df$Spotify.Streams))
df$YouTube.Views <- as.numeric(gsub("[^0-9.-]", "", df$YouTube.Views))
df$TikTok.Views <- as.numeric(gsub("[^0-9.-]", "", df$TikTok.Views))
df$All.Time.Rank <- as.numeric(gsub("[^0-9.-]", "", df$All.Time.Rank))

head(df,10)

View(df)

plot1<-ggplot(df, aes(x = Spotify.Streams, y = YouTube.Views)) +
  geom_point(color = "black", size = 0.1,alpha=0.3) +
  scale_x_continuous(limits = c(0,10000000000/4)) +
  scale_y_continuous(limits = c(0,10000000000/4)) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    x = "Spotify Streams",
    y = "YouTube Views"
  )+labs(title="Comparing streams on Spotify with other platforms")


plot2<-ggplot(df, aes(x = Spotify.Streams, y = TikTok.Views)) +
  geom_point(color = "black", size = 0.10, alpha=0.3) +
  scale_x_continuous(limits = c(0,10000000000/4)) +
  scale_y_continuous(limits = c(0,10000000000/4)) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    y = "TikTok Views"
  )
plot12<-plot1+plot2

ggsave('plots/progress/plot1.png', plot12, height = 5, width = 8)

hist(df$Spotify.Streams,
     xlim = c(0, 10000000000/4),
     breaks = 20,
     main = "Spotify Streams Distribution",
     xlab = "Spotify Streams",
     col = "skyblue",
     border = "black")


# More data cleaning
# Drop rows with missing values
df <- df %>% filter(!is.na(Spotify.Streams) & !is.na(YouTube.Views) & !is.na(TikTok.Views))

# Convert relevant columns to numeric
df$Spotify.Streams <- as.numeric(gsub("[^0-9.-]", "", df$Spotify.Streams))
df$YouTube.Views <- as.numeric(gsub("[^0-9.-]", "", df$YouTube.Views))
df$TikTok.Views <- as.numeric(gsub("[^0-9.-]", "", df$TikTok.Views))

# Select only numerical columns and scale the data
df_numeric <- df %>% select(Spotify.Streams, YouTube.Views, TikTok.Views) %>% scale()

# Preview cleaned data
head(df_numeric)

# Select numerical features
dat <- df %>% select(Spotify.Streams, YouTube.Views, TikTok.Views) %>% as.matrix()

# Center the data
dat1 <- as.matrix(t(t(dat) - colMeans(dat)))

# Compute covariance matrix
Sigma <- 1 / (nrow(dat1) - 1) * t(dat1) %*% dat1

# Eigendecomposition
eigendeomp <- eigen(Sigma)

# View eigenvalues
eigendeomp$values

# Plot eigenvalues to assess explained variance
plot(eigendeomp$values, type = "l", main = "Eigenvalues of Covariance Matrix", 
     xlab = "Principal Component", ylab = "Eigenvalue")

# Run PCA using prcomp
pc <- prcomp(dat1)

# View PCA attributes
attributes(pc)

# Draw a heatmap of the first 3 principal components
heatmap(pc$rotation[, 1:3], Rowv = NA, Colv = NA, main = "Heatmap of Principal Components")

