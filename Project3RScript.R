# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)
library(Rtsne)
library(cluster)


# Update this between 2004 and 2024
nba_data <- read.csv("C:/Users/jimbo/Desktop/NBA Datasets/NBA_2024_Shots.csv")

#Visualize Missing Values
missing_data <- colSums(is.na(nba_data))
ggplot(data.frame(Feature = names(missing_data), Missing = missing_data), aes(x = Feature, y = Missing)) +
  geom_bar(stat = "identity", fill = "lightblue") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = "Missing Values by Feature Before Imputation", x = "Feature", y = "Count of Missing Values")

#Handle Missing Values
nba_data <- nba_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)),
         across(where(is.character), ~ ifelse(is.na(.), names(sort(table(.), decreasing = TRUE))[1], .)))

missing_data_post <- colSums(is.na(nba_data))
ggplot(data.frame(Feature = names(missing_data_post), Missing = missing_data_post), aes(x = Feature, y = Missing)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = "Missing Values by Feature After Imputation", x = "Feature", y = "Count of Missing Values")

#Outlier Detection and Treatment
ggplot(nba_data, aes(x = SHOT_DISTANCE)) +
  geom_histogram(binwidth = 5, fill = "coral", color = "white") +
  theme_minimal() +
  labs(title = "Shot Distance Distribution Before Outlier Treatment", x = "Shot Distance", y = "Frequency")

Q1 <- quantile(nba_data$SHOT_DISTANCE, 0.25, na.rm = TRUE)
Q3 <- quantile(nba_data$SHOT_DISTANCE, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

nba_data <- nba_data %>%
  mutate(SHOT_DISTANCE = ifelse(SHOT_DISTANCE < (Q1 - 1.5 * IQR_value) | SHOT_DISTANCE > (Q3 + 1.5 * IQR_value),
                                NA, SHOT_DISTANCE))

nba_data$SHOT_DISTANCE[is.na(nba_data$SHOT_DISTANCE)] <- median(nba_data$SHOT_DISTANCE, na.rm = TRUE)

ggplot(nba_data, aes(x = SHOT_DISTANCE)) +
  geom_histogram(binwidth = 5, fill = "darkorange", color = "white") +
  theme_minimal() +
  labs(title = "Shot Distance Distribution After Outlier Treatment", x = "Shot Distance", y = "Frequency")

#Normalize Numeric Features
ggplot(nba_data, aes(x = SHOT_DISTANCE)) +
  geom_histogram(fill = "lightgreen", color = "white") +
  theme_minimal() +
  labs(title = "Shot Distance Before Normalization", x = "Shot Distance", y = "Frequency")

nba_data <- nba_data %>%
  mutate(SHOT_DISTANCE = (SHOT_DISTANCE - min(SHOT_DISTANCE, na.rm = TRUE)) / (max(SHOT_DISTANCE, na.rm = TRUE) - min(SHOT_DISTANCE, na.rm = TRUE)))

ggplot(nba_data, aes(x = SHOT_DISTANCE)) +
  geom_histogram(fill = "purple", color = "white") +
  theme_minimal() +
  labs(title = "Shot Distance After Normalization", x = "Normalized Shot Distance", y = "Frequency")

#Feature Engineering for Temporal Attributes
shot_success_rate <- nba_data %>%
  group_by(QUARTER) %>%
  summarize(success_rate = mean(as.numeric(SHOT_MADE), na.rm = TRUE))

ggplot(shot_success_rate, aes(x = QUARTER, y = success_rate)) +
  geom_line(group = 1, color = "darkgreen") +
  theme_minimal() +
  labs(title = "Shot Success Rate by Quarter", x = "Quarter", y = "Success Rate")

# Step 6: Create Derived Shooting Features
position_success <- nba_data %>%
  group_by(POSITION_GROUP) %>%
  summarize(success_rate = mean(as.numeric(SHOT_MADE), na.rm = TRUE))

ggplot(position_success, aes(x = POSITION_GROUP, y = success_rate)) +
  geom_bar(stat = "identity", fill = "orange") +
  theme_minimal() +
  labs(title = "Shot Success Rate by Position", x = "Position Group", y = "Success Rate")

# Step 7: t-SNE
nba_features <- nba_data %>%
  group_by(PLAYER_ID) %>%
  summarize(
    avg_shot_distance = mean(SHOT_DISTANCE, na.rm = TRUE),
    shot_success_rate = mean(as.numeric(SHOT_MADE), na.rm = TRUE),
    avg_time_remaining = mean(SECS_LEFT, na.rm = TRUE),
    total_shots = n(),
    shots_per_quarter = n() / n_distinct(QUARTER),
    clutch_shot_rate = mean(as.numeric(SHOT_MADE[SECS_LEFT < 120]), na.rm = TRUE)
  ) %>%
  ungroup()

set.seed(42)
tsne_result <- Rtsne(scale(nba_features), perplexity = 30, dims = 2)

tsne_df <- data.frame(
  tSNE1 = tsne_result$Y[,1],
  tSNE2 = tsne_result$Y[,2]
)

ggplot(tsne_df, aes(x = tSNE1, y = tSNE2)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "t-SNE Projection of NBA Shooting Patterns")

# Shot Type Distribution by Position Group
ggplot(nba_data, aes(x = POSITION_GROUP, fill = SHOT_TYPE)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Shot Type Distribution by Position Group",
       x = "Position Group", y = "Proportion", fill = "Shot Type") +
  scale_fill_brewer(palette = "Set2")

# Average Shot Distance by Position Group
avg_shot_distance <- nba_data %>%
  group_by(POSITION_GROUP) %>%
  summarize(avg_distance = mean(SHOT_DISTANCE, na.rm = TRUE))

ggplot(avg_shot_distance, aes(x = POSITION_GROUP, y = avg_distance, fill = POSITION_GROUP)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Shot Distance by Position Group",
       x = "Position Group", y = "Average Shot Distance (ft)") +
  scale_fill_brewer(palette = "Set3")

# Shot Zone Usage by Position Group
ggplot(nba_data, aes(x = POSITION_GROUP, fill = BASIC_ZONE)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Shot Zone Usage by Position Group",
       x = "Position Group", y = "Proportion", fill = "Court Zone") +
  scale_fill_brewer(palette = "Paired")

# Shot Success Rate by Position and Shot Type
success_rate <- nba_data %>%
  group_by(POSITION_GROUP, SHOT_TYPE) %>%
  summarize(success_rate = mean(as.numeric(SHOT_MADE), na.rm = TRUE))

ggplot(success_rate, aes(x = POSITION_GROUP, y = success_rate, fill = SHOT_TYPE)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Shot Success Rate by Position and Shot Type",
       x = "Position Group", y = "Success Rate", fill = "Shot Type") +
  scale_fill_brewer(palette = "Dark2")

# Average Shot Distance by Position and Court Zone
avg_distance_zone <- nba_data %>%
  group_by(POSITION_GROUP, BASIC_ZONE) %>%
  summarize(avg_distance = mean(SHOT_DISTANCE, na.rm = TRUE))

ggplot(avg_distance_zone, aes(x = BASIC_ZONE, y = avg_distance, fill = POSITION_GROUP)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Average Shot Distance by Position and Court Zone",
       x = "Court Zone", y = "Average Shot Distance", fill = "Position Group") +
  scale_fill_brewer(palette = "Spectral") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Distribution of Shot Distances by Position Group
ggplot(nba_data, aes(x = SHOT_DISTANCE, fill = POSITION_GROUP)) +
  geom_density(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Distribution of Shot Distances by Position Group",
       x = "Shot Distance", y = "Density", fill = "Position Group") +
  scale_fill_brewer(palette = "Accent")

# Shot Frequency by Position and Zone Range
ggplot(nba_data, aes(x = ZONE_RANGE, fill = POSITION_GROUP)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Shot Frequency by Position and Zone Range",
       x = "Zone Range", y = "Proportion", fill = "Position Group") +
  scale_fill_brewer(palette = "Pastel1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 1: Feature Selection - Aggregate shooting data per player
player_shooting <- nba_data %>%
  group_by(PLAYER_NAME) %>%
  summarize(
    avg_shot_distance = mean(SHOT_DISTANCE, na.rm = TRUE),
    pct_3pt = mean(SHOT_TYPE == "3PT Field Goal", na.rm = TRUE),
    pct_success = mean(as.numeric(SHOT_MADE), na.rm = TRUE),
    restricted_area_rate = mean(BASIC_ZONE == "Restricted Area", na.rm = TRUE),
    midrange_rate = mean(BASIC_ZONE == "Mid-Range", na.rm = TRUE),
    corner_3_rate = mean(BASIC_ZONE %in% c("Left Corner 3", "Right Corner 3"), na.rm = TRUE),
    above_break_rate = mean(BASIC_ZONE == "Above the Break 3", na.rm = TRUE)
  )

# Step 2: PCA (Optional for visualization purposes)
pca_res <- prcomp(player_shooting[,-1], center = TRUE, scale. = TRUE)
player_shooting_pca <- as.data.frame(pca_res$x)

# Step 3: Choose optimal K value for K-Means
# Elbow method to determine the optimal number of clusters
fviz_nbclust(player_shooting_pca, kmeans, method = "wss") +
  labs(title = "Optimal Number of Clusters (Elbow Method)")

# Step 4: Apply K-Means Clustering
set.seed(123) # For reproducibility
k_clusters <- kmeans(player_shooting_pca, centers = 4, nstart = 25)

# Add cluster results to player data
player_shooting$cluster <- as.factor(k_clusters$cluster)

position_groups <- nba_data %>%
  select(PLAYER_NAME, POSITION_GROUP) %>%
  distinct()

player_shooting <- player_shooting %>%
  left_join(position_groups, by = "PLAYER_NAME")

# Step 5: Visualize Clusters in 2D
# Using PCA dimensions for visualization
ggplot(player_shooting_pca, aes(x = PC1, y = PC2, color = player_shooting$cluster, shape = player_shooting$POSITION_GROUP)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Player Clusters Based on Shooting Patterns",
       x = "Principal Component 1", y = "Principal Component 2", color = "Cluster", shape = "Position Group")

# Alternatively, using selected shooting features directly
ggplot(player_shooting, aes(x = avg_shot_distance, y = pct_success, color = cluster, shape = player_shooting$POSITION_GROUP)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Player Clusters by Avg Shot Distance and Success Rate",
       x = "Average Shot Distance", y = "Shooting Success Rate", color = "Cluster", shape = "Position Group")
