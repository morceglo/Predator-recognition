setwd("C:/Users/Gloriana/Desktop/Predator recognition")

library(readr)
data <- read_delim("data.csv", delim = ";", 
                   escape_double = FALSE, col_types = cols(bat = col_character(), 
                                                           experiment = col_character(), order = col_character(), 
                                                           treatment = col_character(), bat_type = col_character(), 
                                                           period = col_character(), response = col_number()), 
                   trim_ws = TRUE)
View(data)



#Generalized linear mixed model (GLMM) to test if the number of responses in each period is determined by the treatment.

# Load necessary package
library(lme4)

data$treatment <- as.factor(data$treatment)

# Fit the GLMM with a Poisson distribution
glmm_poisson <- glmer(response ~ treatment*period + (1 | bat), family = poisson, data = data)

# Summary of the model
summary(glmm_poisson)


# Load the glmmTMB package
library(glmmTMB)

# Fit the GLMM with a Negative Binomial distribution
glmm_nb <- glmmTMB(response ~ treatment*period + (1 | bat), family = nbinom2, data = data)

# Summary of the model
summary(glmm_nb)

library(car)

# Run a Type III Wald chi-squared test for the model terms
anova_results <- Anova(glmm_nb, type = "III")
print(anova_results)


#Diagnostics for the model

library(DHARMa)

# Simulate residuals for diagnostics
residuals <- simulateResiduals(glmm_nb)
plot(residuals)

#Choosing Between Poisson and Negative Binomial

AIC(glmm_poisson, glmm_nb) #results show NB is better


##determine if the number of response calls differ within treatment given the three periods
library(emmeans)
library(dplyr)

# Get estimated marginal means for the interaction
emm <- emmeans(glmm_nb, ~ period | treatment)

# Perform pairwise comparisons with Tukey adjustment
pairwise_results <- contrast(emm, method = "pairwise", adjust = "tukey")
pairwise_df <- as.data.frame(pairwise_results)

#Make a graph with these results

# Load necessary libraries
library(ggplot2)
library(viridis)

# Reorder the 'period' factor
data$period <- factor(data$period, levels = c("before", "during", "after"))

# Create a data frame with significance annotations
signif_data <- data.frame(
  treatment = c("non-predator", "non-predator", "non-predator", 
                "predator", "predator", "predator"),
  x = c(1, 1, 2.1, 1, 1, 2.1),        # Starting x position for each line
  xend = c(1.9, 3, 3, 1.9, 3, 3),     # Ending x position for each line
  y = c(110, 121, 110, 110, 121, 110),  # Y position for each line
  label_x = c(1.5, 2, 2.5, 1.5, 2, 2.5),  # X position for the label
  label_y = c(115, 125, 115, 115, 125, 115),  # Y position for the label
  label = c("ns", "ns", "*", "***", "*", "***")  # Labels
)


# Base plot with boxplots and jittered points
plot <- ggplot(data, aes(x = period, y = response)) +
  # Boxplots with specific fill colors
  geom_boxplot(aes(fill = period), color = "black", position = position_dodge(0.8), outlier.shape = NA) +
  # Lines per bat
  geom_line(aes(group = bat), alpha = 0.5, linetype = "dotted", color = "black") +
  # Individual points jittered, all in black
  geom_point(color = "black", position = position_jitter(width = 0.1, height = 0)) +
  # Facets for treatments
  facet_wrap(~treatment, scales = "free_y", strip.position = "top") +
  # Manual color scale for periods
  scale_fill_manual(values = c("before" = "white", "during" = "grey", "after" = "white")) +
  # Minimal theme
  theme_minimal() +
  labs(
    x = "",
    y = "Number of responses",
    title = ""
  ) +
  theme(
    legend.position = "none",  # Remove legend
    strip.background = element_blank(),  # Remove default strip background
    strip.text = element_text(size = 16, face = "bold"),  # Make facet titles clear
    strip.placement = "outside",  # Move facet titles outside the graph area
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add borders to each facet
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12)
  )

# Add horizontal lines and significance labels dynamically
plot <- plot +
  geom_segment(data = signif_data, aes(x = x, xend = xend, y = y, yend = y), color = "black") +  # Horizontal significance lines
  geom_text(data = signif_data, aes(x = label_x, y = label_y, label = label), size = 4)  # Significance labels

# Print the plot
print(plot)


ggsave("Figure2.png", width = 8, height = 4, dpi = 600)



##------------------------------------------------------------------------------------------------------

#Generalized linear mixed model (GLMM) to test if the number of responses is determined by the species (during playback of echolocation calls).


# Load necessary packages
library(glmmTMB)

# Subset data to only include the 'during' period
data_during <- subset(data, period == "during")

# Fit the GLMM with Negative Binomial distribution
model <- glmmTMB(response ~ bat_type + (1|bat), 
                 family = nbinom2, 
                 data = data_during)

# Check the model summary
summary(model)


library(car)

# Run a Type III Wald chi-squared test for the model terms
anova_results <- Anova(model, type = "III")
print(anova_results)

# Load the emmeans package
library(emmeans)

# Obtain the estimated marginal means (EMMs) for bat_type
emm_results <- emmeans(model, ~ bat_type)

# Display the EMMs
summary(emm_results)

# Run pairwise comparisons between the bat_types
pairwise_comparisons <- contrast(emm_results, method = "pairwise")

# Display the pairwise comparisons
summary(pairwise_comparisons)

##-----------------------------------------------------------------------------------
#Perform PCA on the acoustic variables (columns A to AP).
#Run a k-means cluster analysis to separate the species (based on bat_type in column AQ).
#Compute Euclidean distances between the centroids.

# Load necessary libraries
library(tidyverse)
library(cluster)

# Load the data

library(readxl)
calls <- read_excel("calls.xlsx")
View(calls)

# Select acoustic variables (columns A to AP)
acoustic_data <- calls %>%
  select(1:42)  # Adjust this range as needed to match your columns

# Standardize the acoustic variables
acoustic_data_scaled <- scale(acoustic_data)

# Perform PCA
pca_result <- prcomp(acoustic_data_scaled, center = TRUE, scale. = TRUE)

# View summary of PCA
summary(pca_result)

# Plot the first two principal components
biplot(pca_result, scale = 0)

# Add the first two principal components to the data
call_data <- calls %>%
  mutate(PC1 = pca_result$x[,1], PC2 = pca_result$x[,2])

# Perform k-means clustering (4 clusters, corresponding to species)
set.seed(123)
kmeans_result <- kmeans(acoustic_data_scaled, centers = 4)

# Add cluster assignments to the data
call_data <- calls %>%
  mutate(Cluster = kmeans_result$cluster)

# Compare clusters with species
table(call_data$bat_type, call_data$Cluster)

# Compute Euclidean distances between cluster centroids
dist_matrix <- dist(kmeans_result$centers, method = "euclidean")

# Convert distance matrix to a dataframe
dist_df <- as.data.frame(as.matrix(dist_matrix))
dist_df


#Make a graph showing PC1 and PC2 values per species, with ellipses
# Load necessary library
library(dplyr)

# Calculate centroids for each bat_type
centroids <- call_data %>%
  dplyr::group_by(bat_type) %>%
  dplyr::summarize(PC1 = mean(PC1, na.rm = TRUE), PC2 = mean(PC2, na.rm = TRUE), .groups = "drop")

# Verify the centroids data
print(centroids)

# Plot PCA with ellipses and labels
# Calculate centroids and adjust for label position
centroids <- call_data %>%
  dplyr::group_by(bat_type) %>%
  dplyr::summarize(PC1 = mean(PC1, na.rm = TRUE), 
                   PC2 = mean(PC2, na.rm = TRUE), 
                   .groups = "drop") %>%
  mutate(PC2 = PC2 + 0.2 * (max(call_data$PC2) - min(call_data$PC2)))  # Adjust PC2 upward for label positioning

# Plot PCA with ellipses and adjusted labels
plot_PCA <- ggplot(call_data, aes(x = PC1, y = PC2, color = bat_type)) +
  geom_point(size = 2, alpha = 0.7) +  # Points for individual values
  stat_ellipse(type = "norm", level = 0.95, size = 1) +  # Confidence ellipses
  geom_text(
    data = centroids, 
    aes(x = PC1, y = PC2, label = bat_type),  # Explicit x and adjusted y mapping
    color = "black", 
    size = 5, 
    inherit.aes = FALSE
  ) +  # Add labels above centroids
  labs(
    title = "",
    x = "PC1",
    y = "PC2",
    color = "Species"
  ) +
  theme_minimal() +
  theme(
    legend.position = "",
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

# Print the updated plot
print(plot_PCA)


#Create a boxplot that shows the number of response calls in the "during" trials per species.

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Subset the data to only include the "during" period
data_during <- data %>%
  filter(period == "during")

# Define the order of bat_type categories
bat_type_order <- c("control", "insectivorous", "carnivorous", "frugivorous")

# Ensure bat_type is a factor with the specified order
data_during$bat_type <- factor(data_during$bat_type, levels = bat_type_order)

# Create the box plot
box_plot <- ggplot(data_during, aes(x = bat_type, y = response)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Add individual points
  labs(
    title = "",
    x = "",
    y = "Number of responses"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

# Print the box plot
print(box_plot)


#Merge PCA plot with boxplot
# Load necessary library for merging plots
library(patchwork)

# Merge the PCA plot and box plot
combined_plot <- plot_PCA / box_plot + 
  plot_layout(heights = c(1, 1))  # Adjust the height ratios if necessary

# Print the combined plot
print(combined_plot)

ggsave("Figure3.png", width = 6, height = 6, dpi = 600)


##-----------------------------------------------------------------------------------

#Generalized linear mixed model (GLMM) to test if the number of responses is determined by the order in which the predator calls are presented by call type.

library(glmmTMB)

# Subset data to only include the 'during' period
data_during <- subset(data, period == "during")

# Reorder the 'bat_type' factor
data$bat_type <- factor(data$bat_type, levels = c("control", "insectivorous", "carnivorous", "frugivorous"))

# Fit the GLMM with a Negative Binomial distribution
glmm_nb_order_species <- glmmTMB(response ~ order*bat_type + (1 | bat), family = nbinom2, data = data_during)

# Summary of the model
summary(glmm_nb_order_species)

library(car)

# Run a Type III Wald chi-squared test for the model terms
anova_results <- Anova(glmm_nb_order_species, type = "III")
print(anova_results)


##determine if the number of response calls differ within treatment given the three periods
library(emmeans)
library(dplyr)

# Get estimated marginal means for the interaction
emm <- emmeans(glmm_nb_order_species, ~ bat_type | order)

# Perform pairwise comparisons with Tukey adjustment
pairwise_results <- contrast(emm, method = "pairwise", adjust = "tukey")
pairwise_results

# Summarize the pairwise results to extract the p-values
pairwise_summary <- summary(pairwise_results)

# Now, you can access the p-values from the summary
p_values <- pairwise_summary$p.value

#Graph that shows these results
# Load necessary package
library(ggplot2)


# Create a boxplot showing responses by 'bat_type' and 'order', with individual lines
# Base plot with boxplots showing both bat_type and order in separate panels
plot <- ggplot(data, aes(x = bat_type, y = response, fill = order)) +
  # Boxplots with specific fill colors for 'order'
  geom_boxplot(aes(color = order), position = position_dodge(0.8), outlier.shape = NA) +  # Boxplots by bat_type and order
  scale_fill_manual(values = c("NP-P" = "white", "P-NP" = "gray")) +  # Set NP-P to white and P-NP to gray
  scale_color_manual(values = c("NP-P" = "black", "P-NP" = "black")) +  # Set line color to black (or neutral)
  theme_minimal() +
  labs(
    x = "",
    y = "Number of responses",
    title = ""
  ) +
  theme(
    legend.position = "none",  # Make legend visible
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    strip.placement = "outside",  # Place strip labels outside the panel
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 0),  # Make x-axis labels horizontal
    axis.text.y = element_text(size = 12)
  ) +
  facet_wrap(~ order, scales = "free_y")  # Facet by 'order' with independent y-axis scales

# Create a data frame with significance annotations and p-values
signif_data <- data.frame(
  order = rep(c("NP-P", "P-NP"), each = 3),  # Orders for each facet
  x = c(1, 3, 2, 1, 3, 2),  # X position for the comparison between bat_types (control-carnivorous, insectivorous-carnivorous, etc.)
  xend = c(3, 4, 3, 3, 4, 3),  # X end positions for the comparison lines
  y = c(105, 100, 95, 105, 100, 95),  # Y position for p-values (adjust as needed)
  label = c("***", "ns", "**",  # p-values for NP-P
            "***", "***", "***")  # p-values for P-NP
)

# Add horizontal lines and p-value annotations
plot <- plot +
  geom_segment(data = signif_data, aes(x = x, xend = xend, y = y, yend = y), color = "black") +  # Line between boxes
  geom_text(data = signif_data, aes(x = (x + xend) / 2, y = y + 4, label = label), size = 4)  # p-values (placed slightly above lines)

# Print the plot
print(plot)


ggsave("Figure4.png", width = 10, height = 3, dpi = 600)
































##------------------------------------------------------------------------------------------------------
##Supplementary materials


#Generalized linear mixed model (GLMM) to test if the number of responses is determined by the order in which the predator calls are presented.

library(glmmTMB)

# Subset data to only include the 'during' period
data_during <- subset(data, period == "during")

# Fit the GLMM with a Negative Binomial distribution
glmm_nb_order <- glmmTMB(response ~ treatment*order*period + (1 | bat), family = nbinom2, data = data_during)

# Summary of the model
summary(glmm_nb_order)


##determine if the number of response calls differ within treatment given the three periods
library(emmeans)
library(dplyr)

# Get estimated marginal means for the interaction
emm <- emmeans(glmm_nb_order, ~ period | order | treatment)

# Perform pairwise comparisons with Tukey adjustment
pairwise_results <- contrast(emm, method = "pairwise", adjust = "tukey")
pairwise_results


#Graph that shows these results
# Load necessary package
library(ggplot2)

# Reorder the 'period' factor
data$period <- factor(data$period, levels = c("before", "during", "after"))

# Base plot with boxplots and jittered points
plot <- ggplot(data, aes(x = period, y = response)) +
  # Boxplots with specific fill colors
  geom_boxplot(aes(fill = period), color = "black", position = position_dodge(0.8), outlier.shape = NA) +
  # Lines per bat
  geom_line(aes(group = bat), alpha = 0.5, linetype = "dotted", color = "black") +
  # Individual points jittered, all in black
  geom_point(color = "black", position = position_jitter(width = 0.1, height = 0)) +
  # Facets for treatments and orders
  facet_grid(treatment ~ order) +
  # Manual color scale for periods
  scale_fill_manual(values = c("before" = "white", "during" = "grey", "after" = "white")) +
  # Minimal theme
  theme_minimal() +
  labs(
    x = "",
    y = "Number of responses",
    title = ""
  ) +
  theme(
    legend.position = "none",  # Remove legend
    strip.background = element_blank(),  # Remove default strip background
    strip.text = element_text(size = 16, face = "bold"),  # Make facet titles clear
    strip.placement = "outside",  # Move facet titles outside the graph area
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add borders to each facet
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12)
  )

# Create a data frame with significance annotations
signif_data <- data.frame(
  treatment = rep(c("non-predator", "predator"), each = 6),
  order = rep(c("NP-P", "P-NP"), each = 3, times = 2),
  x = c(1, 2.1, 1, 1, 2.1, 1, 1, 2.1, 1, 1, 2.1, 1),
  xend = c(1.9, 3, 3, 1.9, 3, 3, 1.9, 3, 3, 1.9, 3, 3),
  y = c(120, 120, 112, 120, 120, 112, 120, 120, 112, 120, 120, 112),
  label_x = c(1.5, 2, 2.5, 1.5, 2, 2.5, 1.5, 2, 2.5, 1.5, 2, 2.5),
  label_y = c(124, 116, 124, 124, 116, 124, 124, 116, 124, 124, 116, 124),
  label = c("ns", "ns", "**", "ns", "ns", "ns", "***", "*", "***", "***", "ns", "***")
)

# Add the horizontal lines and significance labels using geom_segment
plot <- plot +
  geom_segment(data = signif_data, aes(x = x, xend = xend, y = y, yend = y), color = "black") +
  geom_text(data = signif_data, aes(x = label_x, y = label_y, label = label), size = 4)

# Print the plot
print(plot)


ggsave("Supplementary figure.png", width = 8, height = 8, dpi = 600)









