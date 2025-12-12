library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)
library(stats)

# --- 0. Set Working Directory ---
# SET THIS TO YOUR LOCAL PATH:
setwd("/home/visal/Downloads/")

# --- 1. Data Loading and Initial Cleaning ---
# Using base R's read.csv and assuming the file is named 'NY-House-Dataset.csv'
df <- read.csv('NY-House-Dataset.csv')

# Renaming and data type conversion (essential for large number/price strings)
df <- df %>%
  rename(
    Price = PRICE,
    PropertySQFT = PROPERTYSQFT
  )

# Convert columns to numeric, handling potential commas (as seen in the original data snippet)
if(is.character(df$Price)) { df$Price <- as.numeric(gsub('[$,]', '', df$Price)) }
if(is.character(df$PropertySQFT)) { df$PropertySQFT <- as.numeric(gsub('[$,]', '', df$PropertySQFT)) }

# Outlier Filtering (Capping at 99th percentile for robustness)
price_cap <- quantile(df$Price, 0.99, na.rm = TRUE)
sqft_cap <- quantile(df$PropertySQFT, 0.99, na.rm = TRUE)

df_filtered <- df %>%
  filter(Price <= price_cap, PropertySQFT <= sqft_cap)


# --- 2. Categorization for ANOVA (SQFT_Group) ---
# Create SQFT_Group (Low / Medium / High by tertiles using the filtered data)
df_filtered$SQFT_Group <- cut(
  df_filtered$PropertySQFT,
  breaks = quantile(df_filtered$PropertySQFT, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("Low", "Medium", "High"),
  ordered_result = TRUE
)

# Check grouping
cat("\n--- SQFT Group Distribution ---\n")
print(table(df_filtered$SQFT_Group))


# --- 3. Set Custom Theme for Visual Consistency ---
theme_set(
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
      axis.title = element_text(face = "bold", size = 12),
      legend.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
)


# --- 4. Visualizations (Matching requested style, colors, and labels) ---

# --- 4.1. Histogram of Price (Appendix D) ---
p_hist <- ggplot(df_filtered, aes(x = Price)) +
  geom_histogram(
    bins = 50,
    aes(y = after_stat(density)),
    color = "black",
    fill = "#4A90E2", # Blue color
    alpha = 0.7
  ) +
  geom_density(color = "#E94B3C", linewidth = 1.2) + # Red color
  scale_x_continuous(labels = label_comma()) +
  labs(
    title = "Distribution of Property Price (99th Percentile Capped)",
    x = "Price (USD)", # Matching previous label
    y = "Density" # Density used in histogram examples
  )
ggsave("price_distribution_histogram.png", p_hist, width = 10, height = 6)


# --- 4.2. Box Plot by SQFT Group (Appendix C) ---
p_box <- ggplot(df_filtered, aes(x = SQFT_Group, y = Price, fill = SQFT_Group)) +
  geom_boxplot(
    outlier.colour = "#E94B3C", # Red color
    outlier.shape = 16,
    outlier.size = 2
  ) +
  scale_y_continuous(labels = label_comma()) +
  # Custom colors for the boxes (adapted from your original code colors)
  scale_fill_manual(values = c("Low" = "#50C878", "Medium" = "#FFA500", "High" = "#E94B3C")) +
  labs(
    title = "Property Price by Square Footage Category",
    x = "Square Footage Group (Low / Medium / High)", # Matching previous label
    y = "Price (USD)" # Matching previous label
  ) +
  theme(legend.position = "none")
ggsave("boxplot_sqft_group.png", p_box, width = 10, height = 6)


# --- 4.3. Scatter Plot (Appendix B) ---
p_scatter <- ggplot(df_filtered, aes(x = PropertySQFT, y = Price)) +
  geom_point(color = "#4A90E2", alpha = 0.6, size = 1.5) + # Blue color for points
  geom_smooth(method = "lm", se = TRUE,
              color = "#E94B3C", fill = "#E94B3C", # Red color for line
              alpha = 0.2, linewidth = 1.2) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Relationship Between Square Footage and Price",
    x = "Property Square Footage (sq ft)", # Matching previous label
    y = "Property Price (USD)" # Matching previous label
  )
ggsave("scatter_sqft_price.png", p_scatter, width = 12, height = 8)


# --- 5. Analysis: One-Way ANOVA ---
# Fit the ANOVA model
anova_model <- aov(Price ~ SQFT_Group, data = df_filtered)

# Print the full ANOVA summary table (Output for the 'Analysis' section)
cat("\n--- One-Way ANOVA Output (Price ~ SQFT_Group) ---\n")
print(summary(anova_model))

# --- 6. Post-hoc Test (Tukey HSD) - Recommended for ANOVA ---
cat("\n--- Tukey HSD Post-hoc Test ---\n")
tukey_res <- TukeyHSD(anova_model, "SQFT_Group")
print(tukey_res)

# --- 7. Summary Statistics by Group ---
group_stats <- df_filtered %>%
  group_by(SQFT_Group) %>%
  summarise(
    n = n(),
    mean_price = mean(Price, na.rm = TRUE),
    median_price = median(Price, na.rm = TRUE),
    sd_price = sd(Price, na.rm = TRUE)
  )
cat("\n--- Summary Statistics by SQFT Group ---\n")
print(group_stats)

cat("\nAll plots (Histogram, Box Plot, Scatter Plot) saved successfully to the working directory.\n")