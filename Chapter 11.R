# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)

# ----------------------------
# 11.2.1 Exercises
# ----------------------------

# Load fuel economy dataset
data(mpg)

# Scatterplot: Highway vs. City MPG with color & shape by drivetrain
ggplot(mpg, aes(x = cty, y = hwy, color = drv, shape = drv)) +
  geom_point() +
  labs(
    title = "Fuel Efficiency Comparison",
    subtitle = "City vs. Highway Fuel Economy",
    x = "City MPG",
    y = "Highway MPG",
    color = "Drive Train",
    shape = "Drive Train",
    caption = "Data Source: ggplot2 mpg dataset"
  ) +
  theme_minimal()

# ----------------------------
# 11.3.1 Exercises
# ----------------------------

# Scatterplot with geom_text() placing text at four corners
ggplot(mpg, aes(x = cty, y = hwy, color = drv, shape = drv)) +
  geom_point() +
  geom_text(aes(x = min(cty), y = min(hwy), label = "Bottom Left"), vjust = -1, hjust = -0.2) +
  geom_text(aes(x = min(cty), y = max(hwy), label = "Top Left"), vjust = 1.5, hjust = -0.2) +
  geom_text(aes(x = max(cty), y = min(hwy), label = "Bottom Right"), vjust = -1, hjust = 1.2) +
  geom_text(aes(x = max(cty), y = max(hwy), label = "Top Right"), vjust = 1.5, hjust = 1.2) +
  labs(
    title = "Fuel Efficiency with Corner Labels",
    subtitle = "Text added using geom_text()",
    x = "City MPG",
    y = "Highway MPG",
    color = "Drive Train",
    shape = "Drive Train"
  ) +
  theme_minimal()

# Annotate a point in the middle of the plot
ggplot(mpg, aes(x = cty, y = hwy, color = drv, shape = drv)) +
  geom_point() +
  annotate("point", x = median(mpg$cty), y = median(mpg$hwy), shape = 8, size = 5, color = "black") +
  labs(
    title = "Fuel Efficiency with Annotated Center Point",
    subtitle = "Using annotate() to mark the median",
    x = "City MPG",
    y = "Highway MPG"
  ) +
  theme_minimal()

# ----------------------------
# 11.4.6 Exercises
# ----------------------------

# Issue with scale_color_gradient() in hex plot
df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)

ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_fill_gradient(low = "white", high = "red") +  # Corrected scale from color to fill
  coord_fixed()

# Presidential terms plot with improved formatting
presidential <- ggplot2::presidential

ggplot(presidential, aes(x = start, y = 1, fill = party)) +
  geom_rect(aes(xmin = start, xmax = end, ymin = 0.5, ymax = 1.5)) +
  geom_text(aes(x = start, y = 1.5, label = name), angle = 45, hjust = 0, size = 4) +
  scale_fill_manual(values = c("Republican" = "red", "Democratic" = "blue")) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  scale_y_continuous(breaks = NULL) +  # Remove y-axis ticks
  labs(
    title = "U.S. Presidential Terms",
    subtitle = "Terms colored by party affiliation",
    x = "Year",
    y = NULL,
    fill = "Political Party"
  ) +
  theme_minimal()

# Modify the diamonds scatterplot for better visibility
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(aes(color = cut), alpha = 1/20) +
  scale_color_manual(values = c("Fair" = "red", "Good" = "blue", 
                                "Very Good" = "green", "Premium" = "purple", 
                                "Ideal" = "orange")) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(
    title = "Diamond Price vs Carat",
    subtitle = "Colored by diamond cut",
    x = "Carat",
    y = "Price ($)",
    color = "Cut"
  ) +
  theme_minimal()

# ----------------------------
# 11.5.1 Exercises
# ----------------------------

# Applying ggthemes to the last plot and making axis labels blue and bold
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(aes(color = cut), alpha = 1/20) +
  scale_color_manual(values = c("Fair" = "red", "Good" = "blue", 
                                "Very Good" = "green", "Premium" = "purple", 
                                "Ideal" = "orange")) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(
    title = "Diamond Price vs Carat",
    subtitle = "Colored by diamond cut",
    x = "Carat",
    y = "Price ($)",
    color = "Cut"
  ) +
  theme_economist() +  # Apply Economist theme from ggthemes
  theme(
    axis.title.x = element_text(color = "blue", face = "bold"), 
    axis.title.y = element_text(color = "blue", face = "bold")
  )

