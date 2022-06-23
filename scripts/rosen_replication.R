# Load Packages ----------------------------------------------------------------
library(tidyverse)   # For creating plots and data
library(showtext)    # For using Google Fonts
library(sf)          # For creating maps
library(cowplot)     # For editing plot details
library(here)        # For path management
library(ggrepel)     # For adding point labels to plots

# Set-up -----------------------------------------------------------------------
font_add_google("Manrope", "manrope")
showtext_auto()
set.seed(100)
red_col = "#9D2235"

# Define Functions -------------------------------------------------------------
# Theme for Rosenberg Research Style Plots
theme_rosen <- function(title_size=12) {
  theme(
    # Set the grids lines to dotted grey
    panel.grid.major = element_line(color = "grey", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    
    # Set x-axis to dotted grey, y-axis to solid grey
    axis.line.x = element_line(color = "grey", linetype = "dotted"),
    axis.line.y = element_line(color = "grey"),
    axis.ticks = element_line(color = "grey"),
    axis.text = element_text(color = "black"),
    
    # Set background to white
    panel.background = element_blank(),
    
    # Set text positions
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(vjust = 2, face = 'bold', color = red_col, size = title_size),
    plot.caption = element_text(hjust = 0),
    plot.subtitle = element_text(vjust = 3, size = title_size)
  )
}

# Time Series ------------------------------------------------------------------
# Simulate time series data
ts_vals1 <- 500 + seq(from = 200/60, to = 200, by = 200/60) + rnorm(n = 60, mean = 0, sd = 20)
ts_vals2 <- 550 + seq(from = 200/60, to = 200, by = 200/60) + rnorm(n = 60, mean = 0, sd = 15)
ts_dates <- seq.Date(from = as.Date("2017/1/1"), length.out = 60, by = "month")
time_series <- tibble(month = ts_dates, value1 = ts_vals1, value2 = ts_vals2)

# Plot time series
timeseries1 <-
  ggplot(data = time_series) +
  geom_line(aes(x = month, y = value1), col = red_col) +
  
  # Set appropriate axis scales
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  scale_y_continuous(n.breaks = 7, limits = c(450, 750), expand = c(0, 0)) +
  
  # Set Labels and theme
  labs(
    x = "", y = "", title = "Simulated Time Series", 
    subtitle = "(index)",
    caption = "CHART STYLE FROM ROSENBERG RESEARCH"
    ) +
  theme_rosen() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))

# Pivot Data to Long Format and Assign Time Series Values to Countries
time_series_long <- 
  time_series |> 
  pivot_longer(cols = c("value1", "value2"), names_to = "series", values_to = "value") |>
  mutate(series = if_else(series == "value1", "Canadian National Index", "Mexican National Index"))

timeseries2 <-
  ggplot(data = time_series_long) +
  geom_line(aes(x = month, y = value, col = series)) +
  
  # Set appropriate axis scales
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  scale_y_continuous(n.breaks = 8, limits = c(450, 800), expand = c(0, 0)) +
  scale_color_manual(values = c("Canadian National Index" = red_col, "Mexican National Index" = "blue")) +
  
  # Set Labels and theme
  labs(
    x = "", y = "", title = "Simulated Time Series", 
    subtitle = "(index)", col = "",
    caption = "CHART STYLE FROM ROSENBERG RESEARCH"
  ) +
  theme_rosen() +
  theme(
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "bottom",
    legend.key = element_blank()
    )

ggsave(filename = "timeseries1.png", path = here("outputs"), plot = timeseries1, width = 8, height = 5)
ggsave(filename = "timeseries2.png", path = here("outputs"), plot = timeseries2, width = 8, height = 5)

# Bar Chart --------------------------------------------------------------------
# Simulate Data for Automotive Bar Chart
bar_vals <- sample(x = seq(10, 40), size = 4)
bar_groups <- c("Auto Sales", "Auto Production", "Auto New Orders", "Auto Investment")
auto <- tibble(value = bar_vals, category = bar_groups)

# Plot bar chart
barplot1 <-
  ggplot(data = auto, aes(x = category, y = value)) +
  geom_col(fill = red_col, width = 0.5) +
  geom_text(aes(label = paste0(value, "%")), nudge_y = 1, size = 4) +
  
  # Set appropriate axis scales
  scale_y_continuous(n.breaks = 10, limits = c(0, 50), expand = c(0, 0)) +
  
  # Set labels and theme
  labs(
    x = "", y = "", title = "Simulated Automotive Data", 
    subtitle = "(year-over-year percent change)",
    caption = "REPLICATION OF CHART STYLE FROM ROSENBERG RESEARCH"
  ) +
  theme_rosen() + 
  theme(
    axis.line.x = element_line(linetype = "solid"), 
    axis.ticks.x = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "cm")
    )

# Simulate Data for Survey Bar Chart
bar_vals <- c(57, 22, 12, 6, 3)
bar_groups <- c("Very Good", "Good", "Okay", "Poor", "Very Poor")
survey <- tibble(percent = bar_vals, rating = ordered(bar_groups, levels = rev(bar_groups)))

# Plot
barplot2 <-
  ggplot(data = survey |> mutate(fill = (rating == "Very Good"))) + 
  geom_col(aes(x = percent, y = rating, fill = fill)) +
  
  # Set scales
  scale_fill_manual(values = c(`TRUE` = red_col, `FALSE` = "grey")) +
  scale_x_continuous(n.break = 6, limits = c(0, 60), labels = paste0(seq(0, 60, 10), "%")) +
  
  # Set labels
  labs(
    x = "", y = "", 
    title = "What is the current state of the Canadian economy?", 
    subtitle = "Simulated Survey Data\n(percent response)",
    caption = "REPLICATION OF CHART STYLE FROM ROSENBERG RESEARCH"
    ) +
 
  # Set theme
  theme_rosen() +
  theme(
    # Remove Features
    panel.grid.major.y = element_blank(), 
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    
    # Add Features
    panel.grid.major.x = element_line(colour = "lightgrey", linetype = "solid"),
    axis.text.y = element_text(margin = margin(r = -10), hjust = 1),
    plot.margin = margin(1, 1, 1, 1, "cm")
    )

ggsave(filename = "barplot1.png", path = here("outputs"), plot = barplot1, width = 6, height = 6)
ggsave(filename = "barplot2.png", path = here("outputs"), plot = barplot2, width = 8, height = 5)

# Choropleth -------------------------------------------------------------------
# Load Toronto Neighbourhood Map and Simulate Data
neighbourhoods <- st_read(dsn = here("data/toronto_neighbourhood_map"), layer = "Neighbourhoods v2_region")
neighbourhood_vals <- sample(x = 1:7, size = 140, replace = TRUE)
neighbourhoods$value <- neighbourhood_vals

# Create gradient color palette using Rosenberg Red
color_pal <- colorRampPalette(colors = c("white", red_col))(10)[3:10]

# Create simple map plot and get plot legend as object
plt <- 
  ggplot(data = neighbourhoods) + geom_sf(aes(fill = value), col = "black") +
  scale_fill_stepsn(show.limits = T, colors = color_pal) + labs(fill = "")
lgd <- get_legend(plt)

# Set plot theme and labels
plt <- 
  plt +
  labs(fill = "", caption = "SIMULATED DATA - TORONTO NEIGHBOURHOOD CHOROPLETH") +
  theme_void() + theme_rosen() + 
  theme(
    # Remove Features
    panel.grid.major = element_blank(), 
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    legend.position = "none",
    # Center Caption
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0.5),
    # Add White Background
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.margin = margin(1, 1, 1, 1, "cm")
    )

# Create choropleth with inset legend
choropleth <-
  ggdraw() +
  draw_plot(plt) +
  draw_grob(lgd, x = 0.75, y = .25, width = 0.3, height = 0.3)

ggsave(filename = "choropleth.png", path = here("outputs"), plot = choropleth)
