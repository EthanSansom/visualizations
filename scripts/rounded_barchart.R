# Load Packages ----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggtext)
library(geomtextpath)

# Create Plot ------------------------------------------------------------------
df <- 
  tibble(
    party = c("New Democrats", "Liberals", "Greens", "Conservatives", "Bloc Quebecois"),
    seats = c(25, 159, 5, 119, 33)
  )

df1 <- 
  tibble(
    party = c("New Democrats", "Liberals", "Greens", "Conservatives", "Bloc Quebecois"),
    seats = c(25, 159, 5, 119, 33)
  )

# The target un-rounded bar plot using geom_col
ggplot(df, aes(x = seats, y = party, fill = party)) +
  geom_col()

# Duplicate every row
df_len <- dim(df)[1]
df <- df[unlist(sapply(1:df_len, function (x) rep(x, 2), simplify = FALSE)), ]

# Set every second value to 0 (to create line from 0 to value)
df <- df |> mutate(seats = if_else(row_number() %% 2 == 1, 0, seats))

# Desired rounded barplot with geom_line
ggplot(df, aes(x = seats, y = party, col = party)) +
  geom_line(lineend = "round", lwd = 10)

# Make it Pretty ---------------------------------------------------------------
# Map each party to their colour
party_colour = 
  c(
    "Bloc Quebecois" = "#003F91",
    "Greens" = "#6DA34D",
    "Conservatives" = "#1B9AAA",
    "New Democrats" = "#F26419",
    "Liberals" = "#C1292E"
  )

# Create horizontal gridlines in the data
df <- df |> mutate(grids = if_else(seats == 0, 0, 200))

ggplot(df, aes(x = seats, y = party, col = party)) +
  
  # Create horizontal grid lines and bars
  geom_line(aes(x = grids), col = "lightgray") +
  geom_line(lineend = "round", lwd = 5) +
  
  # Set scales
  scale_color_manual(values = party_colour) +
  scale_x_continuous(limits = c(0, 200)) +
  
  # Label the seat numbers
  geom_richtext(
    data = df1,
    aes(x = 190, y = party, label = seats),
    fill = NA,
    label.color = NA,
    col = "lightgray",
    nudge_y = 0.2
  ) +
  
  # Add line needed for majority
  geom_textvline(
    xintercept = 170, 
    label = "170 Seats for Majority", 
    linetype = 2, 
    color = "gray",
    gap = TRUE
  ) +
  
  # Set titles
  labs(
    title = "Title",
    x = "",
    y = ""
  ) +
  
  # Set themes
  theme_void() +
  theme(
    # Remove Legend
    legend.position = "none",
    
    # Set margin
    plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm"),
    
    # Set rich text label elements
    plot.caption = ggtext::element_markdown(hjust = 0, lineheight = 1.2),
    plot.title = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown()
  )
