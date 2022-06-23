# Load Packages ----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggtext)
library(geomtextpath)
library(glue)

# Create Plot ------------------------------------------------------------------
df <- 
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
# Map each party to their color
party_colour = 
  c(
    "Bloc Quebecois" = "#003F91",
    "Greens" = "#6DA34D",
    "Conservatives" = "#1B9AAA",
    "New Democrats" = "#F26419",
    "Liberals" = "#C1292E"
  )

# Add rich-text name labels for each party and add each party's colour
df <- 
  df |> 
  left_join(y = enframe(party_colour, name = "party", value = "color"), by = "party") |>
  mutate(name = glue("<i style='font-size:12pt;color:{color}'> \u25cf </i> {party}"))

# Create horizontal gridlines in the data
x_lim <- 250
df <- df |> mutate(grids = if_else(seats == 0, 0, x_lim))

ggplot(df, aes(x = seats, y = party, col = party)) +
  
  # Create horizontal grid lines and bars
  geom_line(aes(x = grids), col = "lightgray", alpha = 0.3) +
  geom_line(lineend = "round", lwd = 5) +
  
  # Set scales
  scale_color_manual(values = party_colour) +
  scale_x_continuous(limits = c(0, x_lim)) +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0.3))) +  # Add some top space for labels
  
  # Add Seats and Party Header Labels
  geom_text(
    data = tibble(x = c(2, x_lim - 10), y = 5.85, label = c("Party", "Seats")),
    aes(x = x, y = y, label = label),
    colour = "gray"
  ) +
  
  # Add the party labels above each bar
  geom_richtext(
    aes(x = 0, y = party, label = name),
    fill = NA,
    col = "black",
    label.color = NA,
    nudge_y = 0.35,
    hjust = 0
  ) +
  
  # Label the seat numbers
  geom_richtext(
    data = df |> filter(row_number() %% 2 == 0),
    aes(x = x_lim - 10, y = party, label = seats),
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
    title = "**Election Districts Tally**",
    subtitle = "Seats in which each party lead the 2021 Federal Election vote count.",
    caption = "With 338 of 338 ridings reporting results.<br>This is a graph replication - not my original design.",
    x = "",
    y = ""
  ) +
  
  # Set themes
  theme_void() +
  theme(
    # Remove Legend
    legend.position = "none",
    
    # Set margin
    plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
    
    # Set rich text label elements
    plot.caption = ggtext::element_markdown(hjust = 0, lineheight = 1.2),
    plot.title = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown()
  )
