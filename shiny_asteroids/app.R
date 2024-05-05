# Load packages ----------------------------------------------------------------
library(lubridate)
library(tidyverse)
library(shiny)
library( rgl )
library(magick)
library(gganimate)
library(ggforce)
library(tidyverse)
library(gganimate)
library(ggforce)
library(gridExtra)
library(bslib)
library(thematic)
source("meteor_classification.R")

earth_traj <- read.csv('daily_orbitals/earth.csv')
flora_traj <- read.csv('daily_orbitals/8flora.csv')
hebe_traj <- read.csv('daily_orbitals/6hebe.csv')
vesta_traj <- read.csv('daily_orbitals/4vesta.csv')
itokawa_traj <- read.csv('daily_orbitals/itokawa.csv')

asteroid_traj <- rbind(flora_traj, hebe_traj, vesta_traj, itokawa_traj, earth_traj) |>
  mutate(c = case_when(
    body == '8flora' ~ '#46d3e6',
    body == '6hebe'~ '#ffc107',
    body == '4vesta'~ '#e83ab8',
    body == 'itokawa'~ '#3bf082',
    body == 'earth' ~ '#1c9ff3'
  ),
  date = as.Date(paste(year,month,day,sep='-')),
  body = case_when(
    body == '8flora' ~ 'flora',
    body == '6hebe'~ 'hebe',
    body == '4vesta'~ 'vesta',
    body == 'itokawa'~ 'itokawa',
    body == 'earth' ~ 'earth'
  )) |>
  filter(date > as.Date("1980-01-01"))

# meteor_data <- read.csv('data/meteorite_landings.csv') 
dated_recents <- read.csv('data/dated_recents.csv') |> 
  drop_na(month) |> 
  mutate(parent_body = case_when(
    parent_body == '4vesta' ~ 'vesta',
    parent_body == '8flora' ~ 'flora',
    parent_body == '6hebe' ~ 'hebe',
    TRUE ~ parent_body
  ))

get_orbital_colnames <- function(prefix) {
  c(paste(rep(prefix, 3), c('x', 'y', 'z'), sep=''))
}

euc_dist <- function(a, b) {
  sqrt(sum((a - b) ^ 2))
}

orbital_data <- cbind(earth_traj,
                      flora_traj[c('x','y','z')],
                      hebe_traj[c('x','y','z')],
                      vesta_traj[c('x','y','z')],
                      itokawa_traj[c('x','y','z')])


colnames(orbital_data) <- c(
  colnames(earth_traj),
  get_orbital_colnames('flora_'),
  get_orbital_colnames('hebe_'),
  get_orbital_colnames('vesta_'),
  get_orbital_colnames('itokawa_')
)

recent_orbits <- orbital_data |>
  filter(year >= 1980) |>
  rowwise() |>
  mutate(flora_dist = euc_dist(c(flora_x, flora_y, flora_z),
                               c(x, y, z))) |>
  mutate(hebe_dist = euc_dist(c(hebe_x, hebe_y, hebe_z),
                              c(x, y, z))) |>
  mutate(vesta_dist = euc_dist(c(vesta_x, vesta_y, vesta_z),
                               c(x, y, z))) |>
  mutate(itokawa_dist = euc_dist(c(itokawa_x, itokawa_y, itokawa_z),
                                 c(x, y, z))) 


plot_orbit_distance <- function(min_date, max_date, asteroids_input) {
  orbit_range <- recent_orbits |> 
    filter(as.Date(paste(year, month, day, sep='-')) >= min_date & 
           as.Date(paste(year, month, day, sep='-')) <= max_date) |>
    mutate(ym = as.Date(paste(year, month, '01', sep ='-'))) |>
    group_by(ym) |>
    summarise(
      min_flora = mean(flora_dist),
      min_hebe = mean(hebe_dist),
      min_vesta = mean(vesta_dist),
      min_itokawa = mean(itokawa_dist)) |>
    ungroup() |>
    pivot_longer(
      cols = -ym,
      names_to = 'asteroid',
      values_to = 'min_dist',
      names_prefix = 'min_',
    ) |>
    filter(asteroid %in% asteroids_input)
  
  orbit_range |>
    mutate(asteroid = fct_relevel(asteroid, c('vesta', 'flora', 'hebe', 'itokawa'))) |>
    ggplot(aes(x=ym, y=min_dist, group=asteroid, color=asteroid)) +
    xlim(min_date, max_date) +
    geom_line(linewidth=1) +
    theme_minimal() +
    scale_y_continuous(labels = function(x){paste(round(x / 1e6, 2), 'K km', sep='') }) +
    theme(axis.text.y = element_text(
            color='white',
            size = 14
          ),
          axis.text.x = element_text(
            color = 'white',
            size = 14, 
          ),
          legend.position = 'none') +
    scale_color_manual(values = c("vesta" = '#e83ab8',
                                 "flora" = '#46d3e6',
                                 "hebe" = '#ffc107',
                                 "itokawa" = '#3bf082'))
}



plot_falls <- function(min_date, max_date, asteroids_input) {
  dated_recents |>
    mutate(ym = as.Date(paste(year, month, '01', sep ='-'))) |>
    filter(ym >= min_date & ym <= max_date) |>
    group_by(ym, group, parent_body) |>
    summarise(n=n()) |>
    mutate(parent_body = fct_relevel(parent_body, c('vesta', 'flora', 'hebe', 'itokawa'))) |> 
    filter(parent_body %in% asteroids_input) |>
    ggplot(aes(x=ym, y=n, xmin=min_date, xmax=max_date, fill=parent_body)) +
    xlim(min_date, max_date) +
    geom_col(width=30) +
    theme_minimal() +
    scale_y_continuous(labels = function(x){paste('            ', x, sep='') }) +
    theme(
      legend.position='top', 
      legend.justification='left',
      legend.text = element_text(
        color='white',
        size=12
      ),
      legend.title  = element_blank(),
      axis.text.y = element_text(
        color='white',
        size = 14
      ),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(
        color='white',
        size = 20
      )
    ) +
    labs(
      title = "Observed Meteor Fall Events and Distance From Probable Asteroid Parent"
    ) +
    scale_fill_manual(values = c("vesta" = "#e83ab8",
                                  "flora" = "#46d3e6",
                                  "hebe" = "#ffc107",
                                  "itokawa" = "#3bf082"))
}


# Load data --------------------------------------------------------------------

# Define UI -------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "vapor"),
  tags$head(includeCSS("index.css")),
  checkboxGroupInput(
    "input_asteroids",
    label = NULL,
    choices = list(
      "Vesta" = "vesta",
      "Flora" = "flora",
      "Hebe" = "hebe",
      "Itokawa" = "itokawa",
      'Show Asteroid Positions through Date Range' = 'show_pos'
    ),
    selected = c("vesta", "flora", 'hebe', 'itokawa'),
    inline = TRUE
  ),
  sliderInput("dateRange",
              "Dates:",
              min = as.Date("1980-01-01","%Y-%m-%d"),
              max = as.Date("2010-01-01","%Y-%m-%d"),
              value=c(as.Date("1980-01-01"), c("2010-01-01")),
              timeFormat="%b-%Y"),
  # actionButton("update", "Update Filters"),
  plotOutput("barPlot"),
  rglwidgetOutput("plot3D",  width = 800, height = 600)
)


# Define server function -------------------
server <- function(input, output, session) {
  plot1 <- reactive({ 
    plot_falls(input$dateRange[1], input$dateRange[2], input$input_asteroids)
  }) 
  
  plot2 <- reactive({ 
    plot_orbit_distance(input$dateRange[1], input$dateRange[2], input$input_asteroids)
  })
  
  m <- reactive({
    asteroid_traj |>
      filter(body %in% c('earth', input$input_asteroids)) |>
        mutate(current_traj = ifelse(
          date >= input$dateRange[1] & date <= input$dateRange[2]  , 1, 0)
          )
  }) 
  
  output$barPlot <- renderPlot({
    grid.arrange(plot1(), 
                 plot2(),
                 nrow=2)
  })
  
  output$plot3D <- renderRglwidget({
    try(close3d())
    open3d()
    
    bg3d(color = "black")
    
    pos_df <- m() |>
      filter(current_traj == 1)
    
    traj_df <- m() |>
      filter(current_traj == 0)
    
    points3d(traj_df$x, traj_df$y, traj_df$z, col=traj_df$c, alpha=0.5)
    
    if ('show_pos' %in% input$input_asteroids) {
      spheres3d(pos_df$x, pos_df$y, pos_df$z, radius=1.5e7, col=pos_df$c)
    }
    
    spheres3d(0, 0, 0, radius=3e7, col='yellow') 
    
    view3d(theta=15, phi=0, zoom=0.7)
    
    rglwidget()
  }) 
    
}

thematic_on("#1a0933", "gray")
# Create the Shiny app object -------------------
shinyApp(ui = ui, server = server)
