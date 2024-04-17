# Dashboard test again

### ----------
### LIBRARIES
pacman::p_load(tidyverse,
               rsconnect,
               quarto,
               shiny)

### ----------
### SETUP

meteorite_data_raw <- read_csv("https://raw.githubusercontent.com/INFO-526-S24/project-final-VizWizards/main/data/Meteorite_Landings.csv")

meteorite_data <- meteorite_data_raw |> 
  filter(
    year != 2101,
    between(reclat, -90, 90),
    between(reclong, -180, 180),
    reclat != 0 & reclong!= 0
  )

yr_most <- meteorite_data |> 
  group_by(year) |> 
  summarize(count = n()) |> 
  arrange(desc(count)) |> 
  slice(1) |> 
  pull(year)

### ----------
### USER INTERFACE
ui <- fluidPage(
  sliderInput("year_of_interest", "Year",
              value = 1999,
              min = 1900,
              # min = min(meteorite_data$year, na.rm = T),
              max = max(meteorite_data$year, na.rm = T),
              step = 1, round = 0),
  plotOutput("geo_plot")
)


### ----------
### SERVER

server <- function(input, output, session) {
  meteorite_data_filtered <- reactive({
    meteorite_data |> 
      filter(year == input$year_of_interest)
  })
  
  # Creating the plot
  output$geo_plot <- renderPlot({
    ggplot(meteorite_data_filtered(), aes(x = reclong, y = reclat)) +
      geom_polygon(
        data = as_tibble(map_data("world")),
        aes(x = long, y = lat, group = group),
        inherit.aes=F,
        fill = "white"
        # fill = "cornsilk"
      ) +
      coord_quickmap() +
      geom_point(
        # alpha = 0.075,
        size = 0.1
      ) +
      theme_void() +
      theme(panel.background = element_rect(fill = "skyblue"))
  })
}

### ----------
### LAUNCH
shinyApp(ui, server)
