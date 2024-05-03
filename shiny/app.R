# Dashboard for VizWizards

## Setup  ---------------------------

### Libraries  ---------------------------
if (!require("pacman")) 
  install.packages("pacman")

pacman::p_load(
  tidyverse,
  rsconnect,
  quarto,
  shiny,
  # shinythemes,
  # shinydashboard,
  bslib,
  DT
  # janitor
)

### Data  ---------------------------

meteorite_data_raw <- read_csv("https://raw.githubusercontent.com/INFO-526-S24/project-final-VizWizards/main/data/Meteorite_Landings.csv")

# General wrangling, may be updated later
# note to self -- may consider separate set to include all obs for table
meteorite_data <- meteorite_data_raw |> 
  filter(
    # Removing impossible observation
    year != 2101,
    # Reasonable lat & long
    between(reclat, -90, 90),
    between(reclong, -180, 180),
    # Removing lazy/unclear records
    reclat != 0 & reclong!= 0
  # ) |> 
  # mutate(
  #   log_mass = log10(`mass (g)`),
  #   log_mass = ifelse(log_mass==-Inf, NA, log_mass)
  )

# Finding year with the most falls
yr_most <- meteorite_data |> 
  group_by(year) |> 
  summarize(count = n()) |> 
  arrange(desc(count)) |> 
  slice(1) |> 
  pull(year)

## User Interface  ---------------------------
ui <- fluidPage(
  ### Theme settings  ---------------------------
  theme = bs_theme(version = 5, bootswatch = "vapor"),
  titlePanel(title = "Meteoric Fall: a comet-ment to data"),
  sidebarLayout(
    ### Sidebar  ---------------------------
    sidebarPanel(
      width = 3,
      #### Inputs  ---------------------------
      tags$hr(),
      tags$h3(icon("filter"), "Data selection inputs"),
      htmlOutput("filtered_count"),
      ##### Year  ---------------------------
      tags$hr(),
      checkboxInput(
        "input_years_full",
        label = "Include pre-1800",
        value = FALSE
      ),
      sliderInput(
        "input_years",
        label = NULL,
        value = c(yr_most, 1999),
        min = 1800,
        max = max(meteorite_data$year, na.rm = T),
        step = 1,
        sep = NULL,
        round = 0
      ),
      ##### Fell  ---------------------------
      tags$hr(),
      checkboxGroupInput(
        "input_observed",
        label = NULL,
        # "How it was observed",
        choices = list(
          "Fall was witnessed" = "Fell",
          "Found after arrival" = "Found"
        ),
        selected = c("Fell", "Found")
      ),
      # radioButtons(
      #   "input_observed",
      #   label = NULL,
      #   # "How it was observed",
      #   choices = list(
      #     "Fall was witnessed" = "Fell",
      #     "Found after arrival" = "Found",
      #     "Both" = c("Fell", "Found")
      #   ),
      #   selected = 3
      # ),
      ##### Type  ---------------------------
      tags$hr(),
      checkboxGroupInput(
        "input_type",
        label = "Dominant composition",
        choices = list(
          "Stony (rocky material)" = "Stony",
          "Iron (metallic)" = "Iron",
          "Stony-iron (mixtures)" = "Stony-iron",
          "Unspecified" = ""
        ),
        selected = c("Stony", "Iron", "Stony-iron", "")
      ),
      ##### Mass  ---------------------------
      tags$hr(),
      # sliderInput(
      #   "input_mass",
      #   label = "Mass (g)",
      #   value = c(0, 1000),
      #   min = 0,
      #   max = 1000,
      #   step = 1,
      #   # sep = NULL,
      #   round = 0
      # ),
      # checkboxInput(
      #   "input_mass_full",
      #   label = "Include meteorites >1 kg (2 lb 3.274 oz)",
      #   value = TRUE
      # ),
      # TBD... Not sure if should include.
      # A log scale version would filter better but isn't readable
      # Alt: t/f button for if >1kg?
      # sliderInput(
      #   "input_size",
      #   label = "Size (log scale)",
      #   value = c(min(meteorite_data$log_mass, na.rm = T),
      #             max(meteorite_data$log_mass, na.rm = T)),
      #   # value = c(yr_most, 1999),
      #   min = min(meteorite_data$log_mass, na.rm = T),
      #   max = max(meteorite_data$log_mass, na.rm = T)
      #   # step = 1,
      #   # sep = NULL,
      #   # round = 0
      # ),
    #   breaks = c(0, 10000, 100000, Inf), 
    #   labels = c("light", "average mass", "heavy")) ("light" = "yellow",
    #                                                  "average mass" = "orange",
    #                                                  "heavy" = "red")) +
      # checkboxGroupInput(
      #   "input_mass_checks",
      #   label = "Mass of metorite",
      #   choices = list(
      #     "Less than or equal to 1 g" = "x_light",
      #     "Between 1 g and 1 kg" = "light",
      #     "Between 1 kg and 1 metric ton" = "med",
      #     "At least 1 metric ton" = "large"
      #   )
      # ),
      #### Prompts  ---------------------------
      tags$hr(),
      tags$hr(),
      tags$h5(icon("lightbulb", class = "fa-solid"), "Here are some prompts to consider exploring!"),
      tags$div(
        tags$ul(
          tags$li("First prompt"),
          tags$li("Second prompt")
        ),
      ),
      #### General info  ---------------------------
      tags$hr(),
      tags$hr(),
      tags$h3(icon("meteor"), "Project information"),
      tags$div(
        icon("hand-point-right", class = "fa-solid"),
        "Please see",
        tags$b(tags$a(href = "https://info-526-s24.github.io/project-final-VizWizards/", "our project website")),
        "for information on the data, the goals of our project, and other information."
      ),
      #### Group info  ---------------------------
      tags$br(),
      tags$div(
        icon("wand-magic-sparkles"),
        "This dashbaord was created in April 2024 by",
        tags$a(href = "https://info-526-s24.github.io/project-final-VizWizards/", "Team Viz Wizards"),
        "as part of a final project for",
        tags$a(href = "https://datavizaz.org/", "INFO 526"),
        "- Data Analysis & Visualization at the University of Arizona.",
        tags$br(),
        tags$br(),
        icon("hat-wizard"),
        "The team was comprised of: Agastya Deshraju, Nick Ferrante, Jeremiah Gaiser, Tanya Evita George, Mrunal Jadhav, Jasdeep Singh Jhajj, & Gillian McGinnis (app lead).",
        tags$br(),
        tags$br(),
        icon("github"),
        " The source code is available on the",
        tags$a(href = "https://github.com/INFO-526-S24/project-final-VizWizards/blob/main/shiny/app.R", "project repository"),
        "."
      ),
      tags$hr()
    ),
    ### Main panel  ---------------------------
    mainPanel(
      #### Outputs
      plotOutput("plot_geo", height = "500px"),
      tags$em("Note: filter settings applied to the table below will not modify the map."),
      dataTableOutput("table_filtered")
    )
  )
)

## Server  ---------------------------

server <- function(input, output, session){
  ### Force
  observe({
    if(input$input_years_full == TRUE){
      updateSliderInput(
        inputId = "input_years",
        value = c(1800, input$input_years[2])
      )
    }
    
  })
  
  ### Filtering the data  ---------------------------
  meteorite_data_filtered <- reactive({
    # meteorite_data |> 
    #   filter(
    #     between(year, input$input_years[1], input$input_years[2]),
    #     fall %in% input$input_observed,
    #     between(`mass (g)`, input$input_mass[1], input$input_mass[2])
    #   )
    meteorite_data |>
      filter(
        between(year, input$input_years[1], input$input_years[2]),
        # between(`mass (g)`, input$input_mass[1], input$input_mass[2]),
        fall %in% input$input_observed
      )
      # filter(year == input$input_years) input_years_full, input_mass_full
  })
  
  #### Count  ---------------------------
  output$filtered_count <- renderText({
    paste(
      "&#9655;",
      "<em>Based on the provided inputs, you are viewing information for <b>",
      prettyNum(nrow(meteorite_data_filtered()), big.mark = ","),
      "meteorites</b> out of",
      prettyNum(nrow(meteorite_data), big.mark = ","),
      "total.</em>"
    )
  })
  
  #### Columns for the table  ---------------------------
  col_list <- reactive({
    # If both Fell & Found are selected, include the Fell var
    # Else; exclude it (redundant)
    if(length(input$input_observed) == 2){
      c("ID" = "id",
        "Name" = "name",
        "Class" = "recclass",
        "Mass (g)" = "mass (g)",
        "Latitude" = "reclat",
        "Longitude" = "reclong",
        "Year" = "year",
        "Observed" = "fall")
    } else {
      c("ID" = "id",
        "Name" = "name",
        "Class" = "recclass",
        "Mass (g)" = "mass (g)",
        "Latitude" = "reclat",
        "Longitude" = "reclong",
        "Year" = "year")
    }
  })
  
  ### Table creation  ---------------------------
  output$table_filtered <- renderDataTable({
    datatable(
      style = "bootstrap",
      filter = "top",
      # options = list(pageLength = 5),
      meteorite_data_filtered() |> 
        select(col_list())
    )
  })
  
  
  ### Plot creation  ---------------------------
  # To be updated with final map
  output$plot_geo <- renderPlot({
    ggplot(meteorite_data_filtered(), aes(x = reclong, y = reclat)) +
      geom_polygon(
        data = as_tibble(map_data("world")),
        aes(x = long, y = lat, group = group),
        inherit.aes=F,
        fill = "white"
      ) +
      coord_quickmap() +
      geom_point(size = 1.5) +
      theme_void() +
      theme(panel.background = element_rect(fill = "skyblue"))
  })
}

## Launch  ---------------------------
shinyApp(ui, server)
