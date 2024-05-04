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
  bslib,
  DT,
  viridis,
  mapproj
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
  ) |>
  mutate(
    mass_cat = case_when(
      `mass (g)` < 1 ~ "x_small",
      between(`mass (g)`, 1, 999.999) ~ "small",
      between(`mass (g)`, 1000, 999999.999) ~ "medium",
      `mass (g)` > 1000000 ~ "large"
    ),
    mass_cat = factor(mass_cat,
                      levels = c("x_small",
                                 "small",
                                 "medium",
                                 "large")),
    mass_lab = recode_factor(mass_cat,
                             "x_small" = "< 1 g",
                             "small" = "≥ 1 g & < 1 kg",
                             "medium" = "≥ 1 kg & < 1 t",
                             "large" = "≥ 1 t")
  )

# Finding year with the most falls
yr_most <- meteorite_data |> 
  group_by(year) |> 
  summarize(count = n()) |> 
  arrange(desc(count)) |> 
  slice(1) |> 
  pull(year)

# Cleaner world map
world_data <- map_data("world") |> 
  as_tibble() |> 
  filter(
    between(lat, -90, 90),
    between(long, -180, 180)
  )

## User Interface  ---------------------------
ui <- fluidPage(
  ### Theme settings  ---------------------------
  theme = bs_theme(version = 5, bootswatch = "vapor"),
  titlePanel(title = "Meteoric Fall: a comet-ment to data"),
  sidebarLayout(
    ### Sidebar  ---------------------------
    sidebarPanel(
      width = 3,
      # tags$em(tags$a(href = "https://info-526-s24.github.io/project-final-VizWizards/", "What's this?")),
      #### Inputs  ---------------------------
      tags$h5(icon("filter"), "Data selection inputs"),
      ##### Fell  ---------------------------
      checkboxGroupInput(
        "input_observed",
        label = "Observation",
        # label = NULL,
        choices = list(
          "Fall was witnessed" = "Fell",
          "Found after arrival" = "Found"
        ),
        selected = c("Fell", "Found")
      ),
      ##### Year  ---------------------------
      "Time range (in years CE)",
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
      ##### Mass  ---------------------------
      checkboxGroupInput(
        "input_mass_checks",
        label = "Mass of meteorites",
        choices = list(
          "Less than 1 gram" = "x_small",
          "Between 1 gram and 1 kilogram" = "small",
          "Between 1 kilogram and 1 metric ton" = "medium",
          "At least 1 metric ton" = "large"
        ),
        selected = c("x_small", "small", "medium", "large")
      ),
      ##### Type  ---------------------------
      ## Only add if this is finalized.
      # "Dominant composition",
      # tags$a(href = "https://en.wikipedia.org/wiki/Meteorite_classification#Traditional_classification_scheme", "(what's this?)"),
      # checkboxGroupInput(
      #   "input_type",
      #   label = NULL,
      #   # label = "Dominant composition",
      #   choices = list(
      #     "Stony (rocky material)" = "Stony",
      #     "Iron (metallic)" = "Iron",
      #     "Stony-iron (mixtures)" = "Stony-iron",
      #     "Unspecified" = ""
      #   ),
      #   selected = c("Stony", "Iron", "Stony-iron", "")
      # ),
      #### Plot customization  ---------------------------
      tags$hr(),
      tags$h5(icon("globe"), "Plot customization options"),
      ##### Map projection  ---------------------------
      selectInput(
        "input_projection",
        label = "Map projection",
        choices = list(
          "Guyou (hemisphere-in-a-square)" = "guyou",
          "Mercator (conformal cylindrical)" = "mercator",
          "Mollweide (pseudocylindrical)" = "mollweide",
          "Aitoff (azimuthal)" = "aitoff",
          "Gilbert (two-world)" = "gilbert",
          "Lagrange (polyconic)" = "lagrange"
          # "Sinusoidal (pseudocylindrical, equal-area)" = "sinusoidal"
        ),
        selected = "mercator"
      ),
      ##### Point size  ---------------------------
      sliderInput(
        "input_pointsize",
        label = "Size of plot points",
        value = 1.5,
        min = 0.1,
        max = 5,
        step = 0.1
      ),
      ##### Point alpha  ---------------------------
      sliderInput(
        "input_alpha",
        label = "Transparency of plot points",
        value = 75,
        min = 5,
        max = 100,
        step = 5,
        post = "%"
      ),
      #### Prompts  ---------------------------
      tags$hr(),
      tags$h5(icon("lightbulb", class = "fa-solid"), "Prompts to consider exploring"),
      tags$div(
        tags$ul(
          tags$li("What areas of the planet have the highest concentration of meteorites?"),
          tags$li("When was there a significant increase in the number of reported metorites?"),
          tags$li("Are massive meteorites more likely to be witnessed falling, or found later?")
        ),
      ),
      #### General info  ---------------------------
      tags$hr(),
      tags$h5(icon("meteor"), "Project information"),
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
        "This dashbaord was created in April & May 2024 for",
        tags$a(href = "https://info-526-s24.github.io/project-final-VizWizards/", "Team Viz Wizards"),
        "as part of a final project for",
        tags$a(href = "https://datavizaz.org/", "INFO 526"),
        "- Data Analysis & Visualization at the University of Arizona.",
        tags$br(),
        tags$br(),
        icon("hat-wizard"),
        "The team was comprised of: Agastya Deshraju, Nick Ferrante, Jeremiah Gaiser, Tanya Evita George, Mrunal Jadhav, Jasdeep Singh Jhajj, & Gillian McGinnis (app author).",
        tags$br(),
        tags$br(),
        icon("github"),
        " The source code is available on the",
        tags$a(href = "https://github.com/INFO-526-S24/project-final-VizWizards/blob/main/shiny/app.R", "project repository.")
      ),
      tags$hr()
    ),
    ### Main panel  ---------------------------
    mainPanel(
      htmlOutput("filtered_count"),
      #### Outputs
      plotOutput("plot_geo", height = "500px"),
      tags$em("Note: filter settings applied to the table below will not modify the map."),
      dataTableOutput("table_filtered")
    )
  )
)

## Server  ---------------------------

server <- function(input, output, session){
  ### Force year slider to 1800 if enabled  ---------------------------
  observe({
    if(input$input_years_full == TRUE & input$input_years[1] != 1800){
      updateSliderInput(
        inputId = "input_years",
        value = c(1800, input$input_years[2])
      )
    }
  })
  
  ### Filtering the data  ---------------------------
  meteorite_data_filtered <- reactive({
    #### If selecting pre-1800, only filter by max date
    if(input$input_years_full == TRUE){
      meteorite_data |> 
        filter(
          year <= input$input_years[2],
          fall %in% input$input_observed,
          mass_cat %in% input$input_mass_checks
        ) |> 
        mutate(mass_lab = fct_drop(mass_lab))
    } else {
      meteorite_data |>
        filter(
          between(year, input$input_years[1], input$input_years[2]),
          fall %in% input$input_observed,
          mass_cat %in% input$input_mass_checks
        ) |> 
        mutate(mass_lab = fct_drop(mass_lab))
    }
  }) |> debounce(500)
  
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
        "Year" = "year",
        "Name" = "name",
        "Class" = "recclass",
        "Mass (g)" = "mass (g)",
        "Latitude" = "reclat",
        "Longitude" = "reclong",
        "Observed" = "fall")
    } else {
      c("ID" = "id",
        "Year" = "year",
        "Name" = "name",
        "Class" = "recclass",
        "Mass (g)" = "mass (g)",
        "Latitude" = "reclat",
        "Longitude" = "reclong")
    }
  })
  
  ### Table creation  ---------------------------
  output$table_filtered <- renderDataTable({
    datatable(
      style = "bootstrap",
      filter = "top",
      meteorite_data_filtered() |> 
        select(col_list())
    )
  })
  
  ### Plot creation  ---------------------------
  output$plot_geo <- renderPlot({
    ggplot(meteorite_data_filtered(), aes(x = reclong, y = reclat)) +
      geom_polygon(
        data = world_data,
        aes(x = long, y = lat, group = group),
        inherit.aes = FALSE,
        fill = "white"
      ) +
      coord_map(projection = input$input_projection) +
      geom_point(
        aes(color = mass_cat),
        size = input$input_pointsize,
        alpha = input$input_alpha/100
      ) +
      scale_color_manual(
        name = "Meteorite mass",
        values = c("x_small" = "purple",
                   "small" = "forestgreen",
                   "medium" = "royalblue2",
                   "large" = "red"),
        labels = levels(meteorite_data_filtered()$mass_lab)
      ) +
      guides(color = guide_legend(override.aes = list(alpha = 1))) +
      theme_void() +
      theme(
        panel.background = element_rect(fill = "skyblue"),
        # plot.background = element_rect(fill = "#1A0833"),
        legend.key = element_blank()
      )
  })
}

## Launch  ---------------------------
shinyApp(ui, server)
