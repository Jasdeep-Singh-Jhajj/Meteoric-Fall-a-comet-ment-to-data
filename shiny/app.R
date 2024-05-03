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
  DT,
  viridis,
  mapproj
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
                                 "large"))
    # ),
    # mass_size = case_when(
    #   mass_cat == "x_small" ~ 1,
    #   mass_cat == "small" ~ 1.5,
    #   mass_cat == "medium" ~ 2,
    #   mass_cat == "large" ~ 2.5
    # )
    # log_mass = log10(`mass (g)`),
    # log_mass = ifelse(log_mass==-Inf, NA, log_mass)
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
      #### Inputs  ---------------------------
      tags$hr(),
      tags$h3(icon("filter"), "Data selection inputs"),
      htmlOutput("filtered_count"),
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
      ##### Year  ---------------------------
      tags$hr(),
      "Time range (years CE)",
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
      checkboxGroupInput(
        "input_mass_checks",
        label = "Mass of meteorite",
        choices = list(
          "Less than 1 gram" = "x_small",
          "Between 1 gram and 1 kilogram" = "small",
          "Between 1 kilogram and 1 metric ton" = "medium",
          "At least 1 metric ton" = "large"
        ),
        selected = c("x_small", "small", "medium", "large")
      ),
      ##### Type  ---------------------------
      tags$hr(),
      "Dominant composition",
      tags$a(href = "https://en.wikipedia.org/wiki/Meteorite_classification#Traditional_classification_scheme", "(what's this?)"),
      checkboxGroupInput(
        "input_type",
        label = NULL,
        # label = "Dominant composition",
        choices = list(
          "Stony (rocky material)" = "Stony",
          "Iron (metallic)" = "Iron",
          "Stony-iron (mixtures)" = "Stony-iron",
          "Unspecified" = ""
        ),
        selected = c("Stony", "Iron", "Stony-iron", "")
      ),
      #### Plot customization  ---------------------------
      tags$hr(),
      sliderInput(
        "input_pointsize",
        label = "Size of plot points",
        value = 1.5,
        min = 0.1,
        max = 5,
        step = 0.1
      ),
      sliderInput(
        "input_alpha",
        label = "Transparency of plot points",
        value = 75,
        min = 5,
        max = 100,
        step = 5,
        post = "%"
      ),
      selectInput(
        "input_projection",
        label = "Map projection",
        choices = list("Mercator" = "mercator",
                       "Guyou (hemisphere-in-a-square)" = "guyou",
                       "Gilbert (two-world)" = "gilbert",
                       "Mollweide (pseudocylindrical)" = "mollweide",
                       "Lee (conformal world in a tetrahedron)" = "tetra"),
        selected = "mercator"
      ),
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
        "This dashbaord was created in April-May 2024 by",
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
  ### Force year slider to 1800  ---------------------------
  # observe({
  #   if(input$input_years_full == TRUE){
  #     updateSliderInput(
  #       inputId = "input_years",
  #       value = c(1800, input$input_years[2])
  #     )
  #   }
  # })
  observe({
    if(input$input_years_full == TRUE & input$input_years[1] != 1800){
      updateSliderInput(
        inputId = "input_years",
        value = c(1800, input$input_years[2])
      )
    # } else if(input$input_years_full == TRUE & input$input_years[1] > 1800){
    #     updateCheckboxInput(
    #       inputId = "input_years_full",
    #       value = FALSE
    #     )
    }
  })
  
  # observe({
  #   if(input$input_years_full == TRUE & input$input_years[1] != 1800){
  #     updateCheckboxInput(
  #       inputId = "input_years_full",
  #       value = FALSE
  #     )
  #   }
  # })
  
  ### Filtering the data  ---------------------------
  meteorite_data_filtered <- reactive({
    # meteorite_data |> 
    #   filter(
    #     between(year, input$input_years[1], input$input_years[2]),
    #     fall %in% input$input_observed,
    #     between(`mass (g)`, input$input_mass[1], input$input_mass[2])
    #   )
    if(input$input_years_full == TRUE){
      meteorite_data |> 
        filter(
          year <= input$input_years[2],
          fall %in% input$input_observed,
          mass_cat %in% input$input_mass_checks
        )
    } else {
      meteorite_data |>
        filter(
          between(year, input$input_years[1], input$input_years[2]),
          fall %in% input$input_observed,
          mass_cat %in% input$input_mass_checks
        )
    }
    # meteorite_data |>
    #   filter(
    #     between(year, input$input_years[1], input$input_years[2]),
    #     # between(`mass (g)`, input$input_mass[1], input$input_mass[2]),
    #     fall %in% input$input_observed
    #   )
    #   # filter(year == input$input_years) input_years_full, input_mass_full
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
        data = world_data,
        aes(x = long, y = lat, group = group),
        inherit.aes=F,
        fill = "white"
      ) +
      # coord_quickmap() +
      coord_map(projection = input$input_projection) +
      geom_point(
        aes(color = mass_cat),
            # size = mass_size),
        size = input$input_pointsize,
        alpha = input$input_alpha/100
        # size = 1,
        # alpha = 0.85
      ) +
    #   mass_cat == "x_small" ~ "purple",
    # mass_cat == "small" ~ "forestgreen",
    # mass_cat == "medium" ~ "royalblue2",
    # mass_cat == "large" ~ "red"
      scale_color_manual(
        name = "Mass",
        values = c("x_small" = "purple",
                   "small" = "forestgreen",
                   "medium" = "royalblue2",
                   "large" = "red"),
        labels = c("< 1 g",
                   "≥ 1 g & < 1 kg",
                   "≥ 1 kg & < 1 t",
                   "≥ 1 t"
                   )
      ) +
      # scale_color_identity(
      #   guide = "legend"
      #   # labels = levels(meteorite_data_filtered()$mass_cat),
      #   # breaks = levels(meteorite_data_filtered()$mass_cat)
      #   # breaks = meteorite_data_filtered()$mass_cat
      # ) +
      # guide_legend(
      #   position = "bottom"
      # ) +
      # scale_color_viridis(
      #   option = "rocket",
      #   discrete = TRUE,
      #   direction = -1,
      #   # begin = 0.15,
      #   end = 0.85
      # ) +
      # scale_color_brewer(palette = "Set1") +
      # scale_color_identity() +
      # scale_color_manual(
      #   breaks = c("x_small", "small", "medium", "large"),
      #   labels = c("Less than 1 g", "Between 1 g & 1 kg", "Between 1 kg & 1 t", "At least 1 t"),
      #   values = c("gold", "darkgoldenrod", "sienna2", "firebrick")
      #   #   breaks = c(0, 10000, 100000, Inf), 
      #   #   labels = c("light", "average mass", "heavy")) ("light" = "yellow",
      #   #                                                  "average mass" = "orange",
      #   #                                                  "heavy" = "red")) +
      # )
      # geom_point(size = 1.5) +
      theme_void() +
      theme(panel.background = element_rect(fill = "skyblue"))
  })
}

## Launch  ---------------------------
shinyApp(ui, server)
