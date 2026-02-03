# install required libraries if not already done so
# install.packages("shiny")
# install.packages("dplyr")
# install.packages("bslib")
# install.packages("bsicons")
# install.packages("ggplot2")

# add required libraries here
library(shiny)
library(dplyr)
library(bslib)
library(bsicons)
library(ggplot2)

# Define UI for application
ui <- page_fluid(

  navset_bar( # creates menu bar

    title = "Exploring Wastewater",
    id = "main_nav",

    nav_panel(
      title = "Viruses", # title displayed in the ui
      value = "viruses", # id for referring to the panel
      icon = bs_icon("virus"), # an icon displayed in the ui
      # the code for what will be displayed
      card(
        card_header("SARS-CoV-2"),
        layout_sidebar(
          sidebar = sidebar(
            # check box input for showing individual data points or not
            checkboxInput(
              "show_points",
              label = "Show data points"
            ),
            uiOutput("wwtp_select_ui")
          ),
          # main card content here
          plotOutput("SARS_plot")
        )
      )
    ),

    selected = "viruses",
    bg = "#46ACC8",
    fillable = FALSE,
    position = "fixed-top" # defines menu bar position

  ),

  theme = bs_theme() %>%
    bs_add_rules("body { padding-top: 50px !important }"), # adds space between navbar and first card
  title = "Exploring Wastewater"

)

# Define server logic
server <- function(input, output) {

  PCRdata <- readr::read_csv("data/PCRdata.csv")

  # dropdown menu input for choosing wastewater treatment plants to show
  output$wwtp_select_ui <- renderUI({

    selectizeInput("wwtp_select",
                   label = "Wastewater treatment plant",
                   choices = unique(PCRdata$wastewater_treatment_plant.name),
                   selected = unique(PCRdata$wastewater_treatment_plant.name),
                   multiple = TRUE)
  })

  # plot for output
  output$SARS_plot <- renderPlot({

    PCRdata %>%
      filter(target == "SARS-N1",
             wastewater_treatment_plant.name %in% input$wwtp_select) %>%
      ggplot(aes(x = collection_date,
                 color = wastewater_treatment_plant.name)) +
      geom_line(aes(y = load_median)) +
      {if(input$show_points) geom_point(aes(y = load))} +
      labs(x = NULL,
           y = "Viral load [gc/person/day]",
           color = "Wastewater Treatment Plant") +
      theme_minimal()

  })

}

# Run the application
shinyApp(ui = ui, server = server)
