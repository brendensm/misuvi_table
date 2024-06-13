# Load libraries

library(shiny)
library(DT)
library(dplyr)
library(misuvi)
library(htmltools)
library(ggplot2)
library(png)
library(base64enc)
library(bslib)

# Load in Data

michigan_df <- readRDS("data/zscore_sub_counties_2.rds")

ui <- page_sidebar(

    title = "Michigan Substance Use Vulnerability Index (MI-SUVI) Exploration Table",

    # Sidebar with two filters
    sidebar = sidebar(

          textInput("county",
                    "Filter by County:"),

          selectInput("region",
                      "Filter by Region:",
                      choices = c("All", "Northeast", "Upper Peninsula", "Southwest",
                                  "Northwest", "Southeast")
                      )


      ),

    accordion(

      multiple = TRUE,
      open = "MI-SUVI Z-Scores",

        accordion_panel("About",
                        "This table shows data from the Michigan Substance Use
                          Vulnerability Index. The table below shows the z-scores
                          for MISUVI composite score which is made up of the burden,
                          resource, and social vulnerability z-scores. Z-scores greater than zero
                          indicate counties are more vulnerable, while z-scores less than zero are
                          less vulnerable.
                          All data can be downloaded from the misuvi package
                          or from the State of Michigan's Website."

        ),


        # Show our main table
        accordion_panel("MI-SUVI Z-Scores",
           DT::dataTableOutput("table_main")
        )



      )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Data filtered from the UI inputs
  filtered_data <- reactive({
    if (input$county == "" & input$region == "All") {
      michigan_df
    } else if (input$region == "All"){
      michigan_df[grepl(input$county, michigan_df$county, ignore.case = TRUE), ]
    } else if (input$county == ""){
      michigan_df[grepl(input$region, michigan_df$region, ignore.case = TRUE), ]
    }else{
      michigan_df[grepl(input$county, michigan_df$county, ignore.case = TRUE), ]

      subset(michigan_df, grepl(input$county, county, ignore.case = TRUE) |
               grepl(input$region, region, ignore.case = TRUE))
    }
  })





    output$table_main <- DT::renderDataTable({
        # main output

      filtered_data() |>
        select(small_map, county, misuvi_score, burden_score,
               resource_score, svi_score) |> # select our variables of interest
        datatable(
          escape = FALSE, # shows our pictures
          rownames = FALSE, # removes row names
          colnames = c("", "County", "MI-SUVI Score", "Burden Score",
                       "Resource Score", "SVI Score"), # nicely formatted column names
          options = list(
            searching = FALSE, # hide the built-in search
            selection = "single",
            lengthChange = FALSE, # no option to change table length
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'font-size': '24px'});", # this makes the column headers a larger text size
              "}"

            )
          )
        ) |>
        formatRound(
          columns = c("misuvi_score", "burden_score", "resource_score", "svi_score") # rounding the z-scores
        ) |>
        formatStyle(
          columns = c("misuvi_score", "burden_score", "resource_score", "svi_score"),
          backgroundColor = styleInterval(0, c('lightgreen', 'lightpink')), # this makes the cells have conditional formatting
# anything less than 0 is green (better than average), greater than 0 is light pink (worse than average).
        ) |>
        formatStyle(
          columns = 1:6,
          selector = list("th"),
          fontSize = '24px', # increase cell text size

        )

    })

    observeEvent(input$table_main_rows_selected, {
      showModal(
        modalDialog(
          title = "Details",
                DTOutput("details_table"),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")

          )
        )
      )
    })

    selected_row <- reactive({
      req(input$table_main_rows_selected)
      row_index <- input$table_main_rows_selected
      filtered_data()[row_index, , drop = FALSE]
    })

    output$details_table <- renderDT({

      metrics <- misuvi_load()

      metrics[metrics$county == selected_row()$county,] |>
        select(-c(fips, county)) |>
        t() |>
        as.data.frame() |>
        tibble::rownames_to_column() |>
        datatable(
          rownames = TRUE, # removes row names
          colnames = c("Variables", selected_row()$county), # nicely formatted column names
          options = list(
            searching = FALSE, # hide the built-in search
            selection = "single",
            lengthChange = FALSE, # no option to change table length
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'font-size': '24px'});", # this makes the column headers a larger text size
              "}"

            )
          )
        ) |>
        formatRound(
          columns = 2, #  rounding the z-scores
          digits = 2
        ) |>
        formatStyle(
          columns = 1:2,
          selector = list("th"),
          fontSize = '24px', # increase cell text size

        )
    })



}

# Run the application
shinyApp(ui = ui, server = server)

#
# t <- metrics[metrics$county == "Monroe",] |>
#   select(-c(fips, county)) |>
#   t() |> as.data.frame() |> tibble::rownames_to_column()
