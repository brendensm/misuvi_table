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
library(leaflet)
library(readxl)


# Load in Data

#michigan_df <- readRDS("data/zscore_sub_counties_2.rds")



mi <- tigris::counties(state = "MI", cb = TRUE)


regions <- read_xlsx("data/regional.xlsx", sheet = 2, skip = 1) |> janitor::clean_names() |>
  select(fips, x5_region_grouping) |>
  rename(region = x5_region_grouping) |>
  mutate(fips = as.character(fips))


ui <- page_sidebar(

  tags$style(".modal-dialog {max-width: 60vw;}"),
    title = "Michigan Substance Use Vulnerability Index (MI-SUVI) Exploration Table",

    # Sidebar with two filters
    sidebar = sidebar(

      width=400,




          selectInput("type", "Select the type of data",
                      choices = c("z-scores", "percentiles", "rankings")),

          textInput("county",
                    "Filter by County:"),

          selectInput("region",
                      "Filter by Region:",
                      choices = c("All", "Northeast", "Upper Peninsula", "Southwest",
                                  "Northwest", "Southeast")
                      ),

          plotOutput("map")


      ),

    accordion(

      multiple = TRUE,
      open = "Main Table",

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
        accordion_panel("Main Table",
           DT::dataTableOutput("table_main")
        )



      )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {


  rv <- reactiveValues()



  type_filter <- reactive({

    switch(input$type,
           "z-scores" = "zscores",
           "rankings" = "ranks",
           "percentiles" = "percentiles")

  })


  # Data filtered from the UI inputs
  filtered_data <- reactive({



    michigan_df <- misuvi_load(type = type_filter()) |>
      left_join( regions, by = "fips")




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
        select(county, misuvi_score, burden_score,
               resource_score, svi_score) |> # select our variables of interest
        datatable(
          escape = FALSE, # shows our pictures
          rownames = FALSE, # removes row names
          colnames = c("County", "MI-SUVI Score", "Burden Score",
                       "Resource Score", "SVI Score"), # nicely formatted column names
          selection = "single",
          options = list(
            searching = FALSE, # hide the built-in search
            iDisplayLength = 12,

            lengthChange = FALSE, # no option to change table length
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'font-size': '24px'});", # this makes the column headers a larger text size
              "}"

            )
          )
        ) |>
        formatRound(
          columns = c("misuvi_score", "burden_score", "resource_score", "svi_score"), # rounding the z-scores
          digits = if_else(type_filter() == "zscores",   2,
                           if_else(type_filter() == "percentiles", 1,
                                   0))
        ) |>
        formatStyle(
          columns = c("misuvi_score", "burden_score", "resource_score", "svi_score"),
          backgroundColor = if_else(type_filter() == "zscores",   styleInterval(0, c('lightgreen', 'lightpink')),
                                   if_else(type_filter() == "percentiles", styleInterval(50, c('lightgreen', 'lightpink')),
                                          styleInterval(42, c('lightpink', 'lightgreen')))), # this makes the cells have conditional formatting
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
          title = paste(selected_row()$county, "County"),
          fluidPage(
            fluidRow(
              column(3, value_box("MI-SUVI Score",
                                  theme = value_box_theme(
                                    bg = "white"),
                                  value = round(filtered_data()[filtered_data()$county == selected_row()$county,]$misuvi_score, if_else(type_filter() == "zscores",   2,
                                                                                                                                        if_else(type_filter() == "percentiles", 1,
                                                                                                                                                0)
                                                                                                                                        )
                                                )
                                  )
                     ),

              column(3, value_box("Burden Score",
                                  value = round(filtered_data()[filtered_data()$county == selected_row()$county,]$burden_score, if_else(type_filter() == "zscores",   2,
                                                                                                                                        if_else(type_filter() == "percentiles", 1,
                                                                                                                                                0))))),
              column(3, value_box("Resource Score",
                                  value = round(filtered_data()[filtered_data()$county == selected_row()$county,]$resource_score, if_else(type_filter() == "zscores",   2,
                                                                                                                                        if_else(type_filter() == "percentiles", 1,
                                                                                                                                                0))))),
              column(3, value_box("SVI Score",
                                  value = round(filtered_data()[filtered_data()$county == selected_row()$county,]$svi_score, if_else(type_filter() == "zscores",   2,
                                                                                                                                        if_else(type_filter() == "percentiles", 1,
                                                                                                                                                0))))),



            ),
              DTOutput("details_table")),
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


      dict <- misuvi::dictionary() |> mutate(original_names = gsub("_", original_names, replacement = " "),
                                             original_names = gsub("100 000", original_names, replacement = "100,000"),
                                             original_names = gsub("1 000", original_names, replacement = "1,000"),
                                             original_names = gsub("2020 2022", original_names, replacement = "2020-2022"),
                                             original_names = gsub("2018 2022", original_names, replacement = "2018-2022")) |>
        filter(cleaned_names %in% names(filtered_data()))






     sub <-  filtered_data()[filtered_data()$county == selected_row()$county,] |>
        select(-c(fips, county)) |>
        t() |>
        as.data.frame() |>
        tibble::rownames_to_column() |>
        filter(!(rowname %in% c("misuvi_score", "svi_score", "burden_score", "resource_score"))) |>
        left_join(dict, by = c("rowname" = "cleaned_names")) |>
        select(original_names, V1)

        sub[1:8,] |>
        datatable(
          rownames = TRUE, # removes row names
          colnames = c("Variables", type_filter()), # nicely formatted column names
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
          columns = c("V1"), # rounding the z-scores
          digits = if_else(type_filter() == "zscores",   2,
                           if_else(type_filter() == "percentiles", 1,
                                   0))
        ) |>

        formatStyle(
          columns = 2,
          backgroundColor = if_else(type_filter() == "zscores",   styleInterval(0, c('lightgreen', 'lightpink')),
                                    if_else(type_filter() == "percentiles", styleInterval(50, c('lightgreen', 'lightpink')),
                                            styleInterval(42, c('lightpink', 'lightgreen')))), # this makes the cells have conditional formatting
          # anything less than 0 is green (better than average), greater than 0 is light pink (worse than average).
        ) |>
        formatStyle(
          columns = 1:2,
          selector = list("th"),
          fontSize = '24px', # increase cell text size

        )
    })


output$map <- renderPlot({


  mi_highlight <- mi |>
    mutate(NAME = case_when(NAME == "St. Joseph" ~ "Saint Joseph",
                            NAME == "St. Clair" ~ "Saint Clair",
                            TRUE ~ NAME)) |>
    filter(NAME %in% filtered_data()$county)


  mi |>
    ggplot() +
    geom_sf() +
    geom_sf(data = mi_highlight, fill = "lightblue", color = "black") +
    theme_void()



})









}

# Run the application
shinyApp(ui = ui, server = server)


