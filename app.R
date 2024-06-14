# Load libraries

library(shiny)
library(DT)
library(dplyr)
library(misuvi)
library(htmltools)
library(ggplot2)
#library(png)
#library(base64enc)
library(bslib)
library(leaflet)
library(readxl)


# Load in Data

mi <- tigris::counties(state = "MI", cb = TRUE) |> # This loads in our shapefile for the clickable map
  mutate(NAME = case_when(NAME == "St. Clair" ~ "Saint Clair", # We have to correct some different spellings for two counties
                          NAME == "St. Joseph" ~ "Saint Joseph",
                          TRUE ~ NAME))


regions <- read_xlsx("data/regional.xlsx", sheet = 2, skip = 1) |> # This code chunk imports some regional groupings we will use for filtering
  janitor::clean_names() |>
  select(fips, x5_region_grouping) |>
  rename(region = x5_region_grouping) |>
  mutate(fips = as.character(fips))


ui <- page_sidebar( # initiate ui

  tags$style(".modal-dialog {max-width: 60vw;} .leaflet-container {background: none !important;}"), # custom css makes our pop-up window a bit bigger, and the leaflet background transparent
    title = "Michigan Substance Use Vulnerability Index (MI-SUVI) Exploration Table", # title for the app

    # Sidebar with filters and map
    sidebar = sidebar(

      width=500,

          selectInput("type", "Select the type of data", # select the type of data set
                      choices = c("z-scores", "percentiles", "rankings")),

          textInput("county", # text filter for county names
                    "Filter by County:"),

          selectInput("region", # our drop-down menu
                      "Filter by Region:",
                      choices = c("All", "Northeast", "Upper Peninsula", "Southwest",
                                  "Northwest", "Southeast")
                      ),

          leafletOutput("inputMap", height = 450)
      ),

    accordion(

      multiple = TRUE, # allows multiple tabs to be open
      open = c("Main Table", "About"), # specifies which should be left open

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

# Define server logic
server <- function(input, output, session) {

  rv <- reactiveValues()

  type_filter <- reactive({

    switch(input$type,
           "z-scores" = "zscores",
           "rankings" = "ranks",
           "percentiles" = "percentiles")

  })


  # Data filtered from the UI inputs
  filtered_data <- reactiveVal(NULL)

  michigan_df <- reactiveVal(NULL)



    observeEvent(input$type,{


      michigan_df(misuvi_load(type = type_filter()) |>
                    add_geometry() |>
                    left_join(regions, by = "fips"))

      filtered_data(michigan_df())

    })


    observeEvent(input$inputMap_shape_click, {

      click <- input$inputMap_shape_click

      if(click$id != "selected" | !is.null(click$id)){

        mapfilter <- michigan_df()[grepl(click$id, michigan_df()$county, ignore.case = TRUE), ]

        filtered_data(mapfilter)

      }

      if (click$id == "selected"){
        michigan_df() |> filtered_data()
      }

    })





    observeEvent(list(input$region, input$county), {

      # Define a reactive variable to store the filtered data
      filtered_data_result <- reactiveVal(NULL)

      # Perform filtering based on county and region
      if (input$county == "" & input$region == "All") {
        filtered_data(michigan_df())
      } else {
        filtered_data_temp <- michigan_df()

        if (input$county != "") {
          filtered_data_temp <- filtered_data_temp %>%
            filter(grepl(input$county, county, ignore.case = TRUE))
        }

        if (input$region != "All") {
          filtered_data_temp <- filtered_data_temp %>%
            filter(region == input$region)
        }

        # Check if any rows match the filter criteria
        if (nrow(filtered_data_temp) > 0) {
          filtered_data(filtered_data_temp)
        } else {
          filtered_data(NULL)
        }
      }

      # Update the map
      if(input$county == "" & input$region == "All"){
        leafletProxy("inputMap", session) %>%
          clearShapes() %>%
          addPolygons(data = michigan_df(),
                      layerId = ~county,
                      label = ~county,
                      fillColor = "#BF40BF",
                      col = "black",
                      weight = 2,
                      fillOpacity = 0.1)
      }else if (!is.null(filtered_data())) {
        leafletProxy("inputMap", session) %>%
          clearShapes() %>%
          addPolygons(data = michigan_df(),
                      layerId = ~county,
                      label = ~county,
                      fillColor = "#BF40BF",
                      col = "black",
                      weight = 2,
                      fillOpacity = 0.1) %>%
          addPolygons(data = filtered_data(),
                      layerId = ~county,
                      label = ~county,
                      fillColor = "#BF40BF",
                      col = "black",
                      weight = 2,
                      fillOpacity = 1)
      } else {
        leafletProxy("inputMap", session) %>%
          clearShapes() %>%
          addPolygons(data = michigan_df(),
                      layerId = ~county,
                      label = ~county,
                      fillColor = "#BF40BF",
                      col = "black",
                      weight = 2,
                      fillOpacity = 0.1)
      }
    })




    output$table_main <- DT::renderDataTable({
        # main output

      if(is.null(filtered_data())){

        print(dplyr::tibble("Error" = "Please enter a valid County name."))

      }else{

      filtered_data() |>
        select(county, misuvi_score, burden_score,  # select our variables of interest
               resource_score, svi_score) |>
        sf::st_set_geometry(NULL) |>
        datatable(
          escape = FALSE, # shows our pictures
          rownames = FALSE, # removes row names
          colnames = c("County", "MI-SUVI Score", "Burden Score",
                       "Resource Score", "SVI Score"), # nicely formatted column names
          selection = "single",
          options = list(
            searching = FALSE, # hide the built-in search
            iDisplayLength = 10,

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
      }
    })

    observeEvent(input$table_main_rows_selected, {

      row_index <- input$table_main_rows_selected
      selected_row(
          filtered_data()[row_index, , drop = FALSE] |> sf::st_set_geometry(NULL)
        )

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

    selected_row <- reactiveVal(NULL)

    output$details_table <- renderDT({


      dict <- misuvi::dictionary() |> mutate(original_names = gsub("_", original_names, replacement = " "),
                                             original_names = gsub("100 000", original_names, replacement = "100,000"),
                                             original_names = gsub("1 000", original_names, replacement = "1,000"),
                                             original_names = gsub("2020 2022", original_names, replacement = "(2020-2022)"),
                                             original_names = gsub("2018 2022", original_names, replacement = "(2018-2022)")) |>
        filter(cleaned_names %in% names(filtered_data()))


     sub <-  filtered_data()[filtered_data()$county == selected_row()$county,] |>
        select(-c(fips, county)) |>
        t() |>
        as.data.frame() |>
        tibble::rownames_to_column() |>
        filter(!(rowname %in% c("misuvi_score", "svi_score", "burden_score", "resource_score"))) |>
        left_join(dict, by = c("rowname" = "cleaned_names"))

     colnames(sub)[2] <- "V1"

     sub <- sub |>
        select(original_names, V1)



        sub[1:8,] |>
        datatable(
          rownames = TRUE, # removes row names
          colnames = c("Variables", paste(type_filter())), # nicely formatted column names
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
          columns = 2, # rounding the z-scores
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


output$inputMap <- renderLeaflet({



  leaflet(mi,
          options = leafletOptions(
            zoomControl = FALSE,
            dragging = FALSE,
            minZoom = 6,
            maxZoom = 6
          )) %>%
    addPolygons(layerId = ~NAME,
                label = ~NAME,
                #   fillColor = "black",
                col = "black",
                fillColor = "#BF40BF",
                weight = 2,
                fillOpacity = .1,
                highlight = highlightOptions(
                  fillOpacity = 1,
                  bringToFront = TRUE
                ))

})


observeEvent(input$inputMap_shape_click, {
  click <- input$inputMap_shape_click

  if (!is.null(click)) {
    # If a county is clicked
    clicked_county <- mi[mi$NAME == click$id, ]



    rv$mi <- clicked_county

    leafletProxy("inputMap", session) |>
      removeShape("selected") |>
      addPolygons(data = mi,
                 layerId = ~NAME,
                 label = ~NAME,
                 fillColor = "#BF40BF",
                 col = "black",
                 weight = 2,
                 fillOpacity = 0.1) |>
      addPolygons(data = clicked_county,
                  layerId = "selected",
                  fillColor = "#BF40BF",
                  col = "black",
                  weight = 2,
                  fillOpacity = 1)



  }
  if(click$id == "selected"){
    # If no county is clicked (unclicked), show full data in table
    rv$mi <- NULL

  }



})




}

# Run the application
shinyApp(ui = ui, server = server)


