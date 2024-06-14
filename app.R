# Load libraries

library(shiny)
library(DT)
library(dplyr)
library(misuvi)
library(htmltools)
library(ggplot2)
library(bslib)
library(leaflet)
library(readxl)


# Load in Data

mi <- tigris::counties(state = "MI", cb = TRUE) |> # This loads in our shapefile for the clickable map
  mutate(NAME = case_when(NAME == "St. Clair" ~ "Saint Clair", # We have to correct some different spellings for two counties
                          NAME == "St. Joseph" ~ "Saint Joseph",
                          TRUE ~ NAME))

sf::st_crs(mi) <- 4326


regions <- read_xlsx("data/regional.xlsx", sheet = 2, skip = 1) |> # This code chunk imports some regional groupings we will use for filtering
  janitor::clean_names() |>
  select(fips, x5_region_grouping) |>
  rename(region = x5_region_grouping) |> # cleaning up the names
  mutate(fips = as.character(fips))


ui <- page_sidebar( # initiate ui

  tags$style(".modal-dialog {max-width: 60vw;} .leaflet-container {background: none !important;}"), # custom css makes our pop-up window a bit bigger, and the leaflet background transparent
  title = "Michigan Substance Use Vulnerability Index (MI-SUVI) Exploration Table", # title for the app

  # Sidebar with filters and map
  sidebar = sidebar(

    width=500,

    selectInput("type", "Select the data set:", # select the type of data set
                choices = c("z-scores", "percentiles", "rankings")),

    textInput("county", # text filter for county names
              "Filter by county:"),

    selectInput("region", # our drop-down menu
                "Filter by region:",
                choices = c("All", "Northeast", "Upper Peninsula", "Southwest",
                            "Northwest", "Southeast")
    ),

    leafletOutput("inputMap", height = 450) # leaflet output for our map filter
  ),

  accordion( # layout is accordion so we can collapse the sidebar and panels if needed

    multiple = TRUE, # allows multiple tabs to be open
    open = c("Main Table"), # specifies which should be left open

    accordion_panel("About", # here is our about section

                    htmlOutput("about_html") # with the output in html for formatting linebreaks and hyperlinks


    ),


    # Show our main table
    accordion_panel("Main Table",
                    DT::dataTableOutput("table_main") # main table output
    )



  )

)

# Define server logic
server <- function(input, output, session) {

  rv <- reactiveValues() # initialize reactive values for our map

  type_filter <- reactive({ # This allows us to take the nicer formatted option from the menu and use the text it corresponds to to query the right misuvi data set

    switch(input$type,
           "z-scores" = "zscores",
           "rankings" = "ranks",
           "percentiles" = "percentiles")

  })


  output$about_html <- renderUI({ # output of our html about section. This was the easiest way I found to format it nicely.

    HTML("This table shows data from the Michigan Substance Use
         Vulnerability Index (MI-SUVI). Below are the z-scores, percentiles, or rankings for all 83 counties in Michigan.
         The MI-SUVI is a standardized score
         calculated from three domains: burden of the opioid crisis, resources available to the county,
         and the Social Vulnerability Index (SVI).
         MI-SUVI data is intended to guide substance use disorder policy and programming.<br><br>
         This project presents an interactive, web-based alternative to downloading and navigating spreadsheets.
         Use the filters on the left side to target specific counties or change the data set. Clicking a row of
         the main table will open a closer view of the county's data. Cells that are green are better than the county average,
         while those that are red are worse than the county average.
         <br><br>",
         "All data can be access via the
         <a href = 'https://cran.r-project.org/web/packages/misuvi/index.html'>misuvi package</a>
         or from the <a href = 'https://www.michigan.gov/opioids/category-data'> State of Michigan's Website.</a>
         You can read the full technical documentation for the data
         <a href = 'https://www.michigan.gov/opioids/-/media/Project/Websites/opioids/documents/edc32Michigan-2022-SUVI-Documentation-562024.pdf?rev=3cd9b9477c194f3fb616292157918cc2'>here.</a><br><br>"
    )




  })

  # Initializing the data filtered from the UI inputs and the michigan_df that we will be filtering
  filtered_data <- reactiveVal(NULL)

  michigan_df <- reactiveVal(NULL)



  observeEvent(input$type,{ # this makes it so each time the input of type is changed, the dataset michigan_df and the filtered data changes accordingly.

    update <- misuvi_load(type = type_filter()) |> # queries data
      add_geometry() |> # add shape file
      left_join(regions, by = "fips") # adds regional groupings

    sf::st_crs(update) <- 4326 # set crs


    michigan_df(update) # updates michigan_df

    filtered_data(michigan_df()) # updates filtered_data

  })


  observeEvent(input$inputMap_shape_click, { # this is the logic behind the "click" of the map and the filtering of the main table.
    # The filtered_data() is fed into the table. This updates that data.

    click <- input$inputMap_shape_click

    if(click$id != "selected" | !is.null(click$id)){ # a second click returns an id of "selected". SO if a county is clicked it will filter by that id

      mapfilter <- michigan_df()[grepl(click$id, michigan_df()$county, ignore.case = TRUE), ]

      filtered_data(mapfilter)

    }

    if (click$id == "selected"){ # and likewise in the county is clicked again, the filter is cleared.
      michigan_df() |> filtered_data()
    }

  })





  observeEvent(list(input$region, input$county), { # this responds everytime the region or county inputs are changed.

    # Define a reactive variable to store the filtered data
    filtered_data_result <- reactiveVal(NULL)

    # Perform filtering based on county and region
    if (input$county == "" & input$region == "All") { # different logic if one filter is used and the other isn't or if both are.
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
      if (nrow(filtered_data_temp) > 0) { # this ensures if a text option returns an empty dataframe the app doesn't crash (':
        filtered_data(filtered_data_temp)
      } else {
        filtered_data(NULL)
      }
    }

    # Update the map
    if(input$county == "" & input$region == "All"){ # Possibly a little redundant (I'm sure there's a more efficient way to do this) but it works
      leafletProxy("inputMap", session) |>  # for each outcome it will update the map filter based on the text and grouping inputs
        clearShapes() |>
        addPolygons(data = michigan_df(),
                    layerId = ~county,
                    label = ~county,
                    fillColor = "#BF40BF",
                    col = "black",
                    weight = 2,
                    fillOpacity = 0.1)
    }else if (!is.null(filtered_data())) {
      leafletProxy("inputMap", session) |>
        clearShapes() |>
        addPolygons(data = michigan_df(),
                    layerId = ~county,
                    label = ~county,
                    fillColor = "#BF40BF",
                    col = "black",
                    weight = 2,
                    fillOpacity = 0.1) |>
        addPolygons(data = filtered_data(),
                    layerId = ~county,
                    label = ~county,
                    fillColor = "#BF40BF",
                    col = "black",
                    weight = 2,
                    fillOpacity = 1)
    } else {
      leafletProxy("inputMap", session) |>
        clearShapes() |>
        addPolygons(data = michigan_df(),
                    layerId = ~county,
                    label = ~county,
                    fillColor = "#BF40BF",
                    col = "black",
                    weight = 2,
                    fillOpacity = 0.1)
    }
  })




  output$table_main <- DT::renderDataTable({ # the main table output
    # first a statement to return somewhat of an error message if an invalid name is entered

    if(is.null(filtered_data())){

      print(dplyr::tibble("Error" = "Please enter a valid County name."))

    }else{

      filtered_data() |> # takes our filtered data
        select(county, misuvi_score, burden_score,  # selects our variables of interest
               resource_score, svi_score) |>
        sf::st_set_geometry(NULL) |>
        datatable(
          class = list(stripe = FALSE),
          #     escape = FALSE, # shows our pictures
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

  observeEvent(input$table_main_rows_selected, { # defines behavior if a row is selected in the main table

    row_index <- input$table_main_rows_selected
    selected_row(
      filtered_data()[row_index, , drop = FALSE] |> sf::st_set_geometry(NULL) # isolates the row clicked
    )

    showModal( # creates the pop-up when clicked
      modalDialog(
        title = paste(selected_row()$county, "County"),
        fluidPage(
          fluidRow( # value boxes for the domain scores
            column(3, value_box("MI-SUVI Score",
                                theme = value_box_theme(
                                  bg = # defines the background color
                                    if_else(type_filter() == "zscores",  if_else(filtered_data()[filtered_data()$county == selected_row()$county,]$misuvi_score < 0, "lightgreen", "lightpink"),
                                            if_else(type_filter() == "percentiles", if_else(filtered_data()[filtered_data()$county == selected_row()$county,]$misuvi_score < 50, "lightgreen", "lightpink"),
                                                    if_else(filtered_data()[filtered_data()$county == selected_row()$county,]$misuvi_score > 42, "lightgreen", "lightpink"))  ),

                                ),
                                value = round(filtered_data()[filtered_data()$county == selected_row()$county,]$misuvi_score, if_else(type_filter() == "zscores",   2,
                                                                                                                                      if_else(type_filter() == "percentiles", 1, # rounding and the value
                                                                                                                                              0)
                                )
                                )
            )
            ),

            column(3, value_box("Burden Score",

                                theme = value_box_theme(
                                  bg =

                                    if_else(type_filter() == "zscores",  if_else(filtered_data()[filtered_data()$county == selected_row()$county,]$burden_score < 0, "lightgreen", "lightpink"),
                                            if_else(type_filter() == "percentiles", if_else(filtered_data()[filtered_data()$county == selected_row()$county,]$burden_score < 50, "lightgreen", "lightpink"),
                                                    if_else(filtered_data()[filtered_data()$county == selected_row()$county,]$burden_score > 42, "lightgreen", "lightpink"))  ),

                                ),

                                value = round(filtered_data()[filtered_data()$county == selected_row()$county,]$burden_score, if_else(type_filter() == "zscores",   2,
                                                                                                                                      if_else(type_filter() == "percentiles", 1,
                                                                                                                                              0))))),
            column(3, value_box("Resource Score",


                                theme = value_box_theme(
                                  bg =

                                    if_else(type_filter() == "zscores",  if_else(filtered_data()[filtered_data()$county == selected_row()$county,]$resource_score < 0, "lightgreen", "lightpink"),
                                            if_else(type_filter() == "percentiles", if_else(filtered_data()[filtered_data()$county == selected_row()$county,]$resource_score < 50, "lightgreen", "lightpink"),
                                                    if_else(filtered_data()[filtered_data()$county == selected_row()$county,]$resource_score > 42, "lightgreen", "lightpink"))  ),

                                ),

                                value = round(filtered_data()[filtered_data()$county == selected_row()$county,]$resource_score, if_else(type_filter() == "zscores",   2,
                                                                                                                                        if_else(type_filter() == "percentiles", 1,
                                                                                                                                                0))))),
            column(3, value_box("SVI Score",


                                theme = value_box_theme(
                                  bg =

                                    if_else(type_filter() == "zscores",  if_else(filtered_data()[filtered_data()$county == selected_row()$county,]$svi_score < 0, "lightgreen", "lightpink"),
                                            if_else(type_filter() == "percentiles", if_else(filtered_data()[filtered_data()$county == selected_row()$county,]$svi_score < 50, "lightgreen", "lightpink"),
                                                    if_else(filtered_data()[filtered_data()$county == selected_row()$county,]$svi_score > 42, "lightgreen", "lightpink"))  ),

                                ),

                                value = round(filtered_data()[filtered_data()$county == selected_row()$county,]$svi_score, if_else(type_filter() == "zscores",   2,
                                                                                                                                   if_else(type_filter() == "percentiles", 1,
                                                                                                                                           0))))),

          ),
          DTOutput("details_table")), # output of the second table
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close")

        )
      )
    )
  })

  selected_row <- reactiveVal(NULL) # initializes selected row as a reactive value

  output$details_table <- renderDT({ # output for the second table

    # This is getting the names of the variables a bit cleaner
    dict <- misuvi::dictionary() |> mutate(original_names = gsub("_", original_names, replacement = " "),
                                           original_names = gsub("100 000", original_names, replacement = "100,000"),
                                           original_names = gsub("1 000", original_names, replacement = "1,000"),
                                           original_names = gsub("2022", original_names, replacement = "(2022)"),
                                           original_names = case_when(original_names == "3 year average nonfatal overdose emergency healthcare visit rate per 100,000 2020 (2022)" ~ "3 year average nonfatal overdose emergency healthcare visit rate per 100,000 (2020-2022)",
                                                                      original_names == "5 year average fatal overdose rate per 100,000 2018 (2022)" ~ "5 year average fatal overdose rate per 100,000 (2018-2022)",
                                                                      TRUE ~ original_names)) |>
      filter(cleaned_names %in% names(filtered_data()))

    # transposing the selected row and merging with the clean names
    sub <-  filtered_data()[filtered_data()$county == selected_row()$county,] |>
      select(-c(fips, county)) |>
      t() |>
      as.data.frame() |>
      tibble::rownames_to_column() |>
      filter(!(rowname %in% c("misuvi_score", "svi_score", "burden_score", "resource_score"))) |>
      left_join(dict, by = c("rowname" = "cleaned_names"))

    colnames(sub)[2] <- "V1"

    sub <- sub |> # filters to only what we need
      select(original_names, V1)



    sub[1:8,] |>
      datatable(
        class = list(stripe = FALSE), # removes striped rows
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
        # anything less than 0 is green (better than average), greater than 0 is light pink (worse than average). different for each data set
      ) |>
      formatStyle(
        columns = 1:2,
        selector = list("th"),
        fontSize = '24px', # increase cell text size

      )
  })


  output$inputMap <- renderLeaflet({ # rendering the filter map

    leaflet(mi,
            options = leafletOptions( # initializing the map
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


  rv <- reactiveValues(selected_counties = character(0)) # Initialize reactive value for selected counties


  observeEvent(input$inputMap_shape_click, { # Update behavior for map click
    click <- input$inputMap_shape_click


    if (!is.null(click)) {
      # If a county is clicked
      clicked_county <- mi[mi$NAME == click$id, ]

      if (click$id %in% rv$selected_counties) {
        # If selected, remove it
        rv$selected_counties <- rv$selected_counties[rv$selected_counties != click$id]
      } else if(click$id == "selected"){ # when a county is clicked again it is removed


        rv$selected_counties <- rv$selected_counties[rv$selected_counties != tail(rv$selected_counties, n = 1)]


        rv$selected_counties <- rv$selected_counties[rv$selected_counties != click$id]


      }else {
        # If not selected, add it
        rv$selected_counties <- c(rv$selected_counties, click$id)
      }

      leafletProxy("inputMap", session) |>
        removeShape("selected") |>
        addPolygons(data = mi,
                    layerId = ~NAME,
                    label = ~NAME,
                    fillColor = "#BF40BF", # Change fill color based on selection
                    col = "black",
                    weight = 2,
                    fillOpacity = ifelse(mi$NAME %in% rv$selected_counties, 1, 0.1)) |>
        addPolygons(data = clicked_county,
                    layerId = "selected",
                    fillColor = "#BF40BF",
                    col = "black",
                    weight = 2,
                    fillOpacity = ifelse(click$id %in% rv$selected_counties, 1, 0.1)) # Adjust fillOpacity based on selection


    }
  })

  observe({ # Update table filtering based on selected counties
    if (!is.null(rv$selected_counties) && length(rv$selected_counties) > 0) { # Check if any counties are selected
      filtered_data(michigan_df() |>
                      filter(county %in% rv$selected_counties))
    } else {
      filtered_data(michigan_df())
    }
  })


}

# Run the application
shinyApp(ui = ui, server = server)

