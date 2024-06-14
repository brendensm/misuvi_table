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



mi <- tigris::counties(state = "MI", cb = TRUE) |>
  mutate(NAME = case_when(NAME == "St. Clair" ~ "Saint Clair",
                          NAME == "St. Joseph" ~ "Saint Joseph",
                          TRUE ~ NAME))


regions <- read_xlsx("data/regional.xlsx", sheet = 2, skip = 1) |> janitor::clean_names() |>
  select(fips, x5_region_grouping) |>
  rename(region = x5_region_grouping) |>
  mutate(fips = as.character(fips))


ui <- page_sidebar(

  tags$style(".modal-dialog {max-width: 60vw;} .leaflet-container {
  background: none !important;}"),
    title = "Michigan Substance Use Vulnerability Index (MI-SUVI) Exploration Table",

    # Sidebar with two filters
    sidebar = sidebar(

      width=500,




          selectInput("type", "Select the type of data",
                      choices = c("z-scores", "percentiles", "rankings")),

          textInput("county",
                    "Filter by County:"),

          selectInput("region",
                      "Filter by Region:",
                      choices = c("All", "Northeast", "Upper Peninsula", "Southwest",
                                  "Northwest", "Southeast")
                      ),

         # plotOutput("map"),
          leafletOutput("inputMap", height = 550)



      ),

    accordion(

      multiple = TRUE,
      open = c("Main Table", "About"),

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
  filtered_data <- reactiveVal(NULL)

  michigan_df <- reactiveVal(NULL)



  # michigan_df <- reactive({misuvi_load(type = type_filter()) |>
  #     left_join( regions, by = "fips")})

    observeEvent(input$type,{


      michigan_df(misuvi_load(type = type_filter()) |>
                    add_geometry() |>
                    left_join(regions, by = "fips"))

      filtered_data(michigan_df())

      print(input$type)

      print(type_filter())

      print(michigan_df())

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


    # observeEvent(input$county, {
    #
    #   if (input$county == "" & input$region == "All") {
    #     michigan_df
    #   } else if (input$region == "All"){
    #     michigan_df[grepl(input$county, michigan_df$county, ignore.case = TRUE), ]
    #   } else if (input$county == ""){
    #     michigan_df[grepl(input$region, michigan_df$region, ignore.case = TRUE), ]
    #   }else{
    #     michigan_df[grepl(input$county, michigan_df$county, ignore.case = TRUE), ]
    #
    #     subset(michigan_df, grepl(input$county, county, ignore.case = TRUE) |
    #              grepl(input$region, region, ignore.case = TRUE))
    #   }
    #
    #
    # })


#  bad solution but the last bit might help
#     observeEvent(list(input$region, input$county), {
#
#       if (input$county == "" & input$region == "All") {
#         filtered_data(michigan_df())
#         leafletProxy("inputMap", session) %>%
#           clearShapes() %>%
#           addPolygons(data = michigan_df(),
#                       layerId = ~county,
#                       label = ~county,
#                       fillColor = "#BF40BF",
#                       col = "black",
#                       weight = 2,
#                       fillOpacity = 0.1)
#       } else {
#         filtered_data_result <- michigan_df()[grepl(input$county, michigan_df()$county, ignore.case = TRUE), ]
#
#         if (nrow(filtered_data_result) > 0) {
#           filtered_data(filtered_data_result)
#
#           leafletProxy("inputMap", session) %>%
#             clearShapes() %>%
#             addPolygons(data = michigan_df(),
#                         layerId = ~county,
#                         label = ~county,
#                         fillColor = "#BF40BF",
#                         col = "black",
#                         weight = 2,
#                         fillOpacity = 0.1) %>%
#             addPolygons(data = filtered_data(),
#                         layerId = ~county,
#                         label = ~county,
#                         fillColor = "#BF40BF",
#                         col = "black",
#                         weight = 2,
#                         fillOpacity = 1)
#         } else {
#           filtered_data(NULL)  # Clear filtered data if no matches found
#           leafletProxy("inputMap", session) %>%
#             clearShapes() %>%
#             addPolygons(data = michigan_df(),
#                         layerId = ~county,
#                         label = ~county,
#                         fillColor = "#BF40BF",
#                         col = "black",
#                         weight = 2,
#                         fillOpacity = 0.1)
#         }
#       }
#     })
#
#
#
#




    #########################################################

    observeEvent(list(input$region, input$county), {

      if (input$county == "" & input$region == "All") {
        michigan_df() |> filtered_data()


        leafletProxy("inputMap", session) %>%
          clearShapes() %>%
          addPolygons(data = michigan_df(),
                      layerId = ~county,
                      label = ~county,
                      fillColor = "#BF40BF",
                      col = "black",
                      weight = 2,
                      fillOpacity = 0.1)


      } else if (input$region == "All"){
        michigan_df()[grepl(input$county, michigan_df()$county, ignore.case = TRUE), ] |> filtered_data()

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


      } else if (input$county == ""){
        michigan_df()[grepl(input$region, michigan_df()$region, ignore.case = TRUE), ] |> filtered_data()

        leafletProxy("inputMap", session) %>%
          clearShapes() %>%
          addPolygons(data = michigan_df(),

                      label = ~county,
                      layerId = ~county,
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

      }else{
        michigan_df()[grepl(input$county, michigan_df()$county, ignore.case = TRUE), ]

        subset(michigan_df(), grepl(input$county, county, ignore.case = TRUE) |
                 grepl(input$region, region, ignore.case = TRUE)) |> filtered_data()


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

      }


   #   if (nrow(filtered_data_result) > 0) {
        #           filtered_data(filtered_data_result)
        #
        #           leafletProxy("inputMap", session) %>%
        #             clearShapes() %>%
        #             addPolygons(data = michigan_df(),
        #                         layerId = ~county,
        #                         label = ~county,
        #                         fillColor = "#BF40BF",
        #                         col = "black",
        #                         weight = 2,
        #                         fillOpacity = 0.1) %>%
        #             addPolygons(data = filtered_data(),
        #                         layerId = ~county,
        #                         label = ~county,
        #                         fillColor = "#BF40BF",
        #                         col = "black",
        #                         weight = 2,
        #                         fillOpacity = 1)
        #         } else {
        #           filtered_data(NULL)  # Clear filtered data if no matches found
        #           leafletProxy("inputMap", session) %>%
        #             clearShapes() %>%
        #             addPolygons(data = michigan_df(),
        #                         layerId = ~county,
        #                         label = ~county,
        #                         fillColor = "#BF40BF",
        #                         col = "black",
        #                         weight = 2,
        #                         fillOpacity = 0.1)
        #         }


      # else {
      #   filtered_data(NULL)  # Clear filtered data if no matches found
      #   leafletProxy("inputMap", session) %>%
      #     clearShapes() %>%
      #     addPolygons(data = michigan_df(),
      #                 layerId = ~county,
      #                 label = ~county,
      #                 fillColor = "#BF40BF",
      #                 col = "black",
      #                 weight = 2,
      #                 fillOpacity = 0.1)
      # }

      # investigate weird crashing when you search for something wrong
      # if(!(input$county %in% michigan_df()$county)){
      #   michigan_df() |> filtered_data()
      #
      #
      #   leafletProxy("inputMap", session) %>%
      #     clearShapes() %>%
      #     addPolygons(data = michigan_df(),
      #                 layerId = ~county,
      #                 label = ~county,
      #                 fillColor = "#BF40BF",
      #                 col = "black",
      #                 weight = 2,
      #                 fillOpacity = 0.1)
      # }






    })








    output$table_main <- DT::renderDataTable({
        # main output


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

    # selected_row <- reactive({
    #   req(input$table_main_rows_selected)
    #   row_index <- input$table_main_rows_selected
    #   filtered_data()[row_index, , drop = FALSE] |> sf::st_set_geometry(NULL)
    # })

    selected_row <- reactiveVal(NULL)

    output$details_table <- renderDT({


      dict <- misuvi::dictionary() |> mutate(original_names = gsub("_", original_names, replacement = " "),
                                             original_names = gsub("100 000", original_names, replacement = "100,000"),
                                             original_names = gsub("1 000", original_names, replacement = "1,000"),
                                             original_names = gsub("2020 2022", original_names, replacement = "(2020-2022)"),
                                             original_names = gsub("2018 2022", original_names, replacement = "(2018-2022)")) |>
        filter(cleaned_names %in% names(filtered_data()))



   # print( filtered_data()[filtered_data()$county == selected_row()$county,])


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


# output$map <- renderPlot({
#
#
#   mi_highlight <- mi |>
#     mutate(NAME = case_when(NAME == "St. Joseph" ~ "Saint Joseph",
#                             NAME == "St. Clair" ~ "Saint Clair",
#                             TRUE ~ NAME)) |>
#     filter(NAME %in% filtered_data()$county)
#
#
#   mi |>
#     ggplot() +
#     geom_sf() +
#     geom_sf(data = mi_highlight, fill = "lightblue", color = "black") +
#     theme_void()
#
#
#
# })


#rv <- reactiveValues()


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


# observeEvent(list(input$county, input$region), {
#
#   if (input$region == "All"){
#     #michigan_df()[grepl(input$county, michigan_df()$county, ignore.case = TRUE), ] |> filtered_data()
#
#     group_hl <- mi[grepl(input$county, mi$NAME, ignore.case = TRUE), ]
#
#   }
#
#   leafletProxy("inputMap", session) %>%
#     removeShape("filt") %>%
#     addPolygons(data = group_hl,
#                 layerId = "filt",
#                 fillColor = "#BF40BF",
#                 weight = 3,
#                 fillOpacity = 1)
#
# })



observeEvent(input$inputMap_shape_click, {
  click <- input$inputMap_shape_click

  if (!is.null(click)) {
    # If a county is clicked
    clicked_county <- mi[mi$NAME == click$id, ]



    rv$mi <- clicked_county

    leafletProxy("inputMap", session) %>%
      removeShape("selected") %>%
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


