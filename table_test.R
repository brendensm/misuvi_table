library(DT)
library(dplyr)
library(misuvi)
library(htmltools)
library(ggplot2)
library(png)
library(base64enc)
library(readxl)

z <- misuvi_load("county", "zscores") |> add_geometry()
rank_c <- misuvi_load("county", "ranks") |> add_geometry()


regions <- read_xlsx("data/regional.xlsx", sheet = 2, skip = 1) |> janitor::clean_names() |>
  select(fips, x5_region_grouping) |>
  rename(region = x5_region_grouping) |>
  mutate(fips = as.character(fips))

z <- left_join(z, regions, by = "fips")

# # Assuming you have michigan_counties sf object loaded
# # Create function to generate small map
# create_small_map <- function(county_sf) {
#   ggplot() +
#     geom_sf(data = county_sf, fill = "steelblue", color = "black") +
#     theme_void() +
#     theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))
# }
#
# # Function to generate HTML for small map
# generate_map_html <- function(county_sf) {
#   map_plot <- create_small_map(county_sf)
#   map_html <- htmltools::tagList(as.tags(map_plot))
#   as.character(map_html)
# }

# Create data frame from sf object and add small maps
michigan_df <- as.data.frame(z)
# michigan_df$small_map <- sapply(1:nrow(rank_c), function(i) {
#   county_sf <- rank_c[i, ]
#   generate_map_html(county_sf)
# })

generate_map_base64 <- function(state_sf, county_sf) {
  # Create a temporary file to save the plot
  tmp_file <- tempfile(fileext = ".png")

  # Save the ggplot as a PNG
  png(tmp_file, width = 200, height = 200, bg = "transparent")
  print(ggplot() +
          geom_sf(data = state_sf, fill = "gray97",  color = "black") +
          geom_sf(data = county_sf, fill = "purple", color = "black") +
          theme_void() +
          theme(
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.border = element_rect(color = "transparent", fill = NA, size = 0.5)
          ))
  dev.off()

  # Read the PNG file and encode it to base64
  base64_img <- base64enc::dataURI(file = tmp_file, mime = "image/png")

  # Remove the temporary file
  unlink(tmp_file)

  # Return the HTML image tag
  sprintf('<img src="%s" width="110" height="110">', base64_img)
}


state_sf <- z

michigan_df$small_map <- sapply(1:nrow(michigan_df), function(i) {
  county_sf <- z[i, ]
  generate_map_base64(state_sf, county_sf)
})

 michigan_df |> select(small_map, county, misuvi_score, burden_score, resource_score, svi_score) |>
  datatable(
  escape = FALSE # This allows HTML rendering in the table
  # options = list(
  #     callback = JS(
  #       "table.column(0).header().to$().css({ 'font-size': '16px' });",
  #       "table.column(1).header().to$().css({ 'font-size': '16px' });",
  #       "table.column(2).header().to$().css({ 'font-size': '16px' });",
  #       "table.column(3).header().to$().css({ 'font-size': '16px' });",
  #       "table.column(4).header().to$().css({ 'font-size': '16px' });"
  #     )
  #   )

  # options = list(
  #   columnDefs = list(list(targets = ncol(michigan_df), render = DT::JS(
  #     "function(data, type, row, meta) { return type === 'display' ? data : ''; }"
  #   )))
  ) |>
  formatRound(
    columns = c("misuvi_score", "burden_score", "resource_score", "svi_score")
  ) |>
  formatStyle(
    columns = c("misuvi_score", "burden_score", "resource_score", "svi_score"),
    backgroundColor = styleInterval(0, c('lightgreen', 'lightpink')),

  ) |>
  formatStyle(
    columns = 1:6,
    selector = list("th"),
    fontSize = '20px'
  )

 sub <-  michigan_df |> select(small_map, county, misuvi_score, burden_score, resource_score, svi_score, region)


# file_path <- "path/to/your/folder/michigan_data.rds"

 # Save the data using saveRDS()
 saveRDS(sub, file = "data/zscore_sub_counties_2.rds")

 # Now, in another script, you can read the saved RDS file and load the data
 saved_data <- readRDS("data/zscore_sub_counties_2.rds")

 datatable(saved_data, escape = FALSE)

 # Create interactive DT table
# datatable(
#   michigan_df,
#   escape = FALSE,
#   options = list(
#     columnDefs = list(list(targets = ncol(michigan_df), render = DT::JS(
#       "function(data, type, row, meta) { return type === 'display' ? data : ''; }"
#     )))
#   )
#)














z |>
  select(county, misuvi_score, burden_score, resource_score, svi_score) |>
  datatable() |>
  formatRound(
    columns = c("misuvi_score", "burden_score", "resource_score", "svi_score")
  ) |>
  formatStyle(
    columns = c("misuvi_score", "burden_score", "resource_score", "svi_score"),
    backgroundColor = styleInterval(0, c('lightgreen', 'lightpink'))
  )

datatable(iris) %>%
 # formatStyle('Sepal.Length', fontWeight = styleInterval(5, c('normal', 'bold'))) %>%
  formatStyle(
    'Sepal.Width',
    color = styleInterval(c(3.4, 3.8), c('white', 'blue', 'red')),
    backgroundColor = styleInterval(3.4, c('gray', 'yellow'))
  ) %>%
  formatStyle(
    'Petal.Length',
    background = styleColorBar(iris$Petal.Length, 'steelblue'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  ) %>%
  formatStyle(
    'Species',
    transform = 'rotateX(45deg) rotateY(20deg) rotateZ(30deg)',
    backgroundColor = styleEqual(
      unique(iris$Species), c('lightblue', 'lightgreen', 'lightpink')
    )
  )
