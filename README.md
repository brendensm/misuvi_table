## Michigan Substance Use Vulnerability Index (MI-SUVI) Exploration Table

This project is a submission to Posit's 2024 table contest. My submission features the Michigan Substance Use Vulnerability Index (MI-SUVI) from the Michigan Department of Health and Human Services (MDHHS). This index features Z-scores, percentiles, and rankings of Michigan's 83 Counties. The composite score is based off of three domains: burden of the opioid crisis, resources available to the county, and the Social Vulnerability Index (SVI).

Currently, this data is available in the form of an Excel workbook, and some data is featured on the State's own PowerBI dashboard. I was inspired to create an interactive table to explore this data in an R Shiny application. My project uses {DT} for the tables and queries data with my own package, {misuvi}.

The most technical features of this app are the filters. My submission includes a leaflet map that filters the table. Clicking on a row of the main table also displays more specific data that make up the domain scores of the county. This app makes it easy to compare counties, look within regions, and understand the data. Users can easily switch between Z-scores, percentiles, and rankings, making it easier to understand for a wider audience.

For full details on the data and methodology of the index, you can read the full [technical documentation](https://www.michigan.gov/opioids/-/media/Project/Websites/opioids/documents/edc32Michigan-2022-SUVI-Documentation-562024.pdf?rev=3cd9b9477c194f3fb616292157918cc2). You can view the data on the State of Michigan's website [here.](https://www.michigan.gov/opioids/category-data)
