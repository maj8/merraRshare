#' Make an example page to see what merraRshare does
#'
#' Builds a demo page from invented creel catch data for five East Neuk
#' harbours: an interactive chart, a map, a table and some notes. It also
#' prints the code used, so you can copy it for your own results.
#'
#' Needs the ggplot2 package; the map needs leaflet and the interactive
#' chart needs plotly (they are skipped if not installed).
#'
#' @param file Where to save the example page (Downloads by default).
#' @param open Open it in the web browser.
#' @return The path to the HTML file, invisibly.
#' @export
share_example <- function(file = "merraRshare-example.html",
                          open = base::interactive()) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The example needs ggplot2: install.packages(\"ggplot2\")", call. = FALSE)
  }
  dat <- example_data()

  items <- list()
  items[["Monthly catch by harbour"]] <- ggplot2::ggplot(
    dat, ggplot2::aes(factor(month, month.abb), catch_kg, colour = site, group = site)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::labs(x = NULL, y = "Catch (kg)", colour = "Harbour") +
    ggplot2::theme_minimal()

  if (requireNamespace("leaflet", quietly = TRUE)) {
    sites <- stats::aggregate(catch_kg ~ site + lat + lon, dat, sum)
    items[["Harbour locations"]] <- leaflet::addCircleMarkers(
      leaflet::addTiles(leaflet::leaflet(sites)),
      lng = ~lon, lat = ~lat, radius = ~sqrt(catch_kg) / 3,
      popup = ~paste0(site, ": ", catch_kg, " kg in the year"))
  }
  items[["Catch records"]] <- dat
  items[["Notes"]] <- paste(
    "These figures are invented, for demonstration only.",
    "",
    "Hover over the chart to see values, zoom the map, and search or sort the table.",
    sep = "\n")

  path <- do.call(share_results, c(items, list(
    title = "East Neuk creel catches (example)",
    subtitle = "Made with merraRshare",
    intro = "This page is one self-contained file: it can be emailed, uploaded to a website or opened offline.",
    file = file, open = open)))

  message("\nThe code behind this example:\n", example_code())
  invisible(path)
}

example_data <- function() {
  utils::read.csv(system.file("extdata", "example_catches.csv", package = "merraRshare"),
                  stringsAsFactors = FALSE)
}

example_code <- function() {
'
library(ggplot2); library(leaflet); library(merraRshare)
dat <- read.csv(system.file("extdata", "example_catches.csv", package = "merraRshare"))

catch_plot <- ggplot(dat, aes(factor(month, month.abb), catch_kg, colour = site, group = site)) +
  geom_line() + geom_point() + labs(x = NULL, y = "Catch (kg)")

sites    <- aggregate(catch_kg ~ site + lat + lon, dat, sum)
site_map <- leaflet(sites) |> addTiles() |> addCircleMarkers(~lon, ~lat, popup = ~site)

share_results(
  "Monthly catch by harbour" = catch_plot,
  "Harbour locations"        = site_map,
  "Catch records"            = dat,
  title = "East Neuk creel catches (example)"
)
'
}

utils::globalVariables(c("month", "catch_kg", "site", "lat", "lon"))
