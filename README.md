# merraRshare — share R results as one web page

Turns the plots, maps and tables you make in R into a **single `.html` file**.
The file opens by double-clicking, can be emailed or uploaded to a website,
and keeps interactive zoom, hover and table search. No server, no Shiny.

## Install (once)

1. Install the packages it uses (only needed once; paste as one line, and if RStudio
   offers to restart R first, click **No**):

   ```r
   install.packages(c("htmltools", "htmlwidgets", "base64enc", "shiny", "ggplot2", "plotly", "DT", "leaflet"))
   ```

2. Install merraRshare from the file you were sent:

   ```r
   install.packages("path/to/merraRshare_0.1.2.tar.gz", repos = NULL, type = "source")
   ```

3. Restart RStudio. **Share results** now appears in the **Addins** menu
   (toolbar, top of the RStudio window).

## Try the example first

```r
merraRshare::share_example()
```

This makes a demo page from invented creel catch data (chart, map, table and
notes), opens it in your browser, and prints the code behind it so you can
copy it for your own results.

## Using it — no code

1. Make your plots, maps and tables as usual, **saved as objects**:

   ```r
   catch_plot <- ggplot(dat, aes(month, catch_kg, colour = site)) + geom_line()
   site_map   <- leaflet(sites) |> addTiles() |> addCircleMarkers(~lon, ~lat)
   ```

2. **Addins → Share results.**
3. Type a title, then click the items to include **in the order you want them**.
   Edit the section headings if you like.
4. Click **Create page**. The file is saved in your working folder and opens in
   your browser.

## Using it — one line of code

```r
library(merraRshare)
share_results(
  "Monthly catch by harbour" = catch_plot,
  "Harbour locations"        = site_map,
  "Catch records"            = dat,
  "Notes"                    = "Provisional figures; see methods in the report.",
  title  = "East Neuk creel catches 2026",
  author = "CRMG"
)
```

Names in quotes become the section headings.

## What can go in

| You have | It becomes |
|---|---|
| a ggplot | an interactive chart (or a picture with `interactive = FALSE`) |
| a leaflet map | an interactive map |
| a plotly chart, DT table or other htmlwidget | itself |
| a data frame / tibble | a searchable, sortable table |
| a base R plot | a picture. Draw it, then straight away: `my_plot <- recordPlot()` |
| text in quotes | paragraphs (leave a blank line between paragraphs) |

## Good to know

- **Everything in the file can be seen by whoever gets it**, including all rows
  of any table. Don't share confidential or unpublished data this way.
- Big tables make big files. Summarise first if you have tens of thousands of rows.
- Map backgrounds load from the internet when the page is opened; everything
  else works offline.
- A typical page with a chart, a map and a table is 3–5 MB, mostly the
  JavaScript that makes them interactive. Fine for email and websites.
