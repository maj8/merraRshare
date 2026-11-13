#' Save plots, maps, tables and notes as one self-contained HTML page
#'
#' Give each item a name to use it as a section heading, e.g.
#' `share_results("Catch by site" = p, "Survey sites" = m, title = "...")`.
#'
#' Supported items:
#' * ggplot objects (made interactive with plotly when `interactive = TRUE`)
#' * any htmlwidget (plotly, leaflet, DT, dygraphs, ...)
#' * data frames / tibbles (shown as a searchable, sortable table)
#' * base R plots captured with `recordPlot()`
#' * character strings (shown as text; blank lines start new paragraphs)
#' * htmltools tags
#'
#' @param ... Items to include, in order. Names become section headings.
#' @param title Page title.
#' @param subtitle Optional line under the title.
#' @param author Optional author name shown in the header.
#' @param intro Optional introductory text shown above the first item.
#' @param file Output file. Defaults to a name made from the title. A bare
#'   file name is saved in your Downloads folder (see [save_folder()]); give a
#'   path with a folder in it to save somewhere else.
#' @param interactive Convert ggplots to interactive plotly charts.
#' @param open Open the finished page in the web browser.
#' @param overwrite Overwrite `file` if it exists. If `FALSE`, a number is
#'   added to the file name instead.
#' @return The path to the HTML file, invisibly.
#' @export
share_results <- function(..., title = "Results", subtitle = NULL,
                          author = NULL, intro = NULL, file = NULL,
                          interactive = TRUE, open = base::interactive(),
                          overwrite = TRUE) {
  items <- list(...)
  if (length(items) == 0) stop("Nothing to share: give at least one plot, map or table.")
  headings <- names(items)
  if (is.null(headings)) headings <- rep("", length(items))

  has_map <- FALSE
  sections <- vector("list", length(items))
  for (i in seq_along(items)) {
    x <- items[[i]]
    if (inherits(x, "leaflet")) has_map <- TRUE
    body <- tryCatch(
      convert_item(x, interactive = interactive),
      error = function(e) {
        stop(sprintf("Couldn't include item %d%s: %s", i,
                     if (nzchar(headings[i])) paste0(" ('", headings[i], "')") else "",
                     conditionMessage(e)), call. = FALSE)
      }
    )
    sections[[i]] <- htmltools::tags$section(
      class = if (is.character(x)) "item text" else "item",
      if (nzchar(headings[i])) htmltools::tags$h2(headings[i]),
      body
    )
  }

  meta <- c(if (!is.null(author) && nzchar(author)) author,
            format(Sys.Date(), "%d %B %Y"))

  page <- htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      htmltools::tags$title(title),
      htmltools::tags$style(htmltools::HTML(page_css()))
    ),
    htmltools::tags$header(
      htmltools::tags$h1(title),
      if (!is.null(subtitle) && nzchar(subtitle)) htmltools::tags$p(class = "subtitle", subtitle),
      htmltools::tags$p(class = "meta", htmltools::HTML(
        paste(htmltools::htmlEscape(meta), collapse = " &middot; ")))
    ),
    htmltools::tags$main(
      if (!is.null(intro) && nzchar(intro)) htmltools::tags$section(class = "item text", text_to_html(intro)),
      sections
    ),
    htmltools::tags$footer(
      paste0("Created ", format(Sys.time(), "%d %B %Y %H:%M"), " with merraRshare."),
      if (has_map) " Map backgrounds need an internet connection; everything else works offline."
    )
  )

  if (is.null(file) || !nzchar(file)) file <- paste0(slugify(title), ".html")
  if (!grepl("\\.html?$", file, ignore.case = TRUE)) file <- paste0(file, ".html")
  # A bare file name goes in the save folder (Downloads unless changed);
  # a name with a folder in it is used as given.
  if (identical(basename(file), file)) file <- file.path(save_folder(), file)
  if (!overwrite) file <- unique_path(file)

  # Build in a temporary folder so no "lib" folder is left next to the output.
  build_dir <- tempfile("merraRshare_")
  dir.create(build_dir)
  on.exit(unlink(build_dir, recursive = TRUE), add = TRUE)
  tmp_html <- file.path(build_dir, "index.html")
  htmltools::save_html(page, tmp_html, libdir = "lib")
  make_self_contained(tmp_html)

  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  if (!file.copy(tmp_html, file, overwrite = TRUE)) stop("Couldn't write ", file)
  path <- normalizePath(file)
  message(sprintf("Saved %s (%s)", path, format_size(file.size(path))))
  if (isTRUE(open)) utils::browseURL(path)
  invisible(path)
}

# ---- converting each item ----------------------------------------------------

convert_item <- function(x, interactive = TRUE) {
  if (inherits(x, "ggplot")) {
    if (interactive && requireNamespace("plotly", quietly = TRUE)) {
      return(size_widget(plotly::ggplotly(x)))
    }
    return(plot_to_img(x))
  }
  if (inherits(x, "leaflet")) return(size_widget(fix_basemap(x)))
  if (inherits(x, "htmlwidget")) return(size_widget(x))
  if (inherits(x, "recordedplot")) return(plot_to_img(x))
  if (is.data.frame(x)) return(table_html(x))
  if (inherits(x, c("shiny.tag", "shiny.tag.list", "html"))) return(x)
  if (is.character(x)) return(text_to_html(x))
  stop("objects of class '", class(x)[1], "' can't be shared yet. ",
       "Supported: ggplot, htmlwidget (plotly, leaflet, DT...), data frame, recordPlot(), text.")
}

# OpenStreetMap's own tile servers block maps opened from a file (they get no
# website address with the request), so swap them for a basemap that allows it.
# Choose with options(merraRshare.basemap = "carto" | "esri-ocean" | "keep").
basemaps <- list(
  carto = list(
    url = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}{r}.png",
    subdomains = "abcd", maxZoom = 20,
    attribution = paste0('&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> ',
                         'contributors &copy; <a href="https://carto.com/attributions">CARTO</a>')),
  `esri-ocean` = list(
    url = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}",
    subdomains = "abc", maxZoom = 13,
    attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri")
)

fix_basemap <- function(m) {
  choice <- getOption("merraRshare.basemap", "carto")
  if (identical(choice, "keep") || is.null(basemaps[[choice]])) return(m)
  b <- basemaps[[choice]]
  calls <- m$x$calls
  for (i in seq_along(calls)) {
    cl <- calls[[i]]
    is_osm <- (identical(cl$method, "addTiles") && is.character(cl$args[[1]]) &&
                 grepl("tile\\.openstreetmap\\.org", cl$args[[1]])) ||
              (identical(cl$method, "addProviderTiles") && is.character(cl$args[[1]]) &&
                 grepl("^OpenStreetMap", cl$args[[1]]))
    if (!is_osm) next
    opts <- if (length(cl$args) >= 4 && is.list(cl$args[[4]])) cl$args[[4]] else list()
    opts$subdomains <- b$subdomains
    opts$maxZoom <- b$maxZoom
    opts$attribution <- b$attribution
    calls[[i]] <- list(method = "addTiles",
                       args = list(b$url,
                                   if (length(cl$args) >= 2) cl$args[[2]] else NULL,
                                   if (length(cl$args) >= 3) cl$args[[3]] else NULL,
                                   opts))
  }
  m$x$calls <- calls
  m
}

size_widget <- function(w) {
  if (is.null(w$width)) w$width <- "100%"
  if (is.null(w$height) && !inherits(w, "datatables")) w$height <- 460
  w
}

table_html <- function(df) {
  if (inherits(df, "sf")) {
    df <- as.data.frame(df)
    df <- df[!vapply(df, inherits, logical(1), what = "sfc")]
  }
  df <- as.data.frame(df)
  if (nrow(df) > 20000) {
    warning(sprintf("Table has %s rows; the page will be large and slow. Consider summarising first.",
                    format(nrow(df), big.mark = ",")), call. = FALSE)
  }
  if (requireNamespace("DT", quietly = TRUE)) {
    w <- DT::datatable(df, rownames = FALSE, filter = "top",
                       extensions = character(0),
                       options = list(pageLength = 10, scrollX = TRUE, autoWidth = FALSE))
    return(size_widget(w))
  }
  # Fallback without DT: plain table of the first 500 rows
  shown <- utils::head(df, 500)
  htmltools::tagList(
    htmltools::tags$div(class = "table-wrap", htmltools::tags$table(
      htmltools::tags$thead(htmltools::tags$tr(lapply(names(shown), htmltools::tags$th))),
      htmltools::tags$tbody(lapply(seq_len(nrow(shown)), function(r) {
        htmltools::tags$tr(lapply(shown[r, , drop = TRUE], function(v) htmltools::tags$td(format(v))))
      }))
    )),
    if (nrow(df) > 500) htmltools::tags$p(class = "note", sprintf("Showing first 500 of %d rows.", nrow(df)))
  )
}

plot_to_img <- function(x, width = 9, height = 5.5, res = 150) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  type <- if (capabilities("cairo")) "cairo" else getOption("bitmapType")
  grDevices::png(f, width = width, height = height, units = "in", res = res, type = type)
  ok <- FALSE
  tryCatch({
    if (inherits(x, "recordedplot")) grDevices::replayPlot(x) else print(x)
    ok <- TRUE
  }, finally = grDevices::dev.off())
  if (!file.exists(f) || file.size(f) == 0) {
    stop("the plot came out empty. For base R plots, draw the plot and then run ",
         "recordPlot() straight away, e.g.  plot(x); my_plot <- recordPlot()")
  }
  htmltools::tags$img(src = base64enc::dataURI(file = f, mime = "image/png"),
                      alt = "plot", class = "static-plot")
}

text_to_html <- function(x) {
  x <- paste(x, collapse = "\n")
  paras <- strsplit(x, "\n[[:space:]]*\n")[[1]]
  paras <- trimws(paras)
  paras <- paras[nzchar(paras)]
  htmltools::tagList(lapply(paras, function(p) {
    lines <- strsplit(p, "\n", fixed = TRUE)[[1]]
    htmltools::tags$p(lapply(seq_along(lines), function(i) {
      if (i < length(lines)) htmltools::tagList(lines[i], htmltools::tags$br()) else lines[i]
    }))
  }))
}

# ---- helpers -------------------------------------------------------------------

#' Where merraRshare saves pages
#'
#' Pages are saved in your Downloads folder unless you choose another one with
#' `options(merraRshare.folder = "C:/path/to/folder")` (put that line in your
#' .Rprofile to make it permanent). If there is no Downloads folder, the
#' working directory is used.
#'
#' @return The folder path.
#' @export
save_folder <- function() {
  opt <- getOption("merraRshare.folder")
  if (!is.null(opt) && nzchar(opt)) return(path.expand(opt))
  home <- if (.Platform$OS.type == "windows") Sys.getenv("USERPROFILE") else path.expand("~")
  downloads <- file.path(home, "Downloads")
  if (nzchar(home) && dir.exists(downloads)) return(normalizePath(downloads, winslash = "/"))
  getwd()
}

slugify <- function(x) {
  s <- tolower(iconv(x, to = "ASCII//TRANSLIT", sub = ""))
  s <- gsub("[^a-z0-9]+", "-", s)
  s <- gsub("^-+|-+$", "", s)
  if (!nzchar(s)) s <- "results"
  substr(s, 1, 60)
}

unique_path <- function(file) {
  if (!file.exists(file)) return(file)
  stem <- sub("\\.html?$", "", file, ignore.case = TRUE)
  i <- 2
  while (file.exists(sprintf("%s-%d.html", stem, i))) i <- i + 1
  sprintf("%s-%d.html", stem, i)
}

format_size <- function(bytes) {
  if (bytes < 1024^2) sprintf("%.0f KB", bytes / 1024) else sprintf("%.1f MB", bytes / 1024^2)
}

page_css <- function() {
'
:root { --ink:#1d2a33; --muted:#5f6f7a; --line:#dde4e8; --accent:#0b6e8a; --bg:#f5f7f8; }
* { box-sizing: border-box; }
body { margin:0; background:var(--bg); color:var(--ink);
  font-family: -apple-system, "Segoe UI", Roboto, Helvetica, Arial, sans-serif; line-height:1.5; }
header { background:#fff; border-bottom:3px solid var(--accent); padding:28px 24px 18px; }
header, main, footer { width:100%; }
header > *, footer { max-width:1100px; margin-left:auto; margin-right:auto; }
h1 { margin:0 0 4px; font-size:1.8rem; font-weight:650; }
.subtitle { margin:0 0 6px; font-size:1.1rem; color:var(--ink); }
.meta { margin:0; color:var(--muted); font-size:.9rem; }
main { max-width:1100px; margin:24px auto; padding:0 16px; }
.item { background:#fff; border:1px solid var(--line); border-radius:8px;
  padding:18px 20px 20px; margin-bottom:20px; overflow:hidden; }
.item.text { font-size:1.02rem; }
.item.text p:first-child { margin-top:0; } .item.text p:last-child { margin-bottom:0; }
h2 { margin:0 0 12px; font-size:1.2rem; font-weight:600; color:var(--accent); }
.static-plot { max-width:100%; height:auto; display:block; margin:0 auto; }
.table-wrap { overflow-x:auto; }
table { border-collapse:collapse; font-size:.9rem; }
th, td { border-bottom:1px solid var(--line); padding:5px 10px; text-align:left; }
.note { color:var(--muted); font-size:.85rem; }
footer { color:var(--muted); font-size:.8rem; padding:4px 16px 32px; }
@media print { body { background:#fff; } .item { break-inside:avoid; border:none; padding:0; } }
'
}
