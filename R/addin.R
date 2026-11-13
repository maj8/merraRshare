#' RStudio addin: pick items from the workspace and save them as a page
#'
#' Opens a small window listing the plots, maps and tables in your
#' workspace. Choose what to include (in the order you click them), give
#' each a heading, and click "Create page".
#'
#' @param env Environment to look in (defaults to the global workspace).
#' @export
share_results_addin <- function(env = globalenv()) {
  found <- find_shareable(env)
  if (length(found) == 0) {
    stop("No plots, maps or tables found in your workspace.\n",
         "Save them to objects first, e.g.  p <- ggplot(...)  or  m <- leaflet(...)", call. = FALSE)
  }
  app <- addin_app(found, env)
  viewer <- shiny::dialogViewer("Share results", width = 720, height = 760)
  shiny::runGadget(app$ui, app$server, viewer = viewer, stopOnCancel = TRUE)
}

# Named character vector: object name -> description
find_shareable <- function(env) {
  nms <- ls(env)
  out <- character(0)
  for (n in nms) {
    x <- get(n, envir = env)
    label <- describe_object(x)
    if (!is.null(label)) out[n] <- label
  }
  out
}

describe_object <- function(x) {
  if (inherits(x, "ggplot")) return("ggplot chart")
  if (inherits(x, "leaflet")) return("map")
  if (inherits(x, "plotly")) return("interactive chart")
  if (inherits(x, "datatables")) return("table")
  if (inherits(x, "htmlwidget")) return(paste(class(x)[1], "widget"))
  if (inherits(x, "recordedplot")) return("base R plot")
  if (is.data.frame(x)) return(sprintf("data, %s rows \u00d7 %d cols",
                                       format(nrow(x), big.mark = ","), ncol(x)))
  NULL
}

pretty_heading <- function(name) {
  h <- gsub("[._]+", " ", name)
  paste0(toupper(substr(h, 1, 1)), substr(h, 2, nchar(h)))
}

addin_app <- function(found, env) {
  choices <- stats::setNames(names(found), sprintf("%s  \u2014  %s", names(found), found))

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      "Share results",
      right = miniUI::miniTitleBarButton("done", "Create page", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      shiny::tags$style(".help-block{margin-top:2px} .form-group{margin-bottom:12px}"),
      shiny::textInput("title", "Page title", value = "Results", width = "100%"),
      shiny::fillRow(
        height = "70px",
        shiny::textInput("subtitle", "Subtitle (optional)", width = "95%"),
        shiny::textInput("author", "Author (optional)", width = "100%")
      ),
      shiny::textAreaInput("intro", "Introduction (optional)", width = "100%", rows = 3,
                           placeholder = "A sentence or two explaining what this page shows."),
      shiny::selectizeInput("items", "What to include \u2014 click items in the order you want them",
                            choices = choices, multiple = TRUE, width = "100%",
                            options = list(placeholder = "Click to choose plots, maps and tables")),
      shiny::uiOutput("headings"),
      shiny::checkboxInput("interactive", "Make ggplot charts interactive (zoom and hover)", TRUE,
                           width = "100%"),
      shiny::textInput("file", "Save as", width = "100%", placeholder = "Leave blank to name it after the title"),
      shiny::helpText(paste("Saved in:", getwd()))
    )
  )

  server <- function(input, output, session) {
    heading_id <- function(name) paste0("h_", make.names(name))

    output$headings <- shiny::renderUI({
      sel <- input$items
      if (length(sel) == 0) return(NULL)
      shiny::tagList(
        shiny::tags$label("Section headings"),
        lapply(sel, function(n) {
          id <- heading_id(n)
          current <- shiny::isolate(input[[id]])
          shiny::textInput(id, label = NULL, width = "100%",
                           value = if (is.null(current)) pretty_heading(n) else current)
        })
      )
    })

    shiny::observeEvent(input$done, {
      sel <- input$items
      if (length(sel) == 0) {
        shiny::showNotification("Choose at least one item to include.", type = "warning")
        return()
      }
      items <- lapply(sel, function(n) get(n, envir = env))
      heads <- vapply(sel, function(n) {
        h <- input[[heading_id(n)]]
        if (is.null(h)) "" else h
      }, character(1))
      names(items) <- heads

      path <- tryCatch(
        do.call(share_results, c(items, list(
          title = input$title, subtitle = input$subtitle, author = input$author,
          intro = input$intro, file = input$file, interactive = input$interactive,
          open = TRUE
        ))),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
          NULL
        }
      )
      if (!is.null(path)) shiny::stopApp(invisible(path))
    })

    shiny::observeEvent(input$cancel, shiny::stopApp(invisible(NULL)))
  }

  list(ui = ui, server = server)
}
