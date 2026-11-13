#' Make an HTML file self-contained
#'
#' Inlines every local script, stylesheet, image and font that the page
#' refers to (e.g. files in a "lib" folder written by htmltools), so the
#' HTML file works on its own. Links to web addresses are left alone.
#'
#' @param file Path to the HTML file; it is rewritten in place.
#' @return `file`, invisibly.
#' @export
make_self_contained <- function(file) {
  base <- dirname(normalizePath(file))
  html <- read_text(file)

  # <script src="local.js"></script>  ->  <script>...</script>
  html <- replace_matches(html, '<script\\b[^>]*\\bsrc="([^"]+)"[^>]*>\\s*</script>', function(tag, src) {
    path <- local_path(src, base)
    if (is.null(path)) return(tag)
    js <- read_text(path)
    js <- gsub("</(script)", "<\\\\/\\1", js, ignore.case = TRUE, perl = TRUE)
    paste0("<script>", js, "</script>")
  })

  # <link href="local.css" rel="stylesheet">  ->  <style>...</style>
  # other local <link>s (e.g. htmlwidget attachments)  ->  data: URI
  html <- replace_matches(html, '<link\\b[^>]*\\bhref="([^"]+)"[^>]*>', function(tag, href) {
    path <- local_path(href, base)
    if (is.null(path)) return(tag)
    if (grepl('rel="stylesheet"', tag, fixed = TRUE)) {
      css <- inline_css_urls(read_text(path), dirname(path))
      return(paste0("<style>", css, "</style>"))
    }
    sub(paste0('href="', href, '"'), paste0('href="', data_uri(path), '"'), tag, fixed = TRUE)
  })

  # <img src="local.png">
  html <- replace_matches(html, '<img\\b[^>]*\\bsrc="([^"]+)"[^>]*>', function(tag, src) {
    path <- local_path(src, base)
    if (is.null(path)) return(tag)
    sub(paste0('src="', src, '"'), paste0('src="', data_uri(path), '"'), tag, fixed = TRUE)
  })

  con <- file(file, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(html)), con)
  invisible(file)
}

inline_css_urls <- function(css, css_dir) {
  replace_matches(css, "url\\(\\s*['\"]?([^'\")]+?)['\"]?\\s*\\)", function(tag, ref) {
    path <- local_path(ref, css_dir)
    if (is.null(path)) return(tag)
    paste0('url("', data_uri(path), '")')
  })
}

# Apply fn(whole_match, first_group) to every match of a perl regex.
replace_matches <- function(text, pattern, fn) {
  m <- gregexpr(pattern, text, perl = TRUE)[[1]]
  if (m[1] == -1) return(text)
  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  gstart <- attr(m, "capture.start")[, 1]
  glen <- attr(m, "capture.length")[, 1]
  out <- character(0)
  pos <- 1L
  for (i in seq_along(starts)) {
    whole <- substr(text, starts[i], starts[i] + lens[i] - 1L)
    group <- substr(text, gstart[i], gstart[i] + glen[i] - 1L)
    out <- c(out, substr(text, pos, starts[i] - 1L), fn(whole, group))
    pos <- starts[i] + lens[i]
  }
  out <- c(out, substr(text, pos, nchar(text)))
  paste(out, collapse = "")
}

# Resolve a reference to a local file, or NULL if it's remote / missing.
local_path <- function(ref, base) {
  if (grepl("^([a-zA-Z][a-zA-Z0-9+.-]*:|//|#)", ref)) return(NULL)
  clean <- sub("[?#].*$", "", ref)
  clean <- utils::URLdecode(clean)
  path <- file.path(base, clean)
  if (!file.exists(path) || dir.exists(path)) return(NULL)
  normalizePath(path)
}

data_uri <- function(path) {
  base64enc::dataURI(file = path, mime = mime_type(path))
}

mime_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    png = "image/png", jpg = , jpeg = "image/jpeg", gif = "image/gif",
    svg = "image/svg+xml", webp = "image/webp", ico = "image/x-icon",
    woff = "font/woff", woff2 = "font/woff2", ttf = "font/ttf", otf = "font/otf",
    eot = "application/vnd.ms-fontobject",
    css = "text/css", js = "application/javascript", json = "application/json",
    "application/octet-stream")
}

read_text <- function(path) {
  txt <- readChar(path, file.size(path), useBytes = TRUE)
  Encoding(txt) <- "UTF-8"
  txt
}
