minify <- function(x) {
  gsub("[[:space:]]+", " ", x)
}

#' @importFrom htmltools tags
#' @importFrom jsonlite toJSON
keys_js <- function(id, keys, global = FALSE, nullOnKeyRelease = FALSE) {
  x <- if (!nullOnKeyRelease) {
    glue::glue(
      "$(document).on('shiny:sessioninitialized', function() {",
      "  Mousetrap.{{bindFunc}}({{keys}}, function(e, combo) {",
      "    Shiny.setInputValue('{{id}}', combo, {priority: 'event'});",
      "  });",
      "});",
      bindFunc = if (global) "bindGlobal" else "bind",
      keys = jsonlite::toJSON(keys),
      id = id,
      .open = "{{",
      .close = "}}"
    )
  } else {
    glue::glue(
      "$(document).on('shiny:sessioninitialized', function() {",
      "  Mousetrap.{{bindFunc}}({{keys}}, function(e, combo) {",
      "    Shiny.setInputValue('{{id}}', combo, {priority: 'event'});",
      "  }, 'keydown');",
      "  Mousetrap.{{bindFunc}}({{keys}}, function(e, combo) {",
      "    Shiny.setInputValue('{{id}}', null, {priority: 'event'});",
      "  }, 'keyup');",
      "});",
      bindFunc = if (global) "bindGlobal" else "bind",
      keys = jsonlite::toJSON(keys),
      id = id,
      .open = "{{",
      .close = "}}"
    )
  }

  tags$head(
    tags$script(
      minify(x)
    )
  )
}

alert_null_session <- function() {
  stop(
    "Could not find a shiny session object.",
    "\n* This usually happens when a {keys} function is called from a context that wasn't set up by a shiny session.",
    call. = FALSE
  )
}
