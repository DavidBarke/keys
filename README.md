
<!-- README.md is generated from README.Rmd. Please edit that file -->

keys
====

<!-- badges: start -->
<!-- badges: end -->

The goal of keys is to add hotkeys to shiny applications using
[`mousetrap.js`](https://github.com/ccampbell/mousetrap).

Installation
------------

You can the development version from GitHub with:

    # install.packages("devtools")
    devtools::install_github("r4fun/keys")

Usage
-----

Simply provide a `keysInput` to the UI:

    library(shiny)
    library(keys)

    hotkeys <- c(
      "1", 
      "command+shift+k", 
      "up up down down left right left right b a enter"
    )

    ui <- fluidPage(
      keysInput("keys", hotkeys)
    )

    server <- function(input, output, session) {
      observeEvent(input$keys, {
        print(input$keys)
      })
    }

    shinyApp(ui, server)

For more information about what types of hotkeys you can use, please
take a look at the mousetrap github
[repository](https://github.com/ccampbell/mousetrap).

Acknowledgements
----------------

All credit goes to [Craig Campbell](https://github.com/ccampbell) who is
the author of [`mousetrap.js`](https://github.com/ccampbell/mousetrap).
