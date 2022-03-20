# Here's a good place to put your top-level package documentation

.onLoad <- function (lib, pkgname="shortcutr") {
    ## Put stuff here you want to run when your package is loaded
  if(is.null(getOption("shortcutr"))) {
    
    options <- list(
      sc_base_url="https://api.app.shortcut.com/api/v3",
      sc_token=NA_character_
    )
    attr(options, "class") <- "shortcut_api"
    options(shortcutr = options)
  }

}
