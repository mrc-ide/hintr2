validate_baseline <- function(input) {
  input <- jsonlite::fromJSON(input)
  validate_func <- switch(input$type,
                          pjnz = hintr:::do_validate_pjnz,
                          shape = hintr:::do_validate_shape,
                          population = hintr:::do_validate_population)
  tryCatch({
    hintr:::assert_file_exists(input$file$path)
    ## This does some validation of the data part of the response
    ## Is that right to do at this ponit or does pkgapi have a way to validate
    ## subsets of the data?
    hintr:::input_response(validate_func(input$file), input$type, input$file)
  },
    error = function(e) {
      pkgapi::pkgapi_stop(e$message, "INVALID_FILE")
  })
}

root_endpoint <- function() {
  scalar("Welcome")
}
