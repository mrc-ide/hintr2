validate_baseline <- function(input) {
  input <- jsonlite::fromJSON(input)
  validate_func <- switch(input$type,
                          pjnz = hintr:::do_validate_pjnz,
                          shape = hintr:::do_validate_shape,
                          population = hintr:::do_validate_population)
  hintr:::assert_file_exists(input$file$path)
  hintr:::input_response(validate_func(input$file), input$type, input$file)
}

root_endpoint <- function() {
  scalar("Welcome")
}
