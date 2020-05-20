validate_baseline <- function(input) {
  input <- jsonlite::fromJSON(input)
  validate_func <- switch(input$type,
                          pjnz = hintr::do_validate_pjnz,
                          shape = hintr::do_validate_shape,
                          population = hintr::do_validate_population)
  assert_file_exists(input$file$path)
  validate_func(input$file)
}
