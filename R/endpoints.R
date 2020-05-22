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

submit_model <- function(queue) {
  submit <- function(input) {
    input <- jsonlite::fromJSON(input)
    if (!hintr:::is_current_version(input$version)) {
      pkgapi::pkgapi_stop("MODEL_SUBMIT_OLD", "VERSION_OUT_OF_DATE")
    }
    tryCatch(
      list(id = scalar(queue$submit(input$data, input$options))),
      error = function(e) {
        pkgapi::pkgapi_stop(e$message, "FAILED_TO_QUEUE")
      }
    )
  }
}

plotting_metadata <- function(iso3) {
  tryCatch(
    hintr:::do_plotting_metadata(iso3),
    error = function(e) {
      pkgapi::pkgapi_stop("FAILED_TO_GET_METADATA", e$message)
    }
  )
}

download_spectrum <- function(queue) {
  download(queue, "spectrum")
}

download_summary <- function(queue) {
  download(queue, "summary")
}

download <- function(queue, type) {
  function(id) {
    tryCatch({
      res <- queue$result(id)
      if (hintr:::is_error(res)) {
        pkgapi::pkgapi_stop(res$message, "MODEL_RUN_FAILED")
      }
      path <- switch(type,
                     "spectrum" = res$spectrum_path,
                     "summary" = res$summary_path)
      list(
        bytes = readBin(path, "raw", n = file.size(path)),
        id = id,
        metadata = response$metadata
      )
    },
    error = function(e) {
      if (is_pkgapi_error(e)) {
        stop(e)
      } else {
        pkgapi::pkgapi_stop(e$message, "FAILED_TO_RETRIEVE_RESULT")
      }
    })
  }
}


