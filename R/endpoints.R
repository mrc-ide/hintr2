root_endpoint <- function() {
  scalar("Welcome")
}

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

validate_baseline_combined <- function(input) {
  input <- jsonlite::fromJSON(input)
  as_file_object <- function(x) {
    if (!is.null(x)) {
      hintr:::file_object(x)
    } else {
      NULL
    }
  }
  tryCatch({
    hintr:::do_validate_baseline(as_file_object(input$pjnz),
                                 as_file_object(input$shape),
                                 as_file_object(input$population))
  },
  error = function(e) {
    pkgapi::pkgapi_stop(e$message, "INVALID_BASELINE")
  })
}

validate_survey_programme <- function(input) {
  input <- jsonlite::fromJSON(input)
  validate_func <- switch(input$type,
                          programme = hintr:::do_validate_programme,
                          anc = hintr:::do_validate_anc,
                          survey = hintr:::do_validate_survey)
  tryCatch({
    shape <- hintr:::file_object(input$shape)
    hintr:::assert_file_exists(input$file$path)
    hintr:::assert_file_exists(shape$path)
    hintr:::input_response(validate_func(input$file, shape),
                           input$type, input$file)
  },
  error = function(e) {
    pkgapi::pkgapi_stop(e$message, "INVALID_FILE")
  })
}

model_options <- function(input) {
  input <- jsonlite::fromJSON(input)
  tryCatch({
    hintr:::assert_file_exists(input$shape$path)
    hintr:::assert_file_exists(input$survey$path)
    hintr:::json_verbatim(
      hintr:::do_endpoint_model_options(input$shape, input$survey,
                                        input$programme, input$anc))
  }, error = function(e) {
    pkgapi::pkgapi_stop(e$message, "INVALID_OPTIONS")
  })
}

model_options_validate <- function(input) {
  input <- jsonlite::fromJSON(input)
  tryCatch({
    ## Update some labels to match what naomi requires
    ## TODO: Some of this is shared between model running and here so we
    ## should use use common code when we merge this back into hintr.
    ## This endpoint currently isn't called see mrc-592.
    data <- input$data
    data$art_number <- data$programme
    data$programme <- NULL
    data$anc_testing <- data$anc
    data$anc <- NULL
    data <- naomi:::format_data_input(data)
    list(valid = scalar(naomi:::validate_model_options(data, input$options)))
  }, error = function(e) {
    pkgapi::pkgapi_stop(e$message, "INVALID_OPTIONS")
  })
}

submit_model <- function(queue) {
  function(input) {
    input <- jsonlite::fromJSON(input)
    if (!hintr:::is_current_version(input$version)) {
      pkgapi::pkgapi_stop(tr_("MODEL_SUBMIT_OLD"),
                          "VERSION_OUT_OF_DATE")
    }
    tryCatch(
      list(id = scalar(queue$submit(input$data, input$options))),
      error = function(e) {
        pkgapi::pkgapi_stop(e$message, "FAILED_TO_QUEUE")
      }
    )
  }
}

model_status <- function(queue) {
  check_orphan <- hintr:::throttle(queue$queue$worker_detect_exited, 10)
  function(id) {
    hintr:::no_error(check_orphan())
    tryCatch({
      out <- queue$status(id)
      hintr:::prepare_status_response(out, id)
    },
    error = function(e) {
      pkgapi::pkgapi_stop(e$message, "FAILED_TO_RETRIEVE_STATUS")
    })
  }
}

model_result <- function(queue) {
  function(id) {
    task_status <- queue$queue$task_status(id)
    if (task_status == "COMPLETE") {
      hintr:::process_result(queue$result(id))
    } else if (task_status == "ERROR") {
      result <- queue$result(id)
      trace <- c(sprintf("# %s", id), result$trace)
      hintr_error(result$message, "MODEL_RUN_FAILED", trace = trace)
    } else if (task_status == "ORPHAN") {
      pkgapi::pkgapi_stop(tr_("MODEL_RESULT_CRASH"), "MODEL_RUN_FAILED")
    } else if (task_status == "INTERRUPTED") {
      pkgapi::pkgapi_stop(tr_("MODEL_RUN_CANCELLED"), "MODEL_RUN_FAILED")
    } else { # ~= MISSING, PENDING, RUNNING
      pkgapi::pkgapi_stop(tr_("MODEL_RESULT_MISSING"),
                          "FAILED_TO_RETRIEVE_RESULT")
    }
  }
}

model_cancel <- function(queue) {
  function(id) {
    tryCatch({
      queue$cancel(id)
      json_null()
    },
    error = function(e) {
      pkgapi::pkgapi_stop(e$message, "FAILED_TO_CANCEL")
    })
  }
}

plotting_metadata <- function(iso3) {
  tryCatch(
    hintr:::do_plotting_metadata(iso3),
    error = function(e) {
      pkgapi::pkgapi_stop(e$message, "FAILED_TO_GET_METADATA")
    }
  )
}

download_spectrum <- function(queue) {
  download(queue, "spectrum", "naomi_spectrum_digest")
}

download_summary <- function(queue) {
  download(queue, "summary", "naomi_coarse_age_groups")
}

download <- function(queue, type, filename) {
  function(id) {
    tryCatch({
      res <- queue$result(id)
      if (hintr:::is_error(res)) {
        pkgapi::pkgapi_stop(res$message, "MODEL_RUN_FAILED")
      }
      path <- switch(type,
                     "spectrum" = res$spectrum_path,
                     "summary" = res$summary_path)
      bytes <- readBin(path, "raw", n = file.size(path))
      bytes <- pkgapi::pkgapi_add_headers(bytes, list(
        "Content-Disposition" =
        sprintf('attachment; filename="%s_%s_%s.zip"',
                paste(res$metadata$areas, collapse = ", "),
                hintr:::iso_time_str(), filename)))
      bytes
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

download_debug <- function(queue) {
  function(id) {
    tryCatch({
      data <- queue$queue$task_data(id)
      files <- unique(unlist(lapply(data$objects$data, function(x) {
        if (!is.null(x)) {
          x$path
        }
      }), FALSE, FALSE))
      tmp <- tempfile()
      path <- file.path(tmp, id)
      dir.create(path, FALSE, TRUE)

      data$sessionInfo <- utils::sessionInfo()
      data$objects$data <- lapply(data$objects$data, function(x) {
        if (!is.null(x)) {
          list(path = basename(x$path), hash = x$hash, filename = x$filename)
        }
      })

      path_files <- file.path(path, "files")
      dir.create(path_files)
      hintr:::file_copy(files, file.path(path_files, basename(files)))
      saveRDS(data, file.path(path, "data.rds"))

      on.exit(unlink(tmp, recursive = TRUE))

      dest <- paste0(id, ".zip")
      withr::with_dir(tmp, zip::zipr(dest, id))

      path <- file.path(tmp, dest)
      bytes <- readBin(path, "raw", n = file.size(path))
      bytes <- pkgapi::pkgapi_add_headers(bytes, list(
        "Content-Disposition" =
          sprintf('attachment; filename="%s_%s_naomi_debug.zip"',
                  id, hintr:::iso_time_str())))
      bytes
    },
    error = function(e) {
      if (is_pkgapi_error(e)) {
        stop(e)
      } else {
        pkgapi::pkgapi_stop(e$message, "INVALID_TASK")
      }
    })
  }
}

worker_status <- function(queue) {
  function() {
    lapply(queue$queue$worker_status(), scalar)
  }
}

hintr_stop <- function(queue) {
  force(queue)
  function() {
    message("Stopping workers")
    queue$queue$worker_stop()
    message("Quitting hintr")
    quit(save = "no")
  }
}
