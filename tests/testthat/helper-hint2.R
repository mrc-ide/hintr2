Sys.setenv(PKGAPI_VALIDATE = "true") # nolint

validate_baseline_input <- function(file_path, type) {
  sprintf(
    '{"type": "%s",
      "file": {
        "path": "%s",
        "hash": "12345",
        "filename": "original"
      }
    }', type, file_path)
}

validate_baseline_all_input <- function(pjnz, shape, population) {
  quote <- function(x) {
    if (is.null(x)) {
      "null"
    } else {
      paste0('"', x, '"')
    }
  }
  sprintf(
    '{"pjnz": %s,
      "shape": %s,
      "population": %s
    }',
    quote(pjnz), quote(shape), quote(population)
  )
}

validate_programme_survey_input <- function(file_path, type, shape) {
  sprintf(
    '{"type": "%s",
      "file": {
        "path": "%s",
        "hash": "12345",
        "filename": "original"
      },
      "shape": "%s"
    }', type, file_path, shape)
}


skip_if_sensitive_data_missing <- function() {
  sensitive_path <- Sys.getenv("NAOMI_SENSITIVE_DATA_PATH",
                               "testdata/sensitive")
  if (!file.exists(file.path("testdata", "sensitive"))) {
    skip("Sensitive data missing, check README for details.")
  }
}
