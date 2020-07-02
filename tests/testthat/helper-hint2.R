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

model_options_input <- function(shape, survey, programme, anc) {
  as_file_object <- function(x) {
    if (is.null(x)) {
      "null"
    } else {
      sprintf(
      '{
        "path": "%s",
        "hash": "12345",
        "filename": "original"
      }', x)
    }
  }
  sprintf(
    '{"shape": %s,
      "survey": %s,
      "programme": %s,
      "anc": %s
    }',
    as_file_object(shape), as_file_object(survey),
    as_file_object(programme), as_file_object(anc)
  )
}

skip_if_sensitive_data_missing <- function() {
  sensitive_path <- Sys.getenv("NAOMI_SENSITIVE_DATA_PATH",
                               "testdata/sensitive")
  if (!file.exists(file.path("testdata", "sensitive"))) {
    skip("Sensitive data missing, check README for details.")
  }
}
