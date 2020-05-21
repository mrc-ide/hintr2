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

skip_if_sensitive_data_missing <- function() {
  sensitive_path <- Sys.getenv("NAOMI_SENSITIVE_DATA_PATH",
                               "testdata/sensitive")
  if (!file.exists(file.path("testdata", "sensitive"))) {
    skip("Sensitive data missing, check README for details.")
  }
}
