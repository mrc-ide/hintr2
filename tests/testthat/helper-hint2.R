Sys.setenv(PKGAPI_VALIDATE = "true") # nolint

validate_baseline_input <- function(file_path) {
  sprintf(
    '{"type": "pjnz",
      "file": {
        "path": "%s",
        "hash": "12345",
        "filename": "original"
      }
    }', file_path)
}
