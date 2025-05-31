# Test file for utility functions
library(testthat)

# Tests for format_timediff
test_that("format_timediff handles various time differences correctly", {
  # Test basic seconds formatting
  td_secs <- difftime(as.POSIXct("2024-01-01 00:00:05"),
                      as.POSIXct("2024-01-01 00:00:00"),
                      units = "secs")
  expect_equal(format_timediff(td_secs), "5 secs")

  # Test minutes with precision
  td_mins <- difftime(as.POSIXct("2024-01-01 00:05:00"),
                      as.POSIXct("2024-01-01 00:00:00"),
                      units = "mins")
  expect_equal(format_timediff(td_mins, precision = 2), "5 mins")

  # Test negative time difference
  td_neg <- difftime(as.POSIXct("2024-01-01 00:00:00"),
                     as.POSIXct("2024-01-01 00:05:00"),
                     units = "mins")
  expect_equal(format_timediff(td_neg), "-5 mins")

  # Test invalid input
  expect_error(format_timediff("not a difftime"),
              "Input must be a difftime object.")
})

# Tests for get_model_config
test_that("get_model_config handles model configurations correctly", {
  # Create a temporary models.csv for testing
  temp_dir <- tempdir()
  temp_models <- file.path(temp_dir, "models.csv")
  test_models <- data.frame(
    model_id = c("test_model", "empty_model"),
    provider = c("test_provider", "empty_provider"),
    base_url = c("http://test.url", ""),
    other_config = c("test_config", NA),
    stringsAsFactors = FALSE
  )
  write.csv(test_models, temp_models, row.names = FALSE)

  # Mock system.file to return our temporary file
  mockery::stub(get_model_config, "system.file", temp_models)

  # Test valid model
  config <- get_model_config("test_model")
  expect_type(config, "list")
  expect_equal(config$provider, "test_provider")
  expect_equal(config$base_url, "http://test.url")
  expect_equal(config$other_config, "test_config")

  # Test non-existent model
  expect_error(get_model_config("non_existent"),
              "Model ID `non_existent` not found in models.csv")

  # Clean up
  unlink(temp_models)
})

# Tests for get_api_key
test_that("get_api_key handles API keys correctly", {
  # Test ollama (no key needed)
  expect_null(get_api_key("ollama"))

  # Test provider with key
  withr::with_envvar(
    new = c("TEST_PROVIDER_API_KEY" = "test_key"),
    {
      expect_equal(get_api_key("test_provider"), "test_key")
    }
  )

  # Test missing key
  expect_error(
    get_api_key("missing_provider"),
    "Environment variable `MISSING_PROVIDER_API_KEY` not found in .env file"
  )
})

# Tests for get_env_var
test_that("get_env_var handles environment variables correctly", {
  # Create temporary .env file
  temp_dir <- tempdir()
  temp_env <- file.path(temp_dir, ".env")
  writeLines(c(
    "TEST_VAR=test_value",
    "EMPTY_VAR="
  ), temp_env)

  # Mock file.exists and readRenviron
  mockery::stub(get_env_var, "file.exists",
               function(x) if(x == "DESCRIPTION") TRUE else TRUE)
  mockery::stub(get_env_var, "readRenviron", function(x) {
    Sys.setenv(TEST_VAR = "test_value")
  })

  # Test existing variable
  expect_equal(get_env_var("TEST_VAR"), "test_value")

  # Test missing variable
  expect_error(get_env_var("MISSING_VAR"),
              "Environment variable `MISSING_VAR` not found in .env file")

  # Clean up
  unlink(temp_env)
}) 