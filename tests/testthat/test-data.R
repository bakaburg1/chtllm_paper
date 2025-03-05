# Test file for data loading and validation functions
library(testthat)
library(dplyr)

# Create test fixtures
create_valid_questions <- function() {
  tibble::tibble(
    item = c("Q1", "Q2"),
    source = c("Source1", "Source2"),
    source_link = c("http://example1.com", "http://example2.com"),
    item_text = c("Question 1", "Question 2"),
    option_A = c("A1", "A2"),
    option_B = c("B1", "B2"),
    option_C = c("C1", "C2"),
    option_D = c("D1", "D2"),
    option_correct = c("A", "B")
  )
}

create_valid_models <- function() {
  tibble::tibble(
    model_id = c("model1", "model2"),
    provider = c("openai", "anthropic"),
    model_name = c("gpt4", "claude"),
    model_type = c("reasoning", "classic"),
    base_url = c("http://api1.com", "http://api2.com")
  )
}

# Tests for format_question
test_that("format_question formats correctly", {
    # Test with options
    question <- "What is the capital of France?"
    options <- list(
        A = "London",
        B = "Paris",
        C = "Berlin",
        D = "Madrid"
    )

    formatted <- format_question(question, options)
    expected <- "Question: What is the capital of France?\n\nOptions:\nA: London\nB: Paris\nC: Berlin\nD: Madrid"

    # Check exact match
    expect_equal(formatted, expected)

    # Test without options
    expect_equal(format_question(question, NULL), question)
})

# Tests for get_formatted_question
test_that("get_formatted_question works with item ID", {
    questions <- create_valid_questions()

    # Test with first item
    formatted <- get_formatted_question(questions, "Q1")
    print("First item test:")
    print("Formatted output:")
    str(formatted)
    expected <- "Question: Question 1\n\nOptions:\nA: A1\nB: B1\nC: C1\nD: D1"
    print("Expected output:")
    str(expected)
    expect_equal(formatted, expected)

    # Test with second item
    formatted <- get_formatted_question(questions, "Q2")
    print("\nSecond item test:")
    print("Formatted output:")
    str(formatted)
    expected <- "Question: Question 2\n\nOptions:\nA: A2\nB: B2\nC: C2\nD: D2"
    print("Expected output:")
    str(expected)
    expect_equal(formatted, expected)
})

test_that("get_formatted_question works with row number", {
    questions <- create_valid_questions()

    # Test with first row
    formatted <- get_formatted_question(questions, 1)
    expected <- "Question: Question 1\n\nOptions:\nA: A1\nB: B1\nC: C1\nD: D1"
    expect_equal(formatted, expected)

    # Test with second row
    formatted <- get_formatted_question(questions, 2)
    expected <- "Question: Question 2\n\nOptions:\nA: A2\nB: B2\nC: C2\nD: D2"
    expect_equal(formatted, expected)
})

test_that("get_formatted_question handles errors", {
    questions <- create_valid_questions()

    # Test with non-existent item ID
    expect_error(
        get_formatted_question(questions, "Q3"),
        "Question with item ID Q3 not found"
    )

    # Test with out of bounds row number
    expect_error(
        get_formatted_question(questions, 3),
        "Row number 3 exceeds number of questions \\(2\\)"
    )
})

# Tests for validate_questions
test_that("validate_questions handles valid data correctly", {
  questions <- create_valid_questions()
  expect_identical(validate_questions(questions), questions)
})

test_that("validate_questions detects missing columns", {
  questions <- create_valid_questions() |>
    select(-item_text, -source)

  expect_error(
    validate_questions(questions),
    "Missing required columns in questions data"
  )
})

test_that("validate_questions detects invalid correct answers", {
  questions <- create_valid_questions()
  questions$option_correct[1] <- "E"

  expect_error(
    validate_questions(questions),
    "Invalid correct answers found"
  )
})

# Tests for validate_models
test_that("validate_models handles valid data correctly", {
  models <- create_valid_models()
  expect_identical(validate_models(models), models)
})

test_that("validate_models detects missing columns", {
  models <- create_valid_models() |>
    select(-model_name)

  expect_error(
    validate_models(models),
    "Missing required columns in model configurations"
  )
})

test_that("validate_models detects invalid providers", {
  models <- create_valid_models()
  models$provider[1] <- "invalid_provider"

  expect_error(
    validate_models(models),
    "Invalid providers found"
  )
})

test_that("validate_models detects invalid model types", {
  models <- create_valid_models()
  models$model_type[1] <- "invalid_type"

  expect_error(
    validate_models(models),
    "Invalid model types found"
  )
})

# Tests for load_questions
test_that("load_questions loads valid CSV correctly", {
  # Create temporary CSV
  temp_dir <- tempdir()
  temp_csv <- file.path(temp_dir, "test_questions.csv")
  questions <- create_valid_questions()
  readr::write_csv(questions, temp_csv)

  # Mock here::here to return our temp file
  mockery::stub(load_questions, "here::here", temp_csv)

  # Test loading
  loaded <- load_questions()
  expect_s3_class(loaded, "tbl_df")
  expect_identical(
    loaded |> select(names(questions)),
    questions
  )

  # Clean up
  unlink(temp_csv)
})

test_that("load_questions handles custom paths", {
  # Create temporary CSV in a different location
  temp_dir <- tempdir()
  temp_csv <- file.path(temp_dir, "custom_questions.csv")
  questions <- create_valid_questions()
  readr::write_csv(questions, temp_csv)

  # Test loading with custom path
  loaded <- load_questions(temp_csv)
  expect_s3_class(loaded, "tbl_df")
  expect_identical(
    loaded |> select(names(questions)),
    questions
  )

  # Clean up
  unlink(temp_csv)
})

# Tests for load_models
test_that("load_models loads valid CSV correctly", {
  # Create temporary CSV
  temp_dir <- tempdir()
  temp_csv <- file.path(temp_dir, "test_models.csv")
  models <- create_valid_models()
  readr::write_csv(models, temp_csv)

  # Mock here::here to return our temp file
  mockery::stub(load_models, "here::here", temp_csv)

  # Test loading
  loaded <- load_models()
  expect_s3_class(loaded, "tbl_df")
  expect_identical(
    loaded |> select(names(models)),
    models
  )

  # Clean up
  unlink(temp_csv)
})

test_that("load_models handles custom paths", {
  # Create temporary CSV in a different location
  temp_dir <- tempdir()
  temp_csv <- file.path(temp_dir, "custom_models.csv")
  models <- create_valid_models()
  readr::write_csv(models, temp_csv)

  # Test loading with custom path
  loaded <- load_models(temp_csv)
  expect_s3_class(loaded, "tbl_df")
  expect_identical(
    loaded |> select(names(models)),
    models
  )

  # Clean up
  unlink(temp_csv)
})
