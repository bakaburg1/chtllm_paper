# Test system prompt generation
test_that("generate_system_prompt returns correct prompts", {
  # Test cold mode
  cold_prompt <- generate_system_prompt("cold")
  expect_type(cold_prompt, "character")
  expect_match(cold_prompt, "ONLY respond with the letter")
  expect_match(cold_prompt, "_X_")

  # Test free mode
  free_prompt <- generate_system_prompt("free")
  expect_type(free_prompt, "character")
  expect_match(free_prompt, "freely discuss")
  expect_match(free_prompt, "_X_")

  # Test reasoning mode
  reasoning_prompt <- generate_system_prompt("reasoning")
  expect_type(reasoning_prompt, "character")
  expect_match(reasoning_prompt, "think carefully")
  expect_match(reasoning_prompt, "step by step")
  expect_match(reasoning_prompt, "_X_")

  # Test invalid mode
  expect_error(generate_system_prompt("invalid"))
})

# Test answer extraction
test_that("extract_answer extracts correctly", {
  testthat::skip_on_cran()

  # Test extraction
  response <- "I think the answer is _B_"
  expect_equal(extract_answer(response), "B")

  # Test with multiple matches (should take last one)
  response <- paste(
    "Let's think about this _A_ might be correct,",
    "but _B_ seems better. However, after careful",
    "consideration, _C_ is the best answer."
  )
  expect_equal(extract_answer(response), "C")

  # Test with wrong formatting
  response <- paste(
    "Let's think about this A might be correct,",
    "but B seems better. However, after careful",
    "consideration, C is the best answer. So the answer is C_"
  )
  expect_match(extract_answer(response), "^(NONE|[ABCD])\\*?$")

  # Test invalid response
  expect_match(extract_answer("No answer here"), "^(NONE|[ABCD])\\*?$")
  expect_match(extract_answer("_E_"), "^(NONE|[ABCD])\\*?$")
})

# Test LLM querying
test_that("query_llm returns expected structure", {
  testthat::skip_on_cran()

  # Load test question
  questions <- load_questions()
  message <- get_formatted_question(1, questions)

  # Get model config and set system prompt
  parser_model_id <- get_env_var("TEST_MODEL_ID")
  model_config <- get_model_config(parser_model_id)
  model_config$system_prompt <- generate_system_prompt("cold")

  # Run query
  result <- query_llm(message, model_config)

  # Test structure
  expect_type(result, "list")
  expect_named(result, c("answer", "metadata", "config", "inputs"))

  # Test answer
  expect_type(result$answer, "character")
  expect_match(extract_answer(result$answer), "^(NONE|[ABCD])\\*?$")

  # Test metadata
  expect_type(result$metadata, "list")
  expect_named(
    result$metadata,
    c(
      "timestamp",
      "generation_time",
      "input_tokens",
      "output_tokens",
      "total_tokens"
    )
  )
  expect_type(result$metadata$timestamp, "character")

  # Test config
  expect_type(result$config, "list")
  expect_true("model_id" %in% names(result$config))
  expect_true("provider" %in% names(result$config))

  # Test inputs
  expect_type(result$inputs, "list")
  expect_named(result$inputs, "message")
  expect_equal(result$inputs$message, message)
})

test_that("query_llm works with different modalities", {
  testthat::skip_on_cran()

  # Load test question
  questions <- load_questions()
  message <- get_formatted_question(1, questions)

  # Get model config
  parser_model_id <- get_env_var("TEST_MODEL_ID")
  model_config <- get_model_config(parser_model_id)

  # Test each modality
  modalities <- c("free", "reasoning") # "cold" is already tested
  for (modality in modalities) {
    # Set system prompt for the modality
    model_config$system_prompt <- generate_system_prompt(modality)
    
    result <- query_llm(message, model_config)

    # Basic structure checks
    expect_type(result, "list")
    expect_named(result, c("answer", "metadata", "config", "inputs"))

    # Check answer format
    expect_match(extract_answer(result$answer), "^(NONE|[ABCD])\\*?$")
  }
})

test_that("query_llm handles model_id input", {
  testthat::skip_on_cran()

  # Load test question
  questions <- load_questions()
  message <- get_formatted_question(1, questions)

  # Use model_id string instead of config
  result <- query_llm(
    message,
    "qwen2.5-coder-1.5b-instruct-mlx@8bit"
  )

  # Basic structure checks
  expect_type(result, "list")
  expect_named(result, c("answer", "metadata", "config", "inputs"))

  # Check model_id is correctly set
  expect_equal(result$config$model_id, "qwen2.5-coder-1.5b-instruct-mlx@8bit")
})

test_that("query_llm handles simple messages", {
  testthat::skip_on_cran()

  # Simple message without options
  message <- "What is the capital of France?"

  # Get model config
  parser_model_id <- get_env_var("TEST_MODEL_ID")
  model_config <- get_model_config(parser_model_id)
  model_config$system_prompt <- generate_system_prompt("cold")

  # Should run without error
  result <- query_llm(message, model_config)

  # Basic structure checks
  expect_type(result, "list")
  expect_named(result, c("answer", "metadata", "config", "inputs"))

  # Check message is preserved
  expect_equal(result$inputs$message, message)
})
