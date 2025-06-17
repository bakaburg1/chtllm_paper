# Evaluation of Large Language Models on Clinical Travel Health Questions

This project systematically evaluates the performance of various Large Language Models (LLMs) on a benchmark dataset of multiple-choice questions related to clinical travel health.

The primary objective is to assess and compare the accuracy and reliability of different LLMs when faced with specialized questions from the clinical travel medicine domain. This involves testing models under different prompting strategies (modalities) to understand how instructions influence their performance.

## Methodology

The analytical pipeline is orchestrated by the `_targets.R` script that includes the following key steps:

### Data Loading

* **Questions:** Loads the benchmark questions from `inst/benchmark_travel_medicine.csv`. This dataset contains multiple-choice questions, each with an identifier (`item`), source information, question text (`item_text`), four options (`option_A` to `option_D`), and the correct option (`option_correct`).
* **Models:** Loads LLM configurations from `inst/models.csv`. This file defines the models to be tested, including their `model_id`, `provider` (e.g., "openai", "anthropic", "ollama"), `model_name`, specific parameters like `base_url` (for local models), `temperature`, `max_tokens`, and a `model_type` ("reasoning" or "classic") indicating their general capabilities. An `active` flag allows selectively including models in the run.

### Experiment Setup

**Modalities:** Defines three distinct prompting strategies:

* `cold`: The LLM is instructed to output only the letter (A, B, C, or D) corresponding to the correct option.
* `free`: The LLM is allowed to explain its reasoning but must conclude its response with the chosen option letter.
* `reasoning`: The LLM is prompted to engage in step-by-step thinking, question its assumptions, and then provide the final answer letter. (Note: This modality is automatically skipped for models designated as `reasoning` type in `models.csv` to avoid redundancy).

**Combinations:** Generates a comprehensive list of all experimental units by creating combinations of:

* Each question item.
* Each active model ID.
* Each modality.
* A specified number of replications (`n_replications` defined in `_targets.R`) for statistical robustness.

**Filtering:** Checks the `results/processed/` directory and filters out combinations that have already been successfully run, allowing the pipeline to resume or incrementally add results. It can also be configured to reprocess combinations that previously resulted in errors (`reprocess_status` in `_targets.R`).

### LLM Querying

For each unique combination, in parallel, the pipeline:

* Formats the question text and options.
* Constructs the appropriate system prompt based on the selected modality.
* Sends the query to the specified LLM via the `ellmer` package, configured with parameters from `models.csv` and global settings (e.g., `temperature`, `max_tokens`).
* Captures the LLM's raw response and metadata (like token usage and generation time).
* Handles potential errors during the API call gracefully.

### Response Processing & Storage

* **Answer Extraction:** Parses the raw LLM response to extract the final answer choice (A, B, C, or D) either via regex or via LLM if not trivially identifiable.
* **Status Determination:** Compares the extracted answer to the ground truth (`option_correct`) and assigns a status: `C` (Correct), `F` (False), `N` (Not Found - if no valid option was extracted), or `E` (Error - if an API or processing error occurred).
* **Incremental Saving:** Saves the results for *each individual combination* as a separate CSV file in the `results/processed/` directory. The filename encodes the status, question item, model ID, modality, and replication number (e.g., `C.1_gpt-4_cold_1.csv`). This ensures results are saved immediately and prevents data loss if the pipeline is interrupted. Files for combinations currently being processed are temporarily stored in `results/processing/`.

### Result Compilation

* After all combinations are processed, the pipeline gathers all individual result files from `results/processed/`.
* It compiles them into a single comprehensive dataset.
* This final dataset is saved as `results/all_results.csv`.
* The final results dataset can be loaded into R using `targets::tar_read(results)`.

### Statistical Modelling & Evaluation

After compiling the raw results, the pipeline advances to a Bayesian multilevel modelling stage implemented with the `brms` package.

* **Correctness model:** A binomial logistic model that estimates the probability of answering correctly as a function of `model_id`, `modality`, and their interaction, with varying (random-effect) intercepts for each question item.
* **Parsing-quality model:** A multinomial ordinal model that captures the probabilities of responses being *clean*, *rescued*, or *failed*.
* **Consistency model:** A multinomial model that measures how consistent a model's answers are across replications using categorical probabilities for options *B*, *C*, and *D*.

All models use mildly regularising Student-t priors and an LKJ prior for correlation structures. Sampling is delegated to **CmdStanR** (configured via `mcmc_config`), and the number of cores/threads is automatically chosen based on the host machine.

### Posterior Analysis

Posterior draws from each model are extracted with `extract_posterior_draws()` and transformed into human-readable summaries with `tidybayes`. The pipeline computes:

* Marginalised summaries by **model**, **modality**, and their **interaction**.
* Three complementary consistency scores—scaled KL divergence, Simpson index, and modal probability—via `compute_consistency_kl()`, `compute_consistency_simpson()`, and `compute_consistency_modal()`.

### Visualisation & Reporting

Several helper targets turn the posterior summaries into publication-ready artefacts:

* `plot_summaries()` – faceted forest plots for each metric (correctness, parsing, and the three consistency variants).
* `plot_pareto_frontier()` – a two-objective plot that highlights trade-offs between correctness and consistency at the model level.
* `create_summary_table()` + `save_gt_table()` – nicely formatted `gt` tables saved under `outputs/tables/` (e.g. `correctness.html`, `parsing.html`, `consistency.html`).

### Correlation Analyses

Finally, `compute_model_correlation()` explores relationships between metrics, for example the link between correctness and parsing quality, or correctness and KL-based consistency.

All of the above steps are fully reproducible and automatically re-executed by **targets** whenever the underlying inputs change.

## Setup and Execution

### Installation

1. **Clone the Repository**:

   ```bash
   git clone https://github.com/bakaburg1/cthllm.git
   cd cthllm
   ```

2. **Install Dependencies with `renv`**:

   This project uses `renv` to manage R package dependencies, ensuring reproducibility.
   * Open R within the cloned project directory. `renv` should activate automatically, installing itself if necessary.
   * Run the following command to install all required packages:

   ```R
   renv::restore()
   ```

### Environment Configuration

1. **Set Up API Keys**:
   * Create a `.env` file in the project's root directory. You can copy the template from `inst/templates/.env` if it exists, or create a new file.
   * Add your API keys for the LLM providers you intend to use (e.g., OpenAI, Anthropic) to this `.env` file.
   * In this file you can also set the `PARSER_MODEL_ID` and `TEST_MODEL_ID` to the IDs of the models you want to use to parse the LLM response and test the pipeline, respectively.

2. **Configure Local Models** (Optional):
   * If you plan to test local models (e.g., using Ollama or LMStudio), ensure the corresponding service is installed, running, and accessible. Update the `models.csv` file with the correct `base_url` if necessary.
   * For larger local models, activate and test them one at a time by toggling the `active` flag in `inst/models.csv`, since loading multiple large models simultaneously may exceed available system memory.

### Running the Pipeline

1. **Execute the `targets` Pipeline**:
    * Once the setup is complete, run the entire analysis pipeline from the R console:

      ```R
      targets::tar_make()
      ```

    * This command will execute all the steps defined in the `_targets.R` file, including data loading, querying LLMs, processing responses, and compiling results.

2. **Debugging**:
    * If you encounter errors, especially during the LLM querying phase which runs in parallel by default, it can be helpful to run the pipeline sequentially in the main R process for easier debugging. Use:

      ```R
      targets::tar_make(callr_function = NULL, use_crew = FALSE)
      ```

    * This allows you to check the status of each LLM query and use standard R debugging tools (like `browser()`) directly within the target steps.

## Further Analysis

* The `results/all_results.csv` file contains the raw data (questions, models, modalities, responses, status, metadata) for further analysis, such as calculating accuracy, confusion matrices, or performing statistical comparisons between models and modalities.
