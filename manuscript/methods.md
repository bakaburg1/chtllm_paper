<html><head>
<script src="methods_files/libs/clipboard/clipboard.min.js"></script>
<script src="methods_files/libs/quarto-html/quarto.js"></script>
<script src="methods_files/libs/quarto-html/popper.min.js"></script>
<script src="methods_files/libs/quarto-html/tippy.umd.min.js"></script>
<script src="methods_files/libs/quarto-html/anchor.min.js"></script>
<link href="methods_files/libs/quarto-html/tippy.css" rel="stylesheet">
<link href="methods_files/libs/quarto-html/quarto-syntax-highlighting-549806ee2085284f45b00abea8c6df48.css" rel="stylesheet" id="quarto-text-highlighting-styles">
<script src="methods_files/libs/bootstrap/bootstrap.min.js"></script>
<link href="methods_files/libs/bootstrap/bootstrap-icons.css" rel="stylesheet">
<link href="methods_files/libs/bootstrap/bootstrap-973236bd072d72a04ee9cd82dcc9cb29.min.css" rel="stylesheet" append-hash="true" id="quarto-bootstrap" data-mode="light"></head><body class="fullcontent"># Methods


## Study design

A comparative evaluation of large-language models (LLMs) was conducted
on a domain-specific benchmark of travel-medicine questions. Each model
was asked to solve the same set of multiple-choice travel-medicine
clinical vignettes under distinct prompting conditions. Performance was
assessed across three complementary dimensions—accuracy, adherence to
the required answer format, and internal consistency—using a fully
reproducible analysis pipeline.

The benchmark comprised 42 single-best-answer questions representative
of real-world scenarios encountered in travel health practice. Items
were collated from freely accessible online repositories, chiefly the
sample question banks curated by the International Society of Travel
Medicine (<https: www.istmfoundation.net="" cth_samplequestions="">) and the
American Society of Tropical Medicine and Hygiene certificate programme
(<https: www.astmh.org="" education-resources="" certificate-programs="">). They
covered diagnosis, prevention, and therapeutic decision-making for
parasitic, bacterial, and viral conditions. The question list is
reported in Supplementary Table X.

## Large language models

A total of 40 large language models were selected to reflect the breadth
of contemporary the technology. The sample included proprietary frontier
systems (for example, OpenAI o3, Google Gemini 2·5 Pro, Anthropic Claude
3·5 Sonnet), high-performance hosted deployments of open-source models,
and locally executed, quantised replicas suitable for desktop-class
hardware. These local models ran offline on a MacBook Pro equipped with
an Apple M1 Pro processor and 32 GB of unified memory, using LM Studio
as the inference interface (<https: lmstudio.ai="" docs="" basics="">) and the
MLX runtime (<https: github.com="" apple="" mlx="">) for model execution. Where
applicable, the most recent public checkpoints were queried through
their native application-programming interfaces. Temperature was fixed
at 0·5 for all calls to balance determinism with variability; OpenAI’s
o-series models ignore this parameter by design. The list of models is
reported in Supplementary Table X.

## Experimental procedure

Three prompting strategies were investigated:

1.  **Concise answer** (`cold`) - the model was instructed to reply with
    a single letter corresponding to the chosen option.
2.  **Free explanation** (`free`) - a brief rationale was permitted
    provided the response ended with the option letter. This approach is
    supported by work showing that allowing language models to verbalise
    a short explanation can boost accuracy (Tam et al. 2024).
3.  **Chain-of-thought** (`reasoning`) - the model was invited to
    articulate its reasoning step by step before stating the final
    answer. Chain-of-thought prompting is a well-established strategy
    for eliciting reasoning in large language models (Wei et al. 2022).

Models whose decoding stack already includes an internal
chain-of-thought module—such as OpenAI’s o-series, DeepSeek R1, and
certain Gemini variants—were exempted from the third prompt strategy to avoid
redundancy. These systems are trained to perform latent deliberation at
inference time, a strategy empirically shown to enhance answer quality
and cost-efficiency (Snell et al. 2024; DeepSeek-AI 2025).

Every combination of question, model, and prompting condition was tested
five times, yielding interactions. Each response was processed
programmatically to extract the final option letter, as well as to
record processing time and token consumption. If the response format did
not permit automatic extraction, a secondary parsing step was performed
using the large language model gemini-2.0-flash-001. Responses that
remained unparseable after this secondary step were classified as
unparseable.

Responses were matched against the correct answer and classified into
four mutually exclusive categories: correct, incorrect, unparseable (no
valid option detected), and generation error. Additionally, correct and
incorrect responses were annotated according to whether they were
directly machine-parsable or required secondary parsing by a large
language model.

## Statistical models

The classification results were analysed with three multilevel Bayesian
models, covering three metrics: accuracy, parsing success, and
consistency. The Bayesian modelling approach has the advantage of
providing more generalisable results compared to crude descriptive
statistics, making the results less dependent on the specific sample of
questions used. All models were fitted in Stan through the *brms*
interface and included a random intercept for items, a random intercept
by model, as well as a random slope for the interaction between
prompt strategy and model. Variance components were assigned
weakly-informative Student-t priors with three degrees of freedom and
scale 1.5, whereas correlation matrices were given an LKJ prior with
shape parameter 2, which gently shrinks correlations towards zero
(Gelman 2006; PyMC Community 2023). Two Hamiltonian Monte Carlo chains
were run for 4 000 iterations (1 000 warm-up) and convergence was
assessed with $\hat R (&lt; 1.01)$ and effective sample sizes.

### Accuracy

To measure accuracy, the probability of a correct response was estimated
for each item, model, prompt combination over $N$ replications.

Let $(i = 1, \dots, N)$ index each unique item, model, prompt
combination. For each cell $i$, $Y_i$ is the number of correct
replications out of $T_i$ trials. These counts were modelled with a
binomial hierarchical model:

\$\$
\$\$

This model corresponds to the **brms** formula:

`correct | trials(total) ~ 0 + (modality | model_id) + (1 | item)` with
`family = binomial()`.

with `model_id` being the model identifier, `modality` the prompting
strategy, and `item` the question.

Posterior samples of $\pi_i$ provide the accuracy score for each
combination of model, question, and prompting strategy.

### Parsing success

To evaluate the reliability of the answer format, for every individual
replication, an ordinal response $Y_n\in\{0,1,2\}$ encoding parsing
success was considered, with:

- $0$ (*none*) - parsing failed,
- $1$ (*rescued*) - the answer needed parsing by an auxiliary LLM,
- $2$ (*clean*) - parsed without intervention.

Let $n = 1, \dots, N$ index individual replications. The ordinal outcome
$Y_n \in \{0,1,2\}$ was modelled using a cumulative logit formulation
with two ordered thresholds, $\kappa_0 &lt; \kappa_1$:

$$
  \Pr(Y_n \le c) = \text{logit}^{-1}(\kappa_c - \eta_n), \qquad c \in \{0,1\}.
$$

The two cumulative probabilities imply three probabilities, one per
parsing category:

The linear predictor $\eta_n$ formulation and priors were identical to
those adopted for the accuracy model.

This model corresponds to the **brms** call:

`y ~ (prompt | model_id) + (1 | item)` with
`family = cumulative("logit")`

Posterior category probabilities $(\pi_{n0}, \pi_{n1}, \pi_{n2})$
derived from the model were condensed to a continuous parsing-success
score on the unit interval:

$$
  S_n = \frac{0\,\pi_{n0} + 1\,\pi_{n1} + 2\,\pi_{n2}}{2},
$$

so that $S_n = 1$ denotes a certainly *clean* reply and $S_n = 0$ a
certainly *none* outcome.

### Consistency

Reproducibility was quantified by counting how many distinct options
$d_i$ were selected across the $T=5$ replications of each (question,
model, prompt) cell, indexed by $i$. Because the possible answers are
four (A, B, C, D), the observed range is $d_i\in\{1,\dots,4\}$.

The count $d_i$ was treated as the number of “successes” in an improper
binomial experiment with $T=5$ trials:

$$
  d_i \sim \text{Binomial}(5,\,\rho_i).
$$

Although the independence assumption is not strictly satisfied, since
two identical answers in the same cell reduce the chance of further
distinct letters, the binomial likelihood nevertheless provides a
convenient monotone link between $\rho_i$ and the degree of variability;
the resulting uncertainty is therefore somewhat underestimated.

The distinct-option rate followed the same hierarchical structure and
priors as the accuracy model.

This model corresponds to the **brms** call:

`y | trials(total) ~ 0 + (modality | model_id) + (1 | item)` with
`family = binomial()`

Posterior draws for the expected number of distinct letters,
$\tilde{d}_i=5\,\rho_i$, were converted to a unit-interval consistency
score by rescaling to the theoretical maximum of four distinct options:

$$
  C_i = 1-\frac{\tilde{d}_i}{4},
$$

so that $C_i=1$ marks perfect reproducibility (all replications choose
the same letter) and $C_i=0$ corresponds to the extreme case in which
all four options are observed within the five attempts.

## Statistical analysis

To evaluate overall performance, the posterior distributions of the
three scores were marginalised over different dimensions of the
experimental design. For each draw from the posterior distribution, the
mean score was computed across all levels of the variable(s) being
marginalised away. This process yielded full posterior distributions for
three complementary summaries:

1.  **Model-level estimates**, obtained by marginalising over items and
    prompt strategies.
2.  **Prompt-strategy estimates**, obtained by marginalising over
    items and models.
3.  **Model–prompt strategy interaction estimates**, obtained by marginalising
    over items.

From these marginalised distributions, the posterior median was computed
as the point estimate of performance and the 95% quantile credible
interval (CrI) for uncertainty quantification.

To investigate the relationships between the different performance
dimensions, Spearman’s rank correlation coefficients ($\rho_s$) were
computed between the model-level posterior scores for accuracy, parsing
success, and consistency. The correlation was calculated separately for
each draw from the posterior distribution, yielding a full posterior
distribution for $\rho_s$ for each pair of metrics.

Finally, the trade-off between model accuracy and financial cost was
analysed. Model-level accuracy estimates were plotted against the
provider’s published price per million tokens. The **Pareto frontier**
was identified, defined as the set of models for which no other model
offered both higher accuracy and lower cost. To visualise the optimal
trade-off, a logistic curve was fitted to the points on the frontier
using a quasi-binomial generalised linear model.

All analyses were conducted using R v4.5.0. The code is available at
<https: github.com="" mattia-p="" llm-travel-medicine="">.

<div id="refs" class="references csl-bib-body hanging-indent" entry-spacing="0">

<div id="ref-deepseek2025" class="csl-entry">

DeepSeek-AI. 2025. “DeepSeek-R1: Incentivizing Reasoning Capability in
LLMs via Reinforcement Learning.” *arXiv Preprint arXiv:2501.12948*.

</div>

<div id="ref-gelman2006" class="csl-entry">

Gelman, Andrew. 2006. “Prior Distributions for Variance Parameters in
Hierarchical Models.” *Bayesian Analysis* 1 (3): 515–33.
<https: doi.org="" 10.1214="" 06-ba117="">.

</https:></div>

<div id="ref-lkjprior" class="csl-entry">

PyMC Community. 2023. “LKJ Cholesky Covariance Priors for Multivariate
Normal Models.”
<https: www.pymc.io="" projects="" examples="" en="" latest="" howto="" lkj.html="">.

</https:></div>

<div id="ref-snell2024" class="csl-entry">

Snell, Charlie, Eric Wallace, Dan Klein, and Sergey Levine. 2024.
“Scaling LLM Test-Time Compute Optimally Can Be More Effective Than
Scaling Model Parameters.” *arXiv Preprint arXiv:2408.03314*.

</div>

<div id="ref-tam2024" class="csl-entry">

Tam, Zhi Rui, Cheng-Kuang Wu, Yi-Lin Tsai, Chieh-Yen Lin, Hung-yi Lee,
and Yun-Nung Chen. 2024. “Let Me Speak Freely? A Study on the Impact of
Format Restrictions on Performance of Large Language Models.” *arXiv
Preprint arXiv:2408.02442*.

</div>

<div id="ref-wei2022" class="csl-entry">

Wei, Jason, Xuezhi Wang, Dale Schuurmans, Maarten Bosma, Brian Ichter,
Fei Xia, Ed H. Chi, Quoc Le, and Denny Zhou. 2022. “Chain-of-Thought
Prompting Elicits Reasoning in Large Language Models.” *arXiv Preprint
arXiv:2201.11903*. <https: doi.org="" 10.48550="" arxiv.2201.11903="">.

</https:></div>

</div>
</https:></https:></https:></https:></https:></body></html>