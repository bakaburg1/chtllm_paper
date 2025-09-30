# Results


- [<span class="toc-section-number">1</span> Performance overview and
  key drivers](#overall-performance)
- [<span class="toc-section-number">2</span> Parsability of responses
  and consistency](#parsability-consistency)
- [<span class="toc-section-number">3</span> Cost-accuracy
  trade-off](#cost-accuracy)
- [<span class="toc-section-number">4</span> Item-level
  analysis](#item-analysis)

## Performance overview and key drivers

<div id="fig-overall-correctness">

![](results_files/figure-commonmark/fig-overall-correctness-1.png)

Figure 1: Model accuracy on the travel-medicine benchmark. Points show
the posterior median accuracy, with bars representing the 95% credible
intervals. Models are ordered by performance.

</div>

<div id="fig-modality-correctness">

![](results_files/figure-commonmark/fig-modality-correctness-1.png)

Figure 2: Effect of prompt strategy on model accuracy. Points show
the posterior median and bars the 95% credible intervals.

</div>

Across the 40 large-language models evaluated, posterior accuracy ranged
from 97.5% \[95.9%-98.6%\] for OpenAI’s o3 to 27.9% \[24.8%-31.0%\] for
the smallest local replica (`qwen2.5-coder-1.5b-instruct-mlx@4bit`)
(<a href="#fig-overall-correctness" class="quarto-xref">Figure 1</a>).

Large parameter count reasoning models dominated: in addition to o3, o1
96.5% \[94.7%-98.0%\], Perplexity *Sonar-Reasoning* 97.2%
\[95.6%-98.5%\], and Gemini-2.5-Pro 95.4% \[93.4%-97.0%\] were among the
top performers.

Instruction-tuned classics plateaued roughly 4 percentage points lower,
led by GPT-4o 91.4% \[89.5%-93.1%\] and Mistral-Large-2411 93.8%
\[92.0%-95.3%\].

Local models performed overall worse, mostly due to limits in deployable
sizes that restricted deployment to very small models (e.g., 1.5B
parameters). The smallest 1.5B quantization languished at 27.9%
\[24.8%-31.0%\]. The largest deployable size on the researchers’
machines was a 32B Qwen distillation of DeepSeek-R1, which achieved
85.1% \[82.2%-87.9%\] but nevertheless performed slightly worse than the
same model accessed via the Groq API (86.2% \[83.4%-88.7%\]), probably
due to the aggressive quantization (4-bit) necessary to make it fit in
memory.

Within provider families, accuracy climbed with successive iterations:
OpenAI’s line rose from GPT-4o 91.4% \[89.5%-93.1%\] through o1 96.5%
\[94.7%-98.0%\] to o3 97.5% \[95.9%-98.6%\]; Google’s advanced from
Gemini-Flash 92.0% \[90.1%-93.7%\] to Gemini-Pro 95.4% \[93.4%-97.0%\].
Similar improvements were observed for LLama and Mistral models.

Search augmentation helped when well integrated: *Sonar-Reasoning* (a
DeepSeek-R1 finetune with search capabilities) surpassed its base model
(97.2% vs 86.2%), whereas the search-enabled GPT-4o variant
underperformed the base (88.7% vs 91.4%).

Prompting altered these figures only marginally
(<a href="#fig-modality-correctness" class="quarto-xref">Figure 2</a>).
Classic models improved from 79.7% with a **cold** prompt to 80.9% under
explicit reasoning instructions, whereas reasoning models performed best
with the concise **cold** prompt (84.3% vs 83.8% for a *free* prompt).
But the largest performance gap was between reasoning models and
classics, independent of prompt strategy.

## Parsability of responses and consistency

<div id="fig-metrics">

<img src="results_files/figure-commonmark/fig-metrics-1.png"
style="width:100.0%" />

Figure 3: Model performance on secondary metrics. (A) Parsing success,
measured as parsing success score. (B) Answer consistency, measuring the
reproducibility of answers across identical prompts. Points are
posterior medians, bars are 95% credible intervals.

</div>

<div id="fig-cross-metrics">

<img src="results_files/figure-commonmark/fig-cross-metrics-1.png"
style="width:100.0%" />

Figure 4: Relationship between accuracy and other performance metrics.
(A) Accuracy versus parsing success. (B) Accuracy versus answer
consistency. Each point represents a model, categorised into quadrants
based on median performance.

</div>

Most models followed formatting instructions and produced the expected
output format, which allowed for automated parsing of the answers
(<a href="#fig-metrics" class="quarto-xref">Figure 3</a>A). Parsability
exceeded 90% for 30 of the 40 systems evaluated. The distribution was
decidedly bimodal: at one extreme, large high-performing frontier models
such as `o3` (100.0%), `gemini-2.5-pro` (99.9%), and `deepseek-r1`
(100.0%) yielded perfect rates well above 95%; at the other, small
locally-running models showed a strong drop in parsability, achieving
barely half that rate with scores around 50%.

In general, accuracy and instruction-following were moderately
correlated (r = 0.62; 95% CrI 0.56-0.67). However, a well-structured
output did not guarantee correctness. When plotted against each other
(<a href="#fig-cross-metrics" class="quarto-xref">Figure 4</a>A), the
models divided into four groups: the best models dominated the
upper-right (accurate and well-formed) quadrant, while notable
exceptions populated the off-diagonal quadrants. For example, GPT-4o
Mini and Claude 3.5 Haiku attained an almost perfect parse rate but
underperformed in terms of accuracy; on the other hand, Perplexity Sonar
Reasoning was extremely reliable in providing correct answers but had
more difficulties in following formatting instructions.

Response consistency was also generally good, ranging from 86.4%
(`r1-distill-qwen-1.5b@4bit`) to 94.9% (`o3`)
(<a href="#fig-metrics" class="quarto-xref">Figure 3</a>B). The
correlation between accuracy and consistency was expectedly high
($\rho_s$ = 0.75; 95% CrI 0.62-0.85;
<a href="#fig-cross-metrics" class="quarto-xref">Figure 4</a>B): large
reasoning models dominated the high-accuracy, high-consistency quadrant
(upper-right) while mostly incorrect and unstable models populated the
bottom-right. Very few models were in the top-left quadrant (mostly
smaller versions of high-performing models, such as o4-mini and Claude
3.5 “Haiku”, but also Llama 4 “Scout”) or the bottom-right quadrant
(models inconsistently right, notably two search-enabled models,
Perplexity Sonar Large and GPT-4o-search).

## Cost-accuracy trade-off

<div id="fig-cost-accuracy">

![](results_files/figure-commonmark/fig-cost-accuracy-1.png)

Figure 5: Cost-accuracy trade-off and Pareto frontier. The frontier (red
line) highlights models offering the best accuracy for a given cost. The
y-axis is on a logit scale and the x-axis on the log10 scale of cost per
million tokens.

</div>

The cost-accuracy analysis identified 5 Pareto-optimal models
(<a href="#fig-cost-accuracy" class="quarto-xref">Figure 5</a>) spanning
four orders of magnitude in price. Accuracy increased almost
monotonically from roughly 85.1% \[82.2%-87.9%\] for a free local 32-B
DeepSeek R1 distillation (at no cost) to 97.5% \[95.9%-98.6%\] for
OpenAI’s o3 (at \$8 per million tokens). Crucially, a drastic (~80%)
recent price cut for o3 positioned it at the apex of the efficiency
frontier.

At the extreme low-cost end, Gemini 2.0 “Flash” achieved 94.2%
\[92.1%-96.0%\]. Its inclusion on the frontier owes mostly to its
temporary free access for experimentation during the study period
(nominal cost \$0); performance, while strong, still trailed the very
best models by roughly two percentage points.

In contrast, o1 delivers a similar accuracy of 96.5% but at a far
steeper cost of \$60 per million tokens, leaving it well below the
frontier. Intermediate points such as DeepSeek-R1 (95.4% at \$2.19) and
Perplexity Sonar Reasoning (97.2% at \$5) illustrate that large gains in
accuracy can be obtained for modest additional spend, whereas costs
beyond ≈\$10 yield diminishing returns. Overall, Perplexity Sonar
Reasoning and o3 currently offer the most favourable balance between
accuracy and operational expense, with o3 being more stable and reliable
(see <a href="#fig-cross-metrics" class="quarto-xref">Figure 4</a>).

## Item-level analysis

<div id="fig-item-mosaic">

<img src="results_files/figure-commonmark/fig-item-mosaic-1.png"
style="width:100.0%" />

Figure 6: Item-level performance across all models. Each cell shows the
median posterior correctness for a given model and question. Color
intensity reflects correctness, and transparency indicates certainty
(narrower credible intervals are more opaque).

</div>

Per-question model performance was highly heterogeneous
(<a href="#fig-item-mosaic" class="quarto-xref">Figure 6</a>). Median
accuracy spanned from 17.5% for the hardest prompt (item 26, a
neurological-symptoms scenario with a past exposure to center-west
Africa rural setting, suggestive of Onchocerciasis) up to virtually
perfect performance on the easiest trio (#39 (99.1%), \#8 (99.2%), \#32
(99.4%)), covering routine travel-medicine advice and basic
epidemiology.

Only 12% of systems crossed the 50% posterior threshold on item 26. In
the raw responses, option A (“Skin snips”) was chosen 40%, D
(“Giemsa-stained smears of blood drawn at night”) 32%, and the correct
option C (“Antifilarial antibody levels”) only 23%. *o3* was internally
consistent yet wrong, selecting A in 100% of its replications. The
spread across alternatives indicates genuine uncertainty rather than a
single systematic misconception. Notably, solution A is the first-line
diagnostic approach for suspected Onchocerciasis.

Notably, for items \#2 (diagnosis of suspected malaria with diarrheal
symptoms) and \#4 (treatment of fever and diarrhea), the second and
third most challenging items after \#26, the o3 model consistently
provided correct responses, yet exhibited posterior medians
(approximately 90% and 77%, respectively) far from a 100% success rate
and with wider credible intervals. This pattern reflects the
regularizing properties of the hierarchical Bayesian model, which
exhibits increased uncertainty when a model’s performance substantially
deviates from the population average, as each question was modelled with
its own intercept and no model/item interaction was included for better
generalization.

Conversely, items such as \#39 (99.1%), \#8 (99.2%), \#32 (99.4%) were
solved by virtually all systems, indicating that a sizeable fraction of
the benchmark targeted plain retrieval rather than complex reasoning.
Variation (range of posterior medians) exceeded 90 percentage points for
several mid-difficulty questions, underscoring that model architecture
and scale interact strongly with item traits. Overall, a small subset of
challenging prompts (e.g., items \#26 (17.5%), \#4 (24.4%), \#2 (46.2%))
accounted for the bulk of performance discrimination across models.
