# Performance of Large Language Models on travel medicine certificate multiple choice questions

## Short Abstract

We benchmarked 40 LLMs on a 40 item travel medicine quiz. Bayesian modelling was used to evaluate accuracy, consistency, parsability and cost metrics. Accuracy spanned 27.9-97.5%; reasoning tuned frontier models (OpenAi o3, Perplexity Sonar Reasoning) topped the benchmark, whereas local small underperformed. Cost accuracy curves revealed five Pareto optimal systems, with o3 being the current best. These findings confirm the performance of current LLMs as public health knowledge support systems.

## Long Abstract

Large language models (LLMs) are rapidly permeating clinical decision‑support workflows, yet their competence in specialised travel‑ and tropical‑medicine contexts has remained anecdotal. We applied a 40‑item single‑best‑answer benchmark assembled from the freely accessible sample question banks curated by the International Society of Travel Medicine¹ and the American Society of Tropical Medicine and Hygiene Certificate Program²; the items cover diagnosis, prevention and therapeutic decision‑making for parasitic, bacterial and viral conditions across all World Health Organization regions³, and deployed it to 40 state‑of‑the‑art commercial and open‑source LLMs. Answers were evaluated with a hierarchical Bayesian item‑response model to provide more generalizable results, and we additionally scored each model's ability to follow formatting instructions (parsability), reproduce its own answers across repeated queries (consistency), and balance performance against provider‑side token costs.

Posterior accuracy ranged from 97.5% [95.9-98.6%] for OpenAI's o3 to 27.9% [24.8-31.0%] for a 1.5 B‑parameter local Qwen2.5 model quantised to four bits. Large reasoning‑optimised models (OpenAI o3, o1, Perplexity Sonar‑Reasoning, Google Gemini‑2.5‑Pro) clustered above 95%, outperforming instruction‑tuned "classic" models such as OpenAI GPT‑4o and Mistral‑Large by ~4 percentage points. Accuracy improvements within provider families were monotonic with successive model generations. Output formatting was virtually flawless for frontier systems (parsability ≥ 99%) but deteriorated to ≈50% in the smallest local model deployments, yielding only a moderate overall correlation between parsability and correctness (r = 0.62). Answer consistency followed a similar gradient, with the best models achieving over 94% reproducibility across identical prompts.

A cost-accuracy analysis identified five Pareto-optimal systems spanning four orders of magnitude in price. A recent 80% price cut placed OpenAI o3 at the apex, delivering the highest accuracy for US$8 per million tokens, whereas Google's temporarily free Gemini‑Flash achieved 94% accuracy at zero marginal cost. Intermediate options such as DeepSeek‑R1 and Sonar‑Reasoning offered favourable trade‑offs, while OpenAI o1's US$60 price put it below the frontier despite near‑maximal accuracy. Search augmentation was beneficial when tightly integrated (Perplexity Sonar-Reasoning +11 pp over its base) but deleterious when grafted onto OpenAI GPT‑4o.

Item‑level analysis underscored substantial heterogeneity. Median posterior correctness spanned from 17.5% on a neuro‑parasitic vignette requiring recognition of Onchocerciasis to ≥99% for routine prophylaxis questions, with only 12% of models crossing the 50% threshold on the hardest item. A small subset of challenging vignettes accounted for most between‑model variance, indicating that even frontier models retain blind spots in niche clinical corners.

Taken together, these results confirm that state‑of‑the‑art reasoning‑optimised LLMs already function as competent public‑health knowledge support systems for travel‑medicine scenarios,⁴ ⁵ delivering near‑expert accuracy, impeccable formatting and strong internal consistency at commodity‑level costs. Conversely, severely size‑constrained local models remain unreliable, and challenging expert‑level vignettes continue to expose residual blind spots even in frontier systems.

These findings should be interpreted with caution. Our evaluation covered only forty closed‑option multiple‑choice vignettes, so performance may not generalise to other travel‑medicine queries, to other clinical domains, or to open‑ended free‑text reasoning tasks. Furthermore, because contemporary LLMs are trained on vast internet corpora, we cannot exclude the possibility that some benchmark questions—or close paraphrases—were present in their training data, potentially inflating scores.



References

International Society of Travel Medicine. Certificate in Travel Health® sample questions [Internet]. ISTM; 2025 [cited 2025 Jul 15]. Available from: https://www.istmfoundation.net/cth_samplequestions

American Society of Tropical Medicine and Hygiene. Certificate Programs sample questions [Internet]. ASTMH; 2025 [cited 2025 Jul 15]. Available from: https://www.astmh.org/education-resources/certificate-programs

World Health Organization. International travel and health. Module 1: General health risks and considerations for travellers. Geneva: WHO; 2024.

Asiedu M. Benchmarking LLMs for global health [Internet]. Google Research Blog; 2025 Apr 30 [cited 2025 Jul 15]. Available from: https://research.google/blog/benchmarking-llms-for-global-health/

Williams CYK, Miao BY, Kornblith AE, Butte AJ, et al. Evaluating the use of large language models to provide clinical recommendations in the Emergency Department. Nature Communications. 2024;15:8236.

