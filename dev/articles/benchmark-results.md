# Benchmark Results

This vignette compares ten context-retrieval strategies implemented in
**rrlmgraph-bench** on a suite of realistic R-project tasks. Results
were pre-computed by the `run-benchmark` GitHub Actions workflow in
[rrlmgraph-bench](https://github.com/davidrsch/rrlmgraph-bench) and
summarised here — no benchmark re-run is needed to render this page.

## Strategies

| Strategy                 | Description                                                                                                                                              |
|--------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------|
| `graph_rag_tfidf`        | Graph-RAG: BFS from TF-IDF seed nodes                                                                                                                    |
| `graph_rag_tfidf_noseed` | Graph-RAG: TF-IDF traversal seeded by the query itself                                                                                                   |
| `graph_rag_ollama`       | Graph-RAG: BFS from Ollama vector-similarity seed nodes                                                                                                  |
| `graph_rag_mcp`          | Graph-RAG: traversal via the rrlmgraph-mcp MCP server                                                                                                    |
| `rlm_graph`              | RLM-Graph: LLM writes R code in sandboxed REPL (no-sub-calls ablation of Algorithm 1, arXiv:2512.24601 §4.3); only compact stdout returned per iteration |
| `full_files`             | Entire source files (**upper baseline**)                                                                                                                 |
| `bm25_retrieval`         | BM25 keyword retrieval (no graph)                                                                                                                        |
| `term_overlap`           | Word-overlap ranking (no graph)                                                                                                                          |
| `no_context`             | No context — LLM answers from training data (**lower baseline**)                                                                                         |
| `random_k`               | Five randomly sampled code chunks (random baseline)                                                                                                      |

## Results

``` r
# When a vignette is built, cwd = vignettes/ so this relative path works
# both during R CMD check and pkgdown site builds.
precomputed_path <- file.path("precomputed", "benchmark_summary.rds")
if (file.exists(precomputed_path)) {
    bench <- readRDS(precomputed_path)
} else {
    bench <- NULL
}
```

``` r
if (is.null(bench)) {
    cat(
        "> **Note:** Benchmark results not yet available.\n",
        "> Trigger the `run-benchmark` workflow in",
        "[rrlmgraph-bench](https://github.com/davidrsch/rrlmgraph-bench/actions)",
        "to generate data, then run the `precompute-vignettes` workflow here.\n"
    )
}
```

``` r
stats <- bench$stats
knitr::kable(
    stats$summary[, c(
        "strategy", "mean_score", "sd_score", "ci_lo_95", "ci_hi_95",
        "mean_total_tokens", "hallucination_rate", "n"
    )],
    digits = 3,
    caption = paste0(
        "Mean score ± 95 % CI per strategy.  ",
        "Captured: ", bench$meta$generated_at, ".  ",
        "rrlmgraph v", bench$meta$rrlmgraph_version, " / ",
        "rrlmgraphbench v", bench$meta$rrlmgraphbench_version, "."
    ),
    col.names = c(
        "Strategy", "Mean score", "SD", "CI lower", "CI upper",
        "Mean tokens", "Hallucination rate", "N"
    )
)
```

| Strategy               | Mean score |    SD | CI lower | CI upper | Mean tokens | Hallucination rate |   N |
|:-----------------------|-----------:|------:|---------:|---------:|------------:|-------------------:|----:|
| graph_rag_tfidf        |      0.723 | 0.244 |    0.622 |    0.810 |     482.840 |              0.560 |  25 |
| graph_rag_tfidf_noseed |      0.699 | 0.213 |    0.602 |    0.781 |     537.348 |              0.609 |  23 |
| graph_rag_ollama       |      0.787 | 0.183 |    0.699 |    0.854 |     406.450 |              0.750 |  20 |
| full_files             |      0.727 | 0.094 |    0.687 |    0.767 |    1549.250 |              0.750 |  20 |
| term_overlap           |      0.704 | 0.135 |    0.640 |    0.755 |    1529.800 |              0.750 |  20 |
| bm25_retrieval         |      0.749 | 0.078 |    0.715 |    0.781 |    1528.300 |              0.750 |  20 |
| no_context             |      0.743 | 0.199 |    0.652 |    0.824 |     112.650 |              0.700 |  20 |
| graph_rag_mcp          |      0.778 | 0.142 |    0.715 |    0.837 |     341.350 |              0.750 |  20 |
| random_k               |      0.735 | 0.091 |    0.695 |    0.773 |    1542.600 |              0.750 |  20 |
| rlm_graph              |      0.298 | 0.422 |   -3.493 |    4.090 |       3.700 |              0.150 |  20 |

Mean score ± 95 % CI per strategy. Captured: 2026-04-04T13:04:43Z.
rrlmgraph v0.1.6.9000 / rrlmgraphbench v0.1.7.9000.

``` r
stats <- bench$stats
df <- stats$summary
df$strategy <- factor(df$strategy, levels = df$strategy[order(df$mean_score)])
baseline <- df$mean_score[df$strategy == "full_files"]

op <- par(mar = c(5, 10, 3, 2))
plot(
    df$mean_score,
    seq_len(nrow(df)),
    xlim = c(0, 1),
    xlab = "Mean score",
    ylab = "",
    yaxt = "n",
    pch = 16,
    col = ifelse(df$strategy == "graph_rag_tfidf", "steelblue",
        ifelse(df$strategy == "graph_rag_mcp", "seagreen", "grey40")
    ),
    main = "Strategy comparison"
)
axis(2, at = seq_len(nrow(df)), labels = df$strategy, las = 1, cex.axis = 0.85)
abline(v = baseline, lty = 2, col = "grey60")
arrows(
    df$ci_lo_95, seq_len(nrow(df)),
    df$ci_hi_95, seq_len(nrow(df)),
    length = 0.05, angle = 90, code = 3,
    col = ifelse(df$strategy == "graph_rag_tfidf", "steelblue",
        ifelse(df$strategy == "graph_rag_mcp", "seagreen", "grey40")
    )
)
```

![Mean score ± 95 % CI by strategy. Dashed line = full_files
baseline.](benchmark-results_files/figure-html/score-plot-1.png)

Mean score ± 95 % CI by strategy. Dashed line = full_files baseline.

``` r
par(op)
```

## Token efficiency

The Token Efficiency Ratio (TER) measures score gain per token relative
to the `full_files` baseline. Values \> 1 indicate better score per
token used.

``` r
ter_df <- data.frame(
    strategy = names(bench$stats$ter),
    TER = round(bench$stats$ter, 3),
    stringsAsFactors = FALSE
)
knitr::kable(ter_df, row.names = FALSE, caption = "Token Efficiency Ratio (TER)")
```

| strategy               |     TER |
|:-----------------------|--------:|
| graph_rag_tfidf        |   3.191 |
| graph_rag_tfidf_noseed |   2.770 |
| graph_rag_ollama       |   4.125 |
| full_files             |      NA |
| term_overlap           |   0.980 |
| bm25_retrieval         |   1.044 |
| no_context             |  14.047 |
| graph_rag_mcp          |   4.857 |
| random_k               |   1.015 |
| rlm_graph              | 171.784 |

Token Efficiency Ratio (TER)

## Pairwise tests

``` r
pw <- bench$stats$pairwise
pw$significant <- pw$p_bonferroni < 0.05
knitr::kable(
    pw[, c("strategy_1", "strategy_2", "p_bonferroni", "cohens_d", "significant")],
    digits    = 4,
    col.names = c("Strategy A", "Strategy B", "p (Bonferroni)", "Cohen's d", "Sig."),
    caption   = "Pairwise Welch t-tests (Bonferroni-corrected)."
)
```

| Strategy A             | Strategy B             | p (Bonferroni) | Cohen’s d | Sig.  |
|:-----------------------|:-----------------------|---------------:|----------:|:------|
| graph_rag_tfidf        | graph_rag_tfidf_noseed |              1 |    0.1060 | FALSE |
| graph_rag_tfidf        | graph_rag_ollama       |              1 |   -0.2911 | FALSE |
| graph_rag_tfidf        | full_files             |              1 |   -0.0211 | FALSE |
| graph_rag_tfidf        | term_overlap           |              1 |    0.0956 | FALSE |
| graph_rag_tfidf        | bm25_retrieval         |              1 |   -0.1350 | FALSE |
| graph_rag_tfidf        | no_context             |              1 |   -0.0872 | FALSE |
| graph_rag_tfidf        | graph_rag_mcp          |              1 |   -0.2687 | FALSE |
| graph_rag_tfidf        | random_k               |              1 |   -0.0625 | FALSE |
| graph_rag_tfidf        | rlm_graph              |              1 |    1.6772 | FALSE |
| graph_rag_tfidf_noseed | graph_rag_ollama       |              1 |   -0.4441 | FALSE |
| graph_rag_tfidf_noseed | full_files             |              1 |   -0.1730 | FALSE |
| graph_rag_tfidf_noseed | term_overlap           |              1 |   -0.0283 | FALSE |
| graph_rag_tfidf_noseed | bm25_retrieval         |              1 |   -0.3117 | FALSE |
| graph_rag_tfidf_noseed | no_context             |              1 |   -0.2139 | FALSE |
| graph_rag_tfidf_noseed | graph_rag_mcp          |              1 |   -0.4394 | FALSE |
| graph_rag_tfidf_noseed | random_k               |              1 |   -0.2224 | FALSE |
| graph_rag_tfidf_noseed | rlm_graph              |              1 |    1.7553 | FALSE |
| graph_rag_ollama       | full_files             |              1 |    0.4099 | FALSE |
| graph_rag_ollama       | term_overlap           |              1 |    0.5176 | FALSE |
| graph_rag_ollama       | bm25_retrieval         |              1 |    0.2714 | FALSE |
| graph_rag_ollama       | no_context             |              1 |    0.2309 | FALSE |
| graph_rag_ollama       | graph_rag_mcp          |              1 |    0.0527 | FALSE |
| graph_rag_ollama       | random_k               |              1 |    0.3580 | FALSE |
| graph_rag_ollama       | rlm_graph              |              1 |    2.4210 | FALSE |
| full_files             | term_overlap           |              1 |    0.2018 | FALSE |
| full_files             | bm25_retrieval         |              1 |   -0.2481 | FALSE |
| full_files             | no_context             |              1 |   -0.1000 | FALSE |
| full_files             | graph_rag_mcp          |              1 |   -0.4232 | FALSE |
| full_files             | random_k               |              1 |   -0.0855 | FALSE |
| full_files             | rlm_graph              |              1 |    3.2534 | FALSE |
| term_overlap           | bm25_retrieval         |              1 |   -0.4086 | FALSE |
| term_overlap           | no_context             |              1 |   -0.2299 | FALSE |
| term_overlap           | graph_rag_mcp          |              1 |   -0.5384 | FALSE |
| term_overlap           | random_k               |              1 |   -0.2731 | FALSE |
| term_overlap           | rlm_graph              |              1 |    2.5086 | FALSE |
| bm25_retrieval         | no_context             |              1 |    0.0393 | FALSE |
| bm25_retrieval         | graph_rag_mcp          |              1 |   -0.2577 | FALSE |
| bm25_retrieval         | random_k               |              1 |    0.1599 | FALSE |
| bm25_retrieval         | rlm_graph              |              1 |    3.7151 | FALSE |
| no_context             | graph_rag_mcp          |              1 |   -0.2053 | FALSE |
| no_context             | random_k               |              1 |    0.0494 | FALSE |
| no_context             | rlm_graph              |              1 |    2.0621 | FALSE |
| graph_rag_mcp          | random_k               |              1 |    0.3612 | FALSE |
| graph_rag_mcp          | rlm_graph              |              1 |    2.8640 | FALSE |
| random_k               | rlm_graph              |              1 |    3.3705 | FALSE |

Pairwise Welch t-tests (Bonferroni-corrected).

## Methodology

Full task definitions and scoring rubrics are in the
[rrlmgraph-bench](https://github.com/davidrsch/rrlmgraph-bench)
repository. Each task is scored 0–1 using
`compute_benchmark_statistics()` from the **rrlmgraphbench** package.
Hallucinations are detected by `count_hallucinations()`.
