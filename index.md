Builds a typed knowledge graph of an R project's structure for use with
large language models. The graph encodes functions, files, packages, and
tests as nodes, with call, import, and test edges capturing
dependencies. Supports TF-IDF embeddings out-of-the-box, with optional
Ollama and OpenAI embedding backends. Chat integration supports multiple
providers (OpenAI, Ollama, GitHub Models, Anthropic) via the ellmer
package.
