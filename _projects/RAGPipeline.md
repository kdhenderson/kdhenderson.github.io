---
layout: page
title: custom RAG system
description: Custom RAG System with Pre-Trained Embeddings, LoRA Fine-Tuning, and Reranking
img: assets/img/RAGPipeline.png
importance: 2
category: data_science
related_publications: false
---

<div id="header">
<h4 class="author">Kristin Henderson</h4>
<h4 class="date">Fall 2025</h4>
<p><br></p>
</div>

### Overview

This project builds a complete Retrieval-Augmented Generation (RAG)
pipeline on Paul Graham's essays. Two embedding models were pre-trained
from random initialization using masked language modeling followed by
contrastive fine-tuning with hard negatives: a general-purpose model
trained on Wikipedia text and a domain-specific model trained on the
essay corpus. Gemma-3-1b-it was post-trained for question answering
using LoRA, a parameter-efficient method that updated about 1.5 million
of its 1 billion parameters. A cross-encoder reranker re-scored the
top-20 retrieved chunks before generation. The domain-specific embedding
model performed best after additional post-training on question-chunk
pairs. The full system was evaluated on an independent test set of 55
questions. The domain-specific embedding model achieved moderate retrieval
performance (Recall@5 of 0.44, nDCG@10 of 0.41), and an Answer Consistency
score of 0.73 indicates the generated answers are well-grounded in
retrieved context.

**[Read the report](/assets/pdf/RAGPipeline.pdf)**

**[View on GitHub](https://github.com/kdhenderson/rag-system-pg-essays)**

<br>

#### Skills
<small>Python · Transformers · Hugging Face · LoRA fine-tuning · Contrastive learning · FAISS · RAG</small>
