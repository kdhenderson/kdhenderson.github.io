---
layout: page
title: Osteoporosis Fracture Risk
description: Logistic regression, LDA, and random forest for fracture risk in women with osteoporosis
img: assets/img/OsteoporosisFractureRisk.png
importance: 11
category: data_science
---

<div id="header">
<h4 class="author">Kristin Henderson</h4>
<h4 class="date">Fall 2024</h4>
<p><br></p>
</div>

### Overview

A two-part logistic regression analysis of fracture risk in women with osteoporosis, completed for SMU's MSDS Applied Statistics course. The data come from the GLOW bone health study, with 500 women and 18 variables on demographics, medical history, and bone medication use. The outcome is whether a patient had a fracture in the first year after enrolling in the study (25% did).

The first objective is to identify which factors most affect a woman's odds of fracture and quantify how much each one matters. Once the other variables in the model are accounted for, maternal history of hip fracture and the composite fracture risk score are the strongest predictors.

The second objective is to compare three modeling approaches and see which predicts best: a logistic regression with interaction terms, discriminant analysis (LDA), and a random forest. Each was evaluated on AUROC, log loss, and standard threshold-based classification metrics. The logistic regression with interactions performed best on the validation split, with the simpler additive model close behind. LDA and random forest underperformed without further tuning, which suggests that for a sample this small, logistic regression is the more reliable choice.

<iframe width="100%" height="400"
  src="https://www.youtube.com/embed/ZiiIy8oOG_4"
  frameborder="0"
  allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
  allowfullscreen></iframe>

<br>

**[View the full analysis notebook](/assets/html/OsteoporosisFractureRisk.html)**

<br>

#### Skills
<small>R · Logistic regression · LDA · Random Forest · Feature selection · Threshold tuning</small>
