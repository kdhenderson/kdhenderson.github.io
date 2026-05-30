---
layout: page
title: Housing Regression
description: Linear regression analysis and prediction of home sale prices in the Ames housing dataset
img: assets/img/HousingRegression.png
importance: 10
category: data_science
---

<div id="header">
<h4 class="author">Kristin Henderson</h4>
<h4 class="date">Summer 2024</h4>
<p><br></p>
</div>

### Overview

A two-part regression analysis of the Ames, Iowa housing dataset (De Cock, 2011), completed for SMU's MSDS Statistical Foundations course. The first part estimates the relationship between sale price and above-ground living area in three Ames neighborhoods (North Ames, Edwards, and Brookside), using log transformations, residual diagnostics, influential-point analysis (Cook's D and leverage), and an interaction term to test whether the price-area relationship depends on neighborhood. The second part builds a predictive model for sale price across all of Ames, comparing forward, backward, stepwise, and custom variable-selection strategies and evaluating models by adjusted R², cross-validated PRESS, AIC, and Kaggle leaderboard score. My best model achieved a Kaggle RMSLE of 0.136.

<div style="display: flex; align-items: flex-start; gap: 1rem; flex-wrap: wrap; margin: 1.5rem 0;">
  <p style="margin: 0;"><strong><a href="/assets/pdf/HousingRegression.pdf">Read the paper</a></strong></p>
  <a href="/assets/pdf/HousingRegression.pdf" style="display: inline-block;">
    <img src="/assets/img/HousingRegressionPaper.png" alt="First page of the housing regression paper" style="max-width: 130px; height: auto;" class="rounded z-depth-1" />
  </a>
</div>

**Interactive companion**

<div>
  {% include HousingRegression.html %}
</div>

<div class="caption">
  Toggle neighborhoods, switch to log scale, remove outliers, and compare simple, parallel-slopes, and independent-slopes linear regression fits. Built on an <a href="https://kdhenderson.shinyapps.io/RegressionOfHousingPricesOnSquareFootage/">earlier R Shiny version</a> of this tool. Data: <a href="https://www.kaggle.com/c/house-prices-advanced-regression-techniques">Kaggle — House Prices: Advanced Regression Techniques</a>.
</div>

<br>

#### Skills
<small>R · SAS · R Shiny · D3.js · Linear regression · Variable selection · Residual diagnostics · Predictive modeling</small>
