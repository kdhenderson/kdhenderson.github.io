---
layout: page
title: Applied ML Case Studies
description: Seven case studies in regression, classification, ensembles, and deep learning
img: assets/img/AppliedMLCaseStudies.png
importance: 5
category: data_science
---

<style>
.cs-table {
  width: 100%;
  border-collapse: collapse;
  margin: 20px 0;
}
.cs-table th, .cs-table td {
  padding: 8px 10px;
  text-align: left;
  vertical-align: top;
  border-bottom: 1px solid #ddd;
  font-size: 0.875rem !important;
  line-height: 1.4;
}
.cs-table th {
  background-color: #f5f5f5;
  font-size: 0.875rem !important;
}
.cs-table td.links a {
  display: inline-block;
  margin-right: 8px;
}
</style>

### Overview

Seven applied case studies in statistical machine learning, completed in a graduate-level course taken near the end of the SMU MS in Data Science program. Each tackles a different kind of problem with the methods that suit it best: regularized regression for a high-dimensional materials dataset, logistic regression with multiple imputation on messy clinical data, a Naive Bayes spam filter, ensemble methods for imbalanced bankruptcy prediction, SVMs and SGD for multi-class network traffic, deep neural networks for high-energy physics event classification, and cost-sensitive learning where false positives and false negatives carry different price tags.

Every case study includes a Jupyter notebook with the full analysis and a LaTeX-typeset report summarizing the methodology, results, and interpretation.

**[View the full repository on GitHub](https://github.com/kdhenderson/applied_stat_ml_case_studies)**

### Case studies at a glance

<table class="cs-table">
  <thead>
    <tr>
      <th>#</th>
      <th>Problem</th>
      <th>Methods</th>
      <th>Dataset</th>
      <th>Links</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>1</td>
      <td>Predict critical temperature of superconductors from material properties</td>
      <td>Linear regression, LASSO, Ridge, ElasticNet; cross-validation; residual diagnostics</td>
      <td>Superconductor materials (UCI, 21K rows, 82 features)</td>
      <td class="links">
        <a href="https://github.com/kdhenderson/applied_stat_ml_case_studies/blob/main/case_study_01_linear_regression_regularization_superconductor/case_study_01_analysis.ipynb">Notebook</a>
        <a href="https://github.com/kdhenderson/applied_stat_ml_case_studies/blob/main/case_study_01_linear_regression_regularization_superconductor/case_study_01_report.pdf">Report</a>
      </td>
    </tr>
    <tr>
      <td>2</td>
      <td>Predict hospital readmission risk among diabetic patients (within 30 days, after 30 days, or none)</td>
      <td>Logistic regression, multiple imputation, multiclass classification, precision-recall curves</td>
      <td>Diabetes hospital readmissions (UCI, 101K encounters, 49 features)</td>
      <td class="links">
        <a href="https://github.com/kdhenderson/applied_stat_ml_case_studies/blob/main/case_study_02_logistic_regression_imputation_diabetes/case_study_02_analysis.ipynb">Notebook</a>
        <a href="https://github.com/kdhenderson/applied_stat_ml_case_studies/blob/main/case_study_02_logistic_regression_imputation_diabetes/case_study_02_report.pdf">Report</a>
      </td>
    </tr>
    <tr>
      <td>3</td>
      <td>Classify spam email and group documents by topic</td>
      <td>Naive Bayes, bag-of-words, TF-IDF, K-Means clustering</td>
      <td>SpamAssassin emails (~9K messages)</td>
      <td class="links">
        <a href="https://github.com/kdhenderson/applied_stat_ml_case_studies/blob/main/case_study_03_naive_bayes_clustering_spam/case_study_03_analysis.ipynb">Notebook</a>
        <a href="https://github.com/kdhenderson/applied_stat_ml_case_studies/blob/main/case_study_03_naive_bayes_clustering_spam/case_study_03_report.pdf">Report</a>
      </td>
    </tr>
    <tr>
      <td>4</td>
      <td>Predict corporate bankruptcy from financial indicators with severe class imbalance</td>
      <td>XGBoost, Random Forest, class weighting, stratified cross-validation, ROC/AUC evaluation, hyperparameter tuning</td>
      <td>Polish company bankruptcy (UCI, 43K records, 64 features)</td>
      <td class="links">
        <a href="https://github.com/kdhenderson/applied_stat_ml_case_studies/blob/main/case_study_04_xgboost_randomForest_bankruptcy/case_study_04_analysis.ipynb">Notebook</a>
        <a href="https://github.com/kdhenderson/applied_stat_ml_case_studies/blob/main/case_study_04_xgboost_randomForest_bankruptcy/case_study_04_report.pdf">Report</a>
      </td>
    </tr>
    <tr>
      <td>5</td>
      <td>Multi-class classification of firewall actions from network traffic features</td>
      <td>SVMs (linear and RBF kernels), SGD-based logistic regression, class weighting, feature scaling</td>
      <td>Internet firewall log data (UCI, 65K records, 11 features)</td>
      <td class="links">
        <a href="https://github.com/kdhenderson/applied_stat_ml_case_studies/blob/main/case_study_05_svm_sgd_networkTraffic/case_study_05_analysis.ipynb">Notebook</a>
        <a href="https://github.com/kdhenderson/applied_stat_ml_case_studies/blob/main/case_study_05_svm_sgd_networkTraffic/case_study_05_report.pdf">Report</a>
      </td>
    </tr>
    <tr>
      <td>6</td>
      <td>Distinguish particle-physics signal events from background</td>
      <td>Feedforward neural networks in PyTorch and PyTorch Lightning, dropout, learning-rate scheduling</td>
      <td>HEPMASS (UCI, ~7M events, 28 features)</td>
      <td class="links">
        <a href="https://github.com/kdhenderson/applied_stat_ml_case_studies/blob/main/case_study_06_neuralNetworks_pytorch_hepmass/case_study_06_analysis.ipynb">Notebook</a>
        <a href="https://github.com/kdhenderson/applied_stat_ml_case_studies/blob/main/case_study_06_neuralNetworks_pytorch_hepmass/case_study_06_report.pdf">Report</a>
      </td>
    </tr>
    <tr>
      <td>7</td>
      <td>Binary classification with asymmetric misclassification costs</td>
      <td>XGBoost, neural network, out-of-fold cross-validation, threshold tuning to minimize total cost</td>
      <td>Anonymized binary classification data (160K records, 50 features)</td>
      <td class="links">
        <a href="https://github.com/kdhenderson/applied_stat_ml_case_studies/blob/main/case_study_07_xgboost_nn_costSensitive_tunedThresholds/case_study_07_analysis.ipynb">Notebook</a>
        <a href="https://github.com/kdhenderson/applied_stat_ml_case_studies/blob/main/case_study_07_xgboost_nn_costSensitive_tunedThresholds/case_study_07_report.pdf">Report</a>
      </td>
    </tr>
  </tbody>
</table>

### What this collection demonstrates

- **Method selection.** Each problem is matched to a model family that fits its structure: regularization for high dimensions, imputation for missingness, ensembles for imbalance, neural nets for scale, threshold tuning for cost asymmetry.
- **Full analysis cycle.** Data cleaning, feature engineering, cross-validation, hyperparameter tuning, diagnostics, and interpretation, all the way to a written report.
- **Tool breadth.** scikit-learn, XGBoost, PyTorch, PyTorch Lightning, statsmodels, and the standard Python data stack.
- **Communication.** Every project includes a LaTeX-typeset PDF report aimed at a technical reader.
- **Concrete outcomes.** A Swish-activation neural network reached 79% accuracy on the HEPMASS independent test set; threshold tuning on a cost-sensitive task cut total misclassification cost by 50% vs. baseline.

<br>

#### Skills
<small>Python · scikit-learn · XGBoost · PyTorch · Machine learning · Statistical modeling</small>
