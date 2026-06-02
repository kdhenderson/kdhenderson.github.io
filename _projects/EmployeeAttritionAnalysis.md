---
layout: page
title: Employee Attrition
description: Classification of employee attrition and regression model for monthly income
img: assets/img/EmployeeAttritionAnalysis.png
importance: 12
category: data_science
---

<div id="header">
<h4 class="author">Kristin Henderson</h4>
<h4 class="date">Spring 2024</h4>
<p><br></p>
</div>

### Overview

A two-part analysis of employee attrition and compensation, completed for SMU's MSDS Doing Data Science course. The dataset is a teaching case study framed around Frito-Lay, with 870 employees and 36 variables on demographics, role, tenure, satisfaction, and pay. About 16% of employees left, with attrition concentrated in job level 1, where more than a quarter left.

The first objective is to identify what drives attrition. EDA, t-tests on numerical variables, and chi-square tests on categorical variables narrowed the 36 features to a smaller candidate set. I then compared Naive Bayes and k-nearest neighbors classifiers, tuning over feature combinations, the smoothing parameter (Naive Bayes), the number of neighbors (kNN), and the decision threshold. The top three drivers of attrition were job level, monthly income, and overtime. Naive Bayes was the chosen model and produced the labels for the held-out competition set.

The second objective fits a linear regression for monthly income, comparing forward, backward, and stepwise selection over the full feature set plus two-way interaction terms, evaluated with cross-validated PRESS and held-out RMSE. The final ten-term model predicts monthly income within about $1,000 on held-out data.

<br>

<iframe width="100%" height="400"
  src="https://www.youtube.com/embed/aY4CYfuHOf4"
  frameborder="0"
  allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
  allowfullscreen></iframe>

<br>

**[View the full analysis notebook](/assets/html/EmployeeAttritionAnalysis.html)**

**[View on GitHub](https://github.com/kdhenderson/EmployeeAttritionAnalysis)**

**Interactive companion**

<small>*First load may take 10 to 30 seconds while the app wakes (shinyapps.io free tier).*</small>

<div id="shiny-embed-wrap" style="position: relative; width: 100%; overflow: hidden;">
  <iframe id="shiny-embed-frame"
    src="https://kdhenderson.shinyapps.io/Employee_Attrition_and_Income/"
    style="position: absolute; top: 0; left: 0; width: 1200px; height: 950px; border: 0; transform-origin: top left;"></iframe>
</div>
<script>
(function () {
  var DESIGN_W = 1200, DESIGN_H = 950;
  var wrap = document.getElementById('shiny-embed-wrap');
  var frame = document.getElementById('shiny-embed-frame');
  function update() {
    var w = wrap.clientWidth;
    var s = w / DESIGN_W;
    frame.style.transform = 'scale(' + s + ')';
    wrap.style.height = (DESIGN_H * s) + 'px';
  }
  update();
  window.addEventListener('resize', update);
})();
</script>

<div class="caption">
  Explore monthly income by job role and attrition group. Built in R Shiny. <a href="https://kdhenderson.shinyapps.io/Employee_Attrition_and_Income/" target="_blank" rel="noopener">Open the app in a new tab</a> for a full-size view.
</div>

<br>

#### Skills
<small>R · R Shiny · EDA · Naive Bayes · k-NN · Linear regression · Variable selection · Hypothesis testing · Cross-validation</small>
