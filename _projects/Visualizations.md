---
layout: page
title: Visualizing Data
description: Exploring Different Tools, Techniques and Design Principles For Data Visualization
img: assets/img/leesburg_map_1748_1078.png
importance: 3
category: recent
---

<style>
    /* Custom CSS for formatting */
    .paragraph {
        margin-bottom: 10px; /* Add margin bottom for paragraphs */
        font-family: Arial, Helvetica, sans-serif; /* Change font family */
    }
    .figure-container {
        margin-top: 20px; /* Add margin to the top of the figure container */
    }
    .acknowledgment {
        font-size: 12px; /* Adjust font size for acknowledgment text */
        margin-top: 20px; /* Add margin to the top of the acknowledgment */
    }
    .caption {
        text-align: left;
    }
        .negative-margin {
      margin-left: -15px;
      margin-right: -15px;
    }
    .wider-than-12 {
      width: calc(100% + 30px);
      margin-left: -15px;
      margin-right: -15px;
    }
</style>

<div class="row justify-content-center figure-container">
    <div class="col-sm-7 mt-3">
        {% include figure.liquid loading="eager" path="assets/img/leesburg_birds_600.png" title="Figure 1" class="img-fluid rounded z-depth-1" %}
    </div>
</div>

<div class="caption">
    A visualization of the relative frequency of commonly photographed birds uploaded on iNaturalist in parks in the town of Leesburg, VA. This project explores design principles such as hierarchy, balance, space, and color, while considering the key components of datasets: structures, attributes, and dimensionality. Created with Inkscape. Map source: <a href="https://openstreetmap.org/copyright">OpenStreetMap</a>. Photo credits from iNaturalist: American Robin – Dimitris Salas, Eastern Bluebird – Michael Gallo, Turkey Vulture – Seig, White-breasted Nuthatch – SW, Mourning Dove – Donna Pomeroy, Chipping Sparrow - SW.
</div>

<br>

<div class="row justify-content-center figure-container">
    <div class="col-sm-12 mt-3 wider-than-12">
        {% include figure.liquid loading="eager" path="assets/img/excelDashboard1200.png" title="Figure 2" class="img-fluid rounded z-depth-1" %}
    </div>
</div>

<div class="caption">
    An image of an Excel dashboard that uses pivot tables, charts and slicers for workforce analysis and interactive visualization of employee attrition data.
</div>

<br>

<div class="row justify-content-center figure-container">
    <div class="col-md-4 wider-than-12">
        {% include figure.liquid loading="eager" path="assets/img/tableauDashboard1200.png" title="Figure 3" class="img-fluid rounded z-depth-1" %}
    </div>
</div>

<div class="caption">
    An image of a unique self-portrait created with a Tableau dashboard, featuring creative data sources and various chart types. The interactive dashboard, complete with slicers, tells a personal story beyond the typical digital footprint.
</div>