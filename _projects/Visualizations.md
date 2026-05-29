---
layout: page
title: Visualizing Data
description: Exploring different tools, techniques, and design principles for data visualization
img: assets/img/Visualizations.jpg
importance: 9
category: data_science
---

<div id="header">
<h4 class="author">Kristin Henderson</h4>
<h4 class="date">Summer 2024</h4>
<p><br></p>
</div>

### Overview

This project explores different tools, techniques, and design principles
for data visualization through four pieces: an interactive D3 cityscape
on global poverty, a hand-designed bird-frequency map of local parks, a
Tableau dashboard built as a personal self-portrait, and an Excel
workforce dashboard for employee attrition. Together they cover
interactive web-based visualization, design and layout, dashboard
design, and visual communication.

<style>
    .figure-container {
        margin-top: 20px;
    }
    .caption {
        text-align: left;
    }
</style>

<div class="row justify-content-center figure-container">
    <div class="col-sm-12 mt-3">
        {% include CityscapePoverty.html %}
    </div>
</div>

<div class="caption">
    An interactive D3 visualization of global poverty across 166 countries. Building height shows population, width shows GDP per capita, and window colors show the proportion of people above (yellow) and below (gray) the national poverty line. Stars show mean income (size) and the national poverty line (height). A solo D3 remake with water reflections, building on a p5.js group project (Adam E., Kristin H., Max P., Kenya R.) heavily adapted from an original design by 'ingriddoubleday'.
</div>

<br>

<div class="row justify-content-center figure-container">
    <div class="col-sm-7 mt-3">
        {% include figure.liquid loading="eager" path="assets/img/leesburg_birds_600.png" title="Elements of Design Visualization" class="img-fluid rounded z-depth-1" %}
    </div>
</div>

<div class="caption">
    A visualization of the relative frequency of commonly photographed birds uploaded on iNaturalist in parks in Leesburg, Virginia. This piece explores design principles such as hierarchy, balance, space, and color. Created with Inkscape. Map source: <a href="https://openstreetmap.org/copyright">OpenStreetMap</a>. Photo credits from iNaturalist: American Robin – Dimitris Salas, Eastern Bluebird – Michael Gallo, Turkey Vulture – Seig, White-breasted Nuthatch – SW, Mourning Dove – Donna Pomeroy, Chipping Sparrow – SW.
</div>

<br>

<div class="row justify-content-center figure-container">
    <div class="col-sm-12 mt-3">
        {% include figure.liquid loading="eager" path="assets/img/tableauDashboard1200.png" title="Tableau Dashboard" class="img-fluid rounded z-depth-1" %}
    </div>
</div>

<div class="caption">
    A self-portrait built as an interactive Tableau dashboard, combining creative data sources, a mix of chart types, and slicers to tell a personal story that goes beyond the typical digital footprint.
</div>

<br>

<div class="row justify-content-center figure-container">
    <div class="col-sm-12 mt-3">
        {% include figure.liquid loading="eager" path="assets/img/excelDashboardV2_600.png" title="Excel Dashboard" class="img-fluid rounded z-depth-1" %}
    </div>
</div>

<div class="caption">
    An Excel dashboard for workforce analysis, using pivot tables, charts, and slicers to provide an interactive view of employee attrition data.
</div>

<br>

#### Skills
<small>D3.js · Tableau · Excel · Inkscape · Interactive data visualization · Design principles</small>
