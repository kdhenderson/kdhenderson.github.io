---
layout: page
title: EM Dataset Access
description: Pipeline for downloading public 3D electron microscopy datasets for ML workflows
img: assets/img/EMDatasetAccess.png
importance: 7
category: data_science
---

<style>
.dataset-table {
  width: 100%;
  border-collapse: collapse;
  margin: 20px 0;
}
.dataset-table th, .dataset-table td {
  padding: 8px 10px;
  text-align: left;
  vertical-align: top;
  border-bottom: 1px solid var(--global-divider-color);
}
.dataset-table th {
  background-color: var(--global-divider-color);
}
</style>

<div id="header">
<h4 class="author">Kristin Henderson</h4>
<h4 class="date">March 2026</h4>
<p><br></p>
</div>

### Overview

A data engineering exercise that downloads public 3D electron microscopy datasets from five different sources and storage protocols (AWS S3, FTP, HTTP, GCS), extracts and consolidates their metadata into a unified table, and outlines a design for block-wise data access to support ML training workflows.

The five datasets span different acquisition methods, image formats, and resolutions:

<table class="dataset-table">
  <thead>
    <tr>
      <th>Dataset</th>
      <th>Source</th>
      <th>Format</th>
      <th>Resolution (nm)</th>
    </tr>
  </thead>
  <tbody>
    <tr><td>OpenOrganelle</td><td>Janelia / AWS S3</td><td>OME-NGFF Zarr</td><td>2.96 x 4 x 4</td></tr>
    <tr><td>EPFL Hippocampus</td><td>EPFL CVLab / HTTP</td><td>Multipage TIFF</td><td>5 x 5 x 5</td></tr>
    <tr><td>EMPIAR-11759</td><td>EBI / FTP</td><td>DM3</td><td>50 x 8 x 8</td></tr>
    <tr><td>IDR idr0086</td><td>IDR / FTP</td><td>TIFF</td><td>20 x 20 x 20</td></tr>
    <tr><td>Hemibrain</td><td>Janelia / GCS</td><td>Neuroglancer precomputed</td><td>8 x 8 x 8</td></tr>
  </tbody>
</table>

**[View the code on GitHub](https://github.com/kdhenderson/em-dataset-access)**

<br>

#### Known limitations and next steps

- Parallel/multi-threaded downloads (currently serial)
- Scrape metadata fields that are currently transcribed manually from dataset landing pages
- Refactor shared logic into reusable dataset classes
- Pin package versions, add automated tests, and validate downloaded volumes against expected dimensions and resolution
- Extend the framework to additional datasets beyond the initial five

<br>

#### Skills
<small>Python · AWS S3 · GCS · FTP · HTTP · ETL pipelines · Metadata consolidation · Zarr</small>
