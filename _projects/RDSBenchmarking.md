---
layout: page
title: AWS RDS Benchmarking
description: Benchmarking PostgreSQL vs MySQL on AWS RDS with HammerDB TPC-C workloads
img: assets/img/RDSBenchmarking.png
importance: 6
category: data_science
---

<div id="header">
<h4 class="author">Kristin Henderson and Jaren Shead</h4>
<h4 class="date">Fall 2024</h4>
<p><br></p>
</div>

### Overview

This team project compared PostgreSQL and MySQL performance on Amazon RDS under identical configurations. Benchmarks ran with HammerDB 4.12 on an EC2 t2.micro instance, using a TPC-C transactional workload which simulates an online wholesale supplier. Both databases used the same configuration: db.t4g.micro instances with 20 GiB of gp3 storage. CloudWatch recorded CPU, IOPS, and memory before, during, and after each run.

On this write-heavy workload, MySQL ran roughly 4.5 times more transactions per minute than PostgreSQL (4,058 vs. 888), and with less than half the latency (179.6 ms vs. 448.5 ms). The two databases are built for different priorities. MySQL's InnoDB storage engine is tuned for fast writes and handling many transactions at once. PostgreSQL uses Write-Ahead Logging (WAL) to protect data through crashes, and its query planner is built to handle more complex queries.

The right database depends on the workload. MySQL is a good fit for write-heavy or latency-sensitive applications. PostgreSQL works better when reads or complex queries are more common.

**[Read the paper](/assets/pdf/RDSBenchmarking.pdf)**

<br>

#### Results at a glance

{% include figure.liquid loading="eager" path="assets/img/RDSBenchmarking_scorecard.png" title="Benchmark results: PostgreSQL vs MySQL on AWS RDS" class="img-fluid rounded z-depth-1" %}

<br>

#### Skills
<small>AWS · Amazon RDS · EC2 · CloudWatch · PostgreSQL · MySQL · HammerDB · Performance benchmarking</small>
