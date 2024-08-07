---
layout: page
title: Mining Infographic
description: An Infographic on Cobalt Mining in the Congo with Click-through to a Tableau Dashboard
img: assets/img/cobaltMiningBackground.png 
importance: 7
category: recent
---

<style>
    body {
        margin: 0;
        overflow: hidden;
        font-family: 'Staatliches', sans-serif;
    }
    .container {
        position: relative;
        width: 100%;
        height: 100vh;
    }
    #background {
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        z-index: -1;
    }
    .title-container {
        position: absolute;
        top: 5%;
        left: 10%;
        width: 75%;
        text-align: left;
        z-index: 1;
    }
    .title {
        font-family: 'Staatliches', sans-serif;
        font-size: 32px;
        color: #8B4513;
    }
    .subtitle {
        font-family: 'Staatliches', sans-serif;
        font-size: 18px;
        color: #8B4513;
        font-style: italic;
        margin-top: 5px;
    }
    svg {
        position: absolute;
        top: 20%;
        left: 10%;
        width: 75%;
        height: 70%;
    }
    .bar {
        fill: green;
        opacity: 0.5;
        cursor: pointer;
    }
    .axis text {
        fill: #FDF5CA;
        font-size: 18px;
        font-family: 'Staatliches', sans-serif;
    }
    .bar-label {
        fill: #FDF5CA;
        font-family: 'Staatliches', sans-serif;
        font-size: 16px;
    }
</style>

<div class="container">
    <img id="background" src="{{ site.baseurl }}/assets/img/cobaltMiningBackground.png" alt="Mining Site">
    <div class="title-container">
        <div class="title">Daily Wages (USD) of Cobalt Miners in Congo</div>
        <div class="subtitle">click bars for details</div>
    </div>
    <svg></svg>
</div>

<script src="https://d3js.org/d3.v7.min.js"></script>
<script>
    const data = [
        { category: '0-4', percentage: 40 },
        { category: '4-10', percentage: 28 },
        { category: '10-15', percentage: 11 },
        { category: '15-20', percentage: 12 },
        { category: '20-50', percentage: 9 }
    ];

    const tableauDashboardURL = 'https://public.tableau.com/app/profile/kristin.henderson/viz/miningDashboard/MiningDashboard';

    const svg = d3.select("svg");
    const margin = { top: 60, right: 20, bottom: 30, left: 60 };
    const width = parseInt(svg.style("width")) - margin.left - margin.right;
    const height = parseInt(svg.style("height")) - margin.top - margin.bottom;
    const g = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

    const x = d3.scaleBand()
        .domain(data.map(d => d.category))
        .rangeRound([0, width])
        .padding(0.4);

    const y = d3.scaleLinear()
        .domain([0, d3.max(data, d => d.percentage)]).nice()
        .rangeRound([height, 0]);

    g.append("g")
        .attr("class", "axis axis--x")
        .attr("transform", `translate(0,${height})`)
        .call(d3.axisBottom(x).tickSize(0).tickPadding(10))
        .selectAll("text")
        .style("fill", "none");

    g.append("g")
        .attr("class", "axis axis--y")
        .call(d3.axisLeft(y).ticks(10).tickSize(-width).tickFormat(d => d + "%"))
        .selectAll("text")
        .style("fill", "#FDF5CA")
        .style("font-size", "18px")
        .style("font-family", 'Staatliches', 'sans-serif');

    g.selectAll(".bar")
        .data(data)
        .enter().append("rect")
        .attr("class", "bar")
        .attr("x", d => x(d.category))
        .attr("y", d => y(d.percentage))
        .attr("width", x.bandwidth())
        .attr("height", d => height - y(d.percentage))
        .on("click", function() {
            window.open(tableauDashboardURL, '_blank');
        });

    g.selectAll(".bar-label")
        .data(data)
        .enter().append("text")
        .attr("class", "bar-label")
        .attr("x", d => x(d.category) + x.bandwidth() / 2)
        .attr("y", d => y(d.percentage) + 20)
        .attr("text-anchor", "middle")
        .text(d => `${d.category}`);

    g.append("text")
        .attr("class", "bar-label")
        .attr("x", x(data[0].category) + x.bandwidth() / 2)
        .attr("y", y(data[0].percentage) + 40)
        .attr("text-anchor", "middle")
        .text("below minimum wage");

    d3.select("#background").on("click", function() {
        window.open(tableauDashboardURL, '_blank');
    });

    window.addEventListener("resize", () => {
        const width = window.innerWidth * 0.75 - margin.left - margin.right;
        const height = window.innerHeight * 0.75 - margin.top - margin.bottom;
        svg.attr("width", window.innerWidth * 0.75).attr("height", window.innerHeight * 0.75);
        g.attr("transform", `translate(${margin.left},${margin.top})`);
        x.rangeRound([0, width]);
        y.rangeRound([height, 0]);
        g.selectAll(".bar")
            .attr("x", d => x(d.category))
            .attr("width", x.bandwidth())
            .attr("height", d => height - y(d.percentage))
            .attr("y", d => y(d.percentage));
        g.selectAll(".bar-label")
            .attr("x", d => x(d.category) + x.bandwidth() / 2)
            .attr("y", d => y(d.percentage) + 20)
            .text(d => `${d.category}`);
        g.select(".axis--x").call(d3.axisBottom(x).tickSize(0).tickPadding(10));
        g.select(".axis--y").call(d3.axisLeft(y).ticks(10).tickSize(-width).tickFormat(d => d + "%"));
    });
</script>
