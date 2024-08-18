---
layout: page
title: Housing Regression
description: Linear Regression Analysis, Interpretation and Prediction of Ames Housing Dataset
img: assets/img/housingRegressionD3.png
importance: 9
category: recent
---
{% raw %}

<html>
<head>
<meta charset="UTF-8">
<title>Animated Scatter Plot with Regression Line</title>
<style>
  body {
    background-color: #3b5999;
    color: #F2F2F2;
    font-family: 'Arial', sans-serif;
  }
  .dot {
    opacity: 0.6;
  }
  .line {
    fill: none;
    stroke-width: 2px;
  }
  .tooltip {
    position: absolute;
    text-align: center;
    width: 80px;
    height: 28px;
    padding: 2px;
    font: 12px 'Arial', sans-serif;
    background: lightsteelblue;
    border: 0px;
    border-radius: 8px;
    pointer-events: none;
  }
  .legend, .axis-label, .chart-title, .regression-equation, .secondary-label {
    font-size: 14px;
    fill: #F2F2F2;
    font-family: 'Arial', sans-serif;
  }
  .chart-title {
    font-size: 24px;
    text-anchor: middle;
  }
  .legend-title {
    fill: white;
    font-weight: normal;
  }
  .legend-item text {
    font-size: 14px;
    font-weight: normal.
  }
  .legend-toggle {
    cursor: pointer.
  }
  .controls {
    font-size: 14px;
    margin-left: 50px;
    margin-top: 10px.
  }
  .regression-equations {
    font-size: 14px;
    fill: #F2F2F2;
    font-family: 'Arial', sans-serif;
    text-anchor: start.
  }
</style>
</head>
<body>
<script src="https://d3js.org/d3.v7.min.js"></script>

<div id="chart"></div>

<div class="controls">
  <label style="color: #F2F2F2;"><input type="checkbox" id="logScale"> Log Scale</label><br>
  <label style="color: #F2F2F2;"><input type="checkbox" id="removeOutliers"> Remove Outliers</label><br>
  <label style="color: #F2F2F2;"><input type="radio" name="regressionType" value="simple" checked> Simple Linear Regression</label><br>
  <label style="color: #F2F2F2;"><input type="radio" name="regressionType" value="parallel"> Multiple Linear Regression (Parallel Slopes)</label><br>
  <label style="color: #F2F2F2;"><input type="radio" name="regressionType" value="independent"> Multiple Linear Regression (Independent Slopes)</label>
</div>

<script>

// Load the data from CSV file
d3.json("train.json").then(data => {
//  // Ensure GrLivArea and SalePrice are numbers
//  data.forEach(d => {
//    d.GrLivArea = +d.GrLivArea;
//    d.SalePrice = +d.SalePrice;
//  });

  // Filter data for the neighborhoods of interest
  const neighborhoods = ["BrkSide", "Edwards", "NAmes"];
  const neighborhoodFullNames = {
    "BrkSide": "Brookside",
    "Edwards": "Edwards",
    "NAmes": "North Ames"
  };
  let filteredData = data.filter(d => neighborhoods.includes(d.Neighborhood));

  // Set up SVG and dimensions
  const width = 800, height = 600, margin = { top: 50, right: 200, bottom: 50, left: 80 };
  const svg = d3.select('#chart').append('svg')
                .attr('width', width + margin.left + margin.right)
                .attr('height', height + margin.top + margin.bottom);

  // Add the background image
  svg.append('image')
     .attr('xlink:href', 'assets/img/processed_house_outline.png')
     .attr('x', -80)
     .attr('y', 0)
     .attr('width', width + margin.left + margin.right)
     .attr('height', height + margin.top + margin.bottom)
     .attr('opacity', 0.05);

  const g = svg.append('g')
                .attr('transform', `translate(${margin.left},${margin.top})`);

  // Set up color and symbol scales
  const colors = d3.scaleOrdinal().domain(neighborhoods).range(["#FF6348", "#69B3A3", "#FFCC01"]);
  const symbols = d3.scaleOrdinal().domain(neighborhoods).range([d3.symbolSquare, d3.symbolCircle, d3.symbolTriangle]);

  // Define scales for X and Y axes
  let x = d3.scaleLinear().domain([300, 6000]).range([0, width]);
  let y = d3.scaleLinear().domain([20000, 400000]).range([height, 0]);

  // Append X and Y axes
  const xAxis = g.append('g').attr('transform', `translate(0,${height})`);
  const yAxis = g.append('g');
  const xAxisTop = g.append('g').attr('transform', `translate(0,${height})`);
  const yAxisRight = g.append('g').attr('transform', `translate(${width},0)`);

  // Set styles for axis lines and text
  g.selectAll("line")
     .attr("stroke", "#F2F2F2")
     .attr("stroke-width", "0.5");

  g.selectAll("text")
     .attr("fill", "#f0f0f0");

  // Add chart title
  g.append('text')
     .attr('class', 'chart-title')
     .attr('x', width / 2)
     .attr('y', -20)
     .text('House Sale Prices by Living Area');

  // Add X axis label
  let xAxisLabel = g.append('text')
     .attr('class', 'axis-label')
     .attr('x', width / 2)
     .attr('y', height + 40)
     .style('text-anchor', 'middle')
     .text('Above Grade Living Area (square feet)');

  // Add Y axis label
  let yAxisLabel = g.append('text')
     .attr('class', 'axis-label')
     .attr('transform', 'rotate(-90)')
     .attr('x', -height / 2)
     .attr('y', -60)
     .style('text-anchor', 'middle')
     .text('Sale Price (dollars)');

  // Create legend
  const legend = g.append('g')
                    .attr('class', 'legend')
                    .attr('transform', `translate(50,40)`);

  legend.append('text')
        .attr('class', 'legend-title')
        .attr('x', 0)
        .attr('y', 0)
        .text('Neighborhood');

  legend.selectAll('.legend-item')
        .data(neighborhoods)
        .enter().append('g')
        .attr('class', 'legend-item')
        .attr('transform', (d, i) => `translate(0,${(i + 1) * 20})`)
        .call(g => {
          g.append('path')
           .attr('d', d3.symbol().type(d => symbols(d)).size(150))
           .attr('transform', 'translate(10, 0)')
           .style('fill', d => colors(d));
          
          g.append('text')
           .attr('x', 30)
           .attr('y', 0)
           .attr('dy', '.35em')
           .style('text-anchor', 'start')
           .text(d => neighborhoodFullNames[d])
           .attr('class', 'legend');
          
          g.append('foreignObject')
           .attr('x', 110)
           .attr('y', -10)
           .attr('width', 20)
           .attr('height', 20)
           .append('xhtml:div')
           .html('<input type="checkbox" class="legend-toggle" checked style="width:14px;height:14px;"/>')
           .on('change', function(event, d) {
             const checked = d3.select(this).select('input').property('checked');
             d3.selectAll(`.dot.${d}`).transition().style('opacity', checked ? 0.6 : 0);
             d3.selectAll(`.line.${d}`).transition().style('opacity', checked ? 1 : 0);
           });
        });

  // Tooltip setup
  const tooltip = d3.select('body').append('div').attr('class', 'tooltip').style('opacity', 0);

  function plotScatterPlot() {
    // Check if log scale and remove outliers options are selected
    const logScale = d3.select("#logScale").property("checked");
    const removeOutliers = d3.select("#removeOutliers").property("checked");

    let plotData = filteredData;

    if (removeOutliers) {
      plotData = removeOutliersFromData(filteredData);
    }

    // Update scales based on log scale selection
    if (logScale) {
      x = d3.scaleLinear().domain([Math.log(300), Math.log(6000)]).range([0, width]);
      y = d3.scaleLinear().domain([Math.log(20000), Math.log(400000)]).range([height, 0]);
      xAxisLabel.text('Log(Above Grade Living Area)');
      yAxisLabel.text('Log(Sale Price)');
    } else {
      x = d3.scaleLinear().domain([300, 6000]).range([0, width]);
      y = d3.scaleLinear().domain([20000, 400000]).range([height, 0]);
      xAxisLabel.text('Above Grade Living Area (square feet)');
      yAxisLabel.text('Sale Price (dollars)');
    }

    // Define tick values and axis calls
    const linearTicksX = d3.ticks(Math.log(300), Math.log(6000), 10).slice(1, -1);
    const linearTicksY = d3.ticks(Math.log(20000), Math.log(400000), 10);
    const logScaleTicksX = [300, 600, 1000, 2000, 3000, 4000, 5000];
    const logScaleTicksY = [50000, 100000, 200000, 300000, 400000];
    
    const xAxisCall = logScale ? d3.axisBottom(x).tickValues(linearTicksX).tickFormat(d => Math.round(d * 100) / 100) : d3.axisBottom(x).ticks(10);
    const xAxisTopCall = logScale ? d3.axisTop(d3.scaleLog().domain([300, 6000]).range([0, width])).tickValues(d3.ticks(300, 6000, 10).slice(1, -1)) : null;
    const yAxisCall = logScale ? d3.axisLeft(y).tickValues(linearTicksY).tickFormat(d => Math.round(d * 100) / 100) : d3.axisLeft(y).ticks(10);
    const yAxisRightCall = logScale ? d3.axisRight(d3.scaleLinear().domain([20000, 400000]).range([height, 0])).tickValues(logScaleTicksY).tickFormat(d3.format("~s")) : null;

    // Update axes
    xAxis.transition().duration(1000).call(xAxisCall);
    yAxis.transition().duration(1000).call(yAxisCall);

    if (logScale) {
      xAxisTop.transition().duration(1000).call(xAxisTopCall);
      yAxisRight.transition().duration(1000).call(yAxisRightCall);
    } else {
      xAxisTop.selectAll('*').remove();
      yAxisRight.selectAll('*').remove();
    }

    // Plot data points
    neighborhoods.forEach((neighborhood, i) => {
      const neighborhoodData = plotData.filter(d => d.Neighborhood === neighborhood);
      neighborhoodData.sort((a, b) => a.GrLivArea - b.GrLivArea);

      let dots = g.selectAll(`.dot.${neighborhood}`)
                  .data(neighborhoodData, d => d.Id);

      dots.exit().remove();

      const dotsEnter = dots.enter().append('path')
          .attr('class', `dot ${neighborhood}`)
          .attr('d', d3.symbol().type(symbols(neighborhood)).size(64))
          .attr('transform', d => `translate(${x(logScale ? Math.log(d.GrLivArea) : d.GrLivArea)}, ${y(logScale ? Math.log(d.SalePrice) : d.SalePrice)})`)
          .style('fill', colors(neighborhood))
          .style('opacity', 0)
          .on('mouseover', function(event, d) {
             tooltip.transition().duration(200).style('opacity', .9);
             tooltip.html(`${d.GrLivArea} sq.ft.<br>$${d.SalePrice}`)
                    .style('left', (event.pageX + 5) + 'px')
                    .style('top', (event.pageY - 28) + 'px');
           })
           .on('mouseout', function(d) {
             tooltip.transition().duration(500).style('opacity', 0);
           });

      dotsEnter.transition()
          .duration(800)
          .delay((d, j) => i * 600 + j * 30)
          .attr('transform', d => `translate(${x(logScale ? Math.log(d.GrLivArea) : d.GrLivArea)}, ${y(logScale ? Math.log(d.SalePrice) : d.SalePrice)})`)
          .style('opacity', 0.6);

      dots.transition()
          .duration(1000)
          .attr('transform', d => `translate(${x(logScale ? Math.log(d.GrLivArea) : d.GrLivArea)}, ${y(logScale ? Math.log(d.SalePrice) : d.SalePrice)})`);
    });

    drawRegressionLines();
  }

  function drawRegressionLines() {
    // Check if log scale and remove outliers options are selected
    const logScale = d3.select("#logScale").property("checked");
    const removeOutliers = d3.select("#removeOutliers").property("checked");
    const regressionType = d3.select('input[name="regressionType"]:checked').node().value;

    let plotData = filteredData;
    if (removeOutliers) {
      plotData = removeOutliersFromData(filteredData);
    }

    // Remove existing regression lines and equations
    g.selectAll('.line').remove();
    d3.selectAll('.regression-equation').remove();
    
    if (regressionType === "simple") {
      const regressionData = plotData.map(d => [logScale ? Math.log(d.GrLivArea) : d.GrLivArea, logScale ? Math.log(d.SalePrice) : d.SalePrice]);
      
      const regression = linearRegression(regressionData.map(d => d[0]), regressionData.map(d => d[1]));
      const line = d3.line()
        .x(d => x(d[0]))
        .y(d => y(d[1]));

      const regressionLineData = [
        [d3.min(regressionData, d => d[0]), regression.predict(d3.min(regressionData, d => d[0]))],
        [d3.max(regressionData, d => d[0]), regression.predict(d3.max(regressionData, d => d[0]))]
      ];

      g.append('path')
        .datum(regressionLineData)
        .attr('class', 'line')
        .attr('d', line)
        .style('stroke', 'white')
        .style('stroke-dasharray', function() { return this.getTotalLength(); })
        .style('stroke-dashoffset', function() { return this.getTotalLength(); })
        .transition()
        .duration(4000)
        .style('stroke-dashoffset', 0);

      g.append('text')
        .attr('class', 'regression-equation')
        .attr('x', width - 150)
        .attr('y', height - 40)
        .style('text-anchor', 'end')
        .style('fill', 'white')
        .text(logScale ? `log(Price) = ${regression.slope.toFixed(2)} * log(Area) + ${regression.intercept.toFixed(2)}` : `Price = ${regression.slope.toFixed(2)} * Area + ${regression.intercept.toFixed(2)}`);

    } else if (regressionType === "parallel") {
      const overallRegressionData = plotData.map(d => [logScale ? Math.log(d.GrLivArea) : d.GrLivArea, logScale ? Math.log(d.SalePrice) : d.SalePrice]);

      const overallRegression = linearRegression(overallRegressionData.map(d => d[0]), overallRegressionData.map(d => d[1]));
      neighborhoods.forEach((neighborhood, i) => {
        const neighborhoodData = plotData.filter(d => d.Neighborhood === neighborhood);
        const line = d3.line()
          .x(d => x(d[0]))
          .y(d => y(d[1]));

        const interceptAdjustment = (logScale ? Math.log(neighborhoodData[0].SalePrice) : neighborhoodData[0].SalePrice) - overallRegression.predict(logScale ? Math.log(neighborhoodData[0].GrLivArea) : neighborhoodData[0].GrLivArea);

        const regressionLineData = [
          [d3.min(neighborhoodData, d => logScale ? Math.log(d.GrLivArea) : d.GrLivArea), overallRegression.predict(logScale ? Math.log(d3.min(neighborhoodData, d => d.GrLivArea)) : d3.min(neighborhoodData, d => d.GrLivArea)) + interceptAdjustment],
          [d3.max(neighborhoodData, d => logScale ? Math.log(d.GrLivArea) : d.GrLivArea), overallRegression.predict(logScale ? Math.log(d3.max(neighborhoodData, d => d.GrLivArea)) : d3.max(neighborhoodData, d => d.GrLivArea)) + interceptAdjustment]
        ];

        g.append('path')
          .datum(regressionLineData)
          .attr('class', `line ${neighborhood}`)
          .attr('d', line)
          .style('stroke', colors(neighborhood))
          .style('stroke-dasharray', function() { return this.getTotalLength(); })
          .style('stroke-dashoffset', function() { return this.getTotalLength(); })
          .transition()
          .duration(4000)
          .style('stroke-dashoffset', 0);

        g.append('text')
          .attr('class', 'regression-equation')
          .attr('x', width - 150)
          .attr('y', height - 40 - (20 * i))
          .style('text-anchor', 'end')
          .style('fill', colors(neighborhood))
          .text(logScale ? `log(Price) = ${overallRegression.slope.toFixed(2)} * log(Area) + ${(logScale ? Math.exp(interceptAdjustment) : interceptAdjustment).toFixed(2)}` : `Price = ${overallRegression.slope.toFixed(2)} * Area + ${interceptAdjustment.toFixed(2)}`);
      });

    } else {
      neighborhoods.forEach((neighborhood, i) => {
        const neighborhoodData = plotData.filter(d => d.Neighborhood === neighborhood);
        const regressionData = neighborhoodData.map(d => [logScale ? Math.log(d.GrLivArea) : d.GrLivArea, logScale ? Math.log(d.SalePrice) : d.SalePrice]);

        const regression = linearRegression(regressionData.map(d => d[0]), regressionData.map(d => d[1]));
        const line = d3.line()
          .x(d => x(d[0]))
          .y(d => y(d[1]));

        const regressionLineData = [
          [d3.min(regressionData, d => d[0]), regression.predict(d3.min(regressionData, d => d[0]))],
          [d3.max(regressionData, d => d[0]), regression.predict(d3.max(regressionData, d => d[0]))]
        ];

        g.append('path')
          .datum(regressionLineData)
          .attr('class', `line ${neighborhood}`)
          .attr('d', line)
          .style('stroke', colors(neighborhood))
          .style('stroke-dasharray', function() { return this.getTotalLength(); })
          .style('stroke-dashoffset', function() { return this.getTotalLength(); })
          .transition()
          .duration(4000)
          .style('stroke-dashoffset', 0);

        g.append('text')
          .attr('class', 'regression-equation')
          .attr('x', width - 150)
          .attr('y', height - 40 - (20 * i))
          .style('text-anchor', 'end')
          .style('fill', colors(neighborhood))
          .text(logScale ? `log(Price) = ${regression.slope.toFixed(2)} * log(Area) + ${regression.intercept.toFixed(2)}` : `Price = ${regression.slope.toFixed(2)} * Area + ${regression.intercept.toFixed(2)}`);
      });
    }
  }

  function removeOutliersFromData(data) {
    // Example outlier removal logic: removing points beyond 3 standard deviations
    const grLivAreaValues = data.map(d => d.GrLivArea);
    const salePriceValues = data.map(d => d.SalePrice);

    const grLivAreaMean = d3.mean(grLivAreaValues);
    const grLivAreaStdDev = d3.deviation(grLivAreaValues);
    const salePriceMean = d3.mean(salePriceValues);
    const salePriceStdDev = d3.deviation(salePriceValues);

    return data.filter(d =>
      Math.abs(d.GrLivArea - grLivAreaMean) <= 3 * grLivAreaStdDev &&
      Math.abs(d.SalePrice - salePriceMean) <= 3 * salePriceStdDev
    );
  }

  plotScatterPlot();

  function updateScales() {
    plotScatterPlot();
  }

  d3.select("#logScale").on("change", updateScales);
  d3.select("#removeOutliers").on("change", plotScatterPlot);
  d3.selectAll('input[name="regressionType"]').on('change', drawRegressionLines);

}).catch(error => {
  console.error('Error loading or processing data:', error);
});

function linearRegression(x, y) {
  // Calculate slope and intercept for linear regression
  const n = x.length;
  const sumX = d3.sum(x);
  const sumY = d3.sum(y);
  const sumXY = d3.sum(x.map((xi, i) => xi * y[i]));
  const sumX2 = d3.sum(x.map(xi => xi * xi));

  const slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX);
  const intercept = (sumY - slope * sumX) / n;

  return {
    slope,
    intercept,
    predict: x => slope * x + intercept
  };
}

</script>
</body>
</html>

{% endraw %}