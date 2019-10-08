// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
//
// r2d3: https://rstudio.github.io/r2d3
//


function brush(cell, circle) {
  const brush = d3.brush()
      .extent([[padding / 2, padding / 2], [size - padding / 2, size - padding / 2]]);

  cell.call(brush)
      .style("touch-action", "none");

  let brushCell;

  // Clear the previously-active brush, if any.
  brush.on("start", function() {
    if (!d3.event.sourceEvent.preventDefault) return;
    d3.event.sourceEvent.preventDefault();
    if (brushCell !== this) {
      d3.select(brushCell).call(brush.move, null);
      brushCell = this;
    }
  });

  // Highlight the selected circles.
  brush.on("brush", ([i, j]) => {
    if (d3.event.selection === null) return;
    const [[x0, y0], [x1, y1]] = d3.event.selection; 
    circle.classed("hidden", d => {
      return x0 > x[i](d[columns[i]])
          || x1 < x[i](d[columns[i]])
          || y0 > y[j](d[columns[j]])
          || y1 < y[j](d[columns[j]]);
    });
  });

  // If the brush is empty, select all circles.
  brush.on("end", () => {
    if (d3.event.selection !== null) return;
    circle.classed("hidden", false);
  });
}

x = columns.map(c => d3.scaleLinear()
    .domain(d3.extent(data, d => d[c]))
    .rangeRound([padding / 2, size - padding / 2]));

y = x.map(x => x.copy().range([size - padding / 2, padding / 2]));

z = d3.scaleOrdinal()
    .domain(data.map(d => d.species))
    .range(d3.schemeCategory10);
    
xAxis = {
  const axis = d3.axisBottom()
      .ticks(6)
      .tickSize(size * columns.length);
  return g => g.selectAll("g").data(x).join("g")
      .attr("transform", (d, i) => `translate(${i * size},0)`)
      .each(function(d) { return d3.select(this).call(axis.scale(d)); })
      .call(g => g.select(".domain").remove())
      .call(g => g.selectAll(".tick line").attr("stroke", "#ddd"));
};

yAxis = {
  const axis = d3.axisLeft()
      .ticks(6)
      .tickSize(-size * columns.length);
  return g => g.selectAll("g").data(y).join("g")
      .attr("transform", (d, i) => `translate(0,${i * size})`)
      .each(function(d) { return d3.select(this).call(axis.scale(d)); })
      .call(g => g.select(".domain").remove())
      .call(g => g.selectAll(".tick line").attr("stroke", "#ddd"));
};

data = d3.csv("https://gist.githubusercontent.com/mbostock/b038321e2a8177baf9e6a547195da966/raw/6c8eb7f5c644be0394f7fc384e42de9fab41927f/iris.csv", d3.autoType);

columns = data.columns.filter(d => d !== "species");

width = 964;

size = (width - (columns.length + 1) * padding) / columns.length + padding;

padding = 20;

d3 = require("d3@5")

{
  const svg = d3.select(DOM.svg(width, width))
      .attr("viewBox", `${-padding} 0 ${width} ${width}`)
      .style("max-width", "100%")
      .style("height", "auto");

  svg.append("style")
      .text(`circle.hidden { fill: #000; fill-opacity: 1; r: 1px; }`);

  svg.append("g")
      .call(xAxis);

  svg.append("g")
      .call(yAxis);

  const cell = svg.append("g")
    .selectAll("g")
    .data(d3.cross(d3.range(columns.length), d3.range(columns.length)))
    .join("g")
      .attr("transform", ([i, j]) => `translate(${i * size},${j * size})`);

  cell.append("rect")
      .attr("fill", "none")
      .attr("stroke", "#aaa")
      .attr("x", padding / 2 + 0.5)
      .attr("y", padding / 2 + 0.5)
      .attr("width", size - padding)
      .attr("height", size - padding);

  cell.each(function([i, j]) {
    d3.select(this).selectAll("circle")
      .data(data)
      .join("circle")
        .attr("cx", d => x[i](d[columns[i]]))
        .attr("cy", d => y[j](d[columns[j]]));
  });

  const circle = cell.selectAll("circle")
      .attr("r", 3.5)
      .attr("fill-opacity", 0.7)
      .attr("fill", d => z(d.species));

  cell.call(brush, circle);

  svg.append("g")
      .style("font", "bold 10px sans-serif")
      .style("pointer-events", "none")
    .selectAll("text")
    .data(columns)
    .join("text")
      .attr("transform", (d, i) => `translate(${i * size},${i * size})`)
      .attr("x", padding)
      .attr("y", padding)
      .attr("dy", ".71em")
      .text(d => d);

  return svg.node();
}

