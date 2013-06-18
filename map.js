var width = 626,
    height = 300,
    centered;

var projection = d3.geo.albersUsa()
    .scale(670)
    .translate([width / 2, height / 2]);

var path = d3.geo.path()
    .projection(projection);

var svg = d3.select("#chart").append("svg")
    .attr("width", width)
    .attr("height", height);

svg.append("rect")
    .attr("class", "background")
    .attr("width", width)
    .attr("height", height);

var g = svg.append("g");

var ready = function(error, us, summer) {
  g.append("g")
      .attr("id", "states")
    .selectAll("path")
      .data(topojson.feature(us, us.objects.states).features)
    .enter().append("path")
      .attr("d", path)
      .attr("class", function(d, i) {
        var visited = summer.indexOf(d.id);

        if(visited > -1) {
          return "active";
        } else {
          return "inactive";
        }
      });

  g.append("path")
      .datum(topojson.mesh(us, us.objects.states, function(a, b) { return a !== b; }))
      .attr("id", "state-borders")
      .attr("d", path);
};

queue()
  .defer(d3.json, "/us.json")
  .defer(d3.json, "/us-summer-2013.json")
  .await(ready);
