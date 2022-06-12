// !preview r2d3 data= data.frame(x = seq(1,40,1), y = runif(40,40,60), e = runif(40,1,5))
function svg_height() {return parseInt(svg.style('height'))}
function svg_width()  {return parseInt(svg.style('width'))}

var x = d3.scaleLinear()
  .range([0, svg_width()])
  .domain([0, 6]);

var y = d3.scaleLinear()
  .range([svg_height()*.8,svg_height()*.2])
  .domain([0,100]);

  var addData = function() {

    var points = svg.selectAll('circle')
      .data(data);
  
    points.enter()
      .append('circle')
      .attr('r', 2)
    .merge( points )
        .attr('cx', function(d) { return x(d.x); })
        .attr('cy', function(d) { return y(d.y); })
  
    var lines = svg.selectAll('line')
      .data(data);
  
    lines.enter()
      .append('line')
      .attr("stroke", "black")
      .attr("stroke-width", "1px")
      .attr("fill","none")
    .merge(lines)
      .attr('x1', function(d) { return x(d.x); })
      .attr('x2', function(d) { return x(d.x); })
      .attr('y1', function(d) { return y(d.ll); })
      .attr('y2', function(d) { return y(d.ul); });

    var ylabels = svg.selectAll('text')
        .data(data);

    ylabels.enter()
       .append('text')
       .style('font-family', 'sans-serif')
    .merge( ylabels)
       .attr('x', function(d) { return x(d.x + 0.1);  })
       .attr('y', function(d) { return y(d.y);  })
       .text(function(d) {return d.ylabel; });
    
    var labels = svg.selectAll()
          .data(data);
     
    labels.enter()
      .append('text')
      .style('font-family', 'sans-serif')
      .attr('x', function(d) { return x(d.x);  })
      .attr('y',  svg_height()*0.9)
      .text(function(d) {return d.label; });
     
        
  };


addData();


