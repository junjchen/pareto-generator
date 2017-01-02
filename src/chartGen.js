window.Renderer = (function (D3) {

    function normalizeData(data) {
        var sum = data
            .sort(function (x, y) {
                return y.v - x.v;
            })
            .reduce(function (m, x) {
                m += x.v;
                return m;
            }, 0);
        return data.reduce(function (m, x, i) {
            var l = i === 0 ? 0 : m[i - 1].p;
            m.push(Object.assign(x, {
                p: l + Math.round(x.v / sum * 100) / 100
            }));
            return m;
        }, []);
    };

    function render(handle, data) {
        if (data == null || data.length < 1) {
            return;
        }

        data = normalizeData(data);

        var svg = D3.select(handle);
        svg.selectAll("*").remove();

        var margin = { top: 20, right: 40, bottom: 40, left: 40 };
        var width = +svg.attr("width") - margin.left - margin.right;
        var height = +svg.attr("height") - margin.top - margin.bottom;

        var x = D3.scaleBand().range([0, width]).padding(0.1);
        var y = D3.scaleLinear().range([height, 0]);
        var y2 = D3.scaleLinear().range([height, 0]);
        var g = svg.append("g").attr("transform", "translate(" + margin.left + "," + margin.top + ")");

        x.domain(data.map(function (x) { return x.n; }));
        y.domain([0, D3.max(data, function (d) { return d.v; })]);
        y2.domain([0, 1]);

        g.append("g")
            .attr("class", "axis axis--x")
            .attr("transform", "translate(0," + height + ")")
            .call(D3.axisBottom(x));

        g.append("g")
            .attr("class", "axis axis--y")
            .call(D3.axisLeft(y).ticks(10));

        g.append("g")
            .attr("class", "axis axis--y")
            .attr("transform", "translate(" + width + ",0)")
            .call(D3.axisRight(y2).ticks(10, "%"));

        g.selectAll(".bar")
            .data(data)
            .enter().append("rect")
            .attr("class", "bar")
            .attr("x", function (d) { return x(d.n); })
            .attr("y", function (d) { return y(d.v); })
            .attr("width", x.bandwidth())
            .attr("height", function (d) { return height - y(d.v); });

        var barWidth = 0.9 * width / (data.length + 0.1);
        var line = D3.line()
            .x(function (d) {
                return d.n ? (barWidth + x(d.n)) : 0;
            })
            .y(function (d) {
                return y2(d.p);
            });

        data.unshift({ p: 0 });
        g.append("path")
            .datum(data)
            .attr("class", "line")
            .attr("d", line);
    };

    return {
        render: render
    };

})(d3);
