HTMLWidgets.widget({
  name: 'vega',

  type: 'output',

  factory(el, width, height) {
    var v;

    return {
      renderValue: function(x) {
        spec = x.spec;
        spec.width = width;
        spec.height = height;

        var logLevels = {"None": vega.None, "Warn": vega.Warn, "Info": vega.Info, "Debug": vega.Debug};

        // console.log(el.id); // DEBUG
        v = new vega.View(vega.parse(spec))
          .logLevel(logLevels[x.logLevel]) // set view logging level
          .initialize("#" + el.id)
          .renderer(x.renderer)  // set renderer (canvas or svg)
          .hover()             // enable hover encode set processing
          .run();             // run the dataflow and render the view
        console.log(x.tooltip_options); // DEBUG
        if (Object.keys(x.tooltip_options).length > 0) {
          vegaTooltip.vega(v, x.tooltip_options);
        } else {
          vegaTooltip.vega(v);
        }
      },

      // resize (not necessary as vega auto-resizes when re-rendered)
      resize: function(width, height) {
        console.log("resizing to " + width + " by " + height);
      },

      // make vega object available
      vega: v
    };
  }
});
