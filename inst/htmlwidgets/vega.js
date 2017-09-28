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

        // console.log(el.id); // DEBUG
        v = new vega.View(vega.parse(spec))
          .logLevel(vega.Warn) // set view logging level
          .initialize("#" + el.id)
          .renderer(x.renderer)  // set renderer (canvas or svg)
          .hover()             // enable hover encode set processing
          .run();             // run the dataflow and render the view
      },

      // resize (not necessary as vega auto-resizes when re-rendered)
      // resize: function(width, height) {
      //  console.log("resizing to " + width + " by " + height);
      //},

      // make vega object available
      vega: v
    };
  }
});
