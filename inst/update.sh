#!/bin/bash

set -e

cd www/lib


cd vega
# curl -O https://raw.github.com/trifacta/vega/master/vega.js
# curl -O https://raw.github.com/trifacta/vega/master/vega.min.js
curl -O https://raw.githubusercontent.com/vega/vega/master/docs/vega.js
curl -O https://raw.githubusercontent.com/vega/vega/master/docs/vega.min.js
curl -O https://vega.github.io/schema/vega/v3.0.json
cp vega.min.js ../../../htmlwidgets/lib/vega-3.0.2

cd ../d3
# curl -O https://raw.github.com/mbostock/d3/master/d3.js
# curl -O https://raw.github.com/mbostock/d3/master/d3.min.js
curl https://d3js.org/d3.v4.js > d3.js
curl https://d3js.org/d3.v4.min.js > d3.min.js

cd ../jquery
curl http://code.jquery.com/jquery-1.11.0.min.js > jquery.min.js
curl http://code.jquery.com/jquery-1.11.0.js > jquery.js

cd ../../../htmlwidgets/lib

cd vega-tooltip-0.4.4
curl -O https://cdnjs.cloudflare.com/ajax/libs/vega-tooltip/0.4.4/vega-tooltip.js
curl -O https://cdnjs.cloudflare.com/ajax/libs/vega-tooltip/0.4.4/vega-tooltip.min.js
curl -O https://cdnjs.cloudflare.com/ajax/libs/vega-tooltip/0.4.4/vega-tooltip.css
curl -O https://cdnjs.cloudflare.com/ajax/libs/vega-tooltip/0.4.4/vega-tooltip.min.css

cd ../../../..
