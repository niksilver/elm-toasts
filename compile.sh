#!/bin/sh

# Create the documentation in the doc dir
./lit --weave --out-dir doc design/Main.lit

# Create the source code in the src dir, including line numbers
./lit --tangle --linenums '-- litline' --out-dir src design/Main.lit

# Compile the application
elm make --output=main.html src/Main2.elm

