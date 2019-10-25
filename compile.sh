#!/bin/sh

# Create the documentation in the doc dir
./lit --weave --out-dir doc design/Main.lit

# Create the source code in the src dir
./lit --tangle --out-dir src design/Main.lit

# Compile the application
elm make --output=main.html src/Main2.elm

