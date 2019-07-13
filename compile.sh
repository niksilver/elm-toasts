#!/bin/sh

./lit --weave --out-dir doc design/design.lit
./lit --tangle --out-dir src design/design.lit
elm make --output=main.html src/Main2.elm

