#!/bin/bash


echo "Building the project"
ghc -i:src -outputdir ./build Main.hs
echo "Build completed"

if [[ $RUN == "1" ]]; then
    echo "Running the project"
    echo "================================"
    ./Main
fi

