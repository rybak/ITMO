#!/bin/bash

set -e

cmake_build() {
    mkdir bin || true
    cd bin
    cmake ..
    cmake --build .
    cd - 
}

cd ./p1.1/golden_segment_intersection/

cmake_build
