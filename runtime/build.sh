#!/bin/bash

cd "${0%/*}"
cc -shared -o target/runtime.so -fPIC src/*.c
