#!/bin/bash

docker run --rm -u `id -u $USER`:`id -g $USER` -v `pwd`:/src/ -w /src cibuilds/hugo:0.50 hugo new $1
