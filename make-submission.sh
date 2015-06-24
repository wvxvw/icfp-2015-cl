#!/usr/bin/env bash

# actually package
cd ..
rm -rf ./packaged
mkdir packaged
mkdir packaged/{solution,code}
cp -r ./icfp-2015-cl/* ./packaged/code
find ./packaged/code -name '*~' -exec rm {} +
tar --transform 's,./packaged,.,' -zcf ./package.tar.gz ./packaged/*
cd ./icfp-2015-cl
