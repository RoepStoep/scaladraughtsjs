#!/bin/bash

fileName=scaladraughtsjs-opt.js

rm -f target/scala-*/$fileName

sbt fullOptJS || exit $?

cp target/scala-*/$fileName build/
