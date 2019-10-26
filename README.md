# scaladraughts.js

scaladraughts.js is a draughts library, forked from the [scalachess.js](https://github.com/veloce/scalachessjs) chess library. It runs in a webworker, with multi-variants support.

It is based on the [scaladraughts](https://github.com/RoepStoep/scaladraughts) library
compiled to JavaScript, thanks to [Scala.js](https://www.scala-js.org/).

It is currently used in production in [lidraughts.org](http://lidraughts.org) mobile
application. So you can see a [real world usage](https://github.com/RoepStoep/lidrobile/blob/master/src/draughts.ts) on [the mobile app repo](https://github.com/RoepStoep/lidrobile).

## Build

    $ git submodule update --init
    $ ./build.sh

Generated file will be in `build` dir.
