# Working with External HTML in Purescript-Halogen Experiment

[![Dependency status](https://img.shields.io/librariesio/github/slamdata/purescript-halogen-template.svg)](https://libraries.io/github/slamdata/purescript-halogen-template)

My goal is to succesful import html via xhr and insert into a component and further more listen to onclick events with in that html


## Prerequisites

This guide assumes you already have Git and Node.js installed with `npm` somewhere on your path.

In the PureScript ecosystem [Bower](http://bower.io/) is currently the most commonly used package manager and we'll be relying on it for this project, so if you don't already have it, you can install it like this:

``` shell
npm install --global bower
```

### Building

from the current directory:

```
$ npm install
$ bower install
$ npm run build
```

The code will be built as `example.js` in the `externalHtml/dist` directory. Run the runnable by opening the corresponding `index.html`.
