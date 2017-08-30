const path = require('path');
module.exports = {
    entry: './js/application.js',
    output: {
        path: path.resolve(__dirname, './dist/js'),
        filename: 'bundle.js',
        publicPath: '/dist/js'
    },
  module: {
    rules: [
      { test: /vissense/, loader: 'exports-loader?VisSense!script-loader'}
    ]
  }
}
