var path = require('path');
const CopyWebpackPlugin = require('copy-webpack-plugin')
const CompressionPlugin = require('compression-webpack-plugin');

const env = process.env.NODE_ENV

let debugMode  = {}

//set debugMode for application 
switch (env) {
	case "production":
		debugMode = { optimize: true }
    case "development":
		debugMode = {}
}

module.exports = {
	entry: "./src/index.js",
	output: {
    publicPath: "/",
		path: path.resolve(__dirname, "dist"),
		filename: "js/app.js"
	},
	module: {
	    rules: [{
        test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file-loader?name=[name].[ext]'
      },
	  {
        test: /\.(css|scss)$/,
        use: [
          'style-loader',
          'css-loader',
        ]
      },
	  {
		test: /\.ttf$/,
		use: {
			loader: "url-loader",
			options: {
				limit: 50000, 
			},
		},
	  },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        // This is what you need in your own work
        loader: "elm-webpack-loader",
        options: {},
      },
	  {
		  test: /\.js$/,
		  exclude: /node_modules/,
		  use: ["babel-loader"]
	  }
    ]
  },

  devServer: {
    inline: true,
    stats: 'errors-only',
    historyApiFallback: true
  },
  plugins: [
    	new CompressionPlugin(),
	  	new CopyWebpackPlugin([
			 //{ from: 'src/images/', to: 'images/' }, 
             //{ from: 'src/fonts/', to: 'fonts/'}, 
		     { from: 'src/service-worker.js', to: 'js/' }
		])
  ],
};
