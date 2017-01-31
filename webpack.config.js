const {resolve} = require('path')
const webpack = require('webpack')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const ExtractTextPlugin = require("extract-text-webpack-plugin")
const BrowserSyncPlugin = require('browser-sync-webpack-plugin')
const ClosureCompilerPlugin = require('webpack-closure-compiler')

module.exports = {
	entry: './src/index.js',
	output: {
		path: resolve(__dirname, "dist"),
		filename: 'index.js',
	},
	target: 'web',
	module: {
		rules: [
			{
				test: /\.elm$/,
				exclude: [/elm-stuff/, /node_modules/],
				loader: 'elm-webpack-loader',
				options: {
					cwd: __dirname,
					maxInstances: 4
				}
			},
			{
				test: /\.styl$/,
				loader: ExtractTextPlugin.extract({
					loader: ['css-loader', 'clean-css-loader', 'stylus-loader']
				})
			}
		]
	},

	plugins: [
		process.env.NODE_ENV !== 'production' ? null :
		new ClosureCompilerPlugin({
			compiler: {
				compilation_level: 'ADVANCED',
				language_in: 'ECMASCRIPT5_STRICT',
				language_out: 'ECMASCRIPT5_STRICT'
			},
			concurrency: 2
		}),

		process.env.NODE_ENV !== 'production' ? null :
		new webpack.optimize.UglifyJsPlugin({
			compress: {
				sequences: true,
				properties: true,
				dead_code: true,
				drop_debugger: true,
				unsafe: true,
				unsafe_comps: true,
				conditionals: true,
				comparisons: true,
				evaluate: true,
				booleans: true,
				loops: true,
				unused: true,
				hoist_funs: true,
				if_return: true,
				join_vars: true,
				cascade: true,
				collapse_vars: true,
				reduce_vars: true,
				warnings: true,
				negate_iife: true,
				pure_getters: true,
				drop_console: true,
				keep_fargs: false,
				keep_fnames: false,
				passes: 2
			},
			mangle: {
				toplevel: true,
				eval: true,
				props: false
			}
		}),

		new ExtractTextPlugin('index.css'),
		new HtmlWebpackPlugin({
			template: './src/index.html',
			minify: {
				collapseWhitespace: true
			}
		}),

		process.env.NODE_ENV === 'production' ? null :
		new BrowserSyncPlugin({
			host: 'localhost',
			port: 3000,
			server: {baseDir: ['dist']},
			injectChanges: true
		})
	].filter(v => v != null)
}
