const path = require('path');

module.exports = {
    mode: "development",
    entry: './assets/js/main.js',
    output: {
        path: path.resolve(__dirname, "dist"),
        publicPath: "/assets/"
    },
    module: {
        rules: [
            {
                test: /\.css$/,
                use: [
                    'style-loader',
                    {
                        loader: 'css-loader',
                        options: {
                            importLoaders: 1
                        }
                    },
                    {
                        loader: 'postcss-loader',
                        options: {
                            ident: 'postcss',
                            plugins: [
                                require('tailwindcss'),
                                require('autoprefixer'),
                            ]
                        }
                    }
                ]
            }
        ]
    }
}