require("babel-polyfill");
require("bootstrap-webpack");

require("file?name=index.html!./html/index.html");
require("file?name=favicon.png!./img/favicon.png");
require("file?name=Boots.svg!./img/Boots.svg");
require("file?name=Cat.svg!./img/Cat.svg");

require("./css/style.styl");

require("Main").main();
