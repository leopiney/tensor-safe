var babel = require("@babel/core");
var fs = require("fs");
var plugin = require("./index");

const code = fs.readFileSync(`${__dirname}/in.js`).toString();

const transformedCode = babel.transform(code, {
  plugins: [plugin],
  code: true,
  ast: false
}).code;

fs.writeFileSync(`${__dirname}/out.js`, transformedCode);
