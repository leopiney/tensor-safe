var babel = require("@babel/core");
var cp = require("child_process");
var fs = require("fs");

/* 
  Verifies that the content of the `safeModel` function is valid using `tensor-safe compile`
  and parses the output to rewrite it
*/
module.exports = () => {
  return {
    visitor: {
      TaggedTemplateExpression(path) {
        var node = path.node;

        if (node.tag.type === "Identifier" && node.tag.name === "safeModel") {
          if (node.quasi.type === "TemplateLiteral") {
            //
            // Extracts the content which is the Haskell code we want to validate
            //
            content = node.quasi.quasis.reduce(
              (curr, n) =>
                curr + (n.value ? n.value.raw : "") + (n.tail ? "" : "\n"),
              ""
            );

            templatePath =
              "/Users/leo/Documents/tensor-safe/extra/javascript/babel-plugin-tensor-safe/templates/ModelModule.hs.template";
            templateContent = fs.readFileSync(templatePath, "utf8");

            targetPath =
              "/Users/leo/Documents/tensor-safe/extra/javascript/babel-plugin-tensor-safe/target/ModelModule.hs";
            targetContent = templateContent.replace("-- { CONTENT }", content);

            moduleName = "ModelModule";

            // Writes target content to a file
            fs.writeFileSync(targetPath, targetContent);

            //
            // Use tensor-safe to check if the created temp file is valid and capture output
            // to rewrite the `safeModel` expression.
            //
            try {
              console.log("Going to execute command");
              console.log(
                `stack exec tensor-safe -- compile --path ${targetPath} --module-name ${moduleName} --javascript`
              );
              commandContent = cp.execSync(
                `stack exec tensor-safe -- compile --path ${targetPath} --module-name ${moduleName} --javascript`,
                { cwd: "/Users/leo/Documents/tensor-safe/", encoding: "utf8" }
              );

              var parsed = babel.parseSync(commandContent, {
                sourceType: "module"
              });

              path.replaceWithMultiple(parsed.program.body);
            } catch (err) {
              console.error(err.message);
            }
          }
        }
      }
    }
  };
};
