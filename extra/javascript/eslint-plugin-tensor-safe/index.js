var cp = require("child_process");
var fs = require("fs");

/* 
  Verifies that the content of the `safeModel` function is valid using `tensor-safe check`
*/
module.exports = {
  rules: {
    "tensor-safe-model-invalid": {
      create: function(context) {
        return {
          TaggedTemplateExpression(node) {
            if (
              node.tag.type === "Identifier" &&
              node.tag.name === "safeModel"
            ) {
              if (node.quasi.type === "TemplateLiteral") {
                //
                // Extracts the content which is the Haskell code we want to validate
                //
                content = node.quasi.quasis.reduce(
                  (curr, n) =>
                    curr + (n.value ? n.value.raw : "") + (n.tail ? "" : "\n"),
                  ""
                );

                //
                // TODO: Hardcoded values
                // Use a template to inject the Haskell code and see if it's valid.
                //
                templatePath = "/Users/leo/Desktop/SomeModule.hs";
                templateContent = fs.readFileSync(templatePath, "utf8");

                targetPath = "/Users/leo/Desktop/SomeModule_.hs";
                targetContent = templateContent.replace(
                  "-- { CONTENT }",
                  content
                );

                // Writes target content to a file
                fs.writeFileSync(targetPath, targetContent);

                //
                // Use tensor-safe to check if the created temp file is valid
                //
                try {
                  commandContent = cp.execSync(
                    `stack exec tensor-safe -- check --path ${targetPath}`,
                    { cwd: "/Users/leo/Documents/tensor-safe" } // TODO: hardcoded
                  );
                } catch (err) {
                  // If the command fails, send a message with the error
                  context.report(
                    node,
                    `Failed to validate model: ${err.message}\n` +
                      `Out: ${err.stdout}\n` +
                      `Error: ${err.stderr}\n` +
                      `Status: ${err.status}\n`
                  );
                }

                // TODO: remove target temp file
              }
            }
          }
        };
      }
    }
  }
};
