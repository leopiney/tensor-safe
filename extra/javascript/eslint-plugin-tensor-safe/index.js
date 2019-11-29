var cp = require("child_process");
var fs = require("fs");

/* 
  Verifies that the content of the `safeModel` function is valid using `tensor-safe check`
*/
module.exports = {
  rules: {
    "invalid-model": {
      create: function(context) {
        return {
          AssignmentExpression(node) {
            node_r = node.right;

            if (
              node.operator === "=" &&
              node_r.type === "TaggedTemplateExpression" &&
              node_r.tag.type === "Identifier" &&
              node_r.tag.name === "safeModel" &&
              node_r.quasi.type === "TemplateLiteral"
            ) {
              //
              // Extracts the content which is the Haskell code we want to validate
              //
              content = node_r.quasi.quasis.reduce(
                (curr, n) =>
                  curr + (n.value ? n.value.raw : "") + (n.tail ? "" : "\n"),
                ""
              );

              templatePath =
                "/Users/leo/Documents/tensor-safe/extra/javascript/eslint-plugin-tensor-safe/templates/ModelModule.hs";
              templateContent = fs.readFileSync(templatePath, "utf8");

              targetPath =
                "/Users/leo/Documents/tensor-safe/extra/javascript/eslint-plugin-tensor-safe/target/ModelModule.hs";
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
                console.log("Going to execute command");
                console.log(
                  `stack exec tensor-safe -- check --path ${targetPath}`
                );
                commandContent = cp.execSync(
                  `stack exec tensor-safe -- check --path ${targetPath}`,
                  {
                    cwd: "/Users/leo/Documents/tensor-safe/",
                    encoding: "utf8"
                  }
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

              // fs.unlinkSync(targetPath);
            }
          }
        };
      }
    }
  }
};
