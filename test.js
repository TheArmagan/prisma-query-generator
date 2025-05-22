const { textToWhereQuery } = require("./index.js");
const util = require("util");
console.log(
  util.inspect(
    textToWhereQuery({
      textQuery: `all?*:["teg1", "mechanical wings"] platforms*:"Android"`,
      keyAliases: {
        all: ["name", "description", "ai_tags"],
      },
      processValues: (key, value) => {
        if (key === "ai_tags") {
          return [value.replace(/ +/g, "_"), value];
        }
        return value;
      }
    }),
    { depth: null, colors: true }
  )
)