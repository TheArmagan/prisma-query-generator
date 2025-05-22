const { textToWhereQuery } = require("./index.js");
const util = require("util");
console.log(
  util.inspect(
    textToWhereQuery({
      textQuery: `all?*:["teg1", "tag2"] platforms*:"Android"`,
      keyAliases: {
        all: ["name", "description", "tags"],
      },
    }),
    { depth: null, colors: true }
  )
)