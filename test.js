const { textToWhereQuery } = require("./index.js");
const util = require("util");
console.log(
  util.inspect(
    textToWhereQuery({
      textQuery: `all_tags*:["boy", "male"] all_tags!*:["anime"]`,
      keyAliases: {
        all_tags: [
          "tags",
          "user_tags",
          "ai_tags",
          "ai_tags_v2"
        ]
      }
    }),
    { depth: null, colors: true }
  )
)