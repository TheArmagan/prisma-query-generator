const { textToWhereQuery } = require("./index.js");
const util = require("util");

const AvatarWhereQueryTypes = {
  id: "string",
  name: "string",
  normalized_name: "string",
  author_id: "string",
  author_name: "string",
  normalized_author_name: "string",
  description: "string",
  normalized_description: "string",
  tags: "string",
  uploader_id: "string",
  platforms: "string",
  user_tags: "string",
  ai_tags: "string",
  ai_tags_v2: "string",
  image_hash: "string",
  favorite_count: "number",
  comment_count: "number",
  selected_count: "number"
}

const AvatarSearchKeyAliases = {
  all: [
    "id",
    "name",
    "normalized_name",
    "author_id",
    "author_name",
    "normalized_author_name",
    "description",
    "normalized_description",
    "tags",
    "uploader_id",
    "platforms",
    "user_tags",
    "ai_tags",
    "ai_tags_v2",
    "image_hash",
  ],
  all_tags: [
    "tags",
    "user_tags",
    "ai_tags",
    "ai_tags_v2"
  ]
}

console.log(
  util.inspect(
    textToWhereQuery({
      textQuery: `author_id:"usr_06e4f60f-9483-406c-afab-2bffc582430c"`,
      keyAliases: AvatarSearchKeyAliases,
      validKeys: AvatarWhereQueryTypes
    }),
    { depth: null, colors: true }
  )
)