# prisma-query-generator


### Usage

```js
const { textToWhereQuery } = require('prisma-query-generator');

/**
 * @param {object} options - The options for generating the query.
 * @param {string} [options.textQuery] - The text query string.
 * @param {Record<string, "number" | "string" | "boolean"> | null} [options.validKeys=null] - A map of valid keys and their expected types.
 * @param {Record<string, string[]>} [options.keyAliases={}] - A map of key aliases to their corresponding original keys.
 * @param {string[]} [options.arrayKeys=[]] - A list of keys that should be treated as arrays.
 * @param {function} [options.processValues=(key, value) => value] - A function to process the values before adding them to the query.
 * @returns {Record<string, any>} The generated Prisma query object.
 */

console.log(
  textToWhereQuery(
    {
      textQuery: `tags:banana tags!:apple likes>:50 likes<:300 type?:[video, "text with space"], file.width>:1000 name*:contains`,
    }
  )
);

// {
//   "OR": [
//     {
//       "type": {
//         "in": [
//           "video",
//           "text with space"
//         ]
//       }
//     }
//   ],
//   "AND": [
//     {
//       "tags": {
//         "in": [
//           "banana"
//         ]
//       }
//     },
//     {
//       "likes": {
//         "gt": 50,
//         "lt": 300
//       }
//     },
//     {
//       "file": {
//         "width": {
//           "gt": 1000
//         }
//       }
//     },
//     {
//       "name": {
//         "contains": "contains"
//       }
//     }
//   ],
//   "NOT": [
//     {
//       "tags": {
//         "in": [
//           "apple"
//         ]
//       }
//     }
//   ]
// }
```

### Syntax
> You can both use [] array or direct value. If you want to use spaces in a value, you should use double quotes (""). 
- `key:value` - key equals value
- `key!:value` - key not equals value
- `key!:[value1, value2]` - key not equals value (multiple)
- `key>:value` - key greater than value
- `key<:value` - key less than value
- `key>=:value` - key greater than or equals value
- `key<=:value` - key less than or equals value
- `key?:[value1, value2]` - key in [value1, value2]
- `key*:value` - key contains value (add)
- `key_*:value` - key starts with value (add)
- `key*_:value` - key ends with value (add)
- `key!*:value` - key not contains value
- `key!_*:value` - key not starts with value
- `key!*_:value` - key not ends with value
- `key?*:value` - key contains value (or)
- `key?_*:value` - key starts with value (or)
- `key?*_:value` - key ends with value (or)