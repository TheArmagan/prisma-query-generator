# prisma-query-generator


### Usage

```js
const { textToWhereQuery } = require('prisma-query-generator');

console.log(
  textToWhereQuery(
    'tags:banana tags!:apple likes>:50 likes<:300 type?:[video, "text with space"], file.width>:1000 name*:contains'
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
- `key?*:value` - key not contains value (or)
- `key?_*:value` - key not starts with value (or)
- `key?*_:value` - key not ends with value (or)