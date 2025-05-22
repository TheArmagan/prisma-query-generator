const _ = require("lodash");

const QueryRegex = /(?<key>[a-zA-Z0-9._-]+)(?<expr>!|\?|>|<|>=|<=|=|\*|_\*|\*_|!\*|!_\*|!\*_|\?\*|\?_\*|\?\*_|):(?<value>\[(?:(?:"[^"]+?"|[^, ]+|\d+)(?: {0,}, {0,})?)+\]|"[^"]+?"|[^, ]+|\d+)/g;
const FixValueRegex1 = /([^,\[\]]+)/g;
const FixValueRegex2 = /^(?:-?\d+(?:\.\d+)?|true|false|null)$/;

function fixValues(value) {
  const val = JSON.parse(value.replace(FixValueRegex1, (_, g) => {
    g = g.trim();
    return FixValueRegex2.test(g) ? g : `"${g.replaceAll('"', "")}"`;
  }));
  return Array.isArray(val) ? val : [val];
}

const MathQueries = {
  ">": "gt",
  "<": "lt",
  ">=": "gte",
  "<=": "lte",
  "=": "equals",
};

/**
 * @param {object} options - The options for generating the query.
 * @param {string} [options.textQuery] - The text query string.
 * @param {Record<string, "number" | "string" | "boolean"> | null} [options.validKeys=null] - A map of valid keys and their expected types.
 * @param {Record<string, string[]>} [options.keyAliases={}] - A map of key aliases to their corresponding original keys.
 * @param {string[]} [options.arrayKeys=[]] - A list of keys that should be treated as arrays.
 * @returns {Record<string, any>} The generated Prisma query object.
 */
function textToWhereQuery({
  textQuery,
  validKeys = null,
  arrayKeys = [],
  keyAliases = {},
} = {}) {
  const query = {
    OR: [],
    AND: [],
    NOT: []
  };

  const matches = [...textQuery.matchAll(QueryRegex)];

  const mappedMatches = matches.map(match => {
    return {
      key: match.groups.key,
      expr: match.groups.expr,
      values: fixValues(match.groups.value)
    };
  });

  mappedMatches.forEach(({ key, expr, values }) => {
    const keysToProcess = keyAliases[key] ? keyAliases[key] : [key];

    keysToProcess.forEach(currentKey => {
      let isArrayKey = arrayKeys.includes(currentKey);

      if (validKeys && validKeys[currentKey] && values.some(x => typeof x !== validKeys[currentKey])) return;

      let currentExpr = expr;
      if (currentExpr === "") currentExpr = "=";

      switch (currentExpr) {
        case "!":
        case "?":
        case "=": {
          let method;

          switch (currentExpr) {
            case "!":
              method = "NOT";
              break;
            case "?":
              method = "OR";
              break;
            case "=":
              method = "AND";
              break;
          }

          let lastKey = isArrayKey ? "hasSome" : "in";

          if (query[method].some(x => _.has(x, currentKey))) {
            let found = query[method].find(x => _.has(x, currentKey));
            let oldValues = _.get(found, `${currentKey}.${lastKey}`, []);
            _.set(found, `${currentKey}.${lastKey}`, [...oldValues, ...values]);
          } else {
            query[method].push(_.set({}, currentKey, { [lastKey]: values }));
          }
          break;
        }
        case "*":
        case "_*":
        case "*_":
        case "!*":
        case "!_*":
        case "!*_":
        case "?*":
        case "?_*":
        case "?*_": {
          let method;

          if (currentExpr.startsWith("!")) method = "NOT";
          else if (currentExpr.startsWith("?")) method = "OR";
          else method = "AND";

          let queryType;

          switch (currentExpr) {
            case "*":
            case "!*":
            case "?*":
              queryType = isArrayKey ? "has" : "contains";
              break;
            case "_*":
            case "!_*":
            case "?_*":
              queryType = "startsWith";
              break;
            case "*_":
            case "!*_":
            case "?*_":
              queryType = "endsWith";
              break;
          }

          values.forEach(value => {
            const o = { [queryType]: value };
            if (!isArrayKey) o.mode = "insensitive";
            query[method].push(_.set({}, currentKey, o));
          });

          break;
        }
        case ">":
        case "<":
        case ">=":
        case "<=": {
          if (query.AND.some(x => _.has(x, currentKey))) {
            let found = query.AND.find(x => _.has(x, currentKey));
            _.set(found, `${currentKey}.${MathQueries[currentExpr]}`, values[0]);
          } else {
            query.AND.push(_.set({}, currentKey, { [MathQueries[currentExpr]]: values[0] }));
          }
          break
        }
      }
    });
  });

  if (query.OR.length === 0) delete query.OR;
  if (query.AND.length === 0) delete query.AND;
  if (query.NOT.length === 0) delete query.NOT;

  return query;
}

module.exports = {
  textToWhereQuery
};