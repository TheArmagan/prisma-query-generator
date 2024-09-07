const _ = require("lodash");

const QueryRegex = /(?<key>[a-zA-Z0-9.]+)(?<expr>!|\?|>|<|>=|<=|=|\*|_\*|\*_|!\*|!_\*|!\*_|\?\*|\?_\*|\?\*_|):(?<value>\[(?:(?:"[^"]+?"|[^, ]+|\d+)(?: {0,}, {0,})?)+\]|"[^"]+?"|[^, ]+|\d+)/g;
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
}

/**
 * @param {string} textQuery 
 * @param {Record<string, "number" | "string" | "boolean"> | null} validKeys 
 * @returns {Record<string, any>}
 */
function textToWhereQuery(textQuery = "", validKeys = null) {
  const query = {
    OR: [],
    AND: [],
    NOT: []
  };

  const matches = [...textQuery.matchAll(QueryRegex)];

  matches.forEach((match) => {
    let key = match.groups.key;
    let expr = match.groups.expr;
    let values = fixValues(match.groups.value);

    if (validKeys && validKeys[key] && values.some(x => typeof x !== validKeys[key])) return;

    if (expr === "") expr = "=";

    switch (expr) {
      case "!":
      case "?":
      case "=": {
        let method;

        switch (expr) {
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

        if (query[method].some(x => _.has(x, key))) {
          let found = query[method].find(x => _.has(x, key));
          let oldValues = _.get(found, `${key}.in`);
          _.set(found, `${key}.in`, [...oldValues, ...values]);
        } else {
          query[method].push(_.set({}, key, { in: values }));
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

        if (expr.startsWith("!")) method = "NOT";
        else if (expr.startsWith("?")) method = "OR";
        else method = "AND";

        let queryType;

        switch (expr) {
          case "*":
          case "!*":
          case "?*":
            queryType = "contains";
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
          query[method].push(_.set({}, key, { [queryType]: value }));
        });

        break;
      }
      case ">":
      case "<":
      case ">=":
      case "<=": {
        if (query.AND.some(x => _.has(x, key))) {
          let found = query.AND.find(x => _.has(x, key));
          _.set(found, `${key}.${MathQueries[expr]}`, values[0]);
        } else {
          query.AND.push(_.set({}, key, { [MathQueries[expr]]: values[0] }));
        }
        break
      }
    }
  });

  if (query.OR.length === 0) delete query.OR;
  if (query.AND.length === 0) delete query.AND;
  if (query.NOT.length === 0) delete query.NOT;

  return query;
}

module.exports = {
  textToWhereQuery
};