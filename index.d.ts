export interface TextToWhereQueryOptions {
  textQuery?: string;
  validKeys?: Record<string, "number" | "string" | "boolean"> | null;
  keyAliases?: Record<string, string[]>;
  arrayKeys?: string[];
}

export function textToWhereQuery(options?: TextToWhereQueryOptions): Record<string, any>;