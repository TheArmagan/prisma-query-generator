export interface TextToWhereQueryOptions {
  textQuery?: string;
  validKeys?: Record<string, "number" | "string" | "boolean"> | null;
  keyAliases?: Record<string, string[]>;
  arrayKeys?: string[];
  processValues?: (key: string, value: any) => any | any[];
}

export function textToWhereQuery(options?: TextToWhereQueryOptions): Record<string, any>;