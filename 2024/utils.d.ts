export type Split<
  TStr extends string,
  TSeparator extends string,
  TResult extends string[] = []
> = TStr extends `${infer chunk}${TSeparator}${infer rest}`
  ? Split<rest, TSeparator, [...TResult, chunk]>
  : [...TResult, TStr]

export type ParseNumber<T> = T extends `${infer num extends number}`
  ? num
  : never

export type MapParseNumber<T, TAcc extends any[] = []> = T extends [
  infer fst,
  ...infer rest
]
  ? MapParseNumber<rest, [...TAcc, ParseNumber<fst>]>
  : TAcc
