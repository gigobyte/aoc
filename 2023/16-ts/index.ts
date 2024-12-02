import type { Add, Subtract } from 'ts-arithmetic'

type Range<To extends number, Result extends any[] = []> = To extends 0
  ? Result
  : Subtract<To, 1> extends infer result extends number
  ? Range<result, [...Result, result]>
  : never

type StringToChars<
  T extends string,
  Result extends string[] = []
> = T extends `${infer head}${infer tail}`
  ? StringToChars<tail, [...Result, head]>
  : Result

type Split<
  TStr extends string,
  TSeparator extends string,
  TResult extends string[] = []
> = TStr extends `${infer chunk}${TSeparator}${infer rest}`
  ? Split<rest, TSeparator, [...TResult, chunk]>
  : [...TResult, TStr]

type input = `.|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....`

type LineToMap<Line extends any[], I extends number> = Range<
  Line['length']
> extends infer result extends number[]
  ? { [j in result[number] as `${I},${j}`]: Line[j] }
  : never

type MakeGrid<T, I extends number = 0> = T extends [
  infer head extends string,
  ...infer tail
]
  ? LineToMap<StringToChars<head>, I> & MakeGrid<tail, Add<I, 1>>
  : {}

type lines = Split<input, '\n'>

type grid = MakeGrid<lines>
