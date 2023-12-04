import { Filter } from 'ts-toolbelt/out/List/Filter'
import { Add } from 'ts-toolbelt/out/Number/Add'
import { Sub } from 'ts-toolbelt/out/Number/Sub'
import { Split } from 'ts-toolbelt/out/String/Split'
import { ListOf } from 'ts-toolbelt/out/Union/ListOf'

type Sum<T, TAcc extends number = 0> = T extends [
  infer fst extends number,
  ...infer rest
]
  ? Sum<rest, Add<fst, TAcc>>
  : TAcc

type Intersect<
  T extends any[],
  U extends any[],
  TAcc extends any[] = []
> = T extends [infer fst, ...infer rest]
  ? fst extends U[number]
    ? Intersect<rest, U, [...TAcc, fst]>
    : Intersect<rest, U, TAcc>
  : TAcc

type Multiply<T extends number, U extends number> = U extends 1
  ? T
  : // @ts-ignore Type instantiation is excessively deep and possibly infinite.
    Add<T, Multiply<T, Sub<U, 1>>>

type Pow<T extends number, TPow extends number> = TPow extends 0
  ? 1
  : TPow extends 1
  ? T
  : // @ts-ignore Type instantiation is excessively deep and possibly infinite.
    Multiply<T, Pow<T, Sub<TPow, 1>>>

type StripPrefix<T> = T extends `Card ${string}: ${infer numbers}`
  ? numbers
  : never

type SplitCards<T> = T extends string ? Split<T, '\n'>[number] : never

type SeparateNumbers<T> = T extends string ? Split<T, ' | '> : never

type ParseNumbers_<T> = T extends string ? Filter<Split<T, ' '>, ''> : never

type ParseNumbers<T> = T extends [infer U, infer I]
  ? [ParseNumbers_<U>, ParseNumbers_<I>]
  : never

type IntersectNumbers<T> = T extends [
  infer U extends any[],
  infer I extends any[]
]
  ? Intersect<U, I>
  : never

type CalculateScore<T> = T extends any[]
  ? T['length'] extends 0
    ? 0
    : Pow<2, Sub<T['length'], 1>>
  : never

type GetListOfIntersections<T> = ListOf<
  IntersectNumbers<ParseNumbers<SeparateNumbers<StripPrefix<SplitCards<T>>>>>
>

type Solve<T> = {
  [k in keyof GetListOfIntersections<T>]: CalculateScore<
    GetListOfIntersections<T>[k]
  >
}

type result = Solve<`Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11`>
