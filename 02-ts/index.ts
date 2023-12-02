import { Add } from 'ts-toolbelt/out/Number/Add'
import { LowerEq } from 'ts-toolbelt/out/Number/LowerEq'
import { Replace } from 'ts-toolbelt/out/String/Replace'
import { Split } from 'ts-toolbelt/out/String/Split'
import { ListOf } from 'ts-toolbelt/out/Union/ListOf'

type Sum<T, TAcc extends number = 0> = T extends [
  infer fst extends number,
  ...infer rest extends number[]
]
  ? Sum<rest, Add<fst, TAcc>>
  : TAcc

type SplitCubes<T> = T extends string ? Split<T, ','> : never

type SplitGames<T> = T extends string ? Split<T, '\n'> : never

type SplitGameSets<T> = T extends string
  ? Split<Replace<T, `Game ${string}:`, ''>, ';'>
  : never

type ParseGameId<T> = T extends `Game ${infer id extends number}:${string}`
  ? id
  : never

type ParseCube<T> = T extends ` ${infer count extends number} red`
  ? { red: count }
  : T extends ` ${infer count extends number} green`
  ? { green: count }
  : T extends ` ${infer count extends number} blue`
  ? { blue: count }
  : never

type ParseGameSet<T> = {
  [i in keyof SplitCubes<T>]: ParseCube<SplitCubes<T>[i]>
}[number]

type ParseGame<T> = {
  id: ParseGameId<T>
  sets: ParseGameSet<SplitGameSets<T>[number]>
}

type IsValidCubeCount<T> = T extends { red: infer count extends number }
  ? LowerEq<count, 12>
  : T extends { green: infer count extends number }
  ? LowerEq<count, 13>
  : T extends { blue: infer count extends number }
  ? LowerEq<count, 14>
  : 0

type FilterInvalidGames<T extends { id: number; sets: any }> =
  0 extends IsValidCubeCount<T['sets']> ? never : T['id']

type Solve<T> = Sum<
  ListOf<
    keyof {
      [i in keyof SplitGames<T> as FilterInvalidGames<
        ParseGame<SplitGames<T>[i]>
      >]: never
    }
  >
>

type result =
  Solve<`Game 1: 7 blue, 5 red; 10 red, 7 blue; 5 blue, 4 green, 15 red; 4 green, 6 red, 7 blue; 5 green, 8 blue, 4 red; 5 red, 4 blue, 3 green
Game 2: 8 green, 3 red; 7 blue, 6 red, 8 green; 7 blue, 3 green, 6 red; 8 green, 6 blue, 11 red; 6 blue, 3 green, 12 red
Game 3: 6 blue, 3 red, 7 green; 3 red, 3 green, 8 blue; 8 blue, 11 red, 4 green; 5 blue, 7 red, 6 green; 9 blue, 7 green, 1 red
Game 4: 3 red, 4 green; 5 red, 1 blue; 2 green; 3 green, 1 blue; 2 green, 1 blue, 1 red
Game 5: 17 red, 5 blue, 3 green; 8 green, 9 red, 10 blue; 2 green, 9 blue, 4 red
Game 6: 5 blue, 6 green, 3 red; 1 green, 8 blue, 12 red; 2 blue, 13 red, 6 green
Game 7: 1 green, 1 blue, 6 red; 1 red, 8 green; 3 red, 8 green, 2 blue; 14 green, 4 blue, 4 red; 4 green, 5 blue; 7 green, 2 blue, 1 red
Game 8: 6 blue, 9 red, 3 green; 2 red, 6 blue; 2 green, 1 red, 2 blue; 2 green, 9 blue, 6 red
Game 9: 5 green, 8 blue, 8 red; 2 blue, 6 green, 8 red; 6 red, 9 green
Game 10: 2 red, 2 blue, 12 green; 8 green, 3 red; 5 blue, 11 red, 6 green; 14 red, 1 green
Game 11: 2 green, 1 red, 1 blue; 4 blue, 7 red; 7 red, 7 green, 5 blue; 2 blue, 3 red, 6 green; 3 blue, 9 red, 7 green
Game 12: 9 green, 7 red, 7 blue; 6 green, 4 blue, 1 red; 3 blue, 5 red, 7 green; 9 green, 10 red, 12 blue; 11 green, 5 red; 9 blue, 12 green, 3 red
Game 13: 7 blue, 7 red, 2 green; 5 blue, 5 green, 7 red; 1 blue, 10 red; 11 red, 2 blue, 1 green; 1 green, 1 blue, 4 red
Game 14: 2 blue, 10 red; 3 blue, 6 green, 17 red; 3 green, 4 blue, 14 red
Game 15: 7 blue, 2 green, 4 red; 7 blue, 3 red, 3 green; 4 red, 2 green, 1 blue; 2 red, 9 green, 5 blue; 2 red, 4 green, 5 blue
Game 16: 5 blue, 1 red; 8 blue, 1 green; 5 green, 3 blue, 2 red; 8 blue, 2 green; 2 red, 2 blue, 5 green`>
