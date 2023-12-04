type input = `Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11`
// what do we need to solve this?
// 1. Split strings
// 2. Intersect arrays
// 3. Calculate exponents -> calculate multiplication -> calculate addition
// 4. Sum all elements of array

type Split<
  TStr extends string,
  TSeparator extends string,
  TResult extends string[] = []
> = TStr extends `${infer chunk}${TSeparator}${infer rest}`
  ? Split<rest, TSeparator, [...TResult, chunk]>
  : [...TResult, TStr]

type lines = Split<input, '\n'>

// Introducing loops

type chunks_<T, TAcc extends any[] = []> = T extends [
  infer fst extends string,
  ...infer rest
]
  ? chunks_<rest, [...TAcc, Split<fst, ' | '>]>
  : TAcc

type chunks = chunks_<lines>

// Now the first element has Card 1: which we don't want

type StripPrefix<T extends string> =
  T extends `Card ${string}: ${infer contents}` ? contents : never

type cleanedChunks_<T, TAcc extends any[] = []> = T extends [
  infer fst extends [string, string],
  ...infer rest
]
  ? cleanedChunks_<rest, [...TAcc, [StripPrefix<fst[0]>, fst[1]]]>
  : TAcc

type cleanedChunks = cleanedChunks_<chunks>

type rawNumbers_<T, TAcc extends any[] = []> = T extends [
  infer fst extends [string, string],
  ...infer rest
]
  ? rawNumbers_<rest, [...TAcc, [Split<fst[0], ' '>, Split<fst[1], ' '>]]>
  : TAcc

type rawNumbers = rawNumbers_<cleanedChunks>

// Now there are "" elements which can cause bugs

type Filter<
  TArr extends any[],
  TUnwanted,
  TResult extends any[] = []
> = TArr extends [infer fst, ...infer rest]
  ? fst extends TUnwanted
    ? Filter<rest, TUnwanted, TResult>
    : Filter<rest, TUnwanted, [...TResult, fst]>
  : TResult

type numbers_<T, TAcc extends any[][] = []> = T extends [
  infer fst extends [string[], string[]],
  ...infer rest extends any[]
]
  ? numbers_<rest, [...TAcc, [Filter<fst[0], ''>, Filter<fst[1], ''>]]>
  : TAcc

type numbers = numbers_<rawNumbers>

// Let's intersect

type Intersect<
  T extends any[],
  U extends any[],
  TAcc extends any[] = []
> = T extends [infer fst, ...infer rest]
  ? fst extends U[number]
    ? Intersect<rest, U, [...TAcc, fst]>
    : Intersect<rest, U, TAcc>
  : TAcc

type intersections_<T, TAcc extends any[][] = []> = T extends [
  infer fst extends [string[], string[]],
  ...infer rest extends any[]
]
  ? intersections_<rest, [...TAcc, Intersect<fst[0], fst[1]>]>
  : TAcc

type intersections = intersections_<numbers>

// Now we need to implement addition and subtraction to be able to implement multiplication

// You can combine arrays

type a = [1, 2, 3, 4]
type b = [3, 4]
type c = [...a, ...b]

// You can get the length of an array

type len = c['length']

// Meaning if we can create an array of arbitrary size we can implement addition!

type ArrayOfN<T, TResult extends any[] = []> = TResult['length'] extends T
  ? TResult
  : ArrayOfN<T, [...TResult, any]>

type Add<T, U> = [...ArrayOfN<T>, ...ArrayOfN<U>]['length']

// Now to implement subtraction

type Pop<TArr> = TArr extends [infer _, ...infer rest] ? rest : never

type PopN<TArr, Tn, TLooper extends any[] = []> = TLooper['length'] extends Tn
  ? TArr
  : PopN<Pop<TArr>, Tn, [...TLooper, any]>

// type Subtract<T, U> = PopN<ArrayOfN<T>, U>['length']

// there will be a Excessive stack depth comparing types error, let's fix it

type Subtract<T, U> = PopN<ArrayOfN<T>, U> extends infer result extends any[]
  ? result['length']
  : never

// Let's implement multiplication

type Multiply<T, U> = U extends 1 ? T : Add<T, Multiply<T, Subtract<U, 1>>>

// Exponents are easy now

type Pow<T, U> = U extends 0
  ? 1
  : U extends 1
  ? T
  : Multiply<T, Pow<T, Subtract<U, 1>>>

// Back to the original task...

type CalculateScore<T> = T extends any[]
  ? T['length'] extends 0
    ? 0
    : Pow<2, Subtract<T['length'], 1>>
  : never

type scores_<T extends any[], TAcc extends any[] = []> = T extends [
  infer fst,
  ...infer rest
]
  ? scores_<rest, [...TAcc, CalculateScore<fst>]>
  : TAcc

type scores = scores_<intersections>

// Last step

type Sum<T, TAcc extends number = 0> = T extends [
  infer fst extends number,
  ...infer rest
]
  ? Sum<rest, Extract<Add<fst, TAcc>, number>>
  : TAcc

type result = Sum<scores>

type Solve<T extends string> = Sum<
  scores_<
    intersections_<
      numbers_<rawNumbers_<cleanedChunks_<chunks_<Split<T, '\n'>>>>>
    >
  >
>

type test = Solve<input>
