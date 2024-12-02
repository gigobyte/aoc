import { Add, And, GtOrEq, LtOrEq, Subtract } from 'ts-arithmetic'
import { Assert, MapParseNumber, RemoveAtIndex, Split } from '../utils'
import { input } from './input'

type lines = Split<input, '\n'>

type reports_<T, TAcc extends any[] = []> = T extends [
  infer fst extends string,
  ...infer rest
]
  ? reports_<rest, [...TAcc, MapParseNumber<Split<fst, ' '>>]>
  : TAcc

type reports = reports_<lines>

type LevelProgression = 'increasing' | 'decreasing' | 'undecided'

type AreLevelsIncreasing<
  Level1 extends number,
  Level2 extends number
> = Subtract<Level1, Level2> extends infer distance extends number
  ? And<LtOrEq<distance, -1>, GtOrEq<distance, -3>> extends 1
    ? true
    : false
  : false

type AreLevelsSafe<
  Level1 extends number,
  Level2 extends number,
  Mode extends LevelProgression = 'undecided'
> = Mode extends 'undecided'
  ? AreLevelsIncreasing<Level1, Level2> extends true
    ? [true, 'increasing']
    : AreLevelsIncreasing<Level2, Level1> extends true
    ? [true, 'decreasing']
    : [false, 'undecided']
  : Mode extends 'increasing'
  ? [AreLevelsIncreasing<Level1, Level2>, 'increasing']
  : [AreLevelsIncreasing<Level2, Level1>, 'decreasing']

/**
 *
 * Part 1
 *
 */
type IsReportSafe<
  Report extends number[],
  Mode extends LevelProgression = 'undecided'
> = Report extends [
  infer fst extends number,
  infer snd extends number,
  infer trd extends number,
  ...infer rest extends number[]
]
  ? AreLevelsSafe<fst, snd, Mode> extends [
      true,
      infer mode extends LevelProgression
    ]
    ? IsReportSafe<[snd, trd, ...rest], mode>
    : false
  : Report extends [infer fst extends number, infer snd extends number]
  ? AreLevelsSafe<fst, snd, Mode>[0]
  : false

type numberOfSafeReports_<
  Reports,
  Result extends number = 0
> = Reports extends [infer report extends number[], ...infer rest]
  ? IsReportSafe<report> extends true
    ? numberOfSafeReports_<rest, Add<Result, 1>>
    : numberOfSafeReports_<rest, Result>
  : Result

type numberOfSafeReports = numberOfSafeReports_<reports>

type OpposideMode = {
  increasing: 'decreasing'
  decreasing: 'increasing'
  undecided: 'undecided'
}

/**
 *
 * Part 2
 *
 */
type IsReportSafeDampened<
  Report extends number[],
  Mode extends LevelProgression = 'undecided',
  CanDampen extends boolean = true,
  Index extends number = 0,
  OriginalReport extends number[] = Report
> = Report extends [
  infer fst extends number,
  infer snd extends number,
  infer trd extends number,
  ...infer rest extends number[]
]
  ? AreLevelsSafe<fst, snd, Mode> extends [
      true,
      infer mode extends LevelProgression
    ]
    ? IsReportSafeDampened<
        [snd, trd, ...rest],
        mode,
        CanDampen,
        Add<Index, 1>,
        OriginalReport
      >
    : CanDampen extends true
    ? // Every iteration we are working with 3 potential elements to remove
      // Remove first
      IsReportSafeDampened<
        RemoveAtIndex<OriginalReport, Subtract<Index, 1>>,
        'undecided',
        false
      > extends false
      ? // Remove second
        IsReportSafeDampened<
          RemoveAtIndex<OriginalReport, Index>,
          'undecided',
          false
        > extends false
        ? // Remove second
          IsReportSafeDampened<
            RemoveAtIndex<OriginalReport, Add<Index, 1>>,
            'undecided',
            false
          >
        : true
      : true
    : false
  : Report extends [infer fst extends number, infer snd extends number]
  ? AreLevelsSafe<fst, snd, Mode>[0] extends false
    ? CanDampen /* case where the last element can be removed */
    : true
  : CanDampen

type numberOfSafeDampenedReports_<
  Reports,
  Result extends number = 0
> = Reports extends [infer report extends number[], ...infer rest]
  ? IsReportSafeDampened<report> extends true
    ? numberOfSafeDampenedReports_<rest, Add<Result, 1>>
    : numberOfSafeDampenedReports_<rest, Result>
  : Result

type numberOfSafeDampenedReports = numberOfSafeDampenedReports_<reports>
