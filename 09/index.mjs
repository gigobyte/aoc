import fs from 'node:fs'

const sum = (arr) => arr.reduce((acc, x) => acc + x)

const getExtrapolations = (prevExtrapolations) => {
  const last = prevExtrapolations[prevExtrapolations.length - 1]

  if (last.every((x) => x === 0)) {
    return prevExtrapolations
  } else {
    const diffs = []

    for (const [i, e] of last.entries()) {
      if (last[i + 1] !== undefined) {
        diffs.push(last[i + 1] - e)
      }
    }

    return getExtrapolations([...prevExtrapolations, diffs])
  }
}

const getNextValue = (backwards) => (history) => {
  const extrapolations = getExtrapolations([
    backwards ? [...history].reverse() : history
  ])

  let nextValue = 0

  for (const extrapolation of extrapolations.reverse().slice(1)) {
    nextValue += extrapolation[extrapolation.length - 1]
  }

  return nextValue
}

const solve = (input, backwards) => {
  return input
    .split('\n')
    .map((x) => x.trim().split(' ').map(Number))
    .map(getNextValue(backwards))
    .reduce((acc, x) => acc + x)
}

const input = fs.readFileSync('./input.txt').toString()

const result1 = solve(input, false)
const result2 = solve(input, true)

console.log(result1, result2)
