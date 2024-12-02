import fs from 'node:fs'

const getNumberOfWaysToWin = ([time, recordDistance]) => {
  return Array(time - 1)
    .fill()
    .map((_, i) => i + 1)
    .filter(
      (secondsHolded) => (time - secondsHolded) * secondsHolded > recordDistance
    ).length
}

const solve1 = (input) => {
  const [times, distances] = input.split('\n').map((x) =>
    x
      .replace('Time:', '')
      .replace('Distance:', '')
      .trim()
      .split(/[ \t]+/g)
      .map(Number)
  )
  const pairs = times.map((t, i) => [t, distances[i]])

  return pairs.map(getNumberOfWaysToWin).reduce((acc, x) => acc * x)
}

const solve2 = (input) => {
  const [time, distance] = input
    .split('\n')
    .map((x) =>
      x
        .replace('Time:', '')
        .replace('Distance:', '')
        .trim()
        .replaceAll(/[ \t]+/g, '')
    )
    .map(Number)

  return getNumberOfWaysToWin([time, distance])
}

const input = fs.readFileSync('./input.txt').toString()

const result1 = solve1(input)
const result2 = solve2(input)

console.log(result1, result2)
