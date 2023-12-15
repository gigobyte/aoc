import fs from 'node:fs'

const transpose = (rows) => {
  return rows[0].map((_, colIndex) => rows.map((row) => row[colIndex]))
}

const rotateBackwards = (rows) => {
  return rows[0].map((_, index) =>
    rows.map((row) => row[row.length - 1 - index])
  )
}

const rotateForwards = (rows) => {
  return rows[0].map((_, index) => rows.map((row) => row[index]).reverse())
}

const blockers = ['#', 'O']

const calculateLoad = (tilted) => {
  let load = 0

  for (let i = tilted.length - 1; i >= 0; i--) {
    if (tilted[i] === 'O') {
      load += tilted.length - i
    }
  }

  return load
}

const tiltColumn = (column) => {
  let tilted = [column[0]]

  for (let i = 1; i < column.length; i++) {
    const rock = column[i]

    if (rock === '.' || rock === '#') {
      tilted.push(rock)
    } else {
      let moved = false

      for (let j = tilted.length - 1; j >= 0; j--) {
        if (blockers.includes(tilted[j])) {
          tilted[i] = '.'
          tilted[j + 1] = rock
          moved = true
          break
        }
      }

      if (!moved) {
        for (let j = 0; j < tilted.length; j++) {
          if (tilted[j] === '.') {
            tilted[i] = '.'
            tilted[j] = rock
            break
          }
        }
      }
    }
  }

  return tilted
}

const getLoad = (column) => {
  return calculateLoad(tiltColumn(column))
}

const parseInput = (input) =>
  input
    .replaceAll(/\r/g, '')
    .split('\n')
    .map((x) => x.split(''))

const runCycle = (rows) => {
  const columns = transpose(rows)

  const north = transpose(columns.map(tiltColumn))
  const west = north.map(tiltColumn)
  const south = rotateBackwards(rotateForwards(west).map(tiltColumn))
  const east = rotateForwards(
    rotateForwards(rotateBackwards(rotateBackwards(south)).map(tiltColumn))
  )

  return east
}

const solve1 = (input) => {
  const columns = transpose(parseInput(input))
  return columns.map(getLoad).reduce((acc, x) => acc + x)
}

const numberOfCycles = 1_000_000_000

const solve2 = (input) => {
  const rows = parseInput(input)

  const cycleCache = new Map()

  let cycle = runCycle(rows)
  cycleCache.set(cycle.toString(), 0)

  for (let i = 1; i < numberOfCycles; i++) {
    const newCycle = runCycle(cycle)

    const key = newCycle.toString()

    if (cycleCache.has(key)) {
      const remainingCycles = numberOfCycles - i
      const cyclesSinceSamePattern = cycleCache.get(key) - i

      if (remainingCycles % cyclesSinceSamePattern === 0) {
        break
      }
    } else {
      cycleCache.set(key, i)
    }

    cycle = newCycle
  }

  return transpose(cycle)
    .map(calculateLoad)
    .reduce((acc, x) => acc + x)
}

const input = fs.readFileSync('./input.txt').toString()

const start = performance.now()
const result1 = solve1(input)
const end = performance.now()

const start2 = performance.now()
const result2 = solve2(input)
const end2 = performance.now()

console.log(`Result 1: ${result1}. Execution time: ${end - start} ms`)
console.log(`Result 2: ${result2}. Execution time: ${end2 - start2} ms`)
