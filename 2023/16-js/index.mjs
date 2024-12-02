import fs from 'node:fs'

const advance = ([x, y], direction) => {
  switch (direction) {
    case 'up':
      return [x - 1, y]
    case 'right':
      return [x, y + 1]
    case 'down':
      return [x + 1, y]
    case 'left':
      return [x, y - 1]
  }
}

const rightMirrorMap = { right: 'up', left: 'down', up: 'right', down: 'left' }
const leftMirrorMap = { right: 'down', left: 'up', up: 'left', down: 'right' }

const getEnergizedTiles = (
  grid,
  position = [0, 0],
  direction = 'right',
  visited = [],
  energized = []
) => {
  if (visited.includes([position, direction].toString())) {
    return energized
  }

  const tile = grid[position.toString()]

  if (tile) {
    energized.push(position.toString())
    visited.push([position, direction].toString())

    const continued = (dir) =>
      getEnergizedTiles(grid, advance(position, dir), dir, visited, energized)

    const splitInto = (dir1, dir2) =>
      energized
        .concat(
          getEnergizedTiles(grid, advance(position, dir1), dir1, visited, [])
        )
        .concat(
          getEnergizedTiles(grid, advance(position, dir2), dir2, visited, [])
        )

    if (tile === '.') {
      return continued(direction)
    }
    if (tile === '/') {
      return continued(rightMirrorMap[direction])
    }
    if (tile === '\\') {
      return continued(leftMirrorMap[direction])
    }
    if (tile === '|') {
      if (['up', 'down'].includes(direction)) {
        return continued(direction)
      } else {
        return splitInto('up', 'down')
      }
    }
    if (tile === '-') {
      if (['left', 'right'].includes(direction)) {
        return continued(direction)
      } else {
        return splitInto('left', 'right')
      }
    }
  } else {
    return energized
  }
}

const makeGrid = (input) => {
  const lines = input.replaceAll(/\r/g, '').split('\n')

  const grid = {}

  for (let i = 0; i < lines.length; i++) {
    for (let j = 0; j < lines[i].length; j++) {
      grid[[i, j].toString()] = lines[i][j]
    }
  }

  return grid
}

const countUniqueEnergizedTiles = (tiles) => [...new Set(tiles)].length

const solve1 = (input) => {
  const grid = makeGrid(input)
  return countUniqueEnergizedTiles(getEnergizedTiles(grid))
}

const solve2 = (input) => {
  const lines = input.replaceAll(/\r/g, '').split('\n')
  const grid = makeGrid(input)
  const tilesFromEachRun = []

  for (let i = 0; i < lines.length; i++) {
    tilesFromEachRun.push(
      countUniqueEnergizedTiles(getEnergizedTiles(grid, [i, 0], 'right'))
    )
  }

  for (let i = 0; i < lines[0].length; i++) {
    tilesFromEachRun.push(
      countUniqueEnergizedTiles(getEnergizedTiles(grid, [0, i], 'down'))
    )
  }

  for (let i = 0; i < lines.length; i++) {
    tilesFromEachRun.push(
      countUniqueEnergizedTiles(
        getEnergizedTiles(grid, [i, lines[0].length - 1], 'left')
      )
    )
  }

  for (let i = 0; i < lines[0].length; i++) {
    tilesFromEachRun.push(
      countUniqueEnergizedTiles(
        getEnergizedTiles(grid, [lines.length - 1, i], 'up')
      )
    )
  }

  return Math.max(...tilesFromEachRun)
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
