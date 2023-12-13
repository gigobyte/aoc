import fs from 'node:fs'

const isEmptyColumn = (grid, col) => {
  for (const row of grid) {
    if (row[col] !== '.') {
      return false
    }
  }
  return true
}

const getPairs = (arr) => {
  const uniquePairs = []

  for (let i = 0; i < arr.length - 1; i++) {
    for (let j = i + 1; j < arr.length; j++) {
      const pair = [arr[i], arr[j]]
      uniquePairs.push(pair)
    }
  }

  return uniquePairs
}

const getAllGalaxyPairs = (grid) => {
  const galaxies = []

  for (const [rowIndex, row] of grid.entries()) {
    for (const [columnIndex, space] of row.entries()) {
      if (space === '#') {
        galaxies.push({ x: rowIndex, y: columnIndex })
      }
    }
  }

  return getPairs(galaxies)
}

const range = (start, end) => {
  if (end < start) {
    ;[end, start] = [start, end]
  }

  return [...Array(Math.abs(end - start)).keys()].map((i) => i + start)
}

const getShortestDistance =
  (grid, expansionRate) =>
  ([p1, p2]) => {
    const p1copy = { ...p1 }
    const p2copy = { ...p2 }

    let numberOfEmptyRows = 0
    let numberOfEmptyCols = 0

    for (const row of grid.slice(p1.x, p2.x)) {
      if (row.some((space) => space !== '.')) {
        continue
      }
      numberOfEmptyRows++
    }

    for (const columnIndex of range(p1.y, p2.y)) {
      if (isEmptyColumn(grid, columnIndex)) {
        numberOfEmptyCols++
      }
    }

    if (p2.x > p1.x) {
      p2copy.x += numberOfEmptyRows * (expansionRate - 1)
    } else {
      p1copy.x += numberOfEmptyRows * (expansionRate - 1)
    }

    if (p2.y > p1.y) {
      p2copy.y += numberOfEmptyCols * (expansionRate - 1)
    } else {
      p1copy.y += numberOfEmptyCols * (expansionRate - 1)
    }

    // https://xlinux.nist.gov/dads/HTML/manhattanDistance.html
    const deltaX = Math.abs(p1copy.x - p2copy.x)
    const deltaY = Math.abs(p1copy.y - p2copy.y)

    return deltaX + deltaY
  }

const solve = (input, expansionRate) => {
  const grid = input.split('\n').map((x) => x.trim().split(''))

  const pairs = getAllGalaxyPairs(grid)

  return pairs
    .map(getShortestDistance(grid, expansionRate))
    .reduce((acc, x) => acc + x)
}

const input = fs.readFileSync('./input.txt').toString()

const result1 = solve(input, 2)
const result2 = solve(input, 1000000)

console.log(result1, result2)
