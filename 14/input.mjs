import fs from 'node:fs'

const replaceAt = (str, i, replacement) => {
  return str.substring(0, i) + replacement + str.substring(i + 1)
}

const tranpose = (rows) => {
  return rows[0]
    .split('')
    .map((_, colIndex) => rows.map((row) => row[colIndex]).join(''))
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
  let tilted = column[0]

  for (let i = 1; i < column.length; i++) {
    const rock = column[i]

    if (rock === '.' || rock === '#') {
      tilted += rock
    } else {
      let moved = false

      for (let j = tilted.length - 1; j >= 0; j--) {
        if (blockers.includes(tilted[j])) {
          tilted = replaceAt(tilted, i, '.')
          tilted = replaceAt(tilted, j + 1, rock)
          moved = true
          break
        }
      }

      if (!moved) {
        for (let j = 0; j < tilted.length; j++) {
          if (tilted[j] === '.') {
            tilted = replaceAt(tilted, i, '.')
            tilted = replaceAt(tilted, j, rock)
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

const solve1 = (input) => {
  const columns = tranpose(input.replaceAll(/\r/g, '').split('\n'))

  return columns.map(getLoad).reduce((acc, x) => acc + x)
}

const input = fs.readFileSync('./input.txt').toString()

const start = performance.now()
const result1 = solve1(input, false)
const end = performance.now()

// const start2 = performance.now()
// const result2 = solve(input, true)
// const end2 = performance.now()

console.log(`Result 1: ${result1}. Execution time: ${end - start} ms`)
// console.log(`Result 2: ${result2}. Execution time: ${end2 - start2} ms`)
