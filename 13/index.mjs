import fs from 'node:fs'

const tranpose = (rows) => {
  return rows[0]
    .split('')
    .map((_, colIndex) => rows.map((row) => row[colIndex]).join(''))
}

const hasExactlyOneDifference = (row1, row2) => {
  if (!row1 || !row2) return false

  let foundDifference = false

  for (let i = 0; i < row1.length; i++) {
    if (row1.charAt(i) !== row2.charAt(i)) {
      if (foundDifference) {
        return false
      } else {
        foundDifference = true
      }
    }
  }

  return foundDifference
}

const getReflection = (rows, mustSmudge, blacklist = []) => {
  for (let i = 0; i < rows.length; i++) {
    if (rows[i] === rows[i + 1] && !blacklist.includes(i)) {
      const rowsToCompare = Math.min(i, rows.length - i - 2)

      for (let j = 0; j < rowsToCompare; j++) {
        const prevRow = rows[i - j - 1]
        const nextRow = rows[i + j + 2]

        if (prevRow !== nextRow) {
          if (mustSmudge && hasExactlyOneDifference(prevRow, nextRow)) {
            const maybeNewReflection = getReflection(
              Object.assign([], rows, { [i - j - 1]: nextRow }),
              false,
              [...blacklist, i]
            )

            if (maybeNewReflection) {
              return maybeNewReflection
            }
          }
          return getReflection(rows, mustSmudge, [...blacklist, i])
        }
      }

      if (mustSmudge) {
        return getReflection(rows, mustSmudge, [...blacklist, i])
      }

      return i + 1
    }

    if (hasExactlyOneDifference(rows[i], rows[i + 1])) {
      const maybeNewReflection = getReflection(
        Object.assign([], rows, { [i]: rows[i + 1] }),
        false,
        [...blacklist, i]
      )

      if (maybeNewReflection) {
        return maybeNewReflection
      }
    }
  }
}

const getPatternNote = (mustSmudge) => (pattern) => {
  const rows = pattern.split('\n')

  const reflectedRowsAbove = getReflection(rows, mustSmudge)

  if (reflectedRowsAbove > 0) {
    return reflectedRowsAbove * 100
  }

  const reflectedColumnsLeft = getReflection(tranpose(rows), mustSmudge)

  return reflectedColumnsLeft
}

const solve = (input, handleSmudge) => {
  const patterns = input.replaceAll(/\r/g, '').split('\n\n')
  return patterns
    .map(getPatternNote(handleSmudge))
    .reduce((acc, x) => console.log(acc) || acc + x)
}

const input = fs.readFileSync('./input.txt').toString()

const start = performance.now()
const result1 = solve(input, false)
const end = performance.now()

const start2 = performance.now()
const result2 = solve(input, true)
const end2 = performance.now()

console.log(`Result 1: ${result1}. Execution time: ${end - start} ms`)
console.log(`Result 2: ${result2}. Execution time: ${end2 - start2} ms`)
