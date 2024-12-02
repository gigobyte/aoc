import fs from 'node:fs'

const tranpose = (rows) => {
  return rows[0]
    .split('')
    .map((_, colIndex) => rows.map((row) => row[colIndex]).join(''))
}

const hasExactlyOneDifference = (row1, row2) => {
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

const getReflection = (rows, mustSmudge, ignoredReflection, deadEnds = []) => {
  for (let i = 0; i < rows.length; i++) {
    if (
      rows[i] === rows[i + 1] &&
      !deadEnds.includes(i) &&
      (ignoredReflection ? i !== ignoredReflection - 1 : true)
    ) {
      const rowsToCompare = Math.min(i, rows.length - i - 2)

      for (let j = 0; j < rowsToCompare; j++) {
        const prevRow = rows[i - j - 1]
        const nextRow = rows[i + j + 2]

        if (nextRow && prevRow !== nextRow) {
          if (mustSmudge && hasExactlyOneDifference(prevRow, nextRow)) {
            const maybeNewReflection = getReflection(
              Object.assign([], rows, { [i - j - 1]: nextRow }),
              false,
              ignoredReflection
            )

            if (maybeNewReflection) {
              return maybeNewReflection
            }
          }

          return getReflection(rows, mustSmudge, ignoredReflection, [
            ...deadEnds,
            i
          ])
        }
      }

      if (!mustSmudge) {
        return i + 1
      }
    }

    if (
      mustSmudge &&
      rows[i + 1] &&
      hasExactlyOneDifference(rows[i], rows[i + 1])
    ) {
      const maybeNewReflection = getReflection(
        Object.assign([], rows, { [i]: rows[i + 1] }),
        false,
        ignoredReflection
      )

      if (maybeNewReflection) {
        return maybeNewReflection
      }
    }
  }
}

const getPatternNote = (mustSmudge) => (pattern) => {
  const rows = pattern.split('\n')

  const unsmudgedRowReflection = mustSmudge ? getReflection(rows, false) : null
  const reflectedRowsAbove = getReflection(
    rows,
    mustSmudge,
    unsmudgedRowReflection
  )

  if (reflectedRowsAbove > 0) {
    return reflectedRowsAbove * 100
  }

  const columns = tranpose(rows)

  const unsmudgedColumnReflection = mustSmudge
    ? getReflection(columns, false)
    : null
  const reflectedColumnsLeft = getReflection(
    columns,
    mustSmudge,
    unsmudgedColumnReflection
  )

  return reflectedColumnsLeft
}

const solve = (input, mustSmudge) => {
  const patterns = input.replaceAll(/\r/g, '').split('\n\n')
  return patterns.map(getPatternNote(mustSmudge)).reduce((acc, x) => acc + x)
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
