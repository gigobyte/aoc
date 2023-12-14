import fs from 'node:fs'

const getColumn = (rows, i) => rows.reduce((acc, x) => `${acc}${x[i]}`, '')

const copyColumn = (rows, fromThisIndex, toThisIndex) => {
  return rows.map(
    (row) =>
      row.substring(0, toThisIndex) +
      row[fromThisIndex] +
      row.substring(toThisIndex + 1)
  )
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

const getHorizontalReflection = (rows, mustSmudge, blacklist = []) => {
  for (let i = 0; i < rows.length; i++) {
    if (rows[i] === rows[i + 1] && !blacklist.includes(i)) {
      const rowsToCompare = Math.min(i, rows.length - i - 2)

      for (let j = 0; j < rowsToCompare; j++) {
        const prevRow = rows[i - j - 1]
        const nextRow = rows[i + j + 2]

        if (prevRow !== nextRow) {
          if (mustSmudge && hasExactlyOneDifference(prevRow, nextRow)) {
            const maybeNewReflection = getHorizontalReflection(
              Object.assign([], rows, { [i - j - 1]: nextRow }),
              false,
              [...blacklist, i]
            )

            if (maybeNewReflection) {
              return maybeNewReflection
            }
          }
          return getHorizontalReflection(rows, mustSmudge, [...blacklist, i])
        }
      }

      if (mustSmudge) {
        return getHorizontalReflection(rows, mustSmudge, [...blacklist, i])
      }

      return i + 1
    }

    if (hasExactlyOneDifference(rows[i], rows[i + 1])) {
      const maybeNewReflection = getHorizontalReflection(
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

const getVerticalReflection = (rows, mustSmudge, blacklist = []) => {
  const numberOfColumns = rows[0].length

  for (let i = 0; i < numberOfColumns; i++) {
    if (i < numberOfColumns - 1) {
      const column = getColumn(rows, i)
      const nextColumn = getColumn(rows, i + 1)

      if (column === nextColumn && !blacklist.includes(i)) {
        const columnsToCompare = Math.min(i, numberOfColumns - i - 2)

        for (let j = 0; j < columnsToCompare; j++) {
          const prevColumn = getColumn(rows, i - j - 1)
          const nextColumn = getColumn(rows, i + j + 2)

          if (prevColumn !== nextColumn) {
            if (mustSmudge && hasExactlyOneDifference(prevColumn, nextColumn)) {
              const maybeNewReflection = getVerticalReflection(
                copyColumn(rows, i + j + 2, i - j - 1),
                false,
                [...blacklist, i]
              )

              if (maybeNewReflection) {
                return maybeNewReflection
              }
            }

            return getVerticalReflection(rows, mustSmudge, [...blacklist, i])
          }
        }

        console.log({ i, mustSmudge })

        if (mustSmudge) {
          return getVerticalReflection(rows, mustSmudge, [...blacklist, i])
        }

        return i + 1
      }

      if (hasExactlyOneDifference(column, nextColumn)) {
        const maybeNewReflection = getVerticalReflection(
          copyColumn(rows, i + 1, i),
          false,
          [...blacklist, i]
        )

        if (maybeNewReflection) {
          return maybeNewReflection
        }
      }
    }
  }
}

const getPatternNote = (mustSmudge) => (pattern) => {
  const rows = pattern.split('\n')

  const reflectedRowsAbove = getHorizontalReflection(rows, mustSmudge)

  if (reflectedRowsAbove > 0) {
    return reflectedRowsAbove * 100
  }

  const reflectedColumnsLeft = getVerticalReflection(rows, mustSmudge)

  return reflectedColumnsLeft
}

const solve = (input, handleSmudge) => {
  const patterns = input.replaceAll(/\r/g, '').split('\n\n')
  return patterns
    .map(getPatternNote(handleSmudge))
    .reduce((acc, x) => console.log(acc) || acc + x)
}

const input = fs.readFileSync('./input.txt').toString()

// const start = performance.now()
// const result1 = solve(input, false)
// const end = performance.now()

const start2 = performance.now()
const result2 = solve(input, true)
const end2 = performance.now()

// console.log(`Result 1: ${result1}. Execution time: ${end - start} ms`)
console.log(`Result 2: ${result2}. Execution time: ${end2 - start2} ms`)
