import fs from 'node:fs'

const replaceAt = (str, i, replacement) => {
  return str.substring(0, i) + replacement + str.substring(i + 1)
}

const isValidIteration = (iteration, maxBrokenSprings) => {
  const numberOfAlreadyBroken = iteration.split('#').length - 1

  if (numberOfAlreadyBroken > maxBrokenSprings) {
    return false
  }

  const numberOfPossiblyBroken = iteration.split(/#|\?/g).length - 1

  if (numberOfPossiblyBroken < maxBrokenSprings) {
    return false
  }

  return true
}

const getAllPossibleArrangements = (springs, sizes) => {
  const maxBrokenSprings = sizes.reduce((acc, x) => acc + x)

  let iterations = [springs]

  for (let i = 0; i < springs.length; i++) {
    if (springs[i] === '?') {
      const currentIterationsNum = iterations.length

      for (let j = 0; j < currentIterationsNum; j++) {
        const dotIteration = replaceAt(iterations[j], i, '.')
        const hashIteration = replaceAt(iterations[j], i, '#')

        if (isValidIteration(dotIteration, maxBrokenSprings)) {
          iterations.push(dotIteration)
        }
        if (isValidIteration(hashIteration, maxBrokenSprings)) {
          iterations.push(hashIteration)
        }

        console.log(iterations)
      }
    }
  }

  return iterations.filter((x) => !x.includes('?'))
}

const getNumberOfArrangements = (record) => {
  let [springs, sizesStr] = record.split(' ')
  const sizes = sizesStr.split(',').map(Number)

  const allPossibleArrangements = getAllPossibleArrangements(springs, sizes)

  console.log(allPossibleArrangements.length)

  let validCount = 0

  for (const arrangement of allPossibleArrangements) {
    const brokenSpringsLengths = arrangement
      .split('.')
      .filter((x) => x.includes('#'))
      .map((x) => x.length)

    if (brokenSpringsLengths.length !== sizes.length) {
      continue
    }

    validCount++

    for (let i = 0; i < sizes.length; i++) {
      if (brokenSpringsLengths[i] !== sizes[i]) {
        validCount--
        break
      }
    }
  }

  return validCount
}

const solve = (input) => {
  const records = input.split('\n').map((x) => x.trim())

  let sum = 0

  for (const [i, record] of records.entries()) {
    console.log('Done with', i, 'of', records.length)
    sum += getNumberOfArrangements(record)
  }

  return sum
}

const input = fs.readFileSync('./input.txt').toString()

const start = performance.now()

const result1 = solve(input)
const end = performance.now()

console.log(result1)
console.log(`Execution time: ${end - start} ms`)
