import fs from 'node:fs'

const runHashAlgorithm = (step) =>
  step.split('').reduce((value, c) => {
    let newValue = value

    const ascii = c.charCodeAt(0)
    newValue += ascii
    newValue *= 17
    newValue %= 256

    return newValue
  }, 0)

const solve1 = (input) => {
  const steps = input.replaceAll(/\r|\n/g, '').split(',')

  return steps.map(runHashAlgorithm).reduce((acc, x) => acc + x)
}

const solve2 = (input) => {
  const steps = input.replaceAll(/\r|\n/g, '').split(',')

  const lensMap = {}
  const boxes = Object.fromEntries([...Array(256).keys()].map((i) => [i, []]))

  for (const step of steps) {
    const [label, operation, focalLengthRaw] = step.split(/(=|-)/)

    const focalLength = Number(focalLengthRaw)
    const boxIndex = runHashAlgorithm(label)
    const box = boxes[boxIndex]

    if (operation === '-') {
      boxes[boxIndex] = box.filter((b) => b !== label)
    }

    if (operation === '=') {
      lensMap[label] = focalLength

      const existingLensIndex = box.findIndex((x) => x === label)

      if (existingLensIndex !== -1) {
        box[existingLensIndex] = label
      } else {
        box.push(label)
      }
    }
  }

  let total = 0

  for (const [i, lenses] of Object.entries(boxes)) {
    for (const [j, lens] of lenses.entries()) {
      total += (1 + Number(i)) * (j + 1) * lensMap[lens]
    }
  }

  return total
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
