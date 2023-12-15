import fs from 'node:fs'

const getValue = (step) =>
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

  return steps.map(getValue).reduce((acc, x) => acc + x)
}

const solve2 = (input) => {}

const input = fs.readFileSync('./input.txt').toString()

const start = performance.now()
const result1 = solve1(input)
const end = performance.now()

// const start2 = performance.now()
// const result2 = solve2(input)
// const end2 = performance.now()

console.log(`Result 1: ${result1}. Execution time: ${end - start} ms`)
// console.log(`Result 2: ${result2}. Execution time: ${end2 - start2} ms`)
