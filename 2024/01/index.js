import fs from 'node:fs'

const LIST_SIZE = 1000

function solve1(input) {
  // Typed array can be sorted faster
  const leftList = new Uint32Array(LIST_SIZE)
  const rightList = new Uint32Array(LIST_SIZE)

  const lines = input.split('\n')

  // comparing to LIST_SIZE is faster than checking array.length on every iteration
  for (let i = 0; i < LIST_SIZE; i++) {
    const line = lines[i]
    // indexOf + substring is faster than split because it doesn't create intermediary arrays
    const separatorIndex = line.indexOf('   ')
    // + is faster than parseInt
    leftList[i] = +line.substring(0, separatorIndex)
    rightList[i] = +line.substring(separatorIndex + 3)
  }

  leftList.sort()
  rightList.sort()

  let totalDistance = 0

  for (let i = 0; i < LIST_SIZE; i++) {
    const leftNumber = leftList[i]
    const rightNumber = rightList[i]
    totalDistance += Math.abs(leftNumber - rightNumber)
  }

  return totalDistance
}

function solve2(input) {
  const leftList = new Uint32Array(LIST_SIZE)
  const rightMap = new Map()

  const lines = input.split('\n')

  for (let i = 0; i < LIST_SIZE; i++) {
    const line = lines[i]
    const separatorIndex = line.indexOf('   ')
    leftList[i] = +line.substring(0, separatorIndex)
    const rightNumber = +line.substring(separatorIndex + 3)
    rightMap.set(rightNumber, (rightMap.get(rightNumber) ?? 0) + 1)
  }

  let similarityScore = 0

  for (let i = 0; i < LIST_SIZE; i++) {
    const leftNumber = leftList[i]
    similarityScore += leftNumber * (rightMap.get(leftNumber) ?? 0)
  }

  return similarityScore
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
