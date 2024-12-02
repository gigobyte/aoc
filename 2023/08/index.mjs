import fs from 'node:fs'

const parseNodes = (lines) =>
  lines.reduce((acc, x) => {
    const [key, elements] = x.split(' = ')
    acc[key] = elements.replace(/\(|\)/g, '').split(', ')
    return acc
  }, {})

const runInstructions = (instructions, nodeMap, startingPoint, isLastPoint) => {
  let steps = 0
  let location = startingPoint

  while (!isLastPoint(location)) {
    for (const instruction of instructions) {
      location = nodeMap[location][instruction === 'L' ? 0 : 1]
      steps++
    }
  }

  return steps
}

function gcd(a, b) {
  return !b ? a : gcd(b, a % b)
}

const solve1 = (input) => {
  const [instructions, , ...lines] = input.split('\n').map((x) => x.trim())
  const nodeMap = parseNodes(lines)

  return runInstructions(instructions, nodeMap, 'AAA', (l) => l === 'ZZZ')
}

const solve2 = (input) => {
  const [instructions, , ...lines] = input.split('\n').map((x) => x.trim())
  const nodeMap = parseNodes(lines)

  const steps = Object.keys(nodeMap)
    .filter((x) => x.endsWith('A'))
    .map((start) =>
      runInstructions(instructions, nodeMap, start, (l) => l.endsWith('Z'))
    )
    .reduce((acc, x) => (acc * x) / gcd(acc, x))

  return steps
}

const input = fs.readFileSync('./input.txt').toString()

const result1 = solve1(input)
const result2 = solve2(input)

console.log(result1, result2)
