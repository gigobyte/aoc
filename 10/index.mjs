import fs from 'node:fs'

const isSamePos = ([x, y], [x2, y2]) => x === x2 && y === y2

const supportedDirections = {
  7: ['up', 'right'],
  F: ['up', 'left'],
  J: ['down', 'right'],
  L: ['down', 'left']
}

const advancePosition = (pipeMeta, map) => {
  const { direction, pos } = pipeMeta

  const pipe = map[pos[0]][pos[1]]

  if (pipe === '.') {
    throw new Error('wtf')
  }

  if (pipe === '-') {
    if (direction === 'right') {
      pipeMeta.pos = [pos[0], pos[1] + 1]
    } else if (direction === 'left') {
      pipeMeta.pos = [pos[0], pos[1] - 1]
    }
    return
  }

  if (pipe === '|') {
    if (direction === 'up') {
      pipeMeta.pos = [pos[0] - 1, pos[1]]
    } else if (direction === 'down') {
      pipeMeta.pos = [pos[0] + 1, pos[1]]
    }
    return
  }

  if (!supportedDirections[pipe].includes(direction)) {
    throw new Error('you messed up')
  }

  if (pipe === '7') {
    pipeMeta.direction = { up: 'left', right: 'down' }[direction]
    pipeMeta.pos = { up: [pos[0], pos[1] - 1], right: [pos[0] + 1, pos[1]] }[
      direction
    ]
  }

  if (pipe === 'F') {
    pipeMeta.direction = { up: 'right', left: 'down' }[direction]
    pipeMeta.pos = { up: [pos[0], pos[1] + 1], left: [pos[0] + 1, pos[1]] }[
      direction
    ]
  }

  if (pipe === 'J') {
    pipeMeta.direction = { down: 'left', right: 'up' }[direction]
    pipeMeta.pos = { down: [pos[0], pos[1] - 1], right: [pos[0] - 1, pos[1]] }[
      direction
    ]
  }

  if (pipe === 'L') {
    pipeMeta.direction = { down: 'right', left: 'up' }[direction]
    pipeMeta.pos = { down: [pos[0], pos[1] + 1], left: [pos[0] - 1, pos[1]] }[
      direction
    ]
  }
}

const solve = (input, returnArea) => {
  const lines = input.split('\n').map((x) => x.trim().split(''))

  let steps = 1

  const lineWithStartIndex = lines.findIndex((x) => x.includes('S'))
  const startPosition = [
    lineWithStartIndex,
    lines[lineWithStartIndex].findIndex((x) => x === 'S')
  ]

  const [start1, start2] = [
    [startPosition[0] - 1, startPosition[1]],
    [startPosition[0], startPosition[1] + 1],
    [startPosition[0] + 1, startPosition[1]],
    [startPosition[0], startPosition[1] - 1]
  ].reduce((acc, [x, y], i) => {
    const pipe = lines[x][y]
    const direction = {
      0: 'up',
      1: 'right',
      2: 'down',
      3: 'left'
    }[i]

    if (
      pipe === '.' ||
      (pipe === '-' && ['up', 'down'].includes(direction)) ||
      (pipe === '|' && ['left', 'right'].includes(direction))
    ) {
      return acc
    }

    acc.push({ direction, pos: [x, y] })

    return acc
  }, [])

  const pipesOfLoop = [startPosition, [...start1.pos], [...start2.pos]]

  while (!isSamePos(start1.pos, start2.pos)) {
    advancePosition(start1, lines)
    advancePosition(start2, lines)
    steps++
    pipesOfLoop.push([...start1.pos], [...start2.pos])
  }

  // some copy pasted formulas from the internet ¯\_(ツ)_/¯
  // ...

  return steps
}

const input = fs.readFileSync('./input.txt').toString()

// const result1 = solve(input, false)
const result2 = solve(input, true)

console.log(result2)
