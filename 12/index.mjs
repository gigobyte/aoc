import fs from 'node:fs'

const maybeBroken = ['#', '?']
const maybeWorking = ['.', '?']

// Yes, it only works if first arg is string and the second is array
// No, I won't make it a generic memoize :)
const memoize2 = (fn) => {
  const cache = new Map()

  return (arg1, arg2) => {
    const key = `${arg1}${arg2.toString()}`

    if (cache.has(key)) {
      return cache.get(key)
    }
    const result = fn(arg1, arg2)

    cache.set(key, result)

    return result
  }
}

const matchesBeginning = (springs, size) => {
  return (
    (springs.length === size || maybeWorking.includes(springs[size])) &&
    springs
      .substring(0, size)
      .split('')
      .every((spring) => maybeBroken.includes(spring))
  )
}

const getNumberOfArrangements = (record) => {
  let [springs, sizesStr] = record.split(' ')
  const sizes = sizesStr.split(',').map(Number)

  const go = memoize2((springs, sizes) => {
    const springsToAdd = sizes.reduce((acc, x) => acc + x, 0)
    const numberOfAlreadyBroken = springs.split('#').length - 1
    const numberOfPossiblyBroken = springs.split(/#|\?/g).length - 1

    if (
      numberOfAlreadyBroken > springsToAdd ||
      numberOfPossiblyBroken < springsToAdd
    ) {
      return 0
    }

    if (springsToAdd === 0) {
      return 1
    }

    if (springs[0] === '.') {
      return go(springs.substring(1), sizes)
    }

    if (springs[0] === '#') {
      const [firstSize] = sizes
      if (matchesBeginning(springs, firstSize)) {
        if (firstSize === springs.length) {
          return 1
        }
        return go(springs.substring(firstSize + 1), sizes.slice(1))
      }
      return 0
    }

    return (
      go(springs.substring(1), sizes) + go(`#${springs.substring(1)}`, sizes)
    )
  })

  return go(springs, sizes)
}

const solve1 = (input) => {
  const records = input.split('\n').map((x) => x.trim())

  let sum = 0

  for (const record of records) {
    sum += getNumberOfArrangements(record)
  }

  return sum
}

const solve2 = (input) => {
  const records = input.split('\n').map((x) => x.trim())

  let sum = 0

  for (const record of records) {
    let [springs, sizesStr] = record.split(' ')

    springs = Array(5).fill(springs).join('?')
    sizesStr = Array(5).fill(sizesStr).join(',')

    sum += getNumberOfArrangements(`${springs} ${sizesStr}`)
  }

  return sum
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
