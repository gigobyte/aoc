import fs from 'node:fs'

const cardStrength = [
  'A',
  'K',
  'Q',
  'J',
  'T',
  '9',
  '8',
  '7',
  '6',
  '5',
  '4',
  '3',
  '2'
]

const cardStrength2 = [
  'A',
  'K',
  'Q',
  'T',
  '9',
  '8',
  '7',
  '6',
  '5',
  '4',
  '3',
  '2',
  'J'
]

const getHandPower = (hand, jAsJoker) => {
  const handSplit = hand.split('')

  if (jAsJoker) {
    return Math.max(
      ...handSplit
        .map((card) => handSplit.map((c) => (c === 'J' ? card : c)))
        .map((cards) => getHandPower(cards.join(''), false))
    )
  }

  const cardOccurances = handSplit.map((c) => hand.split(c).length - 1)
  const nOfAKind = Math.max(...cardOccurances)

  // Five of a kind
  if (nOfAKind === 5) {
    return 7
    //Four of a kind
  } else if (nOfAKind === 4) {
    return 6
  }

  // Full house
  if (cardOccurances.every((x) => x >= 2 && x <= 3)) {
    return 5
  }

  // Three of a kind
  if (nOfAKind === 3) {
    return 4
  }

  const pairs = cardOccurances.filter((x) => x === 2).length

  // Two pair
  if (pairs === 4) {
    return 3
  }

  // One pair
  if (pairs === 2) {
    return 2
  }

  // High card
  return 1
}

const compareByCard = (hand1, hand2, strength) => {
  const hand2Split = hand2.split('')

  for (const [i, c1] of hand1.split('').entries()) {
    const c2 = hand2Split[i]
    if (strength.indexOf(c1) > strength.indexOf(c2)) {
      return -1
    } else if (strength.indexOf(c1) < strength.indexOf(c2)) {
      return 1
    }
  }
}

const compareHands = ([hand1], [hand2]) => {
  const powerDiff = getHandPower(hand1, false) - getHandPower(hand2, false)

  if (powerDiff === 0) {
    return compareByCard(hand1, hand2, cardStrength)
  }

  return powerDiff
}

const compareHands2 = ([hand1], [hand2]) => {
  const powerDiff = getHandPower(hand1, true) - getHandPower(hand2, true)

  if (powerDiff === 0) {
    return compareByCard(hand1, hand2, cardStrength2)
  }

  return powerDiff
}

const solve1 = (input) => {
  const hands = input
    .split('\n')
    .map((x) => x.trim().split(' '))
    .sort(compareHands)
    .reduce((acc, [_, bid], i) => acc + Number(bid) * (i + 1), 0)
  return hands
}

const solve2 = (input) => {
  const hands = input
    .split('\n')
    .map((x) => x.trim().split(' '))
    .sort(compareHands2)
    .reduce((acc, [_, bid], i) => acc + Number(bid) * (i + 1), 0)
  return hands
}

const input = fs.readFileSync('./input.txt').toString()

const result1 = solve1(input)
const result2 = solve2(input)

console.log(result1, result2)
