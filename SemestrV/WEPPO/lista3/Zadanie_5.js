function sum(...nums) {
  return nums.reduce((acc, x) => acc + x, 0)
}

console.log(sum(1,2,3)) // 6

console.log(sum(1,2,3,4,5)) // 15