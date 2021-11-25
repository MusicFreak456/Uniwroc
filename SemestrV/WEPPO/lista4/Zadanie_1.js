var Queue = (() => {
  let listSymbol = Symbol("listSymbol")

  function Queue() {
    this[listSymbol] = []
  }

  Queue.prototype.isEmpty = function() {
    return this[listSymbol] == +[]
  }

  Queue.prototype.toList = function() {
    return this[listSymbol]
  }

  Queue.prototype.enqueue = function(x) {
    this[listSymbol].push(x)
  }

  Queue.prototype.dequeue = function() {
    return this[listSymbol].splice(0, 1)[0]
    // return this[listSymbol].shift() // tak by było ładniej
  }

  return Queue
})()

let q = new Queue()

console.log(q.toList())
console.log([] == [])

function Tree(val, left, right) {
  this.left = left;
  this.right = right;
  this.val = val;
}

// Tree.prototype[Symbol.iterator] = function* () {
//   yield this.val;
//   if (this.left) yield* this.left;
//   if (this.right) yield* this.right;
// }

Tree.prototype[Symbol.iterator] = function* () {
  let queue = new Queue()
  queue.enqueue(this)
  while(!queue.isEmpty()) {
    x = queue.dequeue()
    yield x.val
    if(x.right) queue.enqueue(x.right)
    if(x.left)  queue.enqueue(x.left)
  }
}

var root = new Tree(1,
  new Tree(2, new Tree(3)), new Tree(4));

for (var e of root) {
  console.log(e);
} // 1 2 3 4

// Gdyby zamienić kolejkę na stos to znowu otrzymamy DFS'a