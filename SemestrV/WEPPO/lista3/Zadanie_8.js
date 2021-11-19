function *fib(){
  var curr = 0;
  var next = 1;

  while(true) {
    let temp = curr;
    curr = next;
    next = curr + temp;
    yield temp;
  }
}

function *take(it, top) {
  let taken = 0;
  for (var result; result = it.next(), !result.done;) {
    taken++
    yield result.value
    if(taken == top) break;
  }
}

for(let num of take(fib(), 10)) {
  console.log(num);
}