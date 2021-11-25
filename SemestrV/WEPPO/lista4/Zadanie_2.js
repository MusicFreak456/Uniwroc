var Foo  = (() => {
  function Qux() {
    console.log("Hello")
  }

  function Foo() {}

  Foo.prototype.Bar = function() {
    Qux()
  }

  return Foo
})()

var f = new Foo()

f.Bar()