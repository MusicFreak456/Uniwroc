// Jak zdefiniować nowy obiekt zawierający co najmniej jedno pole, jedną metodę 
// oraz właściwości z akcesorami get i set?
let exampleObject = {
    exampleProperty1: 42,
    exampleMethod: x => x * 2,
    _property: 7,
    get propertyWithAccessors() {
        return this._property
    },
    set propertyWithAccessors(x) {
        this._property = x * 2
    }
}

console.log( exampleObject.propertyWithAccessors ) // 7
exampleObject.propertyWithAccessors = 7
console.log( exampleObject.propertyWithAccessors ) // 14

// Jak do istniejącego obiektu dodać nowe pole, nową metodę i nową właściwość z 
// akcesorami get i set?

Object.defineProperty(exampleObject, "newProperty", {
    value: 44,
    configurable: true,
    writable: true,
    enumerable: true
})
// Można inaczej: exampleObject.newProperty = 44

console.log( exampleObject.newProperty ) // 44

Object.defineProperty(exampleObject, "newMethod", {
    value: function() { return this.exampleProperty1 },
    configurable: true,
    writable: true,
    enumerable: true
})
// Można inaczej: 
// exampleObject.newMethod = function() { return this.exampleProperty1 }

console.log( exampleObject.newMethod ) // [function: value]

Object.defineProperty(exampleObject, "newAccessor", {
    get: function() { return this._property },
    set: function(x) { this._property = x },
})
// Nie można inaczej

console.log( exampleObject.newAccessor ) // 14