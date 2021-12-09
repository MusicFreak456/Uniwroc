const rules =  {
  required: name => value => !!value || `Pole "${name}" jest wymagane`
  // tu można dodać więcej reguł
}

const labels = {
  name : "Imię",
  surname : "Nazwisko",
  class : "Przedmiot"
}

const fieldRules =  {
  // dla każdego pola można przyporządkować więcej reguł
  'name': [rules.required(labels['name'])],
  'surname': [rules.required(labels['surname'])],
  'class': [rules.required(labels['class'])]
}

const initEmptyDeclaration  = () => {
  let emptyDeclaration = {
    name: "",
    surnamne: "",
    class: ""
  }

  for(let i=1; i<=10; i++) {
    emptyDeclaration["exc" + i] = "0"
  }

  return emptyDeclaration
}

const validate = (body) => {
  for(let field in fieldRules) {
    let value = body[field]

    for(let rule of fieldRules[field]) {
      let result = rule(value)
      
      if(result !== true){
        return [false, result]
      }
    }
  }
  return [true, undefined]
}

const urlParamsOfArgs = (args) => {
  let acc = "?"
  for(let key in args) {
    acc += "&" + key.toString() + "=" + args[key].toString()
  }
  return acc
}

module.exports = { 
  validate, 
  urlParamsOfArgs, 
  emptyDeclaration: initEmptyDeclaration(),
  labels
}