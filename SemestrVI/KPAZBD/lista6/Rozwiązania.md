# Kurs projektowania aplikacji z bazami danych -- Lista 6
###### tags: `KPAZBD`

## Zadanie 1

![](https://i.imgur.com/pk6HhFW.png)

```shell=
$ mongosh
test> show databases
```

## Zadanie 2

![](https://i.imgur.com/Lbtoi8F.png)


```javascript=
db.Ksiazka.insertMany([{
    _id: 1,
    ISBN: '83-246-0279-8',
    Tytul: 'Microsoft Access. Podręcznik administratora',
    Autor: 'Helen Feddema',
    Rok_Wydania: 2006,
    Egzemplarze: [{
        Sygnatura: 'S0003'
    }, {
        Sygnatura: 'S0004'
    }]
}, {
    _id: 2,
    ISBN: '978-83-246-0549-1',
    Tytul: 'SQL Server 2005. Wyciśnij wszystko',
    Autor: 'Eric L. Brown',
    Rok_Wydania: 2007,
    Egzemplarze: [{
        Sygnatura: 'S0007'
    }]
}])

db.Czytelnik.insertMany([{
    _id: 1,
    Pesel: '55101011111',
    Nazwisko: 'Kowalski',
    Miasto: 'Wrocław',
    Data_Urodzenia: Date.parse('1955-10-10'),
    Wypozyczenia: [{
        Ksiazka_id: 1,
        Egzemplarz_sygnatura: 'S0003',
        Data: Date.parse('2022-02-01'),
        Liczba_Dni: 12
    }, {
        Ksiazka_id: 2,
        Egzemplarz_sygnatura: 'S0007',
        Data: Date.parse('2022-02-05'),
        Liczba_Dni: 10
    }]
}, {
    _id: 2,
    Pesel: '60101033333',
    Nazwisko: 'Maliniak',
    Miasto: 'Wrocław',
    Data_Urodzenia: Date.parse('1960-10-10'),
    Wypozyczenia: [{
        Ksiazka_id: 1,
        Egzemplarz_sygnatura: 'S0004',
        Data: Date.parse('2022-04-01'),
        Liczba_Dni: 5
    }, {
        Ksiazka_id: 2,
        Egzemplarz_sygnatura: 'S0007',
        Data: Date.parse('2022-03-01'),
        Liczba_Dni: 20
    }]
}])
```

## Zadanie 3

![](https://i.imgur.com/wV6jwvh.png)


```javascript=
const database = 'Biblioteka'

// // The current database to use.
use(database)

let egzemplarzSchema = {
  bsonType: 'object',
  properties: {
    Sygnatura: { bsonType: 'string' }
  }
}

let ksiazkaSchema = {
  bsonType: 'object',
  properties: {
    ISBN: {
      bsonType: 'string',
      pattern: '^([a-z0-9]*-)+[a-z0-9]*$'
    },
    Tytul: { bsonType: 'string' },
    Autor: { bsonType: 'string' },
    Rok_Wydania: { bsonType: 'int' },
    Egzemplarze: {
      bsonType: 'array',
      items: egzemplarzSchema
    }
  }
}

db.runCommand({
  collMod: 'Ksiazka',
  validator: { $jsonSchema: ksiazkaSchema }
})

db.Ksiazka.validate()


let wypozyczenieSchema = {
  bsonType: 'object',
  properties: {
    Ksiazka_id: { bsonType: 'int' },
    Egzemplarz_sygnatura: { bsonType: 'string' },
    Data: { bsonType: 'date' },
    Liczba_Dni: { bsonType: 'int' }
  }
}

let czytelnikSchema = {
  bsonType: 'object',
  properties: {
    Pesel: {
      bsonType: 'string',
      pattern: '^[0-9]{11}$'
    }, 
    Nazwisko: { bsonType: 'string' },
    Miasto: { bsonType: 'string' },
    Data_Urodzenia: { bsonType: 'date' },
    Wypozyczenia: { 
      bsonType: 'array' ,
      items: wypozyczenieSchema
    }
  }
}

db.runCommand({
  collMod: 'Czytelnik',
  validator: { $jsonSchema: czytelnikSchema }
})

db.Czytelnik.validate()

```

## Zadanie 4
![](https://i.imgur.com/6Wc0M5h.png)

```javascript=
db.A.insertMany([{
  value: 2,
  nested: {
    value: 2
  }
}, {
  value: 3,
  nested: {
    value: 1
  }
}, {
  value: 1,
  nested: {
    value: 4
  }
}, {
  value: 4,
  nested: {
    value: 2
  }
}, {
  value: 5,
  nested: {
    value: 2
  }
}])

```

* `db.A.find().sort({value:1})`
* `db.A.find().sort({value:1}).skip(2).limit(2)`
* `db.A.find({"nested.value": {$gt: 2}})`

## Zadanie 5

![](https://i.imgur.com/7zZgXtu.png)

### Uruchomienie i konfiguracja

```shell=
$ mkdir -p /tmp/Data/Databases/MongoDB/db1
$ mkdir -p /tmp/Data/Databases/MongoDB/db2
$ mkdir -p /tmp/Data/Databases/MongoDB/db3
```

```shell=
$ mongod --dbpath '/tmp/Data/Databases/MongoDB/db1' --port 10000 --replSet 'demo' 1> /dev/null &
$ mongod --dbpath '/tmp/Data/Databases/MongoDB/db2' --port 20000 --replSet 'demo' 1> /dev/null &
$ mongod --dbpath '/tmp/Data/Databases/MongoDB/db3' --port 30000 --replSet 'demo' 1> /dev/null &
```

```shell=
mongosh --port 10000
```

```javascript=
test> var rsConfig = {
  _id: 'demo',
  members: [{
      _id: 0,
      host: 'localhost:10000',
      priority: 10
    },
    {
      _id: 1,
      host: 'localhost:20000'
    },
    {
      _id: 2,
      host: 'localhost:30000',
      arbiterOnly: true
    }
  ]
}
test> rs.initialize(rsConfig)
demo [direct: primary] test>
```

### Zabicie primary

```shell=
$ jobs
[1]    running    mongod --dbpath '/tmp/Data/Databases/MongoDB/db1' --port 10000 --replSet  >
[2]  - running    mongod --dbpath '/tmp/Data/Databases/MongoDB/db2' --port 20000 --replSet  >
[3]  + running    mongod --dbpath '/tmp/Data/Databases/MongoDB/db3' --port 30000 --replSet  >
$ kill %1
```

### Wskrzeszenie primary

```shell=
$ mongod --dbpath '/tmp/Data/Databases/MongoDB/db1' --port 10000 --replSet 'demo' 1> /dev/null &
```
## Zadanie 6
![](https://i.imgur.com/hKWCojv.png)


## Zadanie 7

![](https://i.imgur.com/W51VYZM.png)

```javascript=
const database = 'Test';

// The current database to use.
use(database);

db.collection.drop()
db.collection.insertMany([
  {
    key: 'A',
    value: 1
  }, {
    key: 'A',
    value: 2
  }, {
    key: 'B',
    value: 2
  }, {
    key: 'B',
    value: 5
  }, {
    key: 'A',
    value: 3
  }, {
    key: 'D',
    value: 2
  }, {
    key: 'C',
    value: 10
  }
])

// Pipeline
db.collection.aggregate([
  {
    $match: { value: { $gt: 1 } }
  }, {
    $group: {
      _id: '$key',
      sum: { $sum: '$value' }
    }
  }, {
    $sort: { sum: -1 }
  }, {
    $limit: 3
  }, {
    $unset: 'sum'
  }
])

// Single purpose
// db.collection.countDocuments()
// db.collection.estimatedDocumentCount()
// db.collection.distinct()
```