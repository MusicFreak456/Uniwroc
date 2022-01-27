var crypto = require('crypto')
var pg = require('pg')

const BURST_LENGHT = 1000

class PersonRepository {
  constructor(pool) {
    this.pool = pool
  }

  async insert(person) {
    try {
      let queryResult = await this.pool.query(
        `insert into person(name, surname, gender) values 
          ($1, $2, $3) 
          returning id;`,
        [person.name, person.surname, person.gender]
      )
      return queryResult.rows[0].id
    } catch (error) {
      console.log(error)
      return -1
    }
  }

  async fill_database() {
    /* Można wstawić do 1000 elementów na raz */
    try {
      for (let i = 0; i < BURST_LENGHT; i++) {
        let query = 'insert into person(name, surname, gender) values '
        let name  = 'testname'
        let gender = 'male'

        for (let j = 0; j < BURST_LENGHT; j++) {
          /* Generujemy losowe nazwisko */
          let surname = 
            crypto
              .randomBytes(8)
              .map(n => n % 26 + 97)
              .toString()
          query += `('${name}', '${surname}', '${gender}')`
          if(j != BURST_LENGHT - 1) {
            query += ', '
          }
        }

        query += ';'
        await this.pool.query(query)
      }
    } catch (error) {
      console.log(error)
    }
  }

  async retriveBySurname(surname) {
    try {
      let queryResult = await this.pool.query(
        'select * from person where surname=$1',
        [surname]
      )
      return queryResult.rows
    } catch (error) {
      console.log(error)
      return []
    }
  }

}

async function time(fun, arg, label){
  console.time("    " + label)
  let res = await fun(arg)
  console.timeEnd("    " + label);
  return res
}

async function main() {
  var pool = new pg.Pool({
    host: 'localhost',
    database: 'postgres',
    user: 'postgres',
    password: '1234'
  })

  let personRepo = new PersonRepository(pool)

  await personRepo.fill_database()
  await personRepo.insert({
    name: "Jan",
    surname: "Kowalski",
    gender: "male"
  })

  console.time("Przed indeksem")
  await personRepo.retriveBySurname("Kowalski")
  console.timeEnd("Przed indeksem");

  await pool.query('create index person_surname_idx on person(surname);')

  console.time("Po indeksie")
  await personRepo.retriveBySurname("Kowalski")
  console.timeEnd("Po indeksie");

  pool.end();
}

main()