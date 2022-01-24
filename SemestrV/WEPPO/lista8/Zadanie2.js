var pg = require('pg')

class PersonRepository {
  constructor(pool) {
    this.pool = pool
  }

  async retriveAll() {
    try {
      let queryResult = await this.pool.query('select * from person')
      return queryResult.rows
    } catch (error) {
      console.log(error)
      return []
    }
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
}

async function main() {
  var pool = new pg.Pool({
    host: 'localhost',
    database: 'postgres',
    user: 'postgres',
    password: '1234'
  })

  let personRepo = new PersonRepository(pool)

  let allPeople = await personRepo.retriveAll()

  allPeople.forEach( person => {
    console.log(person.name)
  });

  let newPersonId = await personRepo.insert({
    name: 'Stanisław', 
    surname: 'Kowalski',
    gender: 'male'
  })

  console.log(`Id: ${newPersonId}`)

  pool.end();
}

main()