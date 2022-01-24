var pg = require('pg')

class PersonRepository {
  constructor(pool) {
    this.pool = pool
  }

  async update(person) {
    try {
      await this.pool.query(
        `update person set 
          name = $2, 
          surname = $3,
          gender = $4 
          where id = $1;`,
        [person.id, person.name, person.surname, person.gender]
      )
      return true
    } catch (error) {
      console.log(error)
      return false
    }
  }

  async delete(personId) {
    try {
      await this.pool.query(
        `delete from person where id = $1`,
        [personId]
      )
      return true
    } catch (error) {
      console.log(error)
      return false
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

  personRepo.update({
    id: 4,
    name: 'Stanisław',
    surname: 'Kowalski',
    gender: 'other'
  })

  personRepo.delete(3)

  pool.end();
}

main()