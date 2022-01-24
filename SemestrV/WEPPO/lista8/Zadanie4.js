var pg = require('pg')

class PersonRepository {
  constructor(pool) {
    this.pool = pool
  }

  async insert(person) {
    try {
      let queryResult = await this.pool.query(
        `insert into person(name, surname, gender, id_workplace) values 
          ($1, $2, $3, $4) 
          returning id;`,
        [person.name, person.surname, person.gender, person.id_workplace]
      )
      return queryResult.rows[0].id
    } catch (error) {
      console.log(error)
      return -1
    }
  }
}

class WorkplaceRepository {
  constructor(pool) {
    this.pool = pool
  }

  async insert(workplace) {
    try {
      let queryResult = await this.pool.query(
        `insert into workplace(workplace_name) values 
         ($1)
         returning id;
        `,
        [workplace.workplace_name]
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
  let workplaceRepo = new WorkplaceRepository(pool)

  let workId = await workplaceRepo.insert({workplace_name: 'macrosof'})
  let workerId = await personRepo.insert({
    name: "Jan",
    surname: "Kowalski",
    gender: 'male',
    id_workplace: workId
  })

  try {
    let queryResult = await pool.query(
      `select name, workplace_name 
        from person 
          join workplace on (person.id_workplace = workplace.id) 
        where person.id = $1;`,
      [workerId]
    )
    let name = queryResult.rows[0].name
    let workplace = queryResult.rows[0].workplace_name
    console.log(`${name} pracuje w ${workplace}`)
  } catch (error) {
    console.log(error)
  }

  pool.end();
}

main()