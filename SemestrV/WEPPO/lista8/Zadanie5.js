var pg = require('pg')

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

  async assignJob(personId, workId) {
    try {
      let queryResult = await this.pool.query(
        `insert into works_at(id_person, id_workplace) values 
          ($1, $2) 
          returning id;`,
        [personId, workId]
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

  
  let workerId = await personRepo.insert({
    name: "Jan",
    surname: "Kowalski",
    gender: 'male'
  })

  let workId = await workplaceRepo.insert({workplace_name: 'macrosof'})
  await personRepo.assignJob(workerId, workId)
  
  workId = await workplaceRepo.insert({workplace_name: 'goodle'})
  await personRepo.assignJob(workerId, workId)

  try {
    let queryResult = await pool.query(
      `select workplace_name from person
      join works_at 
        join workplace on workplace.id = works_at.id 
      on person.id = works_at.id_person 
      where person.id = $1;`,
      [workerId]
    )
    console.log("Jan pracuje w:");

    queryResult.rows.forEach( row => {
      console.log(row.workplace_name);
    })

  } catch (error) {
    console.log(error)
  }

  pool.end();
}

main()