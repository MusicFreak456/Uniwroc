# Kurs projektowania aplikacji z bazami danych -- Lista 7
###### tags: `KPAZBD`

## Zadanie 1
![](https://i.imgur.com/k0QM6Gf.png)

```
match (m:Movie) return m
```

## Zadanie 2

![](https://i.imgur.com/t0MJPlX.png)

* ```
  create 
  p1=(:Person{name: 'Person 1' }), 
  p2=(:Person{name: 'Person 2' }),
  m1=(:Movie{title: 'Title1' }),
  m2=(:Movie{title: 'Title2' }) 
  return p1, p2, m1, m2  
  ```
* ```
  match (m:Movie) 
  where id(m) = 174 
  set m.prop1 = 42,
      m.prop2 = 'prop2'
  return m
  ```
  
* ```
  match (m:Movie)
  where id(m) = 173
  with m
  match (p:Person)
  where id(p) = 169
  create a=( (p) -[:ACTED_IN]-> (m) )
  return a
  ```
* ```
  match (m:Movie) 
  where id(m) = 173 
  set m.prop2 = 'new value'
  return m
  ```
* ```
  match (p:Person) -[a:ACTED_IN]-> (m:Movie)
  where id(p) = 169 and id(m) = 173
  delete a
  ```

## Zadanie 3

![](https://i.imgur.com/DlZwwOx.png)

* `match (p:Person) -[:ACTED_IN]-> (m:Movie) where ID(p) = 1 return m`
* `match (m:Movie) <-[:DIRECTED]- (p:Person) -[:PRODUCED]-> (m) where id(p) = 27 return m`
* `match (p:Person) where not exists((p) -[:ACTED_IN]-> (:Movie)) return p`
* `match (p:Person) -[:ACTED_IN]-> (m:Movie) with p, count(m) as movieCount where movieCount > 2 return p`