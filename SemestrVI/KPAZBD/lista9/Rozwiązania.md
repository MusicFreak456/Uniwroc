# Kurs projektowania aplikacji z bazami danych -- Lista 9
###### tags: `KPAZBD`

## Zadanie 1

![](https://i.imgur.com/LYI4IH9.png)

```typescript=
namespace Services {
    class Product {...}
    class ProductService {
        constructor(ProductRepository)
        Product[] listProducts(ProductsOrder?, ProductFilter?)
        Product getProduct(ID)
        void createProduct(Product)
        void updateProduct(Product)
    }
    
    class User {...}
    class UserService {
        constructor(UserRepository)
        User getUser(ID)
        void createUser(User)
    }
    
    class Order {...}
    class OrderService {
        constructor(OrderRepository)
        Order[] listOrders(OrdersOrder?, OrdersFilter?)
        Order getOrder(ID)
        void createOrder(Order)
        void updateOrderStatus()
    }
    
    class Category {...}
    class CategoryService {
        constructor(CategoryRepository)
        Category[] listCategories()
        void createCategory(Category)
    }
}
```

## Zadanie 2

![](https://i.imgur.com/2n3QSqH.png)

![](https://i.imgur.com/Gwv9nMs.png)

### Entity: User 

**Identyfikator biznesowy**: `Username`, `Name + Surname`, `Email`
**Cykl życia** : -

### Entity: Order <\<aggregate root>>

**Identyfikator biznesowy**: `OrderNumber`
**Cykl życia** : order created, order payed, order in progress, order sent, order delivered.
**Value objects**: `Delivery`, `Address`, `Payment`, `OrderLine`

### Entity: Product <\<aggregate root>>
**Identyfikator biznesowy**: `name`
**Cykl życia**: -
**Value objects**: `Category`