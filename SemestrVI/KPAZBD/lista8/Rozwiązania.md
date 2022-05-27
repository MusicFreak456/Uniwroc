# Kurs projektowania aplikacji z bazami danych -- Lista 8
###### tags: `KPAZBD`

## Zadanie 1

![](https://i.imgur.com/wD7ykSk.png)


https://www.figma.com/proto/pVBu4NlbiRY0ZLRwOg5e4i/KPAZBD2022?node-id=1%3A6&starting-point-node-id=1%3A6&scaling=scale-down

## Zadanie 2

![](https://i.imgur.com/jmS5kUi.png)

* Operacje realizowane w ramach widoku

| Typ widoku      | Operacja      | Obiekty                  |
| --------------- | ------------- | ------------------------ |
| list products   | index         | Product, Category        |
| show product    | show          | Product                  |
| create product  | create        | Product                  |
| edit product    | update        | Product, Category        |
| list orders     | index         | Order                    |
| show order      | show, update  | Order, Payment, Delivery |
| list categories | index, create | Category                 |
| [cart views]    | create        | Order, Payment, Delivery |
| login           | -             | User                     |
| sign up         | create        | User                     |

* Robocza wersja klas

```typescript=
class User {
    ID id
    string username
    string password
    string name
    string surname
    Date dateOfBirth
    boolean isAdmin
}

class Category {
    ID id
    string name
}

class Product {
    ID id
    string name
    float price
    string summary
    string description
    int availableQuantity
    Category[] categories
}

class Payment {
    ID id
    string method
    enum status
    Date CreatedAt
}

class Address {
    ID id
    string line
    string city
    string zipCode
    string phoneNumber
}

class Delivery {
    ID id
    enum type
    enum status
    Address? address
}

class OrderLine {
    ID id
    Product product
    int quantity
}

class Order {
    ID id
    User user
    Delivery delivery
    Address address
    Payment payment
    OrderLine[] orderLines
    
    enum getOrderStatus()
}

```


![](https://i.imgur.com/ehZa0Ui.png)


```quickdb=
User
-
UserID PK int
Email varchar(30)
Password char(60)
Username varchar(40)
Name varchar(40)
Surname varchar(40)
DateOfBirth date
IsAdmin boolean

Product
-
ProductID PK int
Name varchar(40)
Price float
Summary null text
Description null text
AvailableQuantity int

Category
-
CategoryID PK int
Name varchar(30)

ProductCategory
-
ProductCategoryID PK int
ProductID int FK >- Product.ProductID
CategoryID int FK >- Category.CategoryID

Address
-
AddressID PK int
Line text
City varchar(40)
Zipcode char(6)
PhoneNumber char(9)

Delivery
-
DeliveryID PK int
Type enum
Status enum
AddressID NULL int FK -0 Address.AddressID

Payment
-
PaymentID PK int
Method text
Status enum
CreatedAt timestamp

Order
-
OrderID PK int
UserID int FK >0- User.UserID
DeliveryID int FK - Delivery.DeliveryID
PaymentID int FK - Payment.PaymentID
CreatedAt timestamp

OrderLine
-
OrderLineID PK int
OrderID int FK >- Order.OrderID
ProductID int FK >0- Product.ProductID
Quantity int
```