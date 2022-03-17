select 
    City, 
    count(distinct Customer.CustomerID) as Customers,
    count(distinct Customer.SalesPerson) as SalesPersons
from SalesLT.Address
left join SalesLT.CustomerAddress
    on Address.AddressID = CustomerAddress.AddressID
left join SalesLT.Customer 
    on Customer.CustomerID = CustomerAddress.CustomerID
group by City
