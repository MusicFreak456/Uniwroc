select 
    FirstName, 
    LastName,
    isnull(sum(UnitPriceDiscount * OrderQty),0) as TotalDiscount
from SalesLT.Customer
left join SalesLT.SalesOrderHeader
    on Customer.CustomerID = SalesOrderHeader.CustomerID
left join SalesLT.SalesOrderDetail
    on SalesOrderHeader.SalesOrderID = SalesOrderDetail.SalesOrderID
group by Customer.CustomerID, FirstName, LastName