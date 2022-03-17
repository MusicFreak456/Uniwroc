
select Product.Name, c2.Name
from SalesLT.Product
join SalesLT.ProductCategory c2
    on Product.ProductCategoryID = c2.ProductCategoryID
join SalesLT.ProductCategory c1
    on c2.ProductCategoryID = c1.ParentProductCategoryID
group by ProductID, Product.Name, c2.Name;

insert into SalesLT.Product(ProductCategoryID, Name, ProductNumber, StandardCost, ListPrice, SellStartDate, SellEndDate)
    values(1, 'test', 'test', 0, '1,00', getdate(), getdate());

-- delete from SalesLT.Product where Product.ProductCategoryID = 1;
