select 
    ProductModel.Name, 
    count(Product.ProductModelID) as Number_of_Products 
from SalesLT.Product
join SalesLT.ProductModel on 
    Product.ProductModelID = ProductModel.ProductModelID
group by ProductModel.ProductModelId, ProductModel.Name
having count(Product.ProductModelID) > 1
