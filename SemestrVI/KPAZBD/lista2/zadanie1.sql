select distinct City from SalesLT.SalesOrderHeader 
join SalesLT.Address on ShipToAddressID = AddressID
where status = 5
order by City asc
