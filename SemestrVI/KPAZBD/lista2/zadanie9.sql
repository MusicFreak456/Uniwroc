alter table SalesLT.Customer 
    add CreditCardNumber char(16);

update SalesLT.SalesOrderHeader 
    set CreditCardApprovalCode = '1234'
    where SalesOrderID in (
        select top(3) SalesOrderID 
        from SalesLT.SalesOrderHeader
    );

update SalesLT.Customer 
    set CreditCardNumber = 'X'
    from SalesLT.Customer
    join SalesLT.SalesOrderHeader
        on SalesOrderHeader.CustomerID = Customer.CustomerID
    where SalesOrderHeader.CreditCardApprovalCode is not null;
