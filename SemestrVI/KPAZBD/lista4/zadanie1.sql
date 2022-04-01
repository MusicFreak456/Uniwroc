drop table if exists Prices, Commodities, ExchangeRates;

create table Commodities (
  ID int identity(1,1) primary key,
  CommodityName varchar(10)
);

create table Prices (
  CommodityID int references Commodities(ID),
  Currency varchar(3),
  Price float
);

create table ExchangeRates (
  Currency varchar(3) primary key,
  PLN float
);

insert into Commodities values ('A'), ('B'), ('C');
insert into ExchangeRates values 
  ('EUR', 4.68), ('USD', 4.26), ('GBP', 5.62);
insert into Prices values
  (1, 'PLN', 42.00),
  (2, 'PLN', 4.20),
  (3, 'PLN', 3.14),
  (1, 'GBP', null);
go

-- rozwiązanie:

declare @plnPrices table (
  CommodityID int,
  Price float
)

insert into @plnPrices select CommodityID, Price
  from Prices
  where Currency = 'PLN'

declare PriceCursor cursor dynamic 
  for select CommodityID, Currency from Prices
  where Currency != 'PLN'

declare @commodity_id int, @currency varchar(3), @price float;
declare @exchange float;

open PriceCursor;
  fetch next from PriceCursor 
    into @commodity_id, @currency;
  
  while(@@fetch_status = 0) begin
    
    set @exchange = (select PLN from ExchangeRates 
                      where Currency = @currency);
    set @price = (select Price from @plnPrices 
                  where CommodityID = @commodity_id);
    update Prices set Price=round(@price / @exchange, 2) 
      where current of PriceCursor;
    
    fetch next from PriceCursor
      into @commodity_id, @currency;
  end
close PriceCursor;

deallocate PriceCursor
go
