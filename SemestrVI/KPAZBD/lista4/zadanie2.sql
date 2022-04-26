drop table if exists SalaryHistory, Employees, [Log];

create table Employees (
  ID int identity(1,1) primary key,
  SalaryGros float
);

create table SalaryHistory (
  ID int identity(1,1) primary key,
  EmployeeID int references Employees(ID),
  [Year] int,
  [Month] int,
  SalaryNet float,
  SalaryGros float 
);

create table [Log] (
  ID int identity(1,1) primary key,
  Code int,
  Reason varchar(20)
)

go

insert into Employees values (10000.50), (26000), (3200);
insert into SalaryHistory values
  (1, 2022, 1, null, 10000.50),
  (1, 2022, 2, null, 10000.50),
  (1, 2022, 3, null, 10000.50),
  (1, 2022, 4, null, 10000.50),
  (1, 2022, 5, null, 10000.50),
  (2, 2022, 1, null, 26000),
  (2, 2022, 2, null, 26000),
  (2, 2022, 3, null, 26000),
  (2, 2022, 4, null, 26000),
  (2, 2022, 5, null, 26000),
  (3, 2022, 1, null, 3200);

go
drop procedure if exists calculate_salary 
go

create procedure calculate_salary @month int as 
  declare @result table (EmployeeID int, ToBePaidOut float)
  declare @current_year int;
  set @current_year=year(getdate());

  declare @employee_id int, @employee_salary float;
  declare EmployeesCursor cursor for select * from Employees;
  open EmployeesCursor;

  fetch from EmployeesCursor into @employee_id, @employee_salary;
  while (@@fetch_status = 0) begin
    declare @month_count int;

    select @month_count=sum(iif(SalaryGros is null, 0, 1)) 
      from SalaryHistory
      where EmployeeID = @employee_id 
            and [Year]=@current_year
            and [Month] < @month;

    if (@month_count < @month - 1) begin
      insert into [Log] values (0, 'Missing month');
      continue;
    end

    declare @salary_acc float;
    select @salary_acc=sum(SalaryGros) 
      from SalaryHistory
      where EmployeeID = @employee_id 
            and [Year]=@current_year
            and [Month] < @month;

    declare @reduction_over_threshold float, @reduction_under_threshold float;

    set @reduction_over_threshold = 0.32 *
      greatest(@employee_salary + @salary_acc - greatest(120000, @salary_acc), 0);

    set @reduction_under_threshold = 0.17 *
      greatest(least(120000, @salary_acc + @employee_salary) - @salary_acc, 0);

    insert into @result values (
      @employee_salary, 
      @employee_salary - @reduction_under_threshold - @reduction_over_threshold);

    fetch from EmployeesCursor into @employee_id, @employee_salary;
  end

  close EmployeesCursor;
  deallocate EmployeesCursor;
  select * from @result;
go

exec calculate_salary 6