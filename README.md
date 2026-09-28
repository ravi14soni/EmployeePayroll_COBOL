# Employee Payroll Management System

A modular Employee Payroll Management System developed using COBOL
and GnuCOBOL.

## Features

- Add Employee
- Search Employee
- Update Employee
- Delete Employee
- View Employees
- Salary Calculation
- Salary Grade Management
- Salary Slip Generation
- All Employee payroll report generation

## Technologies

- COBOL
- GnuCOBOL
- VS Code
- Indexed File Handling

## Project Structure

src/
  MAIN.COB
  EMPLOYEE.COB
  PAYROLL.COB

## How to Compile

cobc -m -i copybook -o EMPLOYEE.dll src\EMPLOYEE.COB
cobc -m -i copybook -o PAYROLL.dll src\PAYROLL.COB
cobc -x -i copybook -o MAIN.exe src\MAIN.COB

## How to Run

MAIN.exe