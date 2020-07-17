#
#%%%%%%%%%%%% SQL Practise in R %%%%%%%%%%%%%%%%%
#

getwd()
setwd("C://Users//Suruthi//Desktop//Data")

library(openxlsx)
library(dplyr)
library(sqldf)

data<-read.xlsx("Sql_Practise.xlsx")
names(data)
data$EmpName<-data$Employee.ID
data$EmpId<-data$Employee.Name
data$Dep<-data$Department.No

data<-as.data.frame(data[c(7,6,8,4,5)])

#Ques1:Display the table like below
q1<-sqldf("select EmpName,EmpId,min(Salary) as PreviousSalary,max(Salary) as CurrentSalary,
          min(YEAR) as PreviousYear,max(YEAR) as CurrentYear from data group by EmpId") 

#Ques2:Rank the employees by there salary in department wise
q2<-sqldf("SELECT empname, salary,
     RANK() OVER (PARTITION BY dep ORDER BY salary)
          FROM data")

#Ques3:Get the employees Max Salary in departmentwise
q3<-sqldf("select EmpName as EmployeeName,Dep as DepartmentName,max(Salary) from data group by Dep")

#Ques4:Get the employess who has experience of 2 year and greater than 2 year
q4<-sqldf("select Empid,empname, (max(year)-min(year)) >=2 as Morethan2yrsExp
          from data group by Empid")
q4<-sqldf("select Empid,empname
          from data 
          group by Empid
          having (max(year)-min(year)) >=2")

#Ques5:Display employees joining year and the salary
q5<-sqldf("select empid,empname,dep,salary,min(year) from data
          group by empid")

#Ques6:Write a sql query to fetch the list of employees with the same salary
q6<-sqldf("select empid,empname,dep,max(salary)as sal from data group by empid")

q6<-sqldf("select empid,empname,dep,min(salary)as sal from data group by empid")

#Ques7:Write a sql query to show the second highest salary.
q7<-sqldf("select empid,empname,salary,min(year)from data 
          group by empid
          order by salary DESC limit 1,1")

q7<-sqldf("select empid,empname,salary,max(year)from data 
          group by empid
          order by salary DESC limit 1,1")

#Ques8:Write a sql query to fetch the departments that have less than 2 people in it
q8<-sqldf("select DISTINCT dep,count(*) as Morethan2peoplecount from (select empid,empname,dep,max(year)as Presentyear from data
          group by empid) group by dep
          having count(*) >2")

#Ques9: Write a sql query to show all departments along with the no of people
q9<-sqldf("select DISTINCT dep,count(*) as NoOfPeople  from (select empid,empname,dep,max(year)as Presentyear from data
          group by empid) group by dep")

#Ques10: Write a sql query to fetch three max salaries from a table
q10<-sqldf("select empid,empname,salary,min(year)from data 
          group by empid
           order by salary DESC limit 3")

q10<-sqldf("select empid,empname,salary,max(year)from data 
          group by empid
           order by salary DESC limit 3")
#########################################################################################
eid<-c(1,2,3,4)
eventname<-c("100 meter dash","500 meter dash","cross-country","triathalon")
Event<-as.data.frame(cbind(eid,eventname))

id<-c(1,2,3,2,4,2,2,1)
event<-c("100 meter dash","500 meter dash","cross-country","500 meter dash",
         "trilathon","500 meter dash","500 meter daash","100 meter dash")
winnerid<-c(1,3,2,4,"NULL",3,1,2)
Winner<-as.data.frame(cbind(id,event,winnerid))

#Ques11: Get the below result
# EID Eventname Winnercount

q11<-sqldf("select a.eid as Eid,a.eventname as Name,count(distinct(b.winnerid))as Winnercount
           from Event a, Winner b
           on a.eid = b.id
           group by a.eid")
q12<-sqldf("select winnerid as WinnerId,event as Name,count(id)as Eventcount
           from Winner
           group by winnerid")
