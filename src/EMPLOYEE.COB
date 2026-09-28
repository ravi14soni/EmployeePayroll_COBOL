IDENTIFICATION DIVISION.
PROGRAM-ID. EMPLOYEE.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.

    SELECT EMPLOYEE-FILE
        ASSIGN TO "data\EMPLOYEE.DAT"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS DYNAMIC
        RECORD KEY IS FILE-EMPLOYEE-ID
        FILE STATUS IS FILE-STATUS.

DATA DIVISION.

FILE SECTION.

FD EMPLOYEE-FILE.
COPY EMPLOYEE.

WORKING-STORAGE SECTION.

COPY FILES.

01 EMPLOYEE-MENU-CHOICE       PIC 9 VALUE 0.

01 EMPLOYEE-ID                PIC 9(5).
01 EMPLOYEE-NAME              PIC X(30).
01 DEPARTMENT                 PIC X(20).
01 BASIC-SALARY               PIC 9(7)V99.
01 SALARY-GRADE               PIC X.

01 SEARCH-ID                  PIC 9(5).
01 NEW-SALARY                 PIC 9(7)V99.
01 EMPLOYEE-GRADE             PIC X.
01 DISPLAY-SALARY             PIC Z(7)9.99.

01 DELETE-CONFIRMATION        PIC X.
    88 DELETE-YES             VALUE "Y" "y".
    88 DELETE-NO              VALUE "N" "n".



PROCEDURE DIVISION.

    PERFORM EMPLOYEE-MENU.

    GOBACK.


EMPLOYEE-MENU.

    MOVE 0 TO EMPLOYEE-MENU-CHOICE.

    PERFORM UNTIL EMPLOYEE-MENU-CHOICE = 6

        DISPLAY " "
        DISPLAY "========================================"
        DISPLAY "          EMPLOYEE MANAGEMENT"
        DISPLAY "========================================"
        DISPLAY "1. Add Employee"
        DISPLAY "2. Search Employee"
        DISPLAY "3. Update Employee"
        DISPLAY "4. Delete Employee"
        DISPLAY "5. View All Employees"
        DISPLAY "6. Back to Main Menu"
        DISPLAY "========================================"
        DISPLAY "Enter your choice: "
            WITH NO ADVANCING

        ACCEPT EMPLOYEE-MENU-CHOICE

        EVALUATE EMPLOYEE-MENU-CHOICE

            WHEN 1
                PERFORM ADD-EMPLOYEE

            WHEN 2
                PERFORM SEARCH-EMPLOYEE

            WHEN 3
                PERFORM UPDATE-EMPLOYEE

            WHEN 4
                PERFORM DELETE-EMPLOYEE

            WHEN 5
                PERFORM VIEW-EMPLOYEES

            WHEN 6
                DISPLAY "Returning to Main Menu."

            WHEN OTHER
                DISPLAY "Invalid option."

        END-EVALUATE

    END-PERFORM.



ADD-EMPLOYEE.

    DISPLAY " ".
    DISPLAY "---------- ADD EMPLOYEE ----------".

    DISPLAY "Employee ID: " WITH NO ADVANCING.
    ACCEPT EMPLOYEE-ID.

    DISPLAY "Employee Name: " WITH NO ADVANCING.
    ACCEPT EMPLOYEE-NAME.

    DISPLAY "Department: " WITH NO ADVANCING.
    ACCEPT DEPARTMENT.

    DISPLAY "Basic Salary: " WITH NO ADVANCING.
    ACCEPT BASIC-SALARY.

    DISPLAY "Enter Salary Grade (A/B/C): " WITH NO ADVANCING.
    ACCEPT EMPLOYEE-GRADE.

    MOVE EMPLOYEE-ID TO FILE-EMPLOYEE-ID.
    MOVE EMPLOYEE-NAME TO FILE-EMPLOYEE-NAME.
    MOVE DEPARTMENT TO FILE-DEPARTMENT.
    MOVE BASIC-SALARY TO FILE-BASIC-SALARY.
    MOVE EMPLOYEE-GRADE TO FILE-SALARY-GRADE.

    OPEN I-O EMPLOYEE-FILE.

    IF FILE-STATUS = "35"

        CLOSE EMPLOYEE-FILE

        OPEN OUTPUT EMPLOYEE-FILE

    END-IF.

    IF FILE-STATUS NOT = "00"

        DISPLAY "Unable to open employee file."
        DISPLAY "File Status: " FILE-STATUS

    ELSE

        WRITE EMPLOYEE-RECORD

            INVALID KEY
                DISPLAY "Employee ID already exists."

            NOT INVALID KEY
                DISPLAY "Employee added successfully."

        END-WRITE

        CLOSE EMPLOYEE-FILE

    END-IF.


SEARCH-EMPLOYEE.

    DISPLAY " ".
    DISPLAY "---------- SEARCH EMPLOYEE ----------".

    DISPLAY "Enter Employee ID: " WITH NO ADVANCING.
    ACCEPT SEARCH-ID.

    MOVE SEARCH-ID TO FILE-EMPLOYEE-ID.

    OPEN INPUT EMPLOYEE-FILE.

    IF FILE-STATUS NOT = "00"

        DISPLAY "Unable to open employee file."
        DISPLAY "File Status: " FILE-STATUS

    ELSE

        READ EMPLOYEE-FILE
            KEY IS FILE-EMPLOYEE-ID

            INVALID KEY
                DISPLAY "Employee not found."

            NOT INVALID KEY

                DISPLAY " "
                DISPLAY "Employee Found"
                DISPLAY "--------------------------------"
                DISPLAY "Employee ID : "
                        FILE-EMPLOYEE-ID
                DISPLAY "Name        : "
                        FILE-EMPLOYEE-NAME
                DISPLAY "Department  : "
                        FILE-DEPARTMENT

                MOVE FILE-BASIC-SALARY
                    TO DISPLAY-SALARY

                DISPLAY "Basic Salary: "
                        DISPLAY-SALARY

                DISPLAY "Salary Grade: "
                        FILE-SALARY-GRADE

                DISPLAY "--------------------------------"

        END-READ

        CLOSE EMPLOYEE-FILE

    END-IF.


UPDATE-EMPLOYEE.

    DISPLAY " ".
    DISPLAY "---------- UPDATE EMPLOYEE ----------".

    DISPLAY "Enter Employee ID: " WITH NO ADVANCING.
    ACCEPT SEARCH-ID.

    MOVE SEARCH-ID
        TO FILE-EMPLOYEE-ID.

    OPEN I-O EMPLOYEE-FILE.

    IF FILE-STATUS NOT = "00"

        DISPLAY "Unable to open employee file."
        DISPLAY "File Status: " FILE-STATUS

    ELSE

        READ EMPLOYEE-FILE
            KEY IS FILE-EMPLOYEE-ID

            INVALID KEY
                DISPLAY "Employee not found."

            NOT INVALID KEY

                DISPLAY "Current Employee Details:"
                DISPLAY "Name       : "
                        FILE-EMPLOYEE-NAME
                DISPLAY "Department : "
                        FILE-DEPARTMENT

                MOVE FILE-BASIC-SALARY
                    TO DISPLAY-SALARY

                DISPLAY "Current Salary: "
                        DISPLAY-SALARY

                DISPLAY " "
                DISPLAY "Enter New Salary: " WITH NO ADVANCING
                ACCEPT NEW-SALARY
                MOVE NEW-SALARY TO FILE-BASIC-SALARY

                DISPLAY "Enter New Salary Grade (A/B/C): " WITH NO ADVANCING
                ACCEPT EMPLOYEE-GRADE
                MOVE EMPLOYEE-GRADE TO FILE-SALARY-GRADE
                PERFORM VALIDATE-GRADE

                REWRITE EMPLOYEE-RECORD

                    INVALID KEY
                        DISPLAY "Employee update failed."

                    NOT INVALID KEY
                        DISPLAY "Employee updated successfully."

                END-REWRITE

        END-READ

        CLOSE EMPLOYEE-FILE

    END-IF.

VALIDATE-GRADE.

    EVALUATE EMPLOYEE-GRADE

        WHEN "A"
            CONTINUE

        WHEN "B"
            CONTINUE

        WHEN "C"
            CONTINUE

        WHEN OTHER
            DISPLAY "Invalid salary grade."

    END-EVALUATE.

DELETE-EMPLOYEE.

    DISPLAY " ".
    DISPLAY "---------- DELETE EMPLOYEE ----------".

    DISPLAY "Enter Employee ID: " WITH NO ADVANCING.
    ACCEPT SEARCH-ID.

    MOVE SEARCH-ID
        TO FILE-EMPLOYEE-ID.

    OPEN I-O EMPLOYEE-FILE.

    IF FILE-STATUS NOT = "00"

        DISPLAY "Unable to open employee file."
        DISPLAY "File Status: " FILE-STATUS

    ELSE

        READ EMPLOYEE-FILE
            KEY IS FILE-EMPLOYEE-ID

            INVALID KEY
                DISPLAY "Employee not found."

            NOT INVALID KEY

                DISPLAY "Employee Found: "
                        FILE-EMPLOYEE-NAME

                DISPLAY "Are you sure? (Y/N): "
                        WITH NO ADVANCING

                ACCEPT DELETE-CONFIRMATION

                IF DELETE-YES

                    DELETE EMPLOYEE-FILE

                        INVALID KEY
                            DISPLAY "Delete failed."

                        NOT INVALID KEY
                            DISPLAY "Employee deleted successfully."

                    END-DELETE

                ELSE

                    DISPLAY "Delete cancelled."

                END-IF

        END-READ

        CLOSE EMPLOYEE-FILE

    END-IF.


VIEW-EMPLOYEES.

    DISPLAY " ".
    DISPLAY "---------- EMPLOYEE LIST ----------".

    OPEN INPUT EMPLOYEE-FILE.

    IF FILE-STATUS NOT = "00"

        DISPLAY "Unable to open employee file."
        DISPLAY "File Status: " FILE-STATUS

    ELSE

        PERFORM UNTIL FILE-STATUS = "10"

            READ EMPLOYEE-FILE
                NEXT RECORD

                AT END
                    MOVE "10" TO FILE-STATUS

                NOT AT END

                    MOVE FILE-BASIC-SALARY
                        TO DISPLAY-SALARY

                    DISPLAY
                        FILE-EMPLOYEE-ID
                        " | "
                        FILE-EMPLOYEE-NAME
                        " | "
                        FILE-DEPARTMENT
                        " | "
                        DISPLAY-SALARY
                        " | "
                        FILE-SALARY-GRADE

            END-READ

        END-PERFORM

        CLOSE EMPLOYEE-FILE

    END-IF.

