IDENTIFICATION DIVISION.
PROGRAM-ID. PAYROLL.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.

FILE-CONTROL.

    SELECT EMPLOYEE-FILE
        ASSIGN TO "data\EMPLOYEE.DAT"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS DYNAMIC
        RECORD KEY IS FILE-EMPLOYEE-ID
        FILE STATUS IS FILE-STATUS.

    SELECT SALARY-GRADE-FILE
        ASSIGN TO "master\SALARY-GRADE.DAT"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS GRADE-FILE-STATUS.

    SELECT SALARY-SLIP-FILE
        ASSIGN TO "output\SALARY-SLIP.TXT"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS SLIP-FILE-STATUS.

    SELECT PAYROLL-REPORT-FILE
        ASSIGN TO "output\PAYROLL-REPORT.TXT"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS REPORT-FILE-STATUS.

DATA DIVISION.

FILE SECTION.

FD EMPLOYEE-FILE.
COPY EMPLOYEE.

FD SALARY-GRADE-FILE.
01 SALARY-GRADE-RECORD.
    05 FILE-GRADE-CODE       PIC X.
    05 FILE-GRADE-HRA        PIC 99.
    05 FILE-GRADE-DA         PIC 99.
    05 FILE-GRADE-TRANSPORT  PIC 99.
    05 FILE-GRADE-PF         PIC 99.


FD SALARY-SLIP-FILE.
01 SALARY-SLIP-RECORD         PIC X(100).

FD PAYROLL-REPORT-FILE.
01 PAYROLL-REPORT-RECORD      PIC X(150).

WORKING-STORAGE SECTION.

    COPY PAYROLL.
    COPY FILES.

    01 SALARY-GRADE-TABLE.

    05 SALARY-GRADE-ENTRY OCCURS 3 TIMES
       INDEXED BY GRADE-INDEX.

        10 GRADE-CODE       PIC X.
        10 GRADE-HRA-PCT    PIC 99V99.
        10 GRADE-DA-PCT     PIC 99V99.
        10 GRADE-TRANS-PCT  PIC 99V99.
        10 GRADE-PF-PCT     PIC 99V99.

    01 EMPLOYEE-GRADE        PIC X.
    01 GRADE-FOUND           PIC X VALUE "N".

    01 GRADE-FILE-STATUS PIC XX VALUE "00".
    01 GRADE-COUNT PIC 9 VALUE 1.


    01 MENU-CHOICE               PIC 9 VALUE 0.
    01 DELETE-CONFIRMATION       PIC X.
        88 DELETE-YES            VALUE "Y" "y".
        88 DELETE-NO             VALUE "N" "n".

    01 EMPLOYEE-ID               PIC 9(5).
    01 EMPLOYEE-NAME             PIC X(30).
    01 DEPARTMENT                PIC X(20).
    01 BASIC-SALARY              PIC 9(7)V99.

    01 SEARCH-ID                 PIC 9(5).
    01 NEW-SALARY                PIC 9(7)V99.
    01 DISPLAY-SALARY            PIC Z(7)9.99.


    

    01 DISPLAY-BASIC        PIC Z(7)9.99.
    01 DISPLAY-HRA          PIC Z(7)9.99.
    01 DISPLAY-DA           PIC Z(7)9.99.
    01 DISPLAY-TRANSPORT    PIC Z(7)9.99.
    01 DISPLAY-GROSS        PIC Z(7)9.99.
    01 DISPLAY-PF           PIC Z(7)9.99.
    01 DISPLAY-TAX          PIC Z(7)9.99.
    01 DISPLAY-OTHER        PIC Z(7)9.99.
    01 DISPLAY-TOTAL        PIC Z(7)9.99.
    01 DISPLAY-NET          PIC Z(7)9.99.


    01 SLIP-LINE                  PIC X(100).
    01 SLIP-FILE-NAME             PIC X(100).


    01 TOTAL-EMPLOYEES             PIC 9(5) VALUE 0.
    01 TOTAL-BASIC-SALARY          PIC 9(10)V99 VALUE 0.
    01 TOTAL-GROSS-SALARY          PIC 9(10)V99 VALUE 0.
    01 TOTAL-NET-SALARY            PIC 9(10)V99 VALUE 0.

    01 DISPLAY-TOTAL-EMPLOYEES     PIC Z(5).
    01 DISPLAY-TOTAL-BASIC          PIC Z(9)9.99.
    01 DISPLAY-TOTAL-GROSS          PIC Z(9)9.99.
    01 DISPLAY-TOTAL-NET            PIC Z(9)9.99.
    01 DISPLAY-EMPLOYEE-ID          PIC Z(4)9.

    
    

PROCEDURE DIVISION.

PERFORM LOAD-SALARY-GRADES.

PERFORM PAYROLL-MENU.

           GOBACK.

PAYROLL-MENU.

    PERFORM UNTIL MENU-CHOICE = 4

    DISPLAY " "
    DISPLAY "========================================"
    DISPLAY "           PAYROLL MANAGEMENT"
    DISPLAY "========================================"
    DISPLAY "1. Calculate Salary"
    DISPLAY "2. Generate Salary Slip"
    DISPLAY "3. Generate Payroll Report"
    DISPLAY "4. Back to Main Menu"
    DISPLAY "========================================"
    DISPLAY "Enter your choice: " WITH NO ADVANCING
    ACCEPT MENU-CHOICE

        EVALUATE MENU-CHOICE

            WHEN 1
                PERFORM CALCULATE-SALARY

            WHEN 2
                PERFORM GENERATE-SALARY-SLIP

            WHEN 3
                PERFORM GENERATE-PAYROLL-REPORT

            WHEN 4
                DISPLAY "Returning to Main Menu."

            WHEN OTHER
                DISPLAY "Invalid option."

        END-EVALUATE

    END-PERFORM.


CALCULATE-SALARY.
    DISPLAY " ".
    DISPLAY "========================================".
    DISPLAY "          SALARY CALCULATION".
    DISPLAY "========================================".

    DISPLAY "Enter Employee ID: " WITH NO ADVANCING.
    ACCEPT SEARCH-ID.

    OPEN INPUT EMPLOYEE-FILE

    IF FILE-STATUS NOT = "00"
        DISPLAY "Unable to open employee file."
        DISPLAY "File Status: " FILE-STATUS
        EXIT PARAGRAPH
    END-IF.

    MOVE SEARCH-ID TO FILE-EMPLOYEE-ID.

    READ EMPLOYEE-FILE
        KEY IS FILE-EMPLOYEE-ID

        INVALID KEY
            DISPLAY "Employee not found."

        NOT INVALID KEY

            DISPLAY " "
            DISPLAY "Employee ID   : " FILE-EMPLOYEE-ID
            DISPLAY "Employee Name : " FILE-EMPLOYEE-NAME
            DISPLAY "Department    : " FILE-DEPARTMENT

            PERFORM CALCULATE-PAYROLL

            DISPLAY " "
            DISPLAY "----------------------------------------"
            DISPLAY "              SALARY DETAILS"
            DISPLAY "----------------------------------------"
            DISPLAY "Basic Salary       : " DISPLAY-BASIC
            DISPLAY "HRA                : " DISPLAY-HRA
            DISPLAY "DA                 : " DISPLAY-DA
            DISPLAY "Transport          : " DISPLAY-TRANSPORT
            DISPLAY "----------------------------------------"
            DISPLAY "Gross Salary       : " DISPLAY-GROSS
            DISPLAY "----------------------------------------"
            DISPLAY "PF                 : " DISPLAY-PF
            DISPLAY "Tax                : " DISPLAY-TAX
            DISPLAY "Other Deduction    : " DISPLAY-OTHER
            DISPLAY "----------------------------------------"
            DISPLAY "Total Deduction    : " DISPLAY-TOTAL
            DISPLAY "----------------------------------------"
            DISPLAY "NET SALARY         : " DISPLAY-NET
            DISPLAY "========================================"

    END-READ.

    CLOSE EMPLOYEE-FILE.


GENERATE-SALARY-SLIP.
    DISPLAY " ".
    DISPLAY "========================================".
    DISPLAY "          GENERATE SALARY SLIP".
    DISPLAY "========================================".

    DISPLAY "Enter Employee ID: " WITH NO ADVANCING.
    ACCEPT SEARCH-ID.

    OPEN INPUT EMPLOYEE-FILE

    IF FILE-STATUS NOT = "00"
        DISPLAY "Unable to open employee file."
        DISPLAY "File Status: " FILE-STATUS
        EXIT PARAGRAPH
    END-IF.

    MOVE SEARCH-ID TO FILE-EMPLOYEE-ID.

    READ EMPLOYEE-FILE
        KEY IS FILE-EMPLOYEE-ID

        INVALID KEY
            DISPLAY "Employee not found."

        NOT INVALID KEY

            PERFORM CALCULATE-PAYROLL

            OPEN OUTPUT SALARY-SLIP-FILE

            IF SLIP-FILE-STATUS NOT = "00"
                DISPLAY "Unable to create salary slip."
                DISPLAY "File Status: "
                    SLIP-FILE-STATUS
            ELSE

                MOVE "==========================================" TO SALARY-SLIP-RECORD
                WRITE SALARY-SLIP-RECORD

                MOVE "             ABC TECHNOLOGIES" TO SALARY-SLIP-RECORD
                WRITE SALARY-SLIP-RECORD

                MOVE "                SALARY SLIP" TO SALARY-SLIP-RECORD
                WRITE SALARY-SLIP-RECORD

                MOVE "==========================================" TO SALARY-SLIP-RECORD
                WRITE SALARY-SLIP-RECORD

                MOVE "Employee ID    : " TO SALARY-SLIP-RECORD
                STRING
                    "Employee ID    : "
                    FILE-EMPLOYEE-ID
                    DELIMITED BY SIZE
                    INTO SALARY-SLIP-RECORD
                END-STRING
                WRITE SALARY-SLIP-RECORD

                STRING
                    "Employee Name  : "
                    FILE-EMPLOYEE-NAME
                    DELIMITED BY SIZE
                    INTO SALARY-SLIP-RECORD
                END-STRING
                WRITE SALARY-SLIP-RECORD

                STRING
                    "Department     : "
                    FILE-DEPARTMENT
                    DELIMITED BY SIZE
                    INTO SALARY-SLIP-RECORD
                END-STRING
                WRITE SALARY-SLIP-RECORD

                MOVE
                "------------------------------------------"
                TO SALARY-SLIP-RECORD
                WRITE SALARY-SLIP-RECORD

                STRING
                    "Basic Salary   : "
                    DISPLAY-BASIC
                    DELIMITED BY SIZE
                    INTO SALARY-SLIP-RECORD
                END-STRING
                WRITE SALARY-SLIP-RECORD

                STRING
                    "HRA            : "
                    DISPLAY-HRA
                    DELIMITED BY SIZE
                    INTO SALARY-SLIP-RECORD
                END-STRING
                WRITE SALARY-SLIP-RECORD

                STRING
                    "DA             : "
                    DISPLAY-DA
                    DELIMITED BY SIZE
                    INTO SALARY-SLIP-RECORD
                END-STRING
                WRITE SALARY-SLIP-RECORD

                STRING
                    "Transport      : "
                    DISPLAY-TRANSPORT
                    DELIMITED BY SIZE
                    INTO SALARY-SLIP-RECORD
                END-STRING
                WRITE SALARY-SLIP-RECORD

                MOVE
                "------------------------------------------"
                TO SALARY-SLIP-RECORD
                WRITE SALARY-SLIP-RECORD

                STRING
                    "Gross Salary   : "
                    DISPLAY-GROSS
                    DELIMITED BY SIZE
                    INTO SALARY-SLIP-RECORD
                END-STRING
                WRITE SALARY-SLIP-RECORD

                MOVE
                "------------------------------------------"
                TO SALARY-SLIP-RECORD
                WRITE SALARY-SLIP-RECORD

                STRING
                    "PF             : "
                    DISPLAY-PF
                    DELIMITED BY SIZE
                    INTO SALARY-SLIP-RECORD
                END-STRING
                WRITE SALARY-SLIP-RECORD

                STRING
                    "Tax            : "
                    DISPLAY-TAX
                    DELIMITED BY SIZE
                    INTO SALARY-SLIP-RECORD
                END-STRING
                WRITE SALARY-SLIP-RECORD

                STRING
                    "Other Deduct.  : "
                    DISPLAY-OTHER
                    DELIMITED BY SIZE
                    INTO SALARY-SLIP-RECORD
                END-STRING
                WRITE SALARY-SLIP-RECORD

                MOVE
                "------------------------------------------"
                TO SALARY-SLIP-RECORD
                WRITE SALARY-SLIP-RECORD

                STRING
                    "Total Deduct.  : "
                    DISPLAY-TOTAL
                    DELIMITED BY SIZE
                    INTO SALARY-SLIP-RECORD
                END-STRING
                WRITE SALARY-SLIP-RECORD

                STRING
                    "NET SALARY     : "
                    DISPLAY-NET
                    DELIMITED BY SIZE
                    INTO SALARY-SLIP-RECORD
                END-STRING
                WRITE SALARY-SLIP-RECORD

                MOVE
                "=========================================="
                TO SALARY-SLIP-RECORD
                WRITE SALARY-SLIP-RECORD

                CLOSE SALARY-SLIP-FILE

                DISPLAY " "
                DISPLAY "Salary slip generated successfully."
                DISPLAY "File: output\SALARY-SLIP.TXT"

            END-IF

    END-READ.

    CLOSE EMPLOYEE-FILE.


CALCULATE-PAYROLL.

    COMPUTE HRA ROUNDED = FILE-BASIC-SALARY * GRADE-HRA-PCT(GRADE-INDEX) / 100.
    COMPUTE DA ROUNDED = FILE-BASIC-SALARY * GRADE-DA-PCT(GRADE-INDEX) / 100.
    COMPUTE TRANSPORT ROUNDED = FILE-BASIC-SALARY * GRADE-TRANS-PCT(GRADE-INDEX) / 100.
    COMPUTE PF-FUND ROUNDED =  FILE-BASIC-SALARY * GRADE-PF-PCT(GRADE-INDEX) / 100.
    COMPUTE GROSS-SALARY ROUNDED =  FILE-BASIC-SALARY + HRA + DA + TRANSPORT.

    EVALUATE TRUE
        WHEN GROSS-SALARY > 50000
            COMPUTE TAX ROUNDED =
                GROSS-SALARY * 5 / 100

        WHEN OTHER
            MOVE 0 TO TAX
    END-EVALUATE.

    MOVE 1000 TO OTHER-DEDUCTION.

    COMPUTE TOTAL-DEDUCTION ROUNDED = PF-FUND + TAX + OTHER-DEDUCTION.
    COMPUTE NET-SALARY ROUNDED = GROSS-SALARY - TOTAL-DEDUCTION.

    MOVE FILE-BASIC-SALARY TO DISPLAY-BASIC.
    MOVE HRA TO DISPLAY-HRA.
    MOVE DA TO DISPLAY-DA.
    MOVE TRANSPORT TO DISPLAY-TRANSPORT.
    MOVE GROSS-SALARY TO DISPLAY-GROSS.
    MOVE PF-FUND TO DISPLAY-PF.
    MOVE TAX TO DISPLAY-TAX.
    MOVE OTHER-DEDUCTION TO DISPLAY-OTHER.
    MOVE TOTAL-DEDUCTION TO DISPLAY-TOTAL.
    MOVE NET-SALARY TO DISPLAY-NET.


GENERATE-PAYROLL-REPORT.

    DISPLAY " ".
    DISPLAY "========================================".
    DISPLAY "       GENERATE PAYROLL REPORT".
    DISPLAY "========================================".

    MOVE 0 TO TOTAL-EMPLOYEES.
    MOVE 0 TO TOTAL-BASIC-SALARY.
    MOVE 0 TO TOTAL-GROSS-SALARY.
    MOVE 0 TO TOTAL-NET-SALARY.

    OPEN INPUT EMPLOYEE-FILE.

    IF FILE-STATUS NOT = "00"

        DISPLAY "Unable to open employee file."
        DISPLAY "File Status: " FILE-STATUS

    ELSE

        OPEN OUTPUT PAYROLL-REPORT-FILE

        IF REPORT-FILE-STATUS NOT = "00"

            DISPLAY "Unable to create payroll report."
            DISPLAY "File Status: "
                REPORT-FILE-STATUS

            CLOSE EMPLOYEE-FILE

        ELSE

            PERFORM WRITE-REPORT-HEADER

            PERFORM UNTIL FILE-STATUS = "10"

                READ EMPLOYEE-FILE NEXT RECORD

                    AT END
                        MOVE "10" TO FILE-STATUS

                    NOT AT END
                        PERFORM PROCESS-EMPLOYEE-REPORT

                END-READ

            END-PERFORM

            PERFORM WRITE-REPORT-TOTALS

            CLOSE EMPLOYEE-FILE
            CLOSE PAYROLL-REPORT-FILE

            DISPLAY " "
            DISPLAY "Payroll report generated successfully."
            DISPLAY "File: output\PAYROLL-REPORT.TXT"

        END-IF

    END-IF.


WRITE-REPORT-HEADER.

    MOVE
    "=========================================================================="
    TO PAYROLL-REPORT-RECORD
    WRITE PAYROLL-REPORT-RECORD

    MOVE
    "                        MONTHLY PAYROLL REPORT"
    TO PAYROLL-REPORT-RECORD
    WRITE PAYROLL-REPORT-RECORD

    MOVE
    "=========================================================================="
    TO PAYROLL-REPORT-RECORD
    WRITE PAYROLL-REPORT-RECORD

    MOVE
    "ID     NAME                             DEPARTMENT            BASIC          GROSS          NET            GRADE"
    TO PAYROLL-REPORT-RECORD
    WRITE PAYROLL-REPORT-RECORD

    MOVE
    "--------------------------------------------------------------------------"
    TO PAYROLL-REPORT-RECORD
    WRITE PAYROLL-REPORT-RECORD.


PROCESS-EMPLOYEE-REPORT.

    PERFORM CALCULATE-PAYROLL.

    ADD 1 TO TOTAL-EMPLOYEES.

    ADD FILE-BASIC-SALARY
        TO TOTAL-BASIC-SALARY.

    ADD GROSS-SALARY
        TO TOTAL-GROSS-SALARY.

    ADD NET-SALARY
        TO TOTAL-NET-SALARY.

    MOVE FILE-EMPLOYEE-ID
        TO DISPLAY-EMPLOYEE-ID.

    STRING
        DISPLAY-EMPLOYEE-ID
        " | "
        FILE-EMPLOYEE-NAME
        " | "
        FILE-DEPARTMENT
        " | "
        DISPLAY-BASIC
        " | "
        DISPLAY-GROSS
        " | "
        DISPLAY-NET
        " | "
        FILE-SALARY-GRADE
        DELIMITED BY SIZE
        INTO PAYROLL-REPORT-RECORD
    END-STRING.

    WRITE PAYROLL-REPORT-RECORD.


WRITE-REPORT-TOTALS.

    MOVE TOTAL-EMPLOYEES
        TO DISPLAY-TOTAL-EMPLOYEES.

    MOVE TOTAL-BASIC-SALARY
        TO DISPLAY-TOTAL-BASIC.

    MOVE TOTAL-GROSS-SALARY
        TO DISPLAY-TOTAL-GROSS.

    MOVE TOTAL-NET-SALARY
        TO DISPLAY-TOTAL-NET.

    MOVE
    "--------------------------------------------------------------------------"
    TO PAYROLL-REPORT-RECORD
    WRITE PAYROLL-REPORT-RECORD

    STRING
        "TOTAL EMPLOYEES : "
        DISPLAY-TOTAL-EMPLOYEES
        DELIMITED BY SIZE
        INTO PAYROLL-REPORT-RECORD
    END-STRING
    WRITE PAYROLL-REPORT-RECORD

    STRING
        "TOTAL BASIC SALARY : "
        DISPLAY-TOTAL-BASIC
        DELIMITED BY SIZE
        INTO PAYROLL-REPORT-RECORD
    END-STRING
    WRITE PAYROLL-REPORT-RECORD

    STRING
        "TOTAL GROSS SALARY : "
        DISPLAY-TOTAL-GROSS
        DELIMITED BY SIZE
        INTO PAYROLL-REPORT-RECORD
    END-STRING
    WRITE PAYROLL-REPORT-RECORD

    STRING
        "TOTAL NET SALARY   : "
        DISPLAY-TOTAL-NET
        DELIMITED BY SIZE
        INTO PAYROLL-REPORT-RECORD
    END-STRING
    WRITE PAYROLL-REPORT-RECORD

    MOVE
    "=========================================================================="
    TO PAYROLL-REPORT-RECORD
    WRITE PAYROLL-REPORT-RECORD.


INITIALIZE-GRADE-TABLE.

    MOVE "A" TO GRADE-CODE(1).
    MOVE 25   TO GRADE-HRA-PCT(1).
    MOVE 15   TO GRADE-DA-PCT(1).
    MOVE 10   TO GRADE-TRANS-PCT(1).
    MOVE 12   TO GRADE-PF-PCT(1).

    MOVE "B" TO GRADE-CODE(2).
    MOVE 20   TO GRADE-HRA-PCT(2).
    MOVE 10   TO GRADE-DA-PCT(2).
    MOVE 5    TO GRADE-TRANS-PCT(2).
    MOVE 12   TO GRADE-PF-PCT(2).

    MOVE "C" TO GRADE-CODE(3).
    MOVE 15   TO GRADE-HRA-PCT(3).
    MOVE 8    TO GRADE-DA-PCT(3).
    MOVE 5    TO GRADE-TRANS-PCT(3).
    MOVE 12   TO GRADE-PF-PCT(3).


FIND-SALARY-GRADE.

    MOVE "N" TO GRADE-FOUND.

    SET GRADE-INDEX TO 1.

    SEARCH SALARY-GRADE-ENTRY

        AT END
            DISPLAY "Invalid salary grade."

        WHEN GRADE-CODE(GRADE-INDEX) =
             EMPLOYEE-GRADE

            MOVE "Y" TO GRADE-FOUND

    END-SEARCH.


LOAD-SALARY-GRADES.

    OPEN INPUT SALARY-GRADE-FILE.

    IF GRADE-FILE-STATUS NOT = "00"

        DISPLAY "Unable to open salary grade file."
        DISPLAY "File Status: " GRADE-FILE-STATUS
        EXIT PARAGRAPH

    END-IF.

    MOVE 1 TO GRADE-COUNT.

    PERFORM UNTIL GRADE-FILE-STATUS = "10"

        READ SALARY-GRADE-FILE

            AT END
                CONTINUE

            NOT AT END

                IF GRADE-COUNT <= 3

                    MOVE FILE-GRADE-CODE TO GRADE-CODE(GRADE-COUNT)
                    MOVE FILE-GRADE-HRA TO GRADE-HRA-PCT(GRADE-COUNT)
                    MOVE FILE-GRADE-DA TO GRADE-DA-PCT(GRADE-COUNT)
                    MOVE FILE-GRADE-TRANSPORT TO GRADE-TRANS-PCT(GRADE-COUNT)
                    MOVE FILE-GRADE-PF TO GRADE-PF-PCT(GRADE-COUNT)

                    ADD 1 TO GRADE-COUNT

                END-IF

        END-READ

    END-PERFORM.

    CLOSE SALARY-GRADE-FILE.


