IDENTIFICATION DIVISION.
PROGRAM-ID. MAIN.

DATA DIVISION.
WORKING-STORAGE SECTION.

01 MAIN-CHOICE PIC 9 VALUE 0.

PROCEDURE DIVISION.

    PERFORM UNTIL MAIN-CHOICE = 3

        DISPLAY " "
        DISPLAY "========================================"
        DISPLAY "       EMPLOYEE PAYROLL SYSTEM"
        DISPLAY "========================================"
        DISPLAY "1. Employee Management"
        DISPLAY "2. Payroll Management"
        DISPLAY "3. Exit"
        DISPLAY "========================================"
        DISPLAY "Enter your choice: " WITH NO ADVANCING
        ACCEPT MAIN-CHOICE

        EVALUATE MAIN-CHOICE

            WHEN 1
                CALL "EMPLOYEE"

            WHEN 2
                CALL "PAYROLL"

            WHEN 3
                DISPLAY " "
                DISPLAY "Thank you for using the system."

            WHEN OTHER
                DISPLAY "Invalid option."

        END-EVALUATE

    END-PERFORM.

    STOP RUN.
