      ******************************************************************
      * Author:    VERONICA ALARCON
      * Date:      2021-10-01
      * Purpose:   GRABA UN ARCHIVO CON LA DESCRIPCION DE LOS PAISES DE
      *            LATAM PRESENTES EN PAISLAT.DAT CON SU NUMERO DE
      *            HABITANTES PRESENTE EN PAISHAB.DAT
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APAREO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      ******************************************************************
       FILE-CONTROL.
           SELECT ENTRADA1 ASSIGN TO "PAISHAB.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-ENTRADA1.
           SELECT ENTRADA2 ASSIGN TO "PAISLAT.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-ENTRADA2.
           SELECT SALIDA1 ASSIGN TO "TOTALES.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-SALIDA1.
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  ENTRADA1.
           01  REG-ENTRADA1.
               05 A1-COD-PAIS         PIC X(03).
               05 A1-HAB              PIC 9(09).
       FD  ENTRADA2.
           01  REG-ENTRADA2.
               05 A2-COD-PAIS         PIC X(03).
               05 A2-DES-PAIS         PIC X(40).
       FD  SALIDA1.
           01  REG-SALIDA1.
               05 A3-PAIS             PIC X(40).
               05 A3-HAB              PIC 9(09).

      ******************************************************************
       WORKING-STORAGE SECTION.
       01  FILE-STATUS.
           05  FS-ENTRADA1             PIC X(02).
                   88  FS-ENTRADA1-OK              VALUE '00'.
                   88  FS-ENTRADA1-FIN             VALUE '10'.
           05  FS-ENTRADA2             PIC X(02).
                   88  FS-ENTRADA2-OK              VALUE '00'.
                   88  FS-ENTRADA2-FIN             VALUE '10'.
           05  FS-SALIDA1              PIC X(02).
                   88 FS-SALIDA1-OK                VALUE '00'.
       01  WSS-VARIABLES.
           05 WSS-APAREO.
               10 WSS-CLAVE1           PIC X(03).
               10 WSS-CLAVE2           PIC X(03).
           05 WSS-SALIDA.
               10 WSS-PAIS             PIC X(40).
               10 WSS-HAB              PIC 9(09).
           05 WSS-TOTALES-CONTROL.
               10 WSS-LEIDOS1          PIC 9(04).
               10 WSS-LEIDOS2          PIC 9(04).
               10 WSS-GRABADOS         PIC 9(04).
               10 WSS-ERROR            PIC 9(04).

      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO    UNTIL FS-ENTRADA1-FIN
                                   AND FS-ENTRADA2-FIN
           PERFORM 9000-FINALIZAR
           .

      *-----------------------------------------------------------------*
      *    ABRO ARCHIVOS, INICIALIZO Y LEO EL PRIMER REGISTRO
      *-----------------------------------------------------------------*
       1000-INICIO.
           PERFORM 1100-APERTURA-ARCHIVOS
           INITIALISE WSS-TOTALES-CONTROL
           PERFORM 5000-LECTURA-ARCH1
           PERFORM 5100-LECTURA-ARCH2
           .
       1000-INICIO-EXIT. EXIT.

       1100-APERTURA-ARCHIVOS.
           OPEN INPUT ENTRADA1
           IF NOT FS-ENTRADA1-OK
               DISPLAY 'ERROR APERTURA ENTRADA1 FS: ' FS-ENTRADA1
               PERFORM 9000-FINALIZAR
           END-IF

           OPEN INPUT ENTRADA2
           IF NOT FS-ENTRADA2-OK
               DISPLAY 'ERROR APERTURA ENTRADA1 FS: ' FS-ENTRADA2
               PERFORM 9000-FINALIZAR
           END-IF

           OPEN OUTPUT SALIDA1
           IF NOT FS-SALIDA1-OK
               DISPLAY 'ERROR APERTURA SALIDA FS: ' FS-SALIDA1
               PERFORM 9000-FINALIZAR
           END-IF
           .
       1100-APERTURA-ARCHIVOS-EXIT. EXIT.

      *-----------------------------------------------------------------*
      *    APAREO
      *-----------------------------------------------------------------*
       2000-PROCESO.
           EVALUATE TRUE
               WHEN WSS-CLAVE1 > WSS-CLAVE2
                   PERFORM 5100-LECTURA-ARCH2
               WHEN WSS-CLAVE1 < WSS-CLAVE2
                   PERFORM 5000-LECTURA-ARCH1
               WHEN WSS-CLAVE1 = WSS-CLAVE2
                   PERFORM 5200-WRITE-ARCH
                   PERFORM 5000-LECTURA-ARCH1
           END-EVALUATE
           .
       2000-PROCESO-EXIT. EXIT.


      *-----------------------------------------------------------------*
      *-----------------------------------------------------------------*
       5000-LECTURA-ARCH1.
           READ ENTRADA1
           EVALUATE TRUE
               WHEN FS-ENTRADA1-OK
                   ADD 1 TO WSS-LEIDOS1
                   MOVE A1-COD-PAIS TO WSS-CLAVE1
               WHEN FS-ENTRADA1-FIN
                   MOVE HIGH-VALUE TO WSS-CLAVE1
               WHEN OTHER
                   DISPLAY 'ERROR LECTURA ENTRADA1 FS: ' FS-ENTRADA1
                   PERFORM 9000-FINALIZAR
           END-EVALUATE
           .
       5000-LECTURA-ARCH1-EXIT. EXIT.

       5100-LECTURA-ARCH2.
           READ ENTRADA2
           EVALUATE TRUE
               WHEN FS-ENTRADA2-OK
                   ADD 1 TO WSS-LEIDOS2
                   MOVE A2-COD-PAIS TO WSS-CLAVE2
               WHEN FS-ENTRADA2-FIN
                   MOVE HIGH-VALUE TO WSS-CLAVE2
               WHEN OTHER
                   DISPLAY 'ERROR LECTURA ENTRADA1 FS: ' FS-ENTRADA2
                   PERFORM 9000-FINALIZAR
           END-EVALUATE
           .
       5100-LECTURA-ARCH2-EXIT. EXIT.

       5200-WRITE-ARCH.
           MOVE A2-DES-PAIS TO WSS-PAIS
           MOVE A1-HAB      TO WSS-HAB

           WRITE REG-SALIDA1 FROM WSS-SALIDA

           IF FS-SALIDA1-OK
               ADD 1 TO WSS-GRABADOS
           ELSE
               DISPLAY 'ERROR FS: ' FS-SALIDA1
               PERFORM 9000-FINALIZAR
           END-IF
           .
       5200-WRITE-ARCH-EXIT. EXIT.
      *-----------------------------------------------------------------*
      *    CIERRO ARCHIVOS, DISPLAYO TOTALES DE CONTROL
      *-----------------------------------------------------------------*
       9000-FINALIZAR.
           PERFORM 9100-TOTALES-DE-CONTROL.
           CLOSE ENTRADA1
           CLOSE ENTRADA2
           CLOSE SALIDA1
           STOP RUN
           .
       9000-FINALIZAR-EXIT. EXIT.

       9100-TOTALES-DE-CONTROL.
           DISPLAY '------------------------------------'
           DISPLAY '- REGISTROS ARCH 1 LEIDOS: ' WSS-LEIDOS1
           DISPLAY '- REGISTROS ARCH 2 LEIDOS: ' WSS-LEIDOS2
           DISPLAY '- REGISTROS GRABADOS: ' WSS-GRABADOS
           DISPLAY '- ERRORES: ' WSS-ERROR
           DISPLAY '------------------------------------'
           .
       9100-TOTALES-DE-CONTROL-EXIT. EXIT.
