      ******************************************************************
      * Author:    VERONICA ALARCON
      * Date:      2021-09-20
      * Purpose:   CORTE DE CONTROL POR SUCURSAL Y POR PAIS, QUE INFORMA
      *            CANT DE CUENTAS Y SALDO. LO DEJA GRABADO EN AL ARCH
      *            DE SALIDA BAJO LOS REGISTROS DE ESA SUCURSAR/PAIS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CORTE2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      ******************************************************************
       FILE-CONTROL.
           SELECT ENTRADA1 ASSIGN TO "cuentas.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-ENTRADA1.
           SELECT TOTALES ASSIGN TO "TOTALES.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-TOTALES.
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  ENTRADA1.
           01  REG-ENTRADA1.
               COPY CUENTASC.
       FD  TOTALES.
           01  REG-TOTALES     PIC X(45).

      ******************************************************************
       WORKING-STORAGE SECTION.
       01  FILE-STATUS.
           05  FS-ENTRADA1             PIC X(02).
                   88  FS-ENTRADA1-OK              VALUE '00'.
                   88  FS-ENTRADA1-FIN             VALUE '10'.
           05  FS-TOTALES              PIC X(02).
                   88 FS-TOTALES-OK                VALUE '00'.
       01  WSS-VARIABLES.
           05 CORTE.
               10 WSS-ULT-PAIS         PIC X(03).
               10 WSS-ULT-SUC          PIC 9(02).
               10 WSS-CANT-X-PAIS      PIC 9(03).
               10 WSS-CANT-X-SUC       PIC 9(03).
               10 WSS-SALDO-SUC        PIC 9(09).
               10 WSS-SALDO-PAIS       PIC 9(10).
           05 WSS-SALIDA.
               10 WSS-TEXTO            PIC X(35).
               10 WSS-TOTAL            PIC 9(10).
           05 WSS-TOTALES-CONTROL.
               10 WSS-LEIDOS           PIC 9(04).
               10 WSS-GRABADOS         PIC 9(04).
               10 WSS-ERROR            PIC 9(04).

      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO UNTIL FS-ENTRADA1-FIN
           PERFORM 9000-FINALIZAR
           .

      *-----------------------------------------------------------------*
      *    ABRO ARCHIVOS, INICIALIZO Y LEO EL PRIMER REGISTRO
      *-----------------------------------------------------------------*
       1000-INICIO.
           PERFORM 1100-APERTURA-ARCHIVOS
           INITIALISE WSS-TOTALES-CONTROL
           PERFORM 5000-LECTURA-ARCH
           .
       1000-INICIO-EXIT. EXIT.

       1100-APERTURA-ARCHIVOS.
           OPEN INPUT ENTRADA1
           IF NOT FS-ENTRADA1-OK
               DISPLAY 'ERROR APERTURA ENTRADA1 FS: ' FS-ENTRADA1
               PERFORM 9000-FINALIZAR
           END-IF

           OPEN OUTPUT TOTALES
           IF NOT FS-TOTALES-OK
               DISPLAY 'ERROR APERTURA SALIDA FS: ' FS-TOTALES
               PERFORM 9000-FINALIZAR
           END-IF
           .
       1100-APERTURA-ARCHIVOS-EXIT. EXIT.

      *-----------------------------------------------------------------*
      *    CORTE DE CONTROL
      *-----------------------------------------------------------------*
       2000-PROCESO.
      *    INICIALIZO
           INITIALIZE WSS-CANT-X-PAIS, WSS-SALDO-PAIS
      *    CORTE DE CONTROL POR PAIS
           MOVE CTA-PAIS TO WSS-ULT-PAIS
           PERFORM 2100-PAIS
               UNTIL FS-ENTRADA1-FIN
               OR CTA-PAIS NOT = WSS-ULT-PAIS
      *    GRABO TOTAL
           PERFORM 2050-GRABO-PAIS
           .
       2000-PROCESO-EXIT. EXIT.

       2050-GRABO-PAIS.
           MOVE '-CANTIDAD DE CUENTAS POR PAIS: ' TO WSS-TEXTO
           MOVE WSS-CANT-X-PAIS TO WSS-TOTAL
           PERFORM 5100-WRITE-ARCH

           MOVE '-SALDO TOTAL DEL PAIS: ' TO WSS-TEXTO
           MOVE WSS-SALDO-PAIS TO WSS-TOTAL
           PERFORM 5100-WRITE-ARCH
           .
       2050-GRABO-PAIS-EXIT. EXIT.

       2100-PAIS.
      *    INICIALIZO
           INITIALIZE WSS-CANT-X-SUC, WSS-SALDO-SUC
      *    CORTE DE CONTROL POR SUCURSAL
           MOVE CTA-SUCURSAL TO WSS-ULT-SUC
           PERFORM 2200-SUCURSAL
               UNTIL FS-ENTRADA1-FIN
               OR CTA-PAIS NOT = WSS-ULT-PAIS
               OR CTA-SUCURSAL NOT = WSS-ULT-SUC
      *    GRABO TOTALES
           PERFORM 2150-GRABO-SUCURSAL
           .
       2100-PAIS-EXIT. EXIT.

       2150-GRABO-SUCURSAL.
           MOVE '-CANTIDAD DE CUENTAS POR SUCURSAL: ' TO WSS-TEXTO
           MOVE WSS-CANT-X-SUC TO WSS-TOTAL
           PERFORM 5100-WRITE-ARCH

           MOVE '-SALDO POR SUCURSAL: ' TO WSS-TEXTO
           MOVE WSS-SALDO-SUC TO WSS-TOTAL
           PERFORM 5100-WRITE-ARCH
           .
       2150-GRABO-SUCURSAL-EXIT. EXIT.

       2200-SUCURSAL.
      *    SUMO
           ADD 1 TO WSS-CANT-X-SUC, WSS-CANT-X-PAIS
           ADD CTA-SALDO TO WSS-SALDO-SUC, WSS-SALDO-PAIS
      *    GRABO LINEA ORIGINAL
           PERFORM 2250-GRABO-LINEA-ORIGINAL
      *    LEO
           PERFORM 5000-LECTURA-ARCH
           .
       2200-SUCURSAL-EXIT. EXIT.

       2250-GRABO-LINEA-ORIGINAL.
           MOVE REG-ENTRADA1 TO REG-TOTALES
           WRITE REG-TOTALES
           IF FS-TOTALES-OK
               ADD 1 TO WSS-GRABADOS
           ELSE
               DISPLAY 'ERROR FS: ' FS-ENTRADA1
               PERFORM 9000-FINALIZAR
           END-IF
           .
       2250-GRABO-LINEA-ORIGINAL-EXIT. EXIT.
      *-----------------------------------------------------------------*
      *-----------------------------------------------------------------*
       5000-LECTURA-ARCH.
           READ ENTRADA1
           EVALUATE TRUE
               WHEN FS-ENTRADA1-OK
                   ADD 1 TO WSS-LEIDOS
               WHEN FS-ENTRADA1-FIN
                   CONTINUE
               WHEN OTHER
                   DISPLAY 'ERROR LECTURA ENTRADA1 FS: ' FS-ENTRADA1
                   PERFORM 9000-FINALIZAR
           END-EVALUATE
           .
       5000-LECTURA-ARCH-EXIT. EXIT.

       5100-WRITE-ARCH.
           WRITE REG-TOTALES FROM WSS-SALIDA

           IF FS-TOTALES-OK
               ADD 1 TO WSS-GRABADOS
           ELSE
               DISPLAY 'ERROR FS: ' FS-ENTRADA1
               PERFORM 9000-FINALIZAR
           END-IF
           .
       5100-WRITE-ARCH-EXIT. EXIT.
      *-----------------------------------------------------------------*
      *    CIERRO ARCHIVOS, DISPLAYO TOTALES DE CONTROL
      *-----------------------------------------------------------------*
       9000-FINALIZAR.
           PERFORM 9100-TOTALES-DE-CONTROL.
           CLOSE ENTRADA1
           CLOSE TOTALES
           STOP RUN
           .
       9000-FINALIZAR-EXIT. EXIT.

       9100-TOTALES-DE-CONTROL.
           DISPLAY '-------------------------------'
           DISPLAY '- REGISTROS LEIDOS: ' WSS-LEIDOS
           DISPLAY '- REGISTROS GRABADOS: ' WSS-GRABADOS
           DISPLAY '- ERRORES: ' WSS-ERROR
           DISPLAY '-------------------------------'
           .
       9100-TOTALES-DE-CONTROL-EXIT. EXIT.
