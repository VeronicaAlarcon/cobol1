      ******************************************************************
      * Author:  VERONICA ALARCON
      * Date:    2021-08-11
      * Purpose: PROGRAMA QUE ORDENA NUMEROS DENTRO DE  UN VECTOR
      *          (BURBUJEO)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDENAMIENTO-BURBUJEO.
       DATA DIVISION.
      *-----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01 WSS-VAR.
           03  WSS-ORDENAMIENTO.
               05 WSS-TAM-VECTOR   PIC 9       VALUE 4.
               05 WSS-AUX          PIC S9(10).
               05 WSS-J            PIC 9.
               05 WSS-ORDEN        PIC 9.
                   88 WSS-ORDENADO             VALUE 1.
                   88 WSS-DESORDENADO          VALUE 0.
           03 WSS-VECTOR           PIC S9(10) OCCURS 4 TIMES.
           03 AREADECOM.
               COPY CPY001.

      *-----------------------------------------------------------------*
       PROCEDURE DIVISION.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO UNTIL WSS-ORDENADO
           PERFORM 9000-FINAL.
           STOP RUN.

      *-----------------------------------------------------------------*
      *    CARGO EL VECTOR, PREPARO VARIABLE PARA ENTRAR AL CICLO
      *-----------------------------------------------------------------*
       1000-INICIO.
      *    CARGO VECTOR Y ORDENO CARACTERES
           PERFORM 1100-ORDEN-CARACTERES
           VARYING WSS-J
               FROM 1 BY 1
               UNTIL WSS-J > WSS-TAM-VECTOR
      *    PREPARO VARIABLE PARA ENRAR AL CICLO
           SET WSS-DESORDENADO TO TRUE
           .
       1000-INICIO-EXIT. EXIT.
      *-----------------------------------------------------------------*
       1100-ORDEN-CARACTERES.
           DISPLAY 'INGESE POSICION ' WSS-J
           ACCEPT WSS-VECTOR (WSS-J)
           .
       1100-ORDEN-CARACTERES-EXIT. EXIT.

      *-----------------------------------------------------------------*
      *    ORDENAMIENTO POR BURBUJEO
      *-----------------------------------------------------------------*
       2000-PROCESO.
           SET WSS-ORDENADO TO TRUE
           PERFORM 2100-REACOMODO
           VARYING WSS-J
               FROM 1 BY 1
               UNTIL WSS-J >= WSS-TAM-VECTOR
      *    GUARDO EN DONDE ESTA DESORDENADO MI VECTOR
           MOVE WSS-ORDEN TO WSS-TAM-VECTOR
           .
       2OOO-PROCESO-EXIT. EXIT.
      *-----------------------------------------------------------------*
       2100-REACOMODO.
           IF WSS-VECTOR(WSS-J) > WSS-VECTOR(WSS-J + 1)
               MOVE WSS-VECTOR(WSS-J) TO WSS-AUX
               MOVE WSS-VECTOR(WSS-J + 1) TO WSS-VECTOR(WSS-J)
               MOVE WSS-AUX TO WSS-VECTOR(WSS-J + 1)
               MOVE WSS-J TO WSS-ORDEN
           END-IF
           .
       2100-REACOMODO-EXIT. EXIT.

      *-----------------------------------------------------------------*
      *     DISPLAYO EL VECTOR
      *-----------------------------------------------------------------*
       9000-FINAL.
           DISPLAY '--------------'
           DISPLAY 'ARRAY ORDENADO'
           DISPLAY '--------------'
           PERFORM VARYING WSS-J
               FROM 1 BY 1
               UNTIL WSS-J > 4
                   DISPLAY WSS-VECTOR(WSS-J)
           END-PERFORM
           .
       9000-FINAL-EXIT. EXIT.
