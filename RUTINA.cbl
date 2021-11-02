      ******************************************************************
      * Author:    VERONICA ALARCON
      * Date:      2021-08-31
      * Purpose:   RUTINA QUE COMPLETA UN ALFANUMERICO DE NUMEROS CON
      *            CEROS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.RUTINA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------*
       LINKAGE SECTION.
       01 AREADECOM.
           COPY CPY002.
      *-----------------------------------------------------------------*
       PROCEDURE DIVISION USING AREADECOM.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO.
           GOBACK.


      *-----------------------------------------------------------------*
      *    INICIALIZO
      *-----------------------------------------------------------------*
       1000-INICIO.
           MOVE 10 TO WSS-POS
           INITIALIZE WSS-CONT
           .
       1000-INICIO-EXIT. EXIT.

      *-----------------------------------------------------------------*
      *    TRANSFORMA ALFANUMERICO A NUMERICO
      *-----------------------------------------------------------------*
       2000-PROCESO.
           PERFORM 2100-MOVER-CARACTERES
           VARYING WSS-I
               FROM 10 BY -1
               UNTIL WSS-I < 1

           PERFORM 2200-RELLENO-CEROS
           VARYING WSS-I
               FROM 1 BY 1
               UNTIL WSS-I > 10 - WSS-CONT
           .
       2000-PROCESO-EXIT. EXIT.

       2100-MOVER-CARACTERES.
           IF WSS-NUM (WSS-I:1) NOT = ' '
               MOVE WSS-NUM (WSS-I:1) TO WSS-NUM (WSS-POS:1)
               SUBTRACT 1 FROM WSS-POS
               ADD 1 TO WSS-CONT
           END-IF
           .
       2100-MOVER-CARACTERES-EXIT. EXIT.

       2200-RELLENO-CEROS.
           MOVE '0' TO WSS-NUM (WSS-I:1)
           .
       2200-RELLENO-CEROS-EXIT. EXIT.
