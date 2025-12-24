//B45617CO JOB ,,CLASS=C,REGION=0M,
//         MSGLEVEL=(1,1),MSGCLASS=P
//*********************************************************************
//* JCL PARA PROBAR COMPILACION DE OPEBSYS CON COBOL 6.4
//* USANDO LOS MISMOS DATASETS QUE DBB ZBUILDER
//*********************************************************************
//COBOL64  EXEC PGM=IGYCRCTL,REGION=0M,
//         PARM='LIB'
//*--------------------------------------------------------------------
//* STEPLIB - TASKLIB EQUIVALENTE EN DBB
//*--------------------------------------------------------------------
//STEPLIB  DD DSN=IBM.COB640.SIGYCOMP,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//         DD DSN=CEE.SCEERUN2,DISP=SHR
//*--------------------------------------------------------------------
//* SYSIN - PROGRAMA FUENTE
//*--------------------------------------------------------------------
//SYSIN    DD DSN=B45617.DBB.COBOL(OPEBSYS),DISP=SHR
//*--------------------------------------------------------------------
//* SYSLIB - COPYBOOKS
//*--------------------------------------------------------------------
//SYSLIB   DD DSN=B45617.DBB.COPY,DISP=SHR
//         DD DSN=B45617.DBB.BMS.COPY,DISP=SHR
//*--------------------------------------------------------------------
//* SYSLIN - OBJETO DE SALIDA
//*--------------------------------------------------------------------
//SYSLIN   DD DSN=B45617.DBB.OBJ(OPEBSYS),DISP=SHR
//*--------------------------------------------------------------------
//* SYSPRINT - LOG DE COMPILACION
//*--------------------------------------------------------------------
//SYSPRINT DD SYSOUT=*
//*--------------------------------------------------------------------
//* WORK FILES
//*--------------------------------------------------------------------
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT2   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT3   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT4   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT5   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT6   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT7   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT8   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT9   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT10  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT11  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT12  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT13  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT14  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT15  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSMDECK DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//
