
**FREE
//**************************************************************
Ctl-Opt copyright('(C) Copyright 1996 - 2016 King III Solutions, Inc.+
Rel 6.01 2016-01-01   Program Property of King III Solutions, Inc.+
All rights reserved              +
K3S_Replenish (R) is a Registered Trade Mark of King III Solutions+
Inc.');

Ctl-Opt DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') option(*nodebugio);
Ctl-Opt bnddir('K3SDIR');

//**************************************************************
//
//  K3S-Replenish (R) - Inventory REPLENISHment System
//  Copyright (C) 1996-2013 by King III Solutions, Inc.
//  Program property of King III Solutions, Inc.
//  All rights reserved.
//  K3S_Replenish (R) is a Registered Trade Mark of
//  King III Solutions Inc.
//
//**************************************************************
//
//  Name: AR_NITMISC
//  Type: ILE RPG Program
//  Desc: Is it period end?
//  Auth: David Meyer
//
//****************************************************************
//                                                               *
//   Change ID  Change Date Change Description                   *
//   ---------  ----------  -------------------------------------*
//     865    2022-04-08  Initially written.                   *
//***************************************************************

// --------------- Constants --------------------
Dcl-C SQLSTATEOK Const('00000');
Dcl-C ROW_NOT_FOUND Const('02000');
Dcl-C EMPTY_STRING Const('');
Dcl-C ONE_SPACE Const(' ');

//---------------- Program Variables -------------------
Dcl-S wkfound                    Ind;
Dcl-S wk_codetyp                 Char(3)     inz;
Dcl-S wk_codeval                 Char(20)    inz;
Dcl-s wkflag                     Char(1);

Dcl-DS validationResult Qualified;
  isValid       Ind;
  errorField    Char(20)  inz(*blanks);
  errorMessage  Char(100) inz(*blanks);
End-DS;

/copy ar_dexcer
//--------------------------- Procedure definitions ---------------------------
/copy k3s_apipro
/copy ar_srlnb_h
/copy ar_excer_h
/copy tablcod_h
/copy company_h


//--------------------------- Prototype-----------------------------------------
Dcl-PR AR_NITMISC;
  comp           Char(1);      // Company
  compcod        Char(3);      // Company code
  user           Char(10);     // User profile
  errors         Char(1);      // Errors exist flag
  errmsg         Char(100);    // Error message
  errfield       Char(20);     // Erroneous field
  sbforcint      Zoned(3:0);   // Forcast interval
  rvpetoday      Char(1);      // Period end flag
End-PR;
//--------------------------- Main Procedure Interface -------------------------
Dcl-PI AR_NITMISC;
  comp           Char(1);      // Company
  compcod        Char(3);      // Company code
  user           Char(10);     // User profile
  errors         Char(1);      // Errors exist flag
  errmsg         Char(100);    // Error message
  errfield       Char(20);     // Erroneous field
  sbforcint      Zoned(3:0);   // Forcast interval
  rvpetoday      Char(1);      // Period end flag
End-PI;

//---------------------------------------------------- Local Data Area
/copy ar_pgmdstr
/copy k3s_c030

//--------------------------------------------------------------
//* Setting Defaults...
//--------------------------------------------------------------
exec sql
set option commit = *none,
datfmt = *iso,
closqlcsr = *endactgrp;

//*=============================================================================
//* Monitor flag and  write record in APILOG.
//*=============================================================================
/copy ar_mexcer

// Validate input

validationResult = InputIsValid(comp:sbforcint);
if (validationResult.isValid = *off);
  errfield = validationResult.errorField;
  errmsg = validationResult.errorMessage;
  errors = '1';
  *inlr = *on;
  return;
endif;


//*=============================================================================
//* Determine if it is end of period
//*=============================================================================
wkflag = $_isItEndper(comp:sbforcint);

//*=============================================================================
//* Managing On-Error Condition & update APILOG either by error or Normal Proces
//*=============================================================================
/copy ar_oexcer

*inlr = *on;
return;

//-----------------------------------------------------------------
//-------- Build Members in case that any errors
//-----------------------------------------------------------------
begsr $_bld_mbrs;

  wk_bodypgm = %subst(Pgmds.Pgm_Lib:1:3);
  wk_codetyp = 'BBK';
  wk_codeval = 'RELEASE   LEVEL ';
  wkfound = tablcod_getrecord(comp:wk_codetyp:wk_codeval);
  if (wkfound = *on);
    wk_bodyrls = tablcod_getTA_CODEDS1();
  endif;
  wk_bodygrp = %subst(PgmDs.Proc_Name:7:4);

  wk_bodynam = 'COMP: ';
  wk_bodyval = comp;
  AR_BLDTEXT(wk_bodypgm:wk_bodyrls:wk_bodygrp:wk_bodynam:wk_bodyval:er_bodymbr);

  wk_bodynam = 'COMPCOD: ';
  wk_bodyval = compcod;
  AR_BLDTEXT(wk_bodypgm:wk_bodyrls:wk_bodygrp:wk_bodynam:wk_bodyval:er_bodymbr);

  wk_bodynam = 'USER: ';
  wk_bodyval = user;
  AR_BLDTEXT(wk_bodypgm:wk_bodyrls:wk_bodygrp:wk_bodynam:wk_bodyval:er_bodymbr);

  wk_bodynam = 'ERRORS: ';
  wk_bodyval = errors;
  AR_BLDTEXT(wk_bodypgm:wk_bodyrls:wk_bodygrp:wk_bodynam:wk_bodyval:er_bodymbr);

  wk_bodynam = 'ERRMSG: ';
  wk_bodyval = %subst(errmsg:1:90);
  AR_BLDTEXT(wk_bodypgm:wk_bodyrls:wk_bodygrp:wk_bodynam:wk_bodyval:er_bodymbr);

  wk_bodynam = 'ERRFIELD: ';
  wk_bodyval = errfield;
  AR_BLDTEXT(wk_bodypgm:wk_bodyrls:wk_bodygrp:wk_bodynam:wk_bodyval:er_bodymbr);

  //-----------------------------------------------------------------
  //-------- No more parameters - We sent 999 to remove from memory
  //         AR_BLDTEXT.
  //-----------------------------------------------------------------
  wk_bodypgm= '999';
  AR_BLDTEXT(wk_bodypgm:wk_bodyrls:wk_bodygrp:wk_bodynam:wk_bodyval:er_bodymbr);

endsr;

/copy ar_cexcer

//================================================================== //
// Validate input parameters
//================================================================== //
Dcl-Proc InputIsValid;
Dcl-PI InputIsValid likeds(validationResult);
  comp Char(1);
  forcint Zoned(3:0);
End-PI;

  Dcl-S isValid Ind inz(*on);
  Dcl-S isFound Ind inz(*on);

  Dcl-DS validationResult Qualified;
    isValid Ind inz(*on);
    errorField Char(20) inz(*blanks);
    errorMessage Char(100) inz(*blanks);
  End-DS;

  // Validate company value
  isFound = company_getrecord(comp);
  if (isFound = *off);
    validationResult.isValid = *off;
    validationResult.errorField = 'comp';
    validationResult.errorMessage = 'Company value invalid';

    return validationResult;
  endif;

  // Validate submitted forecast interval
   wk_codetyp = 'FSI';
   wk_codeval = *blanks;
   wk_codeval = %editc(forcint:'X');
   isFound = tablcod_getrecord(comp:wk_codetyp:wk_codeval);
   if (isFound = *off);
      validationResult.isValid = *off;
      validationResult.errorField = 'forcint';
      validationResult.errorMessage = 'Forcast interval invalid';
      return validationResult;
  endif;

  return validationResult;

End-Proc;

//================================================================== //
// Determine if it is period end.
//================================================================== //
Dcl-Proc $_IsitEndper;
Dcl-PI $_IsitEndper char(1);
  comp Char(1);
  forcint Zoned(3:0);
End-PI;
Dcl-s system_date      Date(*iso);
Dcl-s seending         Date(*iso);
Dcl-s petoday          Char(1);

dclsycursor(comp);
opnsycursor();

exec sql
  fetch next from sycursor
      into :system_date;

clssycursor();

if SQLState = SQLStateOk;
   petoday = '0';

   dclsecursor(comp:forcint:system_date);
   opnsecursor();

   exec sql
      fetch next
        from secursor
          into :seending;
   if SQLState = SQLStateOk;
      if seending = system_date;
         petoday = '1';
      endif;
   endif;

   clssecursor();

   return petoday;
endif;

End-proc;

Dcl-Proc dclsycursor;
Dcl-PI dclsycursor;
  comp Char(1);
End-PI;

exec sql
   declare sycursor cursor for
      select sy_sysdate
      from k_scheddy
      where sy_exclude = 0 and
            sy_procflg = 0 and
            sy_comp = :comp;
end-proc;

Dcl-Proc opnsycursor;
exec sql
   open sycursor;
end-proc;

Dcl-Proc clssycursor;
exec sql
   close sycursor;
end-proc;

Dcl-Proc opnsecursor;
exec sql
   open secursor;
end-proc;

Dcl-Proc clssecursor;
exec sql
   close secursor;
end-proc;

Dcl-Proc dclsecursor;
Dcl-PI dclsecursor;
  comp Char(1);
  forcinterval Zoned(3:0);
  system_date Date(*iso);
End-PI;

exec sql
   declare secursor cursor
      for
      select se_ending
      from k_schedpe
      where se_comp = :comp and
          se_forcint = :forcinterval;
//            se_ending <= :hsystem_date;
end-proc;



