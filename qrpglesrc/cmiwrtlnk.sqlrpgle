      *****************************************************************
     h copyright('(C) Copyright 1996 - 2000 King III Solutions, Inc.  +
     h Rel 4.26 2000-06-23       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ALWNULL(*USRCTL) OPTION(*NODEBUGIO)
      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2000 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: CMIWRTLNK
      **   Type: ILE RPG Program
      **   Desc: Create K_PRODLNK records for CMI S & D coffee
      **         consolidation project.
      *****************************************************************

       dcl-f i_prodlnk            usage(*input);
       // k_intprod work file
       dcl-f k_locatns            usage(*input);
       // locations file
       dcl-f k_prodlnk            usage(*output);
       // k_intprod file


       dcl-s suplfrom             char(10);
       dcl-s subfrom              char(10);
       dcl-s suplto               char(10);
       dcl-s subto                char(10);
       dcl-s cmsysdate            date(*iso);
       dcl-s default_date         date(*iso) inz(d'0001-01-01');

       dcl-ds product_rec         extname('K_PRODUCT') end-ds;

       //--------------------------------------------------------------
       dcl-c SQLStateOk           const('00000');
       dcl-c RowNotFound          const('02000');
       //==============================================================
       // Main calcs
       //==============================================================
       //
       //--------------------------- Prototype-----------------------------------------
       Dcl-PR CMIWRTLNK;
         comp           Char(1);      // Company
       End-PR;
       //--------------------------- Main Procedure Interface -------------------------
       Dcl-PI CMIWRTLNK;
         comp           Char(1);      // Company
       End-PI;

       exec sql
          set option commit = *none,
              datfmt = *iso,
              closqlcsr = *endactgrp;

       setll *start i_prodlnk;
       read i_prodlnk;
       dow not %eof(i_prodlnk);
       if frmprod <> toprod;

          setll *start k_locatns;
          read k_locatns;
          dow not %eof(k_locatns);
             exec sql
               select *
                  into :product_rec
                  from k_product
                  where pr_prod = :frmprod and
                        pr_locn = :lc_locn and
                        pr_altsrce = 0 and
                        pr_deltcnt = 0
                  fetch first row only;
             if SQLState = SQLStateOk;
                suplfrom = pr_supl;
                subfrom  = pr_suplsub;
                exec sql
                   select *
                      into :product_rec
                      from k_product
                      where pr_prod = :toprod and
                            pr_locn = :lc_locn and
                            pr_altsrce = 0 and
                            pr_deltcnt = 0
                      fetch first row only;
                if SQLState = SQLStateOk;
                   suplto = pr_supl;
                   subto  = pr_suplsub;
                   exsr write_prodlnk;
                endif;
             endif;
             read k_locatns;
          enddo;
        endif;

        read i_prodlnk;

       enddo;

       *inlr = *on;

       begsr write_prodlnk;

       pk_comp    = comp;             //Core-Mark
       pk_type    = 'C';             //Change
       pk_transts = 0;               //0=open, 1=close
       pk_frmlocn = lc_locn;
       pk_frmsupl = suplfrom;
       pk_frmsub  = subfrom;
       pk_frmprod = frmprod;
       pk_frmstat = ' ';
       pk_frmfrez = default_date;    //date field
       pk_frmclr  = 0;
       exec sql
          select cm_sysdate
             into :cmsysdate
             from k_company
             where cm_comp = :comp
             fetch first row only;
       if SQLState = SQLStateOk;
          pk_birth   = cmsysdate;
          pk_lastupd = cmsysdate;
       endif;
       pk_to_locn = lc_locn;
       pk_to_supl = suplto;
       pk_to_sub  = subto;
       pk_to_prod = toprod;
       pk_hstmult = 1.00;
       pk_chgtype = 'A';             //'A'=add, 'R'=replace
       pk_dislink = default_date;    //date field
       pk_copycmp = default_date;    //date field

       write rk_prodlnk;

       endsr;


